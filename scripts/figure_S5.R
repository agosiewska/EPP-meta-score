library(ggplot2)
library(EloML)
library(patchwork)
library(dplyr)
library(stringr)
library(tidyr)

### Simulations setup


no_reps <- 100
set.seed(1)
seeds <- sample(1:10^8, no_reps)
no_models_list <- c(10, 50, 100, 200, 500)
no_splits_list <- c(3, 5, 10, 15, 20, 30, 40, 50)
auc_mean_mean_list <- c(0.8)
auc_mean_variance_list <- c(0.08)
auc_variance_mean_list <- c(0.05)
auc_variance_variance_list <- c(0.005)

experiment_grid <- expand.grid(no_models_list, no_splits_list, auc_mean_mean_list, auc_mean_variance_list, auc_variance_mean_list, auc_variance_variance_list)
colnames(experiment_grid) <- c("no_models", "no_splits", "auc_mean_mean", "auc_mean_variance", "auc_variance_mean", "auc_variance_variance")

### Functions

generate_models_results <- function(models_mean_auc, models_variance_auc, no_splits){
  no_models <- length(models_mean_auc)
  models_results <- data.frame()
  for (ith_split in 1:no_splits){
    models_auc <- c()
    for (i in 1:no_models){
      models_auc <- c(models_auc, rnorm(1, mean = models_mean_auc[i], models_variance_auc[i])) 
    }
    
    models_results <- rbind(models_results, 
                            data.frame(model=paste("m",1:no_models, sep="_"),
                                       split=ith_split,
                                       auc=models_auc))
    
  }
  models_results
  
  
}

add_new_model_to_results <- function(new_model_auc, new_model_name){
  models_results_new <- rbind(models_results, data.frame(model=new_model_name, 
                                                         split=1:no_splits,
                                                         auc=new_model_auc))
  models_results_new
}

### Simulation

all_results <- list()

for (ith_setup in 1:nrow(experiment_grid)){
  print(experiment_grid[ith_setup, ])
  all_results <- list()
  no_models <- experiment_grid[ith_setup, "no_models"]
  no_splits <- experiment_grid[ith_setup, "no_splits"]
  auc_mean_mean <- experiment_grid[ith_setup, "auc_mean_mean"]
  auc_mean_variance <- experiment_grid[ith_setup, "auc_mean_variance"]
  auc_variance_mean <- experiment_grid[ith_setup, "auc_variance_mean"]
  auc_variance_variance <- experiment_grid[ith_setup, "auc_variance_variance"]
  
  filename = paste0("./data/data_new_player/all_results", "_models_", no_models, "_splits_", no_splits, ".rda")
  if (!file.exists(filename)){
    for (rep in 1:no_reps) {
      print(rep)
      set.seed(seeds[rep])
      models_mean_auc <- rnorm(n = no_models, mean = auc_mean_mean, sd = auc_mean_variance)
      models_variance_auc <- rnorm(n = no_models, mean = auc_variance_mean, sd = auc_variance_variance)
      
      models_results <- generate_models_results(models_mean_auc, models_variance_auc, no_splits)
      epp <- calculate_epp(models_results, estimation ="glmnet", keep_data = TRUE, keep_model=TRUE)
      models_results_new <- add_new_model_to_results(new_model_auc=0.75, new_model_name="m_new")
      epp_new <- calculate_epp(models_results_new, estimation ="glmnet", keep_data = TRUE, keep_model=TRUE)
      
      joined_epp <- rbind(data.frame(epp$epp, ranking="old"), data.frame(epp_new$epp, ranking="new"))
      joined_epp[["ranking"]] <- factor(joined_epp[["ranking"]], levels = c("old", "new"))
      
      rank_1 <- epp$epp %>% arrange(desc(epp)) %>% pull(epp)
      rank_2 <- epp_new$epp %>% arrange(desc(epp)) %>% pull(epp)
      names(rank_1) <- epp$epp %>% arrange(desc(epp)) %>% pull(player)
      names(rank_2) <- epp_new$epp %>% arrange(desc(epp)) %>% pull(player)
      
      new_model_position <- which(names(rank_2) == "m_new")
      
      rank_1_numbers <- as.numeric(substring(names(rank_1), 3))
      rank_2_numbers <- as.numeric(substring(names(rank_2)[-new_model_position], 3))
      
      kendall_tau <- cor.test(rank_1_numbers, rank_2_numbers, method="kendall", alternative = "two.sided", exact=FALSE )
      
      all_results[[length(all_results)+1]] <-  list("no_models" = no_models,
                                                    "no_splits" = no_splits,
                                                    "auc_mean_mean" = auc_mean_mean,
                                                    "auc_mean_variance" = auc_mean_variance,
                                                    "auc_variance_mean" = auc_variance_mean,
                                                    "auc_variance_variance" = auc_variance_variance,
                                                    "models_results" = models_results,
                                                    "models_results_new" = models_results_new,
                                                    "rank_1" = rank_1,
                                                    "rank_2" = rank_2,
                                                    "kendall_tau" = kendall_tau$estimate)
      
    }
    
    print("#####")
    print("#####")
    save(all_results, file = filename)
  }
     
  
}


# Loading data

files_names <- list.files("./data/data_new_player/")
all_all_results <- c()
tkendalls_by_filename <- list()
for (file_name in files_names){
  load(paste0("./data/data_new_player/", file_name))  
  tkendalls_by_filename[[file_name]] <- c()
  for (elem in 1:length(all_results)){
    tkendalls_by_filename[[file_name]] <- c(tkendalls_by_filename[[file_name]], all_results[[elem]][["kendall_tau"]])
  }
  all_all_results <- c(all_all_results, all_results)
  
}


diff_results <- lapply(all_all_results, function(x){
  y <- exp(lag(x$rank_1) - x$rank_1 )
  
  diff_p1 <- y/(1+y)
  
  
  tmp <- x$rank_2[names(x$rank_1)]
  
  y_2 <- exp(lag(tmp) - tmp)
  diff_p2 <-y_2/(1+y_2)
  
  list(no_models = x$no_models,
       no_splits = x$no_splits,
       diff_p1 = diff_p1,
       diff_p2 = diff_p2)
  
})

data.table::rbindlist(lapply(diff_results, function(x) {
  data.frame(no_models = x$no_models,
             no_splits = x$no_splits,
             diff_p = mean(x$diff_p1 - x$diff_p2, na.rm = TRUE))
})) %>% 
  ggplot(aes(x = as.factor(no_models), y = diff_p))+
  geom_boxplot()+
  facet_wrap(~no_splits, nrow = 2) +
  theme_light() +
  xlab("Number of players") +
  ylab("Difference between probability of winning")


ggsave('./figures/figure_S5_new_player_scenario.pdf', width = 9, height = 5)

