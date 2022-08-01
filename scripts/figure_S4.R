library(MASS)
library(EloML)
library(dplyr)
library(ggplot2)

no_reps <- 10
set.seed(1)
seeds <- sample(1:10^8, no_reps)
no_models_list <- c(10, 50, 100, 200, 500)
no_splits_list <- c(3, 5, 10, 15, 20, 30, 40, 50)
max_no_splits <- max(no_splits_list)




simulate_auc <- function(no_splits, mean_auc=0.8){
  corr <- matrix(1, no_splits, no_splits) * 0.0001
  sigma <- diag(x=0.0001, nrow=no_splits, ncol=no_splits)
  sigma <- corr + sigma
  mvrnorm(n = 1, mu=rep(mean_auc, no_splits), Sigma=sigma)
}


sim_results <- lapply(1L:no_reps, function(ith_rep) {
    set.seed(seeds[ith_rep])
    lapply(no_models_list, function(ith_model_n) {
      mean_auc_v <- runif(ith_model_n, 0.5, 0.9)
      model_bagging_auc_list <- lapply(mean_auc_v, function(ith_mean_auc) 
        simulate_auc(max_no_splits, mean_auc = ith_mean_auc)
      )
      
      lapply(1L:length(model_bagging_auc_list), function(ith_model) {
        lapply(no_splits_list, function(ith_split_no) {
          splits_ids <- 1L:ith_split_no
          data.frame(split_id = splits_ids,
                     auc = model_bagging_auc_list[[ith_model]][splits_ids]) %>% 
            mutate(model_id = ith_model,
                   max_split = ith_split_no,
                   n_model = ith_model_n,
                   rep = ith_rep)
        }) %>% bind_rows()
      }) %>% bind_rows()
    }) %>% bind_rows()
  }) %>% bind_rows()
  

epp_simulations <- lapply(unique(sim_results[["n_model"]]), function(ith_n_model)
    lapply(unique(sim_results[["max_split"]]), function(ith_max_split)
      lapply(unique(sim_results[["rep"]]), function(ith_rep)
        filter(sim_results, n_model == ith_n_model, max_split == ith_max_split, rep == ith_rep) %>% 
          select(model_id, split_id, auc) %>% 
          calculate_epp() %>% 
          getElement("epp") %>% 
          mutate(rep = ith_rep,
                 max_split = ith_max_split,
                 n_model = ith_n_model)
      ) %>% bind_rows
    ) %>% bind_rows
  ) %>% bind_rows  

save(epp_simulations, file = "./data/elo_stability.Rd")
load("./data/elo_stability.Rd")

epp_simulations_with_differences <- epp_simulations %>%
  group_by(player, rep, n_model) %>%
  mutate(epp_difference = epp - lag(epp)) %>%
  ungroup()  %>%
  group_by(rep, max_split, n_model) %>%
  summarise(mean_epp_difference = mean(epp_difference, na.rm=TRUE))

ggplot(epp_simulations_with_differences, aes(x = max_split, y = mean_epp_difference, group=rep)) +
  geom_point() +
  geom_line() +
  facet_wrap(~n_model) +
  theme_light() +
  xlab("Number of rounds") +
  ylab("Mean epp difference relative to the \n previous number of rounds") + 
  theme_light()
ggsave('./figures/figure_S4_stability.pdf', width = 9, height = 5)



ggplot(epp_simulations, aes(x = factor(n_model), y = epp)) +
  geom_boxplot() +
  facet_wrap(~max_split) +
  theme_light() +
  xlab("Number of players") +
  ylab("EPP")
ggsave('./figures/figure_S4_inflation.pdf', width = 9, height = 5)
