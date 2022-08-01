library(EloML)
library(dplyr)
library(tidyr)
library(ggplot2)
invlogit <- function(b1, b2){
  exp(b1-b2)/(1+exp((b1-b2)))
}

compare_mse <- function(auc_results, epp_results){
  # browser()
  mean_auc_df <- auc_results %>% 
    group_by(player) %>% 
    summarise(mean_auc = mean(auc)) 
  
  # browser()
  if('elo' %in% names(epp_results)) {
    epp_id <- 'elo'
  }  else {epp_id <- 'epp'}
  
  results <- epp_results[[epp_id]] %>% 
    tidyr::expand(tidyr::nesting(player,epp), player, .name_repair = 'universal') %>% 
    rename(winner_model = `player...1`,
           loser_model = `player...3`,
           epp_winner = epp) %>% 
    left_join(epp_results[[epp_id]], by = c('loser_model' = 'player')) %>% 
    rename(epp_loser = epp) %>% 
    mutate(prob = invlogit(epp_winner, epp_loser),
           players = paste(winner_model, loser_model)) %>% 
    filter(winner_model != loser_model) %>% 
    left_join(epp_results$actual_score, by = 'players') %>% 
    mutate(emp_prob = wins/match,
           winner_model = as.character(winner_model),
           loser_model = as.character(loser_model)) %>% 
    left_join(mean_auc_df, by = c('winner_model' = 'player')) %>% 
    rename(winner_mean_auc = mean_auc) %>% 
    left_join(mean_auc_df, by = c('loser_model' = 'player')) %>% 
    rename(loser_mean_auc = mean_auc) %>% 
    mutate(mean_prob = ifelse(winner_mean_auc >loser_mean_auc, 1, 0))
  
  
  results
}

compare_mse_openml <- function(auc_results, epp_results){
  epp_result$elo <- epp_results$elo %>% 
    rename(player = model)
  
  compare_mse(auc_results, epp_results)
}

compare_mse <- function(auc_results, epp_results){
  # browser()
  mean_auc_df <- auc_results %>% 
    group_by(player) %>% 
    summarise(mean_auc = mean(auc)) 
  
  # browser()
  if('elo' %in% names(epp_results)) {
    epp_id <- 'elo'
  }  else {epp_id <- 'epp'}
  
  results <- epp_results[[epp_id]] %>% 
    tidyr::expand(tidyr::nesting(player,epp), player, .name_repair = 'universal') %>% 
    rename(winner_model = `player...1`,
           loser_model = `player...3`,
           epp_winner = epp) %>% 
    left_join(epp_results[[epp_id]], by = c('loser_model' = 'player')) %>% 
    rename(epp_loser = epp) %>% 
    mutate(prob = invlogit(epp_winner, epp_loser),
           players = paste(winner_model, loser_model)) %>% 
    filter(winner_model != loser_model) %>% 
    left_join(epp_results$actual_score, by = 'players') %>% 
    mutate(emp_prob = wins/match,
           winner_model = as.character(winner_model),
           loser_model = as.character(loser_model)) %>% 
    left_join(mean_auc_df, by = c('winner_model' = 'player')) %>% 
    rename(winner_mean_auc = mean_auc) %>% 
    left_join(mean_auc_df, by = c('loser_model' = 'player')) %>% 
    rename(loser_mean_auc = mean_auc) %>% 
    mutate(mean_prob = ifelse(winner_mean_auc >loser_mean_auc, 1, 0))
  
  
  results
}





setup_simulate <- expand.grid(nplayers = c(10, 50,100, 200, 500),
                              nrounds = c(3, 5, 10, 15, 20, 30, 40, 50),
                              seed = c(123, 542, 981, 6, 78, 1231, 5421, 9811, 61, 781))
setup_simulate_ls <- split(setup_simulate, 1:nrow(setup_simulate))
results_mse <- list()
for(setup in seq.int(setup_simulate_ls)[]){
  # browser()
  print(paste0('Start of experiment: ', setup, '/', length(setup_simulate_ls), ', nplayers: ', setup_simulate_ls[[setup]]$nplayers,
               ', nrounds: ', setup_simulate_ls[[setup]]$nrounds))
  simulated_data <- do.call(simulate_exp, as.list(setup_simulate_ls[[setup]]))
  x_epp <- calculate_epp(simulated_data)
  result_tmp <- compare_mse(auc_results = simulated_data %>% rename(auc = measure), 
                            epp_results = x_epp)
  
  results_mse[[setup]] <- result_tmp %>% 
    summarise(mse_epp = mean((emp_prob - prob)^2),
              mse_mean = mean((emp_prob - mean_prob)^2))
  

}
saveRDS(results_mse, 'data/results_mse/results_mse_simulation_ALL.Rd')


# ADD regularization



setup_simulate <- expand.grid(nplayers = c(10, 50,100, 200, 500),
                              nrounds = c(3, 5, 10, 15, 20, 30, 40, 50),
                              seed = c(123, 542, 981, 6, 78, 1231, 5421, 9811, 61, 781))
setup_simulate_ls <- split(setup_simulate, 1:nrow(setup_simulate))
results_mse_reg <- list()
for(setup in seq.int(setup_simulate_ls)[]){
  # browser()
  print(paste0('Start of experiment: ', setup, '/', length(setup_simulate_ls), ', nplayers: ', setup_simulate_ls[[setup]]$nplayers,
               ', nrounds: ', setup_simulate_ls[[setup]]$nrounds))
  simulated_data <- do.call(simulate_exp, as.list(setup_simulate_ls[[setup]]))
  x_epp <- calculate_epp(simulated_data, 
                         estimation = 'glmnet',
                         add_regularization = TRUE)
  result_tmp <- compare_mse(auc_results = simulated_data %>% rename(auc = measure), 
                            epp_results = x_epp)
  
  results_mse_reg[[setup]] <- result_tmp %>% 
    summarise(mse_epp = mean((emp_prob - prob)^2),
              mse_mean = mean((emp_prob - mean_prob)^2))
  

}
saveRDS(results_mse_reg, 'data/results_mse/results_mse_regularization_simulation_ALL.Rd')

##### PLOT

setup_simulate <- expand.grid(nplayers = c(10, 50,100, 200, 500),
                              nrounds = c(3, 5, 10, 15, 20, 30, 40, 50),
                              seed = c(123, 542, 981, 6, 78, 1231, 5421, 9811, 61, 781))
results_mse <- do.call(rbind,readRDS('data/results_mse/results_mse_simulation_ALL.Rd'))
results_mse_reg <- do.call(rbind,readRDS('data/results_mse/results_mse_regularization_simulation_ALL.Rd'))
colnames(results_mse_reg) <- c('mse_epp_reg', 'mse_mean_v2')

all_results <- cbind(setup_simulate, results_mse, results_mse_reg) 

all_results%>% 
  filter(mse_mean != mse_mean_v2)

all_results <- all_results %>% 
  select(-mse_mean_v2)

all_results %>% 
  tidyr::pivot_longer(cols = mse_epp:mse_epp_reg, names_to = 'type', values_to = 'MSE') %>% 
  mutate(nplayers = factor(paste0('nplayers = ', nplayers), levels = paste0('nplayers = ', sort(unique(nplayers)))),
         nrounds = factor(paste0('nrounds = ', nrounds), levels = paste0('nrounds = ', sort(unique(nrounds)) )))%>% 
  ggplot(aes(x = type, y = MSE))+
  geom_boxplot()+
  facet_grid(nrounds~nplayers)+
  theme_light()+
  scale_x_discrete(labels = c('EPP', 'EPP+Ridge', 'MEAN'), name = NULL) +
  theme_light()


ggsave('figures/figure_S2_mse.pdf', width =  10, height = 10)
