library(EloML)
library(OpenML)
library(ggplot2)
library(dplyr)
library(scales)

data_path <- "./data/elo_results_6_models_400_params/"
list_files <- list.files(data_path)

dst_num <- gsub(pattern = '[^0-9]', replacement = '', x = list_files)

## get OpenML names
dst <- sapply(dst_num, function(x) getOMLDataSet(as.numeric(x))$desc$name)

p_model <- readRDS("./data/epp_openml_glm_models.Rd")
names(p_model) <- list.files(pattern = '^elo_[0-9].+Rd$')

### check that every dataset has 6 algorithms - not at index 31 - we have to remove that
which(sapply(lapply(p_model, function(x) unique(gsub(pattern = '_[0-9]+', replacement = '', rownames(coefficients(x))))), length) != 6)

dst <- dst[-31]
deviance_epp <- sapply(p_model[-31], deviance)


m <- length(coefficients(p_model[[1]])[-1])
degrees_freedom <- m*(m-2)

## we do one-side test for deviance
pvalue_chisquare <- sapply(deviance_epp, function(x)  pchisq(x, df = degrees_freedom, lower.tail = FALSE))
pvalue_norm <- pnorm((deviance_epp - degrees_freedom)/(sqrt(2*degrees_freedom)), lower.tail = FALSE)

stand_deviance <- (deviance_epp - degrees_freedom)/sqrt(2*degrees_freedom)

hist(stand_deviance)


idx_best <- which.min(deviance_epp)
name_best <- dst[idx_best]
idx_worst <- which.max(deviance_epp)
name_worst <- dst[idx_worst]
selected_datasets <- c(names(idx_best), names(idx_worst))

selected_files <- lapply(selected_datasets, readRDS)

EloML::plot_wins_ratio()

### draw plot_wins_ratio for random sample 
sample_random_epp <- function(epp, seed = 123, name){
  set.seed(seed)
  sample_model <- sample(epp$elo$model, size = floor(0.3 * length(epp$elo$model)))
  
  actual_score <- epp$actual_score[epp$actual_score$winner %in% sample_model & epp$actual_score$loser %in% sample_model,] 
  epp_score <- epp$elo[epp$elo$model %in% sample_model, ]
  actual_score[["ratio"]] <- actual_score[["wins"]] / actual_score[["match"]]
  actual_score <- merge(actual_score, epp_score, by.x ="winner", by.y = "model")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_winner"
  actual_score <- merge(actual_score, epp_score, by.x ="loser", by.y = "model")
  names(actual_score)[names(actual_score)=='epp'] <- "epp_loser"
  actual_score[['pred_ratio']] <- exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']])/(1+exp(actual_score[["epp_winner"]] - actual_score[['epp_loser']]))
  actual_score[['algorithm']] <- gsub(pattern = '_[0-9]+', replacement = '', actual_score[['winner']])
  
  ## rename dataset
  actual_score[['dataset']] <- name
  
  return(actual_score)
}


# for two datasets


actual_score_worst <- sample_random_epp(epp = selected_files[[2]], name = name_worst)
actual_score_best <- sample_random_epp(epp = selected_files[[2]], name = name_best)


## rbind results for these datasets

actual_score_bind <- rbind(actual_score_worst, actual_score_best)
actual_score_bind[['algorithm']] <- gsub('randomForest', 'RF', actual_score_bind[['algorithm']])

p_best <- ggplot(actual_score_bind, aes(x=ratio, y = pred_ratio, color  = algorithm))+
  geom_point(alpha = 0.2)+
  geom_abline(slope=1)+
  facet_grid(~dataset)+
  theme_light()+
  theme(legend.position = 'bottom')+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  labs(x = 'Actual empirical probability of winning', y = 'Predicted probability of winning')+
  scale_color_discrete(name='Algorithm')
ggsave(p_best, filename = 'figure_3_deviance_best_worst.pdf', device =  'pdf')
ggsave(p_best, filename = 'figure_3_deviance_best_worst.png', device =  'png', width = 8)
