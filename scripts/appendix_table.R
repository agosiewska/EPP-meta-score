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
names(p_model) <- list.files(data_path, pattern = '^elo_[0-9].+csv$')

### check that every dataset has 6 algorithms - not at index 31 - we have to remove that
which(sapply(lapply(p_model, function(x) unique(gsub(pattern = '_[0-9]+', replacement = '', rownames(coefficients(x))))), length) != 6)

dst <- dst[-31]
deviance_epp <- sapply(p_model[-31], glmnet::deviance)


m <- length(coefficients(p_model[[1]])[-1])
degrees_freedom <- m*(m-2)

## we do one-side test for deviance
pvalue_chisquare <- sapply(deviance_epp, function(x)  pchisq(x, df = degrees_freedom, lower.tail = FALSE))
pvalue_norm <- pnorm((deviance_epp - degrees_freedom)/(sqrt(2*degrees_freedom)), lower.tail = FALSE)


data.frame(id = dst_num[-31],
           dataset = dst,
           deviance = round(deviance_epp, digits = 0),
           stand_deviance = round(stand_deviance, digits = 1),
           # df = format(degrees_freedom,  digits = 2),
           # pvalue_chisquare = format.pval(pvalue_chisquare),
           pvalue = format.pval(pvalue_norm)
           ) %>% 
  arrange(dataset) %>% 
  xtable(digits = c(0,0, 0,1, 0,0)) %>% 
  print(include.rownames = FALSE)
