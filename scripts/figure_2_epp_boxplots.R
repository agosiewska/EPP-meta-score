library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(OpenML)
library(xtable)

data_path <- "./data/elo_results_6_models_400_params/"
list_files <- list.files(data_path)

elo <- data.frame()
for(file in list_files){
  elo_file <- read_csv(paste0(data_path,file), 
                       col_types = cols(X1 = col_skip()))
  elo_file[["dataset"]] <- paste0("dst_", gsub(".csv", "", gsub("elo_", "", file)))
  
  dst_num <- gsub(".csv", "", gsub("elo_", "", file))
  dst <- getOMLDataSet(as.numeric(dst_num))
  elo_file[["name"]] <- dst$desc$name
  elo <- rbind(elo, elo_file)
}

swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)
elo$name_format <- swr(gsub('-', ' ', elo$name))
elo$model <- gsub("randomForest", "RF", elo$model)



elo %>% 
  select(dataset,name_format, model, epp) %>% 
  unique() %>% 
  mutate(dataset= as.character(dataset)) %>% 
  mutate(model_type =  gsub(pattern = '[_0-9]+', replacement = '', model)) %>% 
  ggplot(aes(x = model_type, y = epp, fill =model_type))+
  geom_boxplot()+
  coord_flip()+
  facet_wrap(~name_format, ncol = 4)+
  labs(fill = "Algorithm")+
  theme_light()+
  theme(legend.position = 'bottom', 
        axis.title.y = element_blank(),
        text = element_text(size = 17),
        strip.text.x = element_text( margin = margin( b = 0.5, t = 0) ))

ggsave('./figures/figure_2_epp_boxplots.pdf', width = 10, height = 13)

