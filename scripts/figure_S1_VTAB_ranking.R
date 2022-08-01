# devtools::install_github("ModelOriented/EloML")

library(EloML)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

leaderboard <- read.csv2("./data/VTAB_leaderboard.csv", sep = ";")
colnames(leaderboard)[1] <- "Rank"
leaderbord <- leaderboard[-c(nrow(leaderboard) - 1, nrow(leaderboard)),]


as_num_char <- function(x)as.numeric(as.character(x))
leaderboard_clean <- data.frame(leaderboard[,1], sapply(leaderboard[3:21], as_num_char))


leaderboard_models <- leaderboard_clean %>%
  pivot_longer(leaderbord, cols = CIFAR.100:sNORB.Elev, names_to = "measure", values_to = "value")
colnames(leaderboard_models) <- c("model", "measure", "value")
leaderboard_models[["model"]] <- as.character(leaderboard_models[["model"]])

elo_vtab <- calculate_epp(leaderboard_models, estimation = "glm", keep_model = TRUE)
elo_vtab[["epp"]][["model"]] <- gsub("`", "", elo_vtab[["epp"]][["model"]])

plot_df <- merge(elo_vtab[["epp"]], leaderboard[-c(20,21),], by.x = "model", by.y ="Rank" ) %>%
  pivot_longer(cols = CIFAR.100:sNORB.Elev, names_to = "measure", values_to = "value") %>%
  mutate(Mean = as_num_char(Mean), value = as_num_char(value))

conf <- summary(elo_vtab$model)$coefficients[,2]
names(conf) <- gsub("`", "", names(conf))

conf_df <- plot_df[,c("epp", "Mean", "model")] %>%
  unique() %>%
  mutate(sd = conf[model])
conf_df[is.na(conf_df$sd), "sd"] <- conf["(Intercept)"]

ggplot(conf_df, aes(x = epp, y = Mean)) + 
  geom_errorbarh(aes(xmin = epp-1.96 *sd, xmax=epp+1.96 *sd)) +
  geom_point(aes(x = epp, y = Mean), size = 4) +
  geom_text_repel(aes(label = model),box.padding = 0.6, max.overlaps = 12, color = "#4AC4B6") +
  ggtitle("Ranking of models from the Visual Task Adaptation Benchmark") +
  theme_bw() +
  ylab("Mean score") +
  xlab("EPP") +
  scale_y_continuous(expand = c(0.3, 0.3))

ggsave("./figures/figure_S1_VTAB_ranking.png", height = 4.5, width = 9)

