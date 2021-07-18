library(ggplot2)
library(dplyr)
library(tidyr)

leaderboard <- read.csv2("data/VTAB_leaderboard.csv", sep = ";")
colnames(leaderboard)[1] <- "Rank"
leaderbord <- leaderboard[-c(nrow(leaderboard) - 1, nrow(leaderboard)),]


as_num_char <- function(x)as.numeric(as.character(x))
leaderboard_clean <- data.frame(leaderboard[,1], sapply(leaderboard[3:21], as_num_char))

leaderboard_models <- leaderboard_clean %>%
  pivot_longer(leaderbord, cols = CIFAR.100:sNORB.Elev, names_to = "measure", values_to = "value")
colnames(leaderboard_models) <- c("model", "measure", "value")
leaderboard_models[["model"]] <- as.character(leaderboard_models[["model"]])

leaderboard_models <- leaderboard_models %>% 
   filter(model %in% c("Uncond-BigGAN", "VAE", "Sup-Rotation-100%", "Sup-Exemplar-100%"))

data_agr <- leaderboard_models %>% group_by(model) %>% summarise(mauc = mean(value))

leaderboard_models$model <- reorder(leaderboard_models$model, leaderboard_models$value, mean)

i_same <- 17
i_diff <- 4
selected_measure_same <- unique(leaderboard_models$measure)[i_same]
selected_measure_diff <- unique(leaderboard_models$measure)[i_diff]

leaderboard_models$measure_color <- factor(ifelse(
  leaderboard_models$measure == selected_measure_same, 1, ifelse(
    leaderboard_models$measure == selected_measure_diff, 2, 0)))

leaderboard_models %>%
ggplot(aes(model, value)) +
  geom_boxplot(coef = 0, color = "white", fill = "lightgrey") +
  geom_point(aes(fill = measure_color, color = measure_color), alpha = 0.5, size = 2) +
  coord_flip() +
  geom_line(aes(group = measure, color = measure_color), alpha = 0.5, size = 1)+
  theme_classic() + xlab("") +
  geom_point(data = data_agr, aes(y = mauc), size = 8, color = "red3", position = position_nudge(-0.1), shape = '*', stroke = 5) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "none") +
  ggtitle("Top-1 accuracy for selected models from the VTAB") +
  ylab("Score") +
  scale_color_manual(values = c("grey", "#098208", "blue"))

ggsave('./figures/figure_3_VTAB_boxplots.pdf', width = 8.4, height = 3.5)

