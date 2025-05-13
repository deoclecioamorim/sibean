# -------------------------------------------------------------------------
# Author: Prof. Dr. Deoclecio
# Description: Bootstrap correlation analysis for leaf gas exchange data
# -------------------------------------------------------------------------

# Clear environment -------------------------------------------------------
rm(list = ls())
gc(reset = TRUE)
graphics.off()

# Set seed and RNG kind for reproducibility -------------------------------
set_reproducibility <- function(seed = 1016) {
  set.seed(seed)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
}

# Load required packages --------------------------------------------------
library(boot)
library(readxl)
library(ggcorrplot)
library(writexl)
library(reshape2)
library(tidyverse)

# ======================== Stage R5 =======================================

# Load dataset for stage R5 -----------------------------------------------
data_r5 <- read_excel("data/dataset.xlsx", sheet = 1)[, 4:10]

# Extract treatment group -------------------------------------------------
data_r5 <- data_r5 %>% mutate(TratGroup = substr(Trat, 1, 2))

# Select variables for correlation analysis -------------------------------
vars_r5 <- colnames(data_r5[, 2:7])
groups_r5 <- split(data_r5, data_r5$TratGroup)

# Bootstrap settings ------------------------------------------------------
R <- 10000
nvars <- length(vars_r5)
cor_array_r5 <- array(NA, dim = c(nvars, nvars, R))
sim_corr_r5 <- vector("list", R)

set_reproducibility()

# Bootstrap loop ----------------------------------------------------------
for (i in 1:R) {
  sampled <- bind_rows(lapply(groups_r5, function(df) {
    df[sample(nrow(df), replace = TRUE), ]
  }))
  corr <- cor(sampled[vars_r5], method = "pearson", use = "pairwise.complete.obs")
  cor_array_r5[, , i] <- corr
  sim_corr_r5[[i]] <- corr
}

# Mean correlation matrix -------------------------------------------------
mean_corr_r5 <- apply(cor_array_r5, c(1, 2), mean)
colnames(mean_corr_r5) <- rownames(mean_corr_r5) <- vars_r5
mean_corr_r5

# Confidence intervals ----------------------------------------------------
ci_r5 <- apply(cor_array_r5, c(1, 2), quantile, probs = c(0.025, 0.975), na.rm = TRUE)
ci_lower_r5 <- matrix(ci_r5[1, , ], ncol = nvars)
ci_upper_r5 <- matrix(ci_r5[2, , ], ncol = nvars)
colnames(ci_lower_r5) <- rownames(ci_lower_r5) <- vars_r5
colnames(ci_upper_r5) <- rownames(ci_upper_r5) <- vars_r5

# Export correlation matrix -----------------------------------------------
write_xlsx(as.data.frame(mean_corr_r5), "data/mean_corr_r5.xlsx")

# Prepare long-format data for plotting -----------------------------------
plot_data_r5 <- melt(mean_corr_r5)
plot_data_r5$lower <- melt(ci_lower_r5)$value
plot_data_r5$upper <- melt(ci_upper_r5)$value
plot_data_r5$contains_zero <- ifelse(plot_data_r5$lower <= 0 & plot_data_r5$upper >= 0, "X", "")
plot_data_r5$keep <- with(plot_data_r5, as.numeric(Var1) >= as.numeric(Var2))
plot_data_r5 <- plot_data_r5[plot_data_r5$keep, ]

# Plot customization ------------------------------------------------------
eixo_r5 <- c("E", "ci_A", "A", "WUE", "gs", "ci")
labels_r5 <- c("E", "Ci/Ca", "A", "WUE", "gs", "Ci")

plot_data_r5 <- plot_data_r5 %>% mutate(
  Var1 = factor(Var1, levels = eixo_r5),
  Var2 = factor(Var2, levels = eixo_r5)
)

plot_r5 <- ggplot(plot_data_r5, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 8) +
  scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
  scale_x_discrete(breaks = eixo_r5, labels = labels_r5) +
  scale_y_discrete(breaks = eixo_r5, labels = labels_r5) +
  labs(x = "Leaf gas exchange", y = "Leaf gas exchange", fill = "Correlation") +
  ggtitle("A)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 14, face = "bold"))

plot_r5

# ======================== Stage R7 =======================================

# Load dataset for stage R7 -----------------------------------------------
data_r7 <- read_excel("data/dataset.xlsx", sheet = 2)[, 4:10]

# Extract treatment group -------------------------------------------------
data_r7 <- data_r7 %>% mutate(TratGroup = substr(Trat, 1, 2))

# Select variables for correlation analysis -------------------------------
vars_r7 <- colnames(data_r7[, 2:7])
groups_r7 <- split(data_r7, data_r7$TratGroup)

# Bootstrap settings ------------------------------------------------------
cor_array_r7 <- array(NA, dim = c(length(vars_r7), length(vars_r7), R))
sim_corr_r7 <- vector("list", R)

set_reproducibility()

# Bootstrap loop ----------------------------------------------------------
for (i in 1:R) {
  sampled <- bind_rows(lapply(groups_r7, function(df) {
    df[sample(nrow(df), replace = TRUE), ]
  }))
  corr <- cor(sampled[vars_r7], method = "pearson", use = "pairwise.complete.obs")
  cor_array_r7[, , i] <- corr
  sim_corr_r7[[i]] <- corr
}

# Mean correlation matrix -------------------------------------------------
mean_corr_r7 <- apply(cor_array_r7, c(1, 2), mean)
colnames(mean_corr_r7) <- rownames(mean_corr_r7) <- vars_r7
mean_corr_r7

# Confidence intervals ----------------------------------------------------
ci_r7 <- apply(cor_array_r7, c(1, 2), quantile, probs = c(0.025, 0.975), na.rm = TRUE)
ci_lower_r7 <- matrix(ci_r7[1, , ], ncol = nvars)
ci_upper_r7 <- matrix(ci_r7[2, , ], ncol = nvars)
colnames(ci_lower_r7) <- rownames(ci_lower_r7) <- vars_r7
colnames(ci_upper_r7) <- rownames(ci_upper_r7) <- vars_r7

# Export correlation matrix -----------------------------------------------
write_xlsx(as.data.frame(mean_corr_r7), "data/mean_corr_r7.xlsx")

# Prepare long-format data for plotting -----------------------------------
plot_data_r7 <- melt(mean_corr_r7)
plot_data_r7$lower <- melt(ci_lower_r7)$value
plot_data_r7$upper <- melt(ci_upper_r7)$value
plot_data_r7$contains_zero <- ifelse(plot_data_r7$lower <= 0 & plot_data_r7$upper >= 0, "X", "")
plot_data_r7$keep <- with(plot_data_r7, as.numeric(Var1) >= as.numeric(Var2))
plot_data_r7 <- plot_data_r7[plot_data_r7$keep, ]

# Plot customization ------------------------------------------------------
eixo_r7 <- c("A", "E", "gs", "ci", "ci_A", "WUE")
labels_r7 <- c("A", "E", "gs", "ci", "Ci/Ca", "WUE")

plot_data_r7 <- plot_data_r7 %>% mutate(
  Var1 = factor(Var1, levels = eixo_r7),
  Var2 = factor(Var2, levels = eixo_r7)
)

plot_r7 <- ggplot(plot_data_r7, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 8) +
  scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
  scale_x_discrete(breaks = eixo_r7, labels = labels_r7) +
  scale_y_discrete(breaks = eixo_r7, labels = labels_r7) +
  labs(x = "Leaf gas exchange", y = "Leaf gas exchange", fill = "Correlation") +
  ggtitle("B)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 14, face = "bold"))

plot_r7

# Export combined plot ----------------------------------------------------

#Disposição em coluna
png(filename="figures/correl_disp1.png", # Nome do arquivo e extensão
    width = 12,    # largura
    height = 5,   # Altura
    res= 500,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(plot_r5, plot_r7, nrow=1)
dev.off() # Fecha a janela gráfica
