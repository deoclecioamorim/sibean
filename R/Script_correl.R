#'############################################################################
#'
#'Prof. Dr. Deoclecio

# Limpar do ambiente ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas

# Function to set seed and RNG for reproducibility ------------------------
set_reproducibility <- function(seed = 1016) {
  set.seed(seed)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
}

# Pacotes -----------------------------------------------------------------
library(boot)
library(readxl)
library(ggcorrplot)
library(writexl)
library(reshape2)
library(tidyverse)

# Correlation leaf gases exchange evaluation-------------------------------------
# Estage R5 --------------------------------------------------------------------


#'
#'###############################
#'
#'Estage R5                     #
#'
#'###############################
# Dataset -------------------------------------------------------------------
estage_r5 <- readxl::read_excel("data/dataset.xlsx", sheet = 1) 

estage_r5 <- estage_r5[,4:10]
head(estage_r5)


# Extrair tratamento (ex: "t1", "t2", ...)
estage_r5_boot <- estage_r5 %>%
  mutate(TratGroup = substr(Trat, 1, 2))

# Variáveis para calcular correlação
vars_corr_r5 <- colnames(estage_r5[,2:7])

# Separar por tratamento
list_trat_r5 <- split(estage_r5_boot, estage_r5_boot$TratGroup)

# Número de simulações
R <- 10000
nvars <- length(vars_corr_r5)

# Inicializar estruturas
corr_array <- array(NA, dim = c(nvars, nvars, R))
sim_datasets <- vector("list", R)          # Lista para armazenar datasets
sim_corr_matrices <- vector("list", R)     # Lista para armazenar matrizes de correlação

set_reproducibility()

for (i in 1:R) {
  # Reamostragem estratificada
  data_boot_r5 <- bind_rows(lapply(list_trat_r5, function(df) {
    df[sample(nrow(df), replace = TRUE), ]
  }))
  
  # Calcular matriz de correlação Spearman
  corr_matrix_r5 <- cor(data_boot_r5[vars_corr_r5], method = "pearson", use = "pairwise.complete.obs")
  
  # Armazenar resultados
  corr_array[, , i] <- corr_matrix_r5
  sim_datasets[[i]] <- data_boot_r5
  sim_corr_matrices[[i]] <- corr_matrix_r5
}

# Extração dos resultados -------------------------------------------------

# Matriz de correlação média
mean_corr_1 <- apply(corr_array, c(1, 2), mean)
class(mean_corr_1)
#colnames(mean_corr_1) <- rownames(mean_corr_1) <- vars_corr_r5
round(mean_corr_1, 4)


# Calculando a matriz de correlação Spearman dados originais
matcor1 <- round(cor(estage_r5[,2:7], method = "pearson", use = "pairwise.complete.obs"), 4)
print(matcor1)


# Intervalos de confiança (IC) bootstrap
ci <- apply(corr_array, c(1, 2), function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))

# Matrizes com limites inferior e superior
ci_lower_matrix <- matrix(ci[1, , ], ncol = nvars, byrow = FALSE)
ci_upper_matrix <- matrix(ci[2, , ], ncol = nvars, byrow = FALSE)

rownames(ci_lower_matrix) <- colnames(ci_lower_matrix) <- vars_corr_r5
rownames(ci_upper_matrix) <- colnames(ci_upper_matrix) <- vars_corr_r5

colnames(mean_corr_1) <- rownames(mean_corr_1) <- vars_corr_r5
round(mean_corr_1, 4)




#Exportar a matrix de correlação
writexl::write_xlsx(as.data.frame(mean_corr_1), "data/mean_corr_r5.xlsx")




# Visualização dos resultados ---------------------------------------------

# Convertendo as matrizes para formato longo para incluir ICs
long_data <- melt(mean_corr_1)
long_data$lower <- melt(ci_lower_matrix)$value
long_data$upper <- melt(ci_upper_matrix)$value


# Adicionando coluna para identificar se o IC contém zero
# Colocando "não significativo - ns" nas correlações que o intervalo de confiança contem
# o zero
long_data$contains_zero <- ifelse(long_data$lower <= 0 & long_data$upper >= 0, "X", "")


# Filtrando apenas a matriz triangular inferior
long_data$keep <- with(long_data, as.numeric(Var1) >= as.numeric(Var2))
triangular_data <- long_data[long_data$keep, ]

levels_unicos <- sort(unique(c(triangular_data$Var1, triangular_data$Var2)))
print(levels_unicos)

# Ordem interna das variáveis (como aparecem na matriz original)
eixo_interno <- c("E", "ci_A", "A", "WUE", "gs", "ci")
labels_visuais <- c("E", "Ci/Ca", "A", "WUE","gs", "Ci")



triangular_data_1 <- triangular_data %>%
  mutate(
    Var1 = factor(Var1, levels = eixo_interno),
    Var2 = factor(Var2, levels = eixo_interno)
  )


ggp1 <- ggplot(triangular_data_1, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
  geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 8) + # "X" centralizado
  #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
  scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0)  +
  scale_x_discrete(breaks = eixo_interno, labels = labels_visuais) +
  scale_y_discrete(breaks = eixo_interno, labels = labels_visuais) +
  labs(x="Leaf gases exchange evaluation",y="Leaf gases exchange evaluation",fill = "Correlation")  +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14, face = "bold")
  )+ ggtitle("A)")
# Exibindo o gráfico
print(ggp1)


# Estage R7 --------------------------------------------------------------------
# Dataset -------------------------------------------------------------------
estage_r7 <- readxl::read_excel("data/dataset.xlsx", sheet = 2) 

estage_r7 <- estage_r7[,4:10]
head(estage_r7)


# Extrair tratamento (ex: "t1", "t2", ...)
estage_r7_boot <- estage_r7 %>%
  mutate(TratGroup = substr(Trat, 1, 2))

# Variáveis para calcular correlação
vars_corr_r7 <- colnames(estage_r7[,2:7])

# Separar por tratamento
list_trat_r7 <- split(estage_r7_boot, estage_r7_boot$TratGroup)

# Número de simulações
R <- 10000
nvars <- length(vars_corr_r7)

# Inicializar estruturas
corr_array <- array(NA, dim = c(nvars, nvars, R))
sim_datasets <- vector("list", R)          # Lista para armazenar datasets
sim_corr_matrices <- vector("list", R)     # Lista para armazenar matrizes de correlação

set_reproducibility()

for (i in 1:R) {
  # Reamostragem estratificada
  data_boot_r7 <- bind_rows(lapply(list_trat_r7, function(df) {
    df[sample(nrow(df), replace = TRUE), ]
  }))
  
  # Calcular matriz de correlação Spearman
  corr_matrix_r7 <- cor(data_boot_r7[vars_corr_r7], method = "pearson", use = "pairwise.complete.obs")
  
  # Armazenar resultados
  corr_array[, , i] <- corr_matrix_r7
  sim_datasets[[i]] <- data_boot_r7
  sim_corr_matrices[[i]] <- corr_matrix_r7
}

# Extração dos resultados -------------------------------------------------

# Matriz de correlação média
mean_corr_2 <- apply(corr_array, c(1, 2), mean)
class(mean_corr_2)
#colnames(mean_corr_1) <- rownames(mean_corr_1) <- vars_corr_r5
round(mean_corr_2, 4)


# Calculando a matriz de correlação Spearman dados originais
matcor2 <- round(cor(estage_r7[,2:7], method = "pearson", use = "pairwise.complete.obs"), 4)
print(matcor2)


# Intervalos de confiança (IC) bootstrap
ci <- apply(corr_array, c(1, 2), function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))

# Matrizes com limites inferior e superior
ci_lower_matrix <- matrix(ci[1, , ], ncol = nvars, byrow = FALSE)
ci_upper_matrix <- matrix(ci[2, , ], ncol = nvars, byrow = FALSE)

rownames(ci_lower_matrix) <- colnames(ci_lower_matrix) <- vars_corr_r7
rownames(ci_upper_matrix) <- colnames(ci_upper_matrix) <- vars_corr_r7

colnames(mean_corr_2) <- rownames(mean_corr_2) <- vars_corr_r7
round(mean_corr_2, 4)




#Exportar a matrix de correlação
writexl::write_xlsx(as.data.frame(mean_corr_2), "data/mean_corr_r7.xlsx")


# Visualização dos resultados ---------------------------------------------

# Convertendo as matrizes para formato longo para incluir ICs
long_data <- melt(mean_corr_2)
long_data$lower <- melt(ci_lower_matrix)$value
long_data$upper <- melt(ci_upper_matrix)$value


# Adicionando coluna para identificar se o IC contém zero
# Colocando "não significativo - ns" nas correlações que o intervalo de confiança contem
# o zero
long_data$contains_zero <- ifelse(long_data$lower <= 0 & long_data$upper >= 0, "X", "")


# Filtrando apenas a matriz triangular inferior
long_data$keep <- with(long_data, as.numeric(Var1) >= as.numeric(Var2))
triangular_data <- long_data[long_data$keep, ]

levels_unicos <- sort(unique(c(triangular_data$Var1, triangular_data$Var2)))
print(levels_unicos)

# Ordem interna das variáveis (como aparecem na matriz original)
eixo_interno <- c("A", "E", "gs", "ci", "ci_A", "WUE")
labels_visuais <- c("A", "E", "gs", "ci", "Ci/Ca", "WUE")


triangular_data_2 <- triangular_data %>%
  mutate(
    Var1 = factor(Var1, levels = eixo_interno),
    Var2 = factor(Var2, levels = eixo_interno)
  )


ggp2 <- ggplot(triangular_data_2, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
  geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 8) + # "X" centralizado
  #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
  scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0)  +
  scale_x_discrete(breaks = eixo_interno, labels = labels_visuais) +
  scale_y_discrete(breaks = eixo_interno, labels = labels_visuais) +
  labs(x="Leaf gases exchange evaluation",y="Leaf gases exchange evaluation",fill = "Correlation")  +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14, face = "bold")
  )+ ggtitle("B)")
# Exibindo o gráfico
print(ggp2)



#Disposição em coluna
png(filename="figures/correl_disp1.png", # Nome do arquivo e extensão
    width = 12,    # largura
    height = 5,   # Altura
    res= 500,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(ggp1, ggp2, nrow=1)
dev.off() # Fecha a janela gráfica
