#'############################################################################
#'
#'Prof. Dr. Deoclecio

# Limpar do ambiente ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas


# Pacotes -----------------------------------------------------------------
library(boot)
library(readxl)
library(ggcorrplot)
library(writexl)
library(reshape2)


# Correlação entre Doenças ------------------------------------------------

#'
#'###############################
#'
#'Correlação entre as doenças   #
#'
#'###############################
# Dados -------------------------------------------------------------------
dataset <- readxl::read_excel("dados/dados_paper.xlsx", sheet = 5) #Planilha completa

diases <- dataset[,5:22]


# Matriz de correlação inicial --------------------------------------------
# Calculando a matriz de correlação Spearman
matcor1 <- round(cor(diases, method = "spearman", use = "pairwise.complete.obs"), 2)
print(matcor1)


# Simulação bootstrap -----------------------------------------------------

# Função para calcular a matriz de correlação com bootstrap
bootstrap_corr <- function(data, indices) {
  sampled_data <- data[indices, ] # Reamostragem dos dados
  cor(sampled_data, method = "spearman") # Calcula correlação Spearman
}

# Configurando reprodutibilidade e executando o bootstrap
set.seed(123) # Para reprodutibilidade
results <- boot(data = diases, statistic = bootstrap_corr, R = 10000)


# Extração dos resultados -------------------------------------------------

# Calculando a média das correlações para todas as reamostragens
mean_corr_1 <- apply(results$t, 2, mean)



# Calculando intervalos de confiança (IC) para as correlações
ci <- apply(results$t, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

# Convertendo resultados em matrizes
mean_corr_1_matrix <- matrix(mean_corr_1, ncol = ncol(diases), byrow = TRUE)
ci_lower_matrix <- matrix(ci[1, ], ncol = ncol(diases), byrow = TRUE)
ci_upper_matrix <- matrix(ci[2, ], ncol = ncol(diases), byrow = TRUE)

# Nomeando as matrizes para facilitar a interpretação
rownames(mean_corr_1_matrix) <- colnames(diases)
colnames(mean_corr_1_matrix) <- colnames(diases)
rownames(ci_lower_matrix) <- colnames(diases)
colnames(ci_lower_matrix) <- colnames(diases)
rownames(ci_upper_matrix) <- colnames(diases)
colnames(ci_upper_matrix) <- colnames(diases)


#Exportar a matrix de correlação
writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_diases.xlsx")




# Visualização dos resultados ---------------------------------------------

# Convertendo as matrizes para formato longo para incluir ICs
long_data <- melt(mean_corr_1_matrix)
long_data$lower <- melt(ci_lower_matrix)$value
long_data$upper <- melt(ci_upper_matrix)$value


# Adicionando coluna para identificar se o IC contém zero
# Colocando "não significativo - ns" nas correlações que o intervalo de confiança contem
# o zero
long_data$contains_zero <- ifelse(long_data$lower <= 0 & long_data$upper >= 0, "X", "")


# Filtrando apenas a matriz triangular inferior
long_data$keep <- with(long_data, as.numeric(Var1) >= as.numeric(Var2))
triangular_data <- long_data[long_data$keep, ]

# Gerar matriz de strings com média e IC
corr_with_ci_matrix <- matrix(
  paste0(
    round(mean_corr_1, 2), " [",
    round(ci[1, ], 2), ", ",
    round(ci[2, ], 2), "]"
  ),
  ncol = ncol(diases),
  byrow = TRUE
)

# Nomear linhas e colunas
rownames(corr_with_ci_matrix) <- colnames(diases)
colnames(corr_with_ci_matrix) <- colnames(diases)

# Visualizar (opcional)
print(corr_with_ci_matrix)

# Exportar para Excel
writexl::write_xlsx(
  as.data.frame(corr_with_ci_matrix),
  "dados/correlacoes_com_IC_formatado.xlsx"
)

# Plot: Diases ------------------------------------


ggp1 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
  geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
  #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
  scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
  labs(x="Diseases",y="Diseases",fill = "Correlation")  +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18, face = "bold")
  )+ ggtitle("A)")
# Exibindo o gráfico
print(ggp1)



# Correlação entre variáveis fisico_quimicas_biologicas ------------------------------

# Simulação bootstrap -----------------------------------------------------

# Função para calcular a matriz de correlação com bootstrap
bootstrap_corr <- function(data, indices) {
  sampled_data <- data[indices, ] # Reamostragem dos dados
  cor(sampled_data, method = "spearman") # Calcula correlação Spearman
}

# Configurando reprodutibilidade e executando o bootstrap
va_water <- dataset[,23:38]
set.seed(123) # Para reprodutibilidade
results <- boot(data = va_water, statistic = bootstrap_corr, R = 10000)


# Extração dos resultados -------------------------------------------------

# Calculando a média das correlações para todas as reamostragens
mean_corr_1 <- apply(results$t, 2, mean)



# Calculando intervalos de confiança (IC) para as correlações
ci <- apply(results$t, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

# Convertendo resultados em matrizes
mean_corr_2_matrix <- matrix(mean_corr_1, ncol = ncol(va_water), byrow = TRUE)
ci_lower_matrix <- matrix(ci[1, ], ncol = ncol(va_water), byrow = TRUE)
ci_upper_matrix <- matrix(ci[2, ], ncol = ncol(va_water), byrow = TRUE)

# Nomeando as matrizes para facilitar a interpretação
rownames(mean_corr_2_matrix) <- colnames(va_water)
colnames(mean_corr_2_matrix) <- colnames(va_water)
rownames(ci_lower_matrix) <- colnames(va_water)
colnames(ci_lower_matrix) <- colnames(va_water)
rownames(ci_upper_matrix) <- colnames(va_water)
colnames(ci_upper_matrix) <- colnames(va_water)


#Exportar a matrix de correlação
writexl::write_xlsx(as.data.frame(mean_corr_2_matrix), "dados/mean_corr_va_water.xlsx")





# Visualização dos resultados ---------------------------------------------

# Convertendo as matrizes para formato longo para incluir ICs
long_data <- melt(mean_corr_2_matrix)
long_data$lower <- melt(ci_lower_matrix)$value
long_data$upper <- melt(ci_upper_matrix)$value


# Adicionando coluna para identificar se o IC contém zero
# Colocando "não significativo - ns" nas correlações que o intervalo de confiança contem
# o zero
long_data$contains_zero <- ifelse(long_data$lower <= 0 & long_data$upper >= 0, "X", "")


# Filtrando apenas a matriz triangular inferior
long_data$keep <- with(long_data, as.numeric(Var1) >= as.numeric(Var2))
triangular_data <- long_data[long_data$keep, ]

# Gerar matriz de strings com média e IC
corr_with_ci_matrix <- matrix(
  paste0(
    round(mean_corr_2_matrix, 2), " [",
    round(ci[1, ], 2), ", ",
    round(ci[2, ], 2), "]"
  ),
  ncol = ncol(va_water),
  byrow = TRUE
)

# Nomear linhas e colunas
rownames(corr_with_ci_matrix) <- colnames(va_water)
colnames(corr_with_ci_matrix) <- colnames(va_water)

# Visualizar (opcional)
print(corr_with_ci_matrix)

# Exportar para Excel
writexl::write_xlsx(
  as.data.frame(corr_with_ci_matrix),
  "dados/correlacoes_com_IC_PWB.xlsx"
)





# Plot:Physicochemical-biological water variables ------------------------------------


ggp2 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
  geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
  #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
  scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
  labs(x="Physicochemical-biological water variables",y="Physicochemical-biological water variables",fill = "Correlation")  +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18, face = "bold")
  )+ ggtitle("B)")
# Exibindo o gráfico
print(ggp2)


# Salvando as figuras -----------------------------------------------------
#Disposição em coluna
png(filename="figuras/correl_disp1.png", # Nome do arquivo e extensão
    width = 8,    # largura
    height = 12,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(ggp1, ggp2)
dev.off() # Fecha a janela gráfica

#Disposição em linha

png(filename="figuras/correl_disp2.png", # Nome do arquivo e extensão
    width = 15,    # largura
    height = 7,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(ggp1, ggp2, nrow=1)
dev.off() # Fecha a janela gráfica




#' 
#' # Dados -------------------------------------------------------------------
#' #dataset <- readxl::read_excel("dados/dados_paper.xlsx", sheet = 1) #Doenças ap_respiratoria
#' #dataset <- readxl::read_excel("dados/dados_paper.xlsx", sheet = 2) #Doenças ap_disgestivo
#' #dataset <- readxl::read_excel("dados/dados_paper.xlsx", sheet = 3) #Doenças s_nervoso
#' #dataset <- readxl::read_excel("dados/dados_paper.xlsx", sheet = 4) #Outras doenças
#' dataset <- readxl::read_excel("dados/dados_paper.xlsx", sheet = 5) #Planilha completa
#' 
#' #'Inspecionando a estrutura do dataset
#' str(dataset)
#' head(dataset)
#' 
#' # Subconjunto de colunas para análises de correlação
#' #diases <- dataset[, 4:21] #Doenças ap_respiratoria
#' #diases <- dataset[, 4:23] #Doenças ap_disgetivo
#' #diases <- dataset[, 4:19] #Doenças sistema nervoso
#' #diases <- dataset[, 4:23] #Outras doenças
#' #diases <- dataset[, 4:21] #Correlação somente doenças
#' diases <- dataset[, 5:38] #Correlação todas as variáveis
#' 
#' 
#' 
#' # Matriz de correlação inicial --------------------------------------------
#' # Calculando a matriz de correlação Spearman
#' matcor <- round(cor(diases, method = "spearman", use = "pairwise.complete.obs"), 2)
#' print(matcor)
#' 
#' 
#' # Simulação bootstrap -----------------------------------------------------
#' 
#' # Função para calcular a matriz de correlação com bootstrap
#' bootstrap_corr <- function(data, indices) {
#'   sampled_data <- data[indices, ] # Reamostragem dos dados
#'   cor(sampled_data, method = "spearman") # Calcula correlação Spearman
#' }
#' 
#' # Configurando reprodutibilidade e executando o bootstrap
#' set.seed(123) # Para reprodutibilidade
#' results <- boot(data = diases, statistic = bootstrap_corr, R = 10000)
#' 
#' 
#' # Extração dos resultados -------------------------------------------------
#' 
#' # Calculando a média das correlações para todas as reamostragens
#' mean_corr_1 <- apply(results$t, 2, mean)
#' 
#' 
#' 
#' # Calculando intervalos de confiança (IC) para as correlações
#' ci <- apply(results$t, 2, function(x) quantile(x, probs = c(0.025, 0.975)))
#' 
#' # Convertendo resultados em matrizes
#' mean_corr_1_matrix <- matrix(mean_corr_1, ncol = ncol(diases), byrow = TRUE)
#' ci_lower_matrix <- matrix(ci[1, ], ncol = ncol(diases), byrow = TRUE)
#' ci_upper_matrix <- matrix(ci[2, ], ncol = ncol(diases), byrow = TRUE)
#' 
#' # Nomeando as matrizes para facilitar a interpretação
#' rownames(mean_corr_1_matrix) <- colnames(diases)
#' colnames(mean_corr_1_matrix) <- colnames(diases)
#' rownames(ci_lower_matrix) <- colnames(diases)
#' colnames(ci_lower_matrix) <- colnames(diases)
#' rownames(ci_upper_matrix) <- colnames(diases)
#' colnames(ci_upper_matrix) <- colnames(diases)
#' 
#' 
#' #Exportar a matrix de correlação
#' #writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_1_dr.xlsx")
#' #writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_1_sdg.xlsx")
#' #writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_1_sn.xlsx")
#' #writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_1_outras.xlsx")
#' #writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_1_doenca.xlsx")
#' writexl::write_xlsx(as.data.frame(mean_corr_1_matrix), "dados/mean_corr_1_tudo.xlsx")
#' 
#' 
#' 
#' 
#' # Visualização dos resultados ---------------------------------------------
#' 
#' # Convertendo as matrizes para formato longo para incluir ICs
#' long_data <- melt(mean_corr_1_matrix)
#' long_data$lower <- melt(ci_lower_matrix)$value
#' long_data$upper <- melt(ci_upper_matrix)$value
#' 
#' 
#' # Adicionando coluna para identificar se o IC contém zero
#' # Colocando "não significativo - ns" nas correlações que o intervalo de confiança contem
#' # o zero
#' long_data$contains_zero <- ifelse(long_data$lower <= 0 & long_data$upper >= 0, "X", "")
#' 
#' 
#' # Filtrando apenas a matriz triangular inferior
#' long_data$keep <- with(long_data, as.numeric(Var1) >= as.numeric(Var2))
#' triangular_data <- long_data[long_data$keep, ]
#' 
#' 
#' # Gráfico doenças sistema respiratorio ------------------------------------
#' 
#' 
#' # ggp1 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
#' #   geom_tile(color = "white") +
#' #   geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
#' #   geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
#' #   #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
#' #   scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
#' #   labs(x="Variables",y="Variables",fill = "Correlation")  +
#' #   theme_minimal() +
#' #   theme(
#' #     axis.text.x = element_text(angle = 45, hjust = 1),
#' #     axis.title = element_text(size = 12, face = "bold")
#' #   )+ ggtitle("A)")
#' # # Exibindo o gráfico
#' # print(ggp1)
#' 
#' 
#' 
#' # Gráfico sistema disgestivo------------------------------------------- --------
#' # ggp2 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
#' #   geom_tile(color = "white") +
#' #   geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
#' #   geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
#' #   #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
#' #   scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
#' #   labs(x="Variables",y="Variables",fill = "Correlation")  +
#' #   theme_minimal() +
#' #   theme(
#' #     axis.text.x = element_text(angle = 45, hjust = 1),
#' #     axis.title = element_text(size = 12, face = "bold")
#' #   )+ ggtitle("B)")
#' # # Exibindo o gráfico
#' # print(ggp2)
#' 
#' 
#' 
#' 
#' 
#' # Gráfico sistema nervoso --------------------------------------------------
#' #Criando o gráfico com a matriz triangular inferior: Sistema nervoso
#' # ggp3 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
#' #   geom_tile(color = "white") +
#' #   geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
#' #   geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
#' #   #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
#' #   scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
#' #   labs(x="Variables",y="Variables",fill = "Correlation")  +
#' #   theme_minimal() +
#' #   theme(
#' #     axis.text.x = element_text(angle = 45, hjust = 1),
#' #     axis.title = element_text(size = 12, face = "bold")
#' #   )+ ggtitle("C)")
#' # # Exibindo o gráfico
#' # print(ggp3)
#' 
#' # Gráfico outras doenças --------------------------------------------------
#' 
#' # ggp4 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
#' #   geom_tile(color = "white") +
#' #   geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
#' #   geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
#' #   #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
#' #   scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
#' #   labs(x="Variables",y="Variables",fill = "Correlation")  +
#' #   theme_minimal() +
#' #   theme(
#' #     axis.text.x = element_text(angle = 45, hjust = 1),
#' #     axis.title = element_text(size = 12, face = "bold")
#' #   )+ ggtitle("D)")
#' # # Exibindo o gráfico
#' # print(ggp4)
#' 
#' 
#' 
#' # Gráfico somente doenças --------------------------------------------------
#' 
#' # ggp5 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
#' #   geom_tile(color = "white") +
#' #   geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
#' #   geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
#' #   #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
#' #   scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
#' #   labs(x="Variables",y="Variables",fill = "Correlation")  +
#' #   theme_minimal() +
#' #   theme(
#' #     axis.text.x = element_text(angle = 45, hjust = 1),
#' #     axis.title = element_text(size = 12, face = "bold")
#' #   )+ ggtitle("E)")
#' # # Exibindo o gráfico
#' # print(ggp5)
#' 
#' 
#' 
#' # Gráfico todas as variaveis --------------------------------------------------
#' 
#' ggp6 <- ggplot(triangular_data, aes(Var1, Var2, fill = value)) +
#'   geom_tile(color = "white") +
#'   geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
#'   geom_text(aes(label = ifelse(contains_zero == "X", "X", "")), color = "blue", size = 5) + # "X" centralizado
#'   #geom_text(aes(label = contains_zero), vjust = -0.9, color = "blue", size = 2) + # Anotação "X" em vermelho
#'   scale_fill_gradient2(low = "tomato2", high = "springgreen3", mid = "white", midpoint = 0) +
#'   labs(x="Variables",y="Variables",fill = "Correlation")  +
#'   theme_minimal() +
#'   theme(
#'     axis.text.x = element_text(angle = 45, hjust = 1),
#'     axis.title = element_text(size = 12, face = "bold")
#'   )+ ggtitle("F)")
#' # Exibindo o gráfico
#' print(ggp6)
#' 
#' 
#' # Salvando as figuras -----------------------------------------------------
#' 
#' # png(filename="figuras/correl_dr.png", # Nome do arquivo e extensão
#' #     width = 11,    # largura
#' #     height = 6,   # Altura
#' #     res= 400,# Resolução em dpi
#' #     family = "serif", #fonte
#' #     units = "in")  # Unidades.
#' # ggp1
#' # dev.off() # Fecha a janela gráfica
#' 
#' # 
#' # 
#' # png(filename="figuras/correl_sdg.png", # Nome do arquivo e extensão
#' #     width = 11,    # largura
#' #     height = 6,   # Altura
#' #     res= 400,# Resolução em dpi
#' #     family = "serif", #fonte
#' #     units = "in")  # Unidades.
#' # ggp2
#' # dev.off() # Fecha a janela gráfica
#' 
#' 
#' 
#' # png(filename="figuras/correl_sn.png", # Nome do arquivo e extensão
#' #     width = 11,    # largura
#' #     height = 6,   # Altura
#' #     res= 400,# Resolução em dpi
#' #     family = "serif", #fonte
#' #     units = "in")  # Unidades.
#' # ggp3
#' # dev.off() # Fecha a janela gráfica
#' 
#' 
#' 
#' # png(filename="figuras/correl_outras.png", # Nome do arquivo e extensão
#' #     width = 11,    # largura
#' #     height = 6,   # Altura
#' #     res= 400,# Resolução em dpi
#' #     family = "serif", #fonte
#' #     units = "in")  # Unidades.
#' # ggp4
#' # dev.off() # Fecha a janela gráfica
#' 
#' #Todas as doenças
#' # png(filename="figuras/correl_todas_doencas.png", # Nome do arquivo e extensão
#' #     width = 11,    # largura
#' #     height = 6,   # Altura
#' #     res= 400,# Resolução em dpi
#' #     family = "serif", #fonte
#' #     units = "in")  # Unidades.
#' # ggp5
#' # dev.off() # Fecha a janela gráfica
#' 
#' # #Todas as variaveis
#' # png(filename="figuras/correl_tudo.png", # Nome do arquivo e extensão
#' #     width = 11,    # largura
#' #     height = 6,   # Altura
#' #     res= 400,# Resolução em dpi
#' #     family = "serif", #fonte
#' #     units = "in")  # Unidades.
#' # ggp6
#' # dev.off() # Fecha a janela gráfica
#' 
#' 
