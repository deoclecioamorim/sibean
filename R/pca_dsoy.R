
#'############################################################################
#'
#'Prof. Dr. Deoclecio

# Limpar do ambiente ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas


# Pacotes usados-------------------------------------------------------------------------------

if(!require(readxl)) install.packages("readxl", dep = TRUE)
if(!require(tidyverse)) install.packages("tidyverse", dep = TRUE)
if(!require(ggcorrplot)) install.packages("ggcorrplot", dep = TRUE)
if(!require(factoextra)) install.packages("factoextra", dep = TRUE)
if(!require(FactoMineR)) install.packages("FactoMineR", dep = TRUE)
if(!require(nlme)) install.packages("nlme", dep = T)
if(!require(psych)) install.packages("psych", dep = T)
if(!require(readxl))install.packages("readxl", dep = TRUE)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE)
if(!require(caret))install.packages("caret", dep = TRUE)
if(!require(MuMIn)) install.packages("MuMIn", dep = TRUE)
if(!require(writexl))install.packages("writexl", dep = TRUE)
if(!require(FSA))install.packages("FSA", dep = TRUE)
if(!require(forecast))install.packages("forecast", dep = TRUE)
if(!require(glmmTMB))install.packages("glmmTMB", dep = TRUE)
if(!require(lme4))install.packages("lme4", dep = TRUE)
if(!require(extrafont))install.packages("extrafont", dep = TRUE)
if(!require(viridis))install.packages("viridis", dep = TRUE)
if(!require(gridExtra))install.packages("gridExtra", dep = TRUE)
if(!require(ggplotify))install.packages("ggplotify", dep = TRUE)


# Dataset----------------------------------------------------------------------
# Stage v4 ----------------------------------------------------------------
dataset_v4 <- readxl::read_excel("data/dataset.xlsx", sheet = 2) #Planilha completa
dataset_v4 <- transform(dataset_v4, Trat=as.factor(Trat))
str(dataset_v4)


# PCA --------------------------------------------------------------------------
#'
#'Análise de PCA para variáveis fisico-quimicas-biologicas
dataset_pca <- dataset_v4[,5:13]



# Análise paralela
psych::fa.parallel(dataset_pca, fa = "pc", n.iter = 200, show.legend = T,
                   main = "Parallel Analysis")



# Extrair componentes principais
#Com prcomp
pca.prcomp <- prcomp(dataset_pca,scale=TRUE)
pca.prcomp
# Retornar apenas as componentes retidas
num_pcs <- 2  # Substitua pelo número de componentes retidos com base na análise paralela
retained_pcs <- as.data.frame(pca.prcomp$x[, 1:num_pcs])
colnames(retained_pcs) <- paste0("PC", 1:num_pcs)

# Adicionar as PCs retidas ao conjunto de dados original
dataset <- cbind(dataset_pca, retained_pcs)
str(dataset)


# PCA dados fisico-quimico-biologicos -------------------------------------
# PCA: FactoMineR-----------------------------------------------------------
res.pca <- FactoMineR::PCA(dataset_pca, 
                           scale.unit = T, graph = FALSE)



#  scale.unit = T -> trabalhando com correlação
#  scale.unit = F -> trabalhando com covariância
print(res.pca)
res.pca$eig #Autovalores onde eu vejo a contribuição de cada eixo para explicação da variância
res.pca$svd #Autovetores onde eu vejo a importância das minhas variàveis para cada eixo
#'
##Scree plot
fviz_eig(res.pca, addlabels = T)


# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
#'

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)




# Contributions of variables to PC1
plot_pc1 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10, fill = "gray",
                         color = "black") +
  ggtitle("A)") +
  labs(x="Variables",y="Contribuition PC1 (%)")  +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(size = 12, face = "bold")
  )

plot_pc1 

# Contributions of variables to PC2
plot_pc2 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10, fill = "gray",
                         color = "black") +
  ggtitle("B)") +
  labs(x="Variables",y="Contribuition PC2 (%)")  +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(size = 12, face = "bold")
  )

plot_pc2



# jpeg(filename="figures/pca.jpeg", # Nome do arquivo e extensão
#     width = 15,    # largura
#     height = 10,   # Altura
#     res= 500,# Resolução em dpi
#     family = "Palatino", #fonte
#     units = "in")  # Unidades.
# gridExtra::grid.arrange(plot_pc1, plot_pc2)
# dev.off() # Fecha a janela gráfica
# 

# Gráfico biplot PCA ------------------------------------------------------
library(factoextra)
library(viridis)
library(scales)

# Função para aplicar formatação dos números nos eixos
usar_menos_correto <- function(p) {
  p +
    scale_x_continuous(labels = label_number(negative = "\u2212")) +
    scale_y_continuous(labels = label_number(negative = "\u2212"))
}

# Função para biplots com menos correto
biplot_custom <- function(res.pca, dims, title_letter) {
  p <- fviz_pca_biplot(
    res.pca,
    axes = dims,
    repel = TRUE,
    label = "var",
    #col.var = "contrib",                # Cor com base na contribuição das variáveis
    #gradient.cols = viridis(100),       # Paleta viridis para variáveis
    #col.ind = "black",                  # Indivíduos em preto discreto
    habillage = dataset_v4$Trat, 
    pointsize = 2.5,
    arrowsize = 0.8,
    labelsize = 4.5,
    ggtheme = theme_classic()
  ) +
    ggtitle(paste0(title_letter, ") PC", dims[1], " vs PC", dims[2])) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
  # Aplica a correção do sinal de menos
  p <- p +
    # Outra forma de corrigir o sinal de menos
    scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
    scale_x_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) 
  
  return(p)
}

# # Função para biplots destacando a importância das variáveis
# biplot_custom <- function(res.pca, dims, title_letter) {
#   fviz_pca_biplot(
#     res.pca,
#     axes = dims,
#     repel = TRUE,
#     label = "var",
#     col.var = "contrib",                # Cor com base na contribuição das variáveis
#     gradient.cols = viridis(100),       # Paleta viridis para variáveis
#     col.ind = "black",                 # Indivíduos em cinza discreto
#     pointsize = 2.5,
#     arrowsize = 0.8,
#     labelsize = 4.5,
#     ggtheme = theme_classic()
#   ) +
#     ggtitle(paste0(title_letter, ") PC", dims[1], " vs PC", dims[2])) +
#     theme(
#       plot.title = element_text(size = 14, face = "bold", hjust = 0),
#       axis.title = element_text(size = 12),
#       axis.text = element_text(size = 12),
#       legend.title = element_text(size = 10),
#       legend.text = element_text(size = 9)
#     )
# }
# 
# 
# 
# 
# Criando os biplots com as variáveis coloridas por importância
pA <- biplot_custom(res.pca, c(1, 2), "A")
pA


tiff(filename="figuras/fig3.tiff", # Nome do arquivo e extensão
     width = 15,    # largura
     height = 12,   # Altura
     res= 500,# Resolução em dpi
     family = "Palatino", #fonte
     units = "in")  # Unidades.
grid.arrange(pA, pB, pC, pD, ncol = 2)
dev.off() # Fecha a janela gráfica


jpeg(filename="figuras/fig3.jpeg", # Nome do arquivo e extensão
     width = 15,    # largura
     height = 12,   # Altura
     res= 1000,# Resolução em dpi
     family = "Palatino", #fonte
     units = "in")  # Unidades.
grid.arrange(pA, pB, pC, pD, ncol = 2)
dev.off() # Fecha a janela gráfica


# Modelos de contagem -----------------------------------------------------
# Doenças respiratórias ---------------------------------------------------
str(dataset)

mod1.null <- glmmTMB(DRS ~ 1, family = nbinom2(),data=dataset)
mod1.fixed <- glmmTMB(DRS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod1.mixed <- glmmTMB(DRS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod1.null)
summary(mod1.fixed)
summary(mod1.mixed)

AICc(mod1.null)
AICc(mod1.fixed)
AICc(mod1.mixed)

anova(mod1.null, mod1.fixed)
anova(mod1.fixed, mod1.mixed)

fit1  <- glmmTMB(DRS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)
summary(fit1)
# Extract and visualize random effects
# Extração dos efeitos aleatórios
ranef_drs <- ranef(mod1.mixed)$cond

# Obtenção dos erros padrão
random_effect_var_drs <- TMB::sdreport(mod1.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_drs <- sqrt(random_effect_var_drs$diag.cov.random) 

# Combinação das estimativas com o erro padrão
ranef_df1 <- do.call(rbind, lapply(names(ranef_drs), function(grupo, idx = 1) {
  efeitos <- ranef_drs[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_drs[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança bilateral de 95%
ranef_df1 <- ranef_df1 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar aprimorado
plot_drs <- ggplot(ranef_df1, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(aes(color = significant), size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +
  facet_wrap(~ group, scales = "free_y") +
  coord_flip() +
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  # Aqui corrigimos o sinal do eixo y
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    x = "Levels",
    y = "Random Effects"
  ) +
  ggtitle("A)")

# Visualização
plot_drs

#'
#'Conclusão:
#'
#'Os efeitos aleatórios são significativos
#'

mod2.null <- glmmTMB(DRS_A ~ 1, family = nbinom2(),data=dataset)
mod2.fixed <- glmmTMB(DRS_A ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod2.mixed <- glmmTMB(DRS_A ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod2.null)
summary(mod2.fixed)
summary(mod2.mixed)





AICc(mod2.null)
AICc(mod2.fixed)
AICc(mod2.mixed)

anova(mod2.null, mod2.fixed)
anova(mod2.fixed, mod2.mixed)

# Tabela de resultados
library(sjPlot)
tab_model(mod2.mixed)



#'Conclusão:
#'
#'mod2.fixed foi escolhido
#'
#'

mod3.null <- glmmTMB(DRS_ODNPS ~ 1, family = nbinom2(),data=dataset)
mod3.fixed <- glmmTMB(DRS_ODNPS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod3.mixed <- glmmTMB(DRS_ODNPS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod3.null)
summary(mod3.fixed)
summary(mod3.mixed)

AICc(mod3.null)
AICc(mod3.fixed)
AICc(mod3.mixed)

anova(mod3.null, mod3.fixed)
anova(mod3.fixed, mod3.mixed)



# Extract and visualize random effects
ranef_DRS_ODNPS <- ranef(mod3.mixed, condVar = T)
ranef_DRS_ODNPS


# Extração dos efeitos aleatórios
ranef_DRS_ODNPS <- ranef(mod3.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DRS_ODNPS <- TMB::sdreport(mod3.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_DRS_ODNPS <- sqrt(random_effect_var_DRS_ODNPS$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df2 <- do.call(rbind, lapply(names(ranef_DRS_ODNPS), function(grupo, idx = 1) {
  efeitos <- ranef_DRS_ODNPS[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DRS_ODNPS[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df2 <- ranef_df2 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DRS_ODNPS <- ggplot(ranef_df2, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("B)")


plot_DRS_ODNPS 


#Disposição em linha

tiff(filename="figuras/fig4.tiff", # Nome do arquivo e extensão
    width = 15,    # largura
    height = 7,   # Altura
    res= 500,# Resolução em dpi
    family = "Palatino", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(plot_drs, plot_DRS_ODNPS, nrow=1)
dev.off() # Fecha a janela gráfica

jpeg(filename="figuras/fig4.jpeg", # Nome do arquivo e extensão
     width = 15,    # largura
     height = 7,   # Altura
     res= 1000,# Resolução em dpi
     family = "Palatino", #fonte
     units = "in")  # Unidades.
gridExtra::grid.arrange(plot_drs, plot_DRS_ODNPS, nrow=1)
dev.off() # Fecha a janela gráfica


mod4.null <- glmmTMB(DRS_ODRS ~ 1, family = nbinom2(),data=dataset)
mod4.fixed <- glmmTMB(DRS_ODRS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod4.mixed <- glmmTMB(DRS_ODRS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod4.null)
summary(mod4.fixed)
summary(mod4.mixed)

AICc(mod4.null)
AICc(mod4.fixed)
AICc(mod4.mixed)

anova(mod4.null, mod4.fixed)
anova(mod4.fixed, mod4.mixed)



# Tabela de resultados
library(sjPlot)
tab_model(mod1.mixed)


# Doenças do sistema disgestório ------------------------------------------
str(dataset)

mod5.null <- glmmTMB(DDS ~ 1, family = nbinom2(),data=dataset)
mod5.fixed <- glmmTMB(DDS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod5.mixed <- glmmTMB(DDS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod5.null)
summary(mod5.fixed)
summary(mod5.mixed)

AICc(mod5.null)
AICc(mod5.fixed)
AICc(mod5.mixed)

anova(mod5.null, mod5.fixed)
anova(mod5.fixed, mod5.mixed)



# Extração dos efeitos aleatórios
ranef_DDS <- ranef(mod5.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DDS <- TMB::sdreport(mod5.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_DDS <- sqrt(random_effect_var_DDS$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df3 <- do.call(rbind, lapply(names(ranef_DDS), function(grupo, idx = 1) {
  efeitos <- ranef_DDS[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DDS[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df3 <- ranef_df3 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DDS <- ggplot(ranef_df3, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("A)")


plot_DDS 









#'
#'Modelo misto
#'
mod6.null <- glmmTMB(DDS_ODDS ~ 1, family = nbinom2(),data=dataset)
mod6.fixed <- glmmTMB(DDS_ODDS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod6.mixed <- glmmTMB(DDS_ODDS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod6.null)
summary(mod6.fixed)
summary(mod6.mixed)

AICc(mod6.null)
AICc(mod6.fixed)
AICc(mod6.mixed)

anova(mod6.null, mod6.fixed)
anova(mod6.fixed, mod6.mixed)


#DDS_DGPI
mod7.null <- glmmTMB(DDS_DGPI ~ 1, family = nbinom2(),data=dataset)
mod7.fixed <- glmmTMB(DDS_DGPI ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod7.mixed <- glmmTMB(DDS_DGPI ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)

mod7.mixedB <- update(mod7.mixed, control=glmmTMBControl(optimizer=optim,
                                         optArgs=list(method="BFGS")))

summary(mod7.mixedB)

summary(mod7.null)
summary(mod7.fixed)
summary(mod7.mixedB)

AICc(mod7.null)
AICc(mod7.fixed)
AICc(mod7.mixedB)

anova(mod7.null, mod7.fixed)
anova(mod7.fixed, mod7.mixedB)


# Extração dos efeitos aleatórios
ranef_DDS_DGPI <- ranef(mod7.mixedB)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DDS_DGPI <- TMB::sdreport(mod7.mixedB$obj, getJointPrecision = TRUE)
random_effect_sd_DDS_DGPI <- sqrt(random_effect_var_DDS_DGPI$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df4 <- do.call(rbind, lapply(names(ranef_DDS_DGPI), function(grupo, idx = 1) {
  efeitos <- ranef_DDS_DGPI[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DDS_DGPI[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df4 <- ranef_df4 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DDS_DGPI <- ggplot(ranef_df4, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("B)")


plot_DDS_DGPI 







#DDS_OLV
mod8.null <- glmmTMB(DDS_OLV ~ 1, family = nbinom2(),data=dataset)

mod8.nullB <- update(mod8.null, control=glmmTMBControl(optimizer=optim,
                                                        optArgs=list(method="BFGS")))

mod8.fixed <- glmmTMB(DDS_OLV ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)

mod8.fixedB <- update(mod8.fixed, control=glmmTMBControl(optimizer=optim,
                                                       optArgs=list(method="BFGS")))


mod8.mixed <- glmmTMB(DDS_OLV~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)

mod8.mixedB <- update(mod8.mixed, control=glmmTMBControl(optimizer=optim,
                                                         optArgs=list(method="BFGS")))

summary(mod8.nullB)
summary(mod8.fixedB)
summary(mod8.mixedB)

AICc(mod8.nullB)
AICc(mod8.fixedB)
AICc(mod8.mixedB)

anova(mod8.nullB, mod8.fixedB)
anova(mod8.fixedB, mod8.mixedB)






#'Não se ajustou!

#DDS_OIID
mod9.null <- glmmTMB(DDS_OIID ~ 1, family = nbinom2(),data=dataset)
mod9.nullB <- update(mod9.null, control=glmmTMBControl(optimizer=optim,
                                                       optArgs=list(method="BFGS")))


mod9.fixed <- glmmTMB(DDS_OIID ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod9.fixedB  <- update(mod9.fixed, control=glmmTMBControl(optimizer=optim,
                                                       optArgs=list(method="BFGS")))

mod9.mixed <- glmmTMB(DDS_OIID ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)

mod9.mixedB <- update(mod9.fixed, control=glmmTMBControl(optimizer=optim,
                                                          optArgs=list(method="BFGS")))


summary(mod9.nullB)
summary(mod9.fixedB)
summary(mod9.mixedB)

AICc(mod9.nullB)
AICc(mod9.fixedB)
AICc(mod9.mixedB)

anova(mod9.nullB, mod9.fixedB)
anova(mod9.fixedB, mod9.mixedB)



#DDS_ODESD
mod10.null <- glmmTMB(DDS_ODESD ~ 1, family = nbinom2(),data=dataset)
mod10.fixed <- glmmTMB(DDS_ODESD ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod10.mixed <- glmmTMB(DDS_ODESD ~ PC1+PC2+PC3+PC4+(1|year), family = nbinom2(),data=dataset)


summary(mod10.null)
summary(mod10.fixed)
summary(mod10.mixed)

AICc(mod10.null)
AICc(mod10.fixed)
AICc(mod10.mixed)

anova(mod10.null, mod10.fixed)
anova(mod10.fixed, mod10.mixed)

# Extração dos efeitos aleatórios
ranef_DDS_ODESD <- ranef(mod10.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DDS_ODESD <- TMB::sdreport(mod10.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_DDS_ODESD <- sqrt(random_effect_var_DDS_ODESD$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df5 <- do.call(rbind, lapply(names(ranef_DDS_ODESD), function(grupo, idx = 1) {
  efeitos <- ranef_DDS_ODESD[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DDS_ODESD[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df5 <- ranef_df5 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DDS_ODESD <- ggplot(ranef_df5, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("C)")


plot_DDS_ODESD 






#Disposição em linha

tiff(filename="figuras/fig5.tiff", # Nome do arquivo e extensão
    width = 15,    # largura
    height = 7,   # Altura
    res= 500,# Resolução em dpi
    family = "Palatino", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(plot_DDS, plot_DDS_DGPI, plot_DDS_ODESD,nrow=1)
dev.off() # Fecha a janela gráfica


jpeg(filename="figuras/fig5.jpeg", # Nome do arquivo e extensão
     width = 15,    # largura
     height = 7,   # Altura
     res= 1000,# Resolução em dpi
     family = "Palatino", #fonte
     units = "in")  # Unidades.
gridExtra::grid.arrange(plot_DDS, plot_DDS_DGPI, plot_DDS_ODESD,nrow=1)
dev.off() # Fecha a janela gráfica


# Doenças do sistema nervoso ----------------------------------------------

#DNS
mod11.null <- glmmTMB(DNS ~ 1, family = nbinom2(),data=dataset)
mod11.fixed <- glmmTMB(DNS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod11.mixed <- glmmTMB(DNS ~ PC1+PC2+PC3+PC4+(1|year), family = nbinom2(),data=dataset)


summary(mod11.null)
summary(mod11.fixed)
summary(mod11.mixed)

AICc(mod11.null)
AICc(mod11.fixed)
AICc(mod11.mixed)

anova(mod11.null, mod11.fixed)
anova(mod11.fixed, mod11.mixed)



# Extração dos efeitos aleatórios
ranef_DNS <- ranef(mod11.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DNS <- TMB::sdreport(mod11.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_DNS <- sqrt(random_effect_var_DNS$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df6 <- do.call(rbind, lapply(names(ranef_DNS), function(grupo, idx = 1) {
  efeitos <- ranef_DNS[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DNS[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df6 <- ranef_df6 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DNS <- ggplot(ranef_df6, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("A)")


plot_DNS 







#DDS_ODDS
str(dataset)
mod12.null <- glmmTMB(DNS_ODNS ~ 1, family = nbinom2(),data=dataset)
mod12.fixed <- glmmTMB(DNS_ODNS ~ PC1+PC2+PC3+PC4, family = nbinom2(),data=dataset)
mod12.mixed <- glmmTMB(DNS_ODNS ~ PC1+PC2+PC3+PC4+(1|year/season), family = nbinom2(),data=dataset)


summary(mod12.null)
summary(mod12.fixed)
summary(mod12.mixed)

AICc(mod12.null)
AICc(mod12.fixed)
AICc(mod12.mixed)

anova(mod12.null, mod12.fixed)
anova(mod12.fixed, mod12.mixed)





# Outras Doenças ----------------------------------------------------------


# Modelos para DSST
mod13.null <- glmmTMB(DSST ~ 1, family = nbinom2(), data = dataset)
mod13.fixed <- glmmTMB(DSST ~ PC1 + PC2 + PC3 + PC4, family = nbinom2(), data = dataset)
mod13.mixed <- glmmTMB(DSST ~ PC1 + PC2 + PC3 + PC4 + (1 | year), family = nbinom2(), data = dataset)
mod13.mixedB <- glmmTMB(DSST ~ PC1 + PC2 + PC3 + PC4 + (1 | year/season), family = nbinom2(), data = dataset)

summary(mod13.null)
summary(mod13.fixed)
summary(mod13.mixed)
summary(mod13.mixedB)

AICc(mod13.null)
AICc(mod13.fixed)
AICc(mod13.mixed)
AICc(mod13.mixedB)

anova(mod13.null, mod13.fixed)
anova(mod13.fixed, mod13.mixed)
anova(mod13.mixedB, mod13.mixed)

# Extração dos efeitos aleatórios
ranef_DSST <- ranef(mod13.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DSST <- TMB::sdreport(mod13.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_DSST <- sqrt(random_effect_var_DSST$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df7 <- do.call(rbind, lapply(names(ranef_DSST), function(grupo, idx = 1) {
  efeitos <- ranef_DSST[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DSST[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df7 <- ranef_df7 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DSST <- ggplot(ranef_df7, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("B)")


plot_DSST 






# Modelos para ODSST
mod14.null <- glmmTMB(ODSST ~ 1, family = nbinom2(), data = dataset)
mod14.fixed <- glmmTMB(ODSST ~ PC1 + PC2 + PC3 + PC4, family = nbinom2(), data = dataset)
mod14.mixed <- glmmTMB(ODSST ~ PC1 + PC2 + PC3 + PC4 + (1 | year), family = nbinom2(), data = dataset)
mod14.mixedB <- glmmTMB(ODSST ~ PC1 + PC2 + PC3 + PC4 + (1 | year/season), family = nbinom2(), data = dataset)
summary(mod14.null)
summary(mod14.fixed)
summary(mod14.mixed)
summary(mod14.mixedB)

AICc(mod14.null)
AICc(mod14.fixed)
AICc(mod14.mixed)
AICc(mod14.mixedB)
anova(mod14.null, mod14.fixed)
anova(mod14.fixed, mod14.mixed)
anova(mod14.mixedB, mod14.mixed)



# Extração dos efeitos aleatórios
ranef_ODSST <- ranef(mod14.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_ODSST <- TMB::sdreport(mod14.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_ODSST <- sqrt(random_effect_var_ODSST$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df8 <- do.call(rbind, lapply(names(ranef_ODSST), function(grupo, idx = 1) {
  efeitos <- ranef_ODSST[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_ODSST[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df8 <- ranef_df8 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_ODSST <- ggplot(ranef_df8, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("C)")


plot_ODSST 

tab_model(mod14.mixed)


# Modelos para SSTI
mod15.null <- glmmTMB(SSTI ~ 1, family = nbinom2(), data = dataset)
mod15.fixed <- glmmTMB(SSTI ~ PC1 + PC2 + PC3 + PC4, family = nbinom2(), data = dataset)
mod15.mixed <- glmmTMB(SSTI ~ PC1 + PC2 + PC3 + PC4 + (1 |year), family = nbinom2(), data = dataset)
mod15.mixedB <- glmmTMB(SSTI ~ PC1 + PC2 + PC3 + PC4 + (1 |year/season), family = nbinom2(), data = dataset)

summary(mod15.null)
summary(mod15.fixed)
summary(mod15.mixed)
summary(mod15.mixedB)

AICc(mod15.null)
AICc(mod15.fixed)
AICc(mod15.mixed)
AICc(mod15.mixedB)

anova(mod15.null, mod15.fixed)
anova(mod15.fixed, mod15.mixed)
anova(mod15.mixedB, mod15.mixed)



# Extração dos efeitos aleatórios
ranef_SSTI <- ranef(mod15.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_SSTI <- TMB::sdreport(mod15.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_SSTI <- sqrt(random_effect_var_SSTI$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df9 <- do.call(rbind, lapply(names(ranef_SSTI), function(grupo, idx = 1) {
  efeitos <- ranef_SSTI[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_SSTI[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df9 <- ranef_df9 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_SSTI <- ggplot(ranef_df9, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("D)")


plot_SSTI 

tab_model(mod15.mixed)




# Modelos para IPSOCEC
mod16.null <- glmmTMB(IPSOCEC ~ 1, family = nbinom2(), data = dataset)
mod16.fixed <- glmmTMB(IPSOCEC ~ PC1 + PC2 + PC3 + PC4, family = nbinom2(), data = dataset)
mod16.mixed <- glmmTMB(IPSOCEC ~ PC1 + PC2 + PC3 + PC4 + (1 | year/season), family = nbinom2(), data = dataset)

summary(mod16.null)
summary(mod16.fixed)
summary(mod16.mixed)

AICc(mod16.null)
AICc(mod16.fixed)
AICc(mod16.mixed)

anova(mod16.null, mod16.fixed)
anova(mod16.fixed, mod16.mixed)

# Modelos para DEA
mod17.null <- glmmTMB(DEA ~ 1, family = nbinom2(), data = dataset)
mod17.fixed <- glmmTMB(DEA ~ PC1 + PC2 + PC3 + PC4, family = nbinom2(), data = dataset)
mod17.mixed <- glmmTMB(DEA ~ PC1 + PC2 + PC3 + PC4 + (1 | year/season), family = nbinom2(), data = dataset)

summary(mod17.null)
summary(mod17.fixed)
summary(mod17.mixed)

AICc(mod17.null)
AICc(mod17.fixed)
AICc(mod17.mixed)

anova(mod17.null, mod17.fixed)
anova(mod17.fixed, mod17.mixed)


# Extração dos efeitos aleatórios
ranef_DEA <- ranef(mod17.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_DEA <- TMB::sdreport(mod17.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_DEA <- sqrt(random_effect_var_DEA$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df10 <- do.call(rbind, lapply(names(ranef_DEA), function(grupo, idx = 1) {
  efeitos <- ranef_DEA[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_DEA[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df10 <- ranef_df10 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_DEA <- ggplot(ranef_df10, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("E)")


plot_DEA 

 tab_model(mod15.mixed)







# Modelos para ODEA
mod18.null <- glmmTMB(ODEA ~ 1, family = nbinom2(), data = dataset)
mod18.fixed <- glmmTMB(ODEA ~ PC1 + PC2 + PC3 + PC4, family = nbinom2(), data = dataset)
mod18.mixed <- glmmTMB(ODEA ~ PC1 + PC2 + PC3 + PC4 + (1 | year), family = nbinom2(), data = dataset)
mod18.mixedB <- glmmTMB(ODEA ~ PC1 + PC2 + PC3 + PC4 + (1 | year/season), family = nbinom2(), data = dataset)

summary(mod18.null)
summary(mod18.fixed)
summary(mod18.mixed)
summary(mod18.mixedB)

AICc(mod18.null)
AICc(mod18.fixed)
AICc(mod18.mixed)
AICc(mod18.mixedB)

anova(mod18.null, mod18.fixed)
anova(mod18.fixed, mod18.mixed)
anova(mod18.mixedB, mod18.mixed)


# Extração dos efeitos aleatórios
ranef_ODEA <- ranef(mod18.mixed)$cond

# Obtenção das variâncias dos efeitos aleatórios usando TMB::sdreport
random_effect_var_ODEA <- TMB::sdreport(mod18.mixed$obj, getJointPrecision = TRUE)
random_effect_sd_ODEA <- sqrt(random_effect_var_ODEA$diag.cov.random)  # Erros padrão

# Combinação das estimativas com o erro padrão
# Criando um data frame com os efeitos aleatórios e erros padrão
ranef_df11 <- do.call(rbind, lapply(names(ranef_ODEA), function(grupo, idx = 1) {
  efeitos <- ranef_ODEA[[grupo]]
  n <- nrow(efeitos)
  
  data.frame(
    group = grupo,
    level = rownames(efeitos),
    estimate = efeitos[, 1],
    std.error = random_effect_sd_ODEA[idx:(idx + n - 1)]
  )
}))

# Cálculo do intervalo de confiança (IC 95%)
ranef_df11 <- ranef_df11 %>%
  mutate(
    lower = estimate - 1.645 * std.error,
    upper = estimate + 1.645 * std.error,
    significant = ifelse(lower > 0 | upper < 0, "Significant", "Not Significant")  # Destaque
  )

# Gráfico Caterpillar
plot_ODEA <- ggplot(ranef_df11, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = significant), width = 0.1, size = 0.8) +  # Alteração da cor
  geom_hline(yintercept = 0, linetype = "dashed", color = "magenta", size=0.8) +  # Linha vertical no zero
  facet_wrap(~ group, scales = "free_y") +  # Separar por grupos (year/season)
  coord_flip() +  # Inverter eixos para melhor visualização
  scale_color_manual(values = c("Significant" = "#0072B2", "Not Significant" = "gray20")) +  # Cores personalizadas
  scale_y_continuous(labels = function(x) gsub("-", "\u2212", as.character(x))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = -0.05),
    legend.position = "top",
    legend.title = element_blank()
  )+
  labs(
    x = "Levels",
    y = "Random effects"
  ) + ggtitle("F)")


plot_ODEA 

gridExtra::grid.arrange(plot_DNS, plot_DSST,plot_ODSST, plot_SSTI, plot_DEA, plot_ODEA,nrow=2)
#Disposição em linha

tiff(filename="figuras/fig6.tiff", # Nome do arquivo e extensão
    width = 15,    # largura
    height = 12,   # Altura
    res= 500,# Resolução em dpi
    family = "Palatino", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(plot_DNS, plot_DSST,plot_ODSST, plot_SSTI, plot_DEA, plot_ODEA,nrow=2)
dev.off() # Fecha a janela gráfica


jpeg(filename="figuras/fig6.jpeg", # Nome do arquivo e extensão
     width = 15,    # largura
     height = 12,   # Altura
     res= 1000,# Resolução em dpi
     family = "Palatino", #fonte
     units = "in")  # Unidades.
gridExtra::grid.arrange(plot_DNS, plot_DSST,plot_ODSST, plot_SSTI, plot_DEA, plot_ODEA,nrow=2)
dev.off() # Fecha a janela gráfica


