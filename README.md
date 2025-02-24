# Deteccao_Anomalia_Transasao_Financeira
Objetivo principal é apresentar que é possível unir analises gráficas em R junto com o Power BI.

#   Observação
Os dados ja foram tratados anteriomente, então, as analises sao feitas pulando essas etapas. 

O foco principal é apresentar analises em R 

# Codigo da Analise primaria 

# Instala os pacotes
install.packages("tidyverse")
install.packages("dplyr")
install.packages("solitude")
install.packages("ggplot2")
install.packages("readr")

# Carrega os pacotes nesta sessão R
library(tidyverse)
library(dplyr)
library(solitude)
library(ggplot2)
library(readr)

# Carrega os dados históricos
dados_historicos <- read_csv("dados_historicos.csv")
View(dados_historicos)

# Cria o modelo de Machine Learning com algoritmo isolationForest
?isolationForest  # ? ISSO PUXA UMA EXPLICACAO
modelo_ml = isolationForest$new() 

# Treina o modelo
modelo_ml$fit(dados_historicos)

# Faz as previsões com o modelo usando os dados histórico
# organizar em ordem decrescente

previsoes_historico = dados_historicos %>%
  modelo_ml$predict() %>%
  arrange(desc(anomaly_score))

View(previsoes_historico)

# Density Plot 
plot(density(previsoes_historico$anomaly_score))

# Quanto maior o anomaly score maior a chance do registro ser uma anomalia
# Vamos definir como regra que anomaly score acima de 0.62 é uma anomalia
indices_historico = previsoes_historico[which(previsoes_historico$anomaly_score > 0.62)]

# Faz o filtro
anomalias_historico = dados_historicos[indices_historico$id, ]
normais_historico = dados_historicos[-indices_historico$id, ]

# Gráfico
colors() # BANCO DE DADOS DAS CORES
ggplot() + 
  geom_point(data = normais_historico, 
             mapping = aes(transacao1,transacao2), 
             col = "skyblue3", 
             alpha = 0.5) + 
  geom_point(data = anomalias_historico,
             mapping = aes(transacao1,transacao2), 
             col = "red2", 
             alpha = 0.8)

# Agora carregamos novos dados
novos_dados <- read.csv("novos_dados.csv")
View(novos_dados)

# Previsões com o modelo treinado
previsoes_novos_dados = modelo_ml$predict(novos_dados)

# Se o anomaly score é maior que 0.62 consideramos como anomalia
indices_novos_dados = previsoes_novos_dados[which(previsoes_novos_dados$anomaly_score > 0.62)]

# Filtro
anomalias_novos_dados = novos_dados[indices_novos_dados$id, ]
normais_novos_dados = novos_dados[-indices_novos_dados$id, ]

# Gráfico das previsões
ggplot() + 
  geom_point(data = normais_novos_dados, 
             mapping = aes(transacao1,transacao2), 
             col = "turquoise3", 
             alpha = 0.5) + 
  geom_point(data = anomalias_novos_dados, 
             mapping = aes(transacao1,transacao2), 
             col = "tomato3", 
             alpha = 0.8)

View(previsoes_novos_dados)

# Arredondando a coluna 'anomaly_score' para 2 casas decimais
previsoes_novos_dados <- previsoes_novos_dados %>%
  mutate(anomaly_score = round(anomaly_score, 2))

View(previsoes_novos_dados)

# Criando uma nova coluna com base na condição
previsoes_novos_dados <- previsoes_novos_dados %>%
  mutate(status = ifelse(anomaly_score > 0.62, "anomalia", "normal"))

View(previsoes_novos_dados)

library(ggplot2)

# Criando o box plot

#QUANDO FOR NOMEAR, PARA FACILITAR COLOCAR O NOME DE "dataset"
ggplot(previsoes_novos_dados, aes(x = status, y = anomaly_score, fill = status)) +
  geom_boxplot() +
  labs(title = "Box Plot de Anomalias e Normais",
       x = "Status",
       y = "Anomaly Score") +
  theme_minimal() +
  scale_fill_manual(values = c("anomalia" = "red", "normal" = "blue")) +
  theme(legend.position = "none")

# Salva em disco
write.csv(previsoes_novos_dados, "previsoes_novos_dados.csv")



