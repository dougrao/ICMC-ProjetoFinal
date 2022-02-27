rm(list = ls())

#carga das bibliotecas
library(bnlearn)
library(StatMeasures)
library(bnviewer)
library(purrr)
library(Hmisc)
library(janitor)
library(vtree)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(viridis)
library(xlsx)


options(digits = 10)
#carga dos dados
setwd("./Documents/DataScience/USP - MBA Ciencia de Dados/CEMEAI - 2021 - M&P/TCC/Dados/")
# dados de producao
dados <- read.csv("prod_agro_parana08-19.csv",
                  header = TRUE, sep = ";", dec = ",")
df <- data.frame(dados)
head(df, 10)
nrow(df)
# dados de custo
dados_custo <- read.xlsx("custo_produtor_08_18.xls", sheetIndex = 1)
df_custo <- data.frame(dados_custo)
head(df_custo, 10)
nrow(df_custo)
# dados de venda
dados_venda <- read.xlsx("preco_venda_produtor08_18.xls", sheetIndex = 1)
df_venda <- data.frame(dados_venda)
head(df_venda, 10)
nrow(df_venda)
# dados de dolar
dados_dolar <- read.xlsx("taxa_cambio.xls", sheetIndex = 1)
df_dolar <- data.frame(dados_dolar)
head(df_dolar, 10)
nrow(df_dolar)
# dados de pib
dados_pib <- read.xlsx("pib_agro.xls", sheetIndex = 1)
df_pib <- data.frame(dados_pib)
head(df_pib, 10)
nrow(df_pib)
# dados clima
dados_clima <- read.xlsx("INMET_20082018.xlsx", sheetIndex = 1)
df_clima <- data.frame(dados_clima)
head(df_clima, 10)
nrow(df_clima)

#validacao valores unicos
unique(df[c("CULTURA")])
unique(df[c("NUCLEO_REGIONAL")])

#exploracao de dados
colSums(is.na(df))
describe(df)

tabyl(df, CULTURA)
vtree(df, "CULTURA", horiz = FALSE, palette = 10)

#ajuste de grafia das CULTURAS
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Mandioca  Indústria/Consumo Animal", "Mandioca Indústria/Consumo Animal")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "algodão", "Algodão")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Batata (1ª safra)", "Batata")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Batata (2ª safra)", "Batata")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Feijão (1ª safra)", "Feijão")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Feijão (2ª safra)", "Feijão")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Feijão (3ª safra)", "Feijão")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Milho (1ª safra)", "Milho")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Milho (2ª safra)", "Milho")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Soja (1ª safra)", "Soja")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Soja (2ª safra)", "Soja")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Tomate (1ª safra)", "Tomate")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Tomate (2ª safra)", "Tomate")
df$CULTURA <- replace(df$CULTURA, df$CULTURA == "Amendoim (1ª safra)", "Amendoim")

#exclusão de valores com menos de 500 ocorrências
df2 <- df[!(df$CULTURA == "Algodão" | df$CULTURA == "Canola" | df$CULTURA == "Centeio" | 
              df$CULTURA == "Cevada" | df$CULTURA == "Girassol" | df$CULTURA == "Mamona" | 
              df$CULTURA == "Rami" | df$CULTURA == "Sorgo "),]

nrow(df2)

#filtragem das CULTURAS excluindo valores vazis apos enriquecimento
df3 <- df2[(df2$CULTURA == "Café" | df2$CULTURA == "Feijão" | df2$CULTURA == "Mandioca" |
              df2$CULTURA == "Milho" | df2$CULTURA == "Soja" | df2$CULTURA == "Trigo"),]

#exploracao de dados
tabyl(df3, MUNICIPIO)
vtree(df2, "CULTURA", horiz = FALSE, palette = 8)

#ajuste tipos de dados
colnames(df3)
sapply(df3, class)
sapply(df3, mode)
double(length = 2)
df3 <- transform(df3, AREA_TOTAL = as.double(AREA_TOTAL))
df3 <- transform(df3, AREA_PERDIDA = as.double(AREA_PERDIDA))
df3 <- transform(df3, PRODUCAO = as.double(PRODUCAO))
df3 <- transform(df3, SAFRA = as.factor(SAFRA))
df3 <- transform(df3, CULTURA = as.factor(CULTURA))
df3 <- transform(df3, REGIAO = as.factor(REGIAO))

sapply(df3, class)
sapply(df3, mode)

#exclusao de valores nulos apos ajuste de tipos de dados
describe(df3)
df3 <- df3[complete.cases(df3), ]
describe(df3)

df3 <- df3[!(df3$PRODUCAO == 0),]
df3 <- df3[!(df3$AREA_TOTAL == 0),]
describe(df3)
nrow(df3)

#plotagem de graficos
aggregate(df3$PRODUCAO, by=list(Category=df3$SAFRA), FUN=sum)
tapply(df3$PRODUCAO, df3$SAFRA, FUN=sum)

sum_by_year <- aggregate(df3$PRODUCAO, by=list(Category=df3$SAFRA), FUN=sum)
colnames(sum_by_year)
ggplot(sum_by_year, aes(x = Category, y = x, fill = Category)) +
  geom_bar(stat = 'identity') + ggtitle("Produção agregada por safra") +
  xlab("Safra") + ylab("Produção (em toneladas)") + labs (fill = ("Safra")) +
  theme(plot.title = element_text(hjust = 0.5))


df4 <- df4 
df4$SAFRA <- as.Date(as.character(df3$SAFRA), "%Y")
head(df4)

aggregate(df4$PRODUCAO, by=list(Category=df4$SAFRA, df4$CULTURA), FUN=sum)

ggplot(data = df4, aes(x = SAFRA, y = PRODUCAO, group = CULTURA, color = CULTURA)) +
  geom_line() + scale_color_viridis(discrete = TRUE) +
  ggtitle("Produção agregada por safra e cultura") +
  xlab("Safra") + ylab("Produção (em toneladas)") + labs (fill = ("Cultura")) +
  scale_color_manual(values=c("#000000", "#263248", "#7E8AA2", "#FF9800", "#AB47BC",
                              "#22575C", "#1C8200", "#F0CA4D", "#E37B40", "#DE5B49",
                              "#4d73f0", "#5c2722", "#660082", "#40a8e3", "#49ccde",
                              "#F5EFE0", "#F0C418", "#23B99A", "#502a09", "#e53c2a",
                              "#1844f0", "#2f260f", "#4B385E", "#4b5e38", "#00080a")) +
  theme(legend.position = "bottom") + theme(legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(nrow =3))

#construcao de dataframe final do todos os conjuntos
df4 <- merge (df3, df_venda, by=c('CULTURA', 'SAFRA'))
df4 <- merge (df4, df_custo, by=c('CULTURA', 'SAFRA'))
df4 <- merge (df4, df_pib, by='SAFRA')
df4 <- merge (df4, df_dolar, by='SAFRA')

head(df4, 10)
unique(df4[c("CULTURA")])
df4 <- df4[complete.cases(df4), ]



df5 <- merge (df4, df_clima, by=c('NUCLEO_REGIONAL', 'SAFRA'))

head(df5, 10)
describe(df5)
df5 <- df5[complete.cases(df5), ]
describe(df5)

#plotagem da distribuição de CULTURA
vtree(df5, "CULTURA", horiz = FALSE, palette = 10)

#encode das variaveis categoricas
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

#preparacao dataframe para treinamento
df_scaled = df5
df_scaled$AREA_TOTAL <- scale(df5$AREA_TOTAL)
df_scaled$AREA_PERDIDA <- scale(df5$AREA_PERDIDA)
df_scaled$PRECO_VENDA_PRODUTOR <- scale(df5$PRECO_VENDA_PRODUTOR)
df_scaled$DOLAR <- scale(df5$DOLAR)
df_scaled$CUSTO <- scale(df5$CUSTO)
df_scaled$PIB_AGRO <- scale(df5$PIB_AGRO)
df_scaled$PRECIPTACAO_TOTAL <- scale(df5$PRECIPTACAO_TOTAL)
df_scaled$TOTAL_DIAS_S_CHUVA <- scale(df5$TOTAL_DIAS_S_CHUVA)
df_scaled$TOTAL_DIAS_C_CHUVA <- scale(df5$TOTAL_DIAS_C_CHUVA)
df_scaled$RADIACAO_GLOBAL <- scale(df5$RADIACAO_GLOBAL)
df_scaled$PRESSAO_ATM <- scale(df5$PRESSAO_ATM)
df_scaled$TEMP_AR <- scale(df5$TEMP_AR)
df_scaled$UMID_MAX <- scale(df5$UMID_MAX)


df_scaled[["CULTURA_ENC"]] <- encode_ordinal(df_scaled[["CULTURA"]])
df_scaled[["REGIAO_ENC"]] <- encode_ordinal(df_scaled[["REGIAO"]])
df_scaled[["NUCLEO_ENC"]] <- encode_ordinal(df_scaled[["NUCLEO_REGIONAL"]]) #
df_scaled[["MUNICIPIO_ENC"]] <- encode_ordinal(df_scaled[["MUNICIPIO"]]) #


sub_df <- subset(df_scaled, select = -c(NUCLEO_REGIONAL, MUNICIPIO, CULTURA,
                                        REGIAO, Estacao))

sub_df <- subset(df_scaled, select = -c(NUCLEO_REGIONAL, MUNICIPIO, CULTURA,
                                        REGIAO, Estacao, TOTAL_DIAS_C_CHUVA,
                                        RADIACAO_GLOBAL, UMID_MAX,
                                        TOTAL_DIAS_S_CHUVA, PRESSAO_ATM,
                                        VENTO_RAJADA_MAX))

sub_df$SAFRA <- as.numeric(levels(sub_df$SAFRA))[sub_df$SAFRA] 
nrow(sub_df)
colnames(sub_df)
colnames(df5)

#divisao em treino e teste
training.set <- sub_df[(sub_df$SAFRA != "2018"),]
test.set <- sub_df[(sub_df$SAFRA == "2018"),]

#Aprendizado de estrutura usando Hill Climbing
hc.boot.network = boot.strength(training.set, algorithm = "hc")
hc.avg.network.norm = averaged.network(hc.boot.network, threshold = 0.9)

#Validação Cruzada com a validação da variável alvo
hc.xval = bn.cv(hc.avg.network.norm,
             data=training.set,
             bn="hc",
             loss = "mse",
             k=10,
             loss.args = list(target = "PRODUCAO"))

hc.xval

#Visualização da Rede
bnviewer::viewer(hc.avg.network.norm, 
                 bayesianNetwork.title = "Aprendizado usando Hill Clibing",
                 bayesianNetwork.width = "500px", bayesianNetwork.height = "500px",
                 bayesianNetwork.layout = "layout_components")


#Ajuste parametros HC
hc_bayesian.fit <- bn.fit(hc.avg.network.norm,
                       data = training.set,
                       method="mle")

hc_bayesian.fit$PRODUCAO

#Predicao HC
hc_bayesian.predict <- predict(hc_bayesian.fit, "PRODUCAO", test.set)
real <- test.set[,"PRODUCAO"]
predict <- hc_bayesian.predict

#Validacao usando MAPE
mape_score <- StatMeasures::mape(real, predict)
mape_score

#Validacao da Densidade Real x Previsto
par(mfrow=c(2, 1))
d <- density(real)
p <- density(predict)
plot(d, type="n", main="Densidade Real")
polygon(d, col="green", border="darkgreen")
plot(p, type="n", main="Densidade Previsão")
polygon(p, col="blue", border="darkblue")

###########
#Aprendizado de estrutura usando Tabu Search
tabu.boot.network = boot.strength(training.set, algorithm = "tabu")
tabu.avg.network.norm = averaged.network(tabu.boot.network, threshold = 0.9)

#Validação Cruzada com a validação da variável alvo
tabu.xval = bn.cv(tabu.avg.network.norm,
                data=training.set,
                bn="tabu",
                loss = "mse",
                k=10,
                loss.args = list(target = "PRODUCAO"))

tabu.xval

#Visualização da Rede
bnviewer::viewer(tabu.avg.network.norm, 
                 bayesianNetwork.title = "Aprendizado usando Tabu Search",
                 bayesianNetwork.width = "500px", bayesianNetwork.height = "500px",
                 bayesianNetwork.layout = "layout_components")


#Ajuste parametros Tabu Search
tabu_bayesian.fit <- bn.fit(tabu.avg.network.norm,
                          data = training.set,
                          method="mle")

tabu_bayesian.fit$PRODUCAO

#Predicao Tabu Search
tabu_bayesian.predict <- predict(tabu_bayesian.fit, "PRODUCAO", test.set)
real <- test.set[,"PRODUCAO"]
predict <- tabu_bayesian.predict

#Validacao usando MAPE
mape_score <- StatMeasures::mape(real, predict)
mape_score

#Validacao da Densidade Real x Previsto
par(mfrow=c(2, 1))
d <- density(real)
p <- density(predict)
plot(d, type="n", main="Densidade Real")
polygon(d, col="green", border="darkgreen")
plot(p, type="n", main="Densidade Previsto")
polygon(p, col="blue", border="darkblue")


#Teste de predição Hill Climbing
head(test.set)
teste1 <- subset(head(test.set), select = -c(PRODUCAO))
pred_hcteste1 <- predict(hc_bayesian.fit, node = "PRODUCAO", data = teste1)
pred_hcteste1

#Teste de predição Tabu Search
head(test.set)
teste1 <- subset(head(test.set), select = -c(PRODUCAO))
pred_tsteste1 <- predict(tabu_bayesian.fit, node = "PRODUCAO", data = teste1)
pred_tsteste1
