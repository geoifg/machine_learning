
########################################################################
########################################################################
###                                                                  ###
###      Script elaborado por Édipo Cremon para a disciplina de      ###
### Topicos Especiais em Inteligência Artificial - Machine Learning  ###
##                        em dados geográficos                       ###
###         pelo Instituto Federal de Goiás - Campus Goiânia         ###
###                                                                  ###
########################################################################
########################################################################

install.packages("caret")

library(ggplot2)


# importar dados csv -------------------------------------------------------------

dados_imobiliarios <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/dados_imobiliarios.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a variável como o denominamos, no caso "dados_imobiliarios"
dados_imobiliarios

#Converter as variáveis de numéricas para factors (categóricas)
dados_imobiliarios$bairro <- as.factor(dados_imobiliarios$bairro)

#Conferir os dados convertidos para factors
str(dados_imobiliarios)


#Dividir as amostras de treinamento e validação
#No caso usaremos as mesmas amostras para 70% para calibração (treinamento) e 30% para validação
#Na calibração usaremos a abordagem de validação-cruzada de k-fold,
#onde as todo conjunto amostral será dividido em 10 partes (10-folds)
#São usadas 9 partes para gerar o modelo e 1 para validação. Isso repetido com todas as partes.
#O melhor modelo é escolhido. Essa validação cruzada servirá apenas como
#métrica para avaliar o potencial do modelo
#a validação mesmo, será feita com os 30% de amostras independentes.

library(caret)
set.seed(100)
dados_ML <- as.vector(createDataPartition(dados_imobiliarios$preco, list=FALSE, p=0.7))
treinamento <- dados_imobiliarios[dados_ML,]
validacao <- dados_imobiliarios[-dados_ML,]


##Verificar quantas amostras de treinamento
nrow(treinamento)

##Verificar quantas amostras de validação
nrow(validacao)

names(treinamento)

#Definir as variaveis preditoras e a resposta
preditoras <- c("condominio",  "quartos",     "suites",     
                "vagas", "area", "bairro",
                "atualizacao", "distancia", "pm2")


resposta <- "preco"

##Elaborando o modelo de regressão pelo método de
## regressão linear "lm"
set.seed(100)
lm_imobiliario <- train(x = treinamento[, preditoras], y = treinamento$preco, method = "lm",
                     trControl=trainControl(method = "CV", number = 10), metric="RMSE")


##Podemos fazer a regressão normalizando os dados entre 0 e 1 com
## o comando method = range
lm_imobiliario <- train(x = treinamento[, preditoras], y = treinamento$preco, method = "lm",
                        trControl=trainControl(method = "CV", number = 10, method = "range"), metric="RMSE") 




#Para checar as métricas da calibração
print(lm_imobiliario)



#Para conferir os coeficientes do modelo
lm_imobiliario$finalModel

#Verificando a importância das variáveis
#No caso é baseado nos valores dos coeficientes 
#da regressão linear
plot(varImp(lm_imobiliario))
importancia <- varImp(lm_imobiliario)

install.packages("Metrics")
library(Metrics)
valid_lm <- predict(lm_imobiliario, validacao)
rmse(validacao$preco, valid_lm)

#Plotar gráfico entre o estimado e o dado real

#Cria um data.frame entre o dado estimado e o dado real
plot_reg <- as.numeric(data.frame(validacao$preco, valid_lm))
plot_reg$validacao.preco <- as.numeric(plot_reg$validacao.preco)


plot(plot_reg)


#Conferir a normalidade dos resíduos
#Primeiro, obtendo os resíduos
residuos <- validacao$preco - valid_lm

plot(residuos)

#aplicando o teste estatístico Shapiro-Wilks
shapiro.test(residuos)

#Para melhor visualização
qqnorm(residuos)
qqline(residuos)





