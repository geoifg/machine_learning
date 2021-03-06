
########################################################################
########################################################################
###                                                                  ###
###      Script elaborado por �dipo Cremon para a disciplina de      ###
### Topicos Especiais em Intelig�ncia Artificial - Machine Learning  ###
##                        em dados geogr�ficos                       ###
###         pelo Instituto Federal de Goi�s - Campus Goi�nia         ###
###                                                                  ###
########################################################################
########################################################################

install.packages("caret")

library(ggplot2)


# importar dados csv -------------------------------------------------------------

dados_imobiliarios <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/dados_imobiliarios.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a vari�vel como o denominamos, no caso "dados_imobiliarios"
dados_imobiliarios

#Converter as vari�veis de num�ricas para factors (categ�ricas)
dados_imobiliarios$bairro <- as.factor(dados_imobiliarios$bairro)

#Conferir os dados convertidos para factors
str(dados_imobiliarios)


#Dividir as amostras de treinamento e valida��o
#No caso usaremos as mesmas amostras para 70% para calibra��o (treinamento) e 30% para valida��o
#Na calibra��o usaremos a abordagem de valida��o-cruzada de k-fold,
#onde as todo conjunto amostral ser� dividido em 10 partes (10-folds)
#S�o usadas 9 partes para gerar o modelo e 1 para valida��o. Isso repetido com todas as partes.
#O melhor modelo � escolhido. Essa valida��o cruzada servir� apenas como
#m�trica para avaliar o potencial do modelo
#a valida��o mesmo, ser� feita com os 30% de amostras independentes.

library(caret)
set.seed(100)
dados_ML <- as.vector(createDataPartition(dados_imobiliarios$preco, list=FALSE, p=0.7))
treinamento <- dados_imobiliarios[dados_ML,]
validacao <- dados_imobiliarios[-dados_ML,]


##Verificar quantas amostras de treinamento
nrow(treinamento)

##Verificar quantas amostras de valida��o
nrow(validacao)

names(treinamento)

#Definir as variaveis preditoras e a resposta
preditoras <- c("condominio",  "quartos",     "suites",     
                "vagas", "area", "bairro",
                "atualizacao", "distancia", "pm2")


resposta <- "preco"

##Elaborando o modelo de regress�o pelo m�todo de
## regress�o linear "lm"
set.seed(100)
lm_imobiliario <- train(x = treinamento[, preditoras], y = treinamento$preco, method = "lm",
                     trControl=trainControl(method = "CV", number = 10), metric="RMSE")


##Podemos fazer a regress�o normalizando os dados entre 0 e 1 com
## o comando method = range
lm_imobiliario <- train(x = treinamento[, preditoras], y = treinamento$preco, method = "lm",
                        trControl=trainControl(method = "CV", number = 10, method = "range"), metric="RMSE") 




#Para checar as m�tricas da calibra��o
print(lm_imobiliario)



#Para conferir os coeficientes do modelo
lm_imobiliario$finalModel

#Verificando a import�ncia das vari�veis
#No caso � baseado nos valores dos coeficientes 
#da regress�o linear
plot(varImp(lm_imobiliario))
importancia <- varImp(lm_imobiliario)

install.packages("Metrics")
library(Metrics)
valid_lm <- predict(lm_imobiliario, validacao)
rmse(validacao$preco, valid_lm)

#Plotar gr�fico entre o estimado e o dado real

#Cria um data.frame entre o dado estimado e o dado real
plot_reg <- as.numeric(data.frame(validacao$preco, valid_lm))
plot_reg$validacao.preco <- as.numeric(plot_reg$validacao.preco)


plot(plot_reg)


#Conferir a normalidade dos res�duos
#Primeiro, obtendo os res�duos
residuos <- validacao$preco - valid_lm

plot(residuos)

#aplicando o teste estat�stico Shapiro-Wilks
shapiro.test(residuos)

#Para melhor visualiza��o
qqnorm(residuos)
qqline(residuos)





