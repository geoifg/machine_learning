
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


      
install.packages(c("rgdal","randomForest","raster","sf", "caret"),dependencies=TRUE)
library(rgdal)
library(raster)
library(sf)
library(caret)
      
#Comando para ler todos os arquivos raster em geotiff em um dado diretÃ³rio
variaveis <- list.files(path="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RASTER", 
                                 pattern =".tif$", full.names=TRUE)
 
variaveis
     
# Comando stack faz o empilhamento dos dados raster em um arquivo sÃ³.
# Aqui chamaremos os raster empilhados de xvars
xvars <- stack(variaveis) 

xvars

# Com o comando "st_read" nós leremos o arquivo vetorial em gpkg 
# de pontos das amostras de treinamento      
amostras <- st_read("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RASTER/Ptos_UsoeCob.gpkg")

     
#Para plotar os pontos
plot(amostras)     
      

# Extrair valores do raster para os pontos e 
# manter valores da tabela de atributos
# aqui usaremos as biliotecas tibble e dplyr para facilitar
amostras_xvars <- raster::extract(xvars, amostras) %>% 
  tibble::as_tibble() %>% 
  dplyr::bind_cols(amostras, .)
amostras_xvars

head(amostras_xvars)

#Declarando a variável resposta como dado categórico
amostras_xvars$Class_num <- as.factor(amostras_xvars$Class_num)
amostras_xvars$Class <- as.factor(amostras_xvars$Class)
str(amostras_xvars)

     
#Como o algoritmo não trabalho com raster ou vetor
#iremos converter o dado em data.frame
xvars_df <- as.data.frame(amostras_xvars)

xvars_df$geom <- NULL
xvars_df$Class_num <- NULL


#Dividir as amostras de treinamento e validação
#No caso usaremos 70% de amostras para treinamento e 30% validação.
#A divisão entre 70 e 30% será baseada com base no dado de classes
set.seed(100)
xvars_df2 <- as.vector(createDataPartition(xvars_df$Class, list=FALSE, p=0.7))
treinamento_class <- xvars_df[xvars_df2,]
validacao_class <- xvars_df[-xvars_df2,]

##Verificar quantas amostras de treinamento
nrow(treinamento_class)
str(treinamento_class)

##Verificar quantas amostras de validao
nrow(validacao_class)

head(treinamento_class)

##Elaborando o modelo C4.5
library(caret)
set.seed(100)
modelo_c45 <- train(Class ~ .,  data = treinamento_class, method = "J48",
                   trControl=trainControl(method = "CV", number = 5), metric="Accuracy")

print(modelo_c45)

#Conferindo a árvore de decisão
modelo_c45$finalModel



##Elaborando o modelo C5.0
library(C50)
set.seed(100)
modelo_c50 <- train(Class ~ .,  data = treinamento_class, method = "C5.0",
                    trControl=trainControl(method = "CV", number = 5), metric="Accuracy")

print(modelo_c50)


##Elaborando o modelo RF
modelo_RF <- train(Class ~ .,  data = treinamento_class, method = "rf",
                   trControl=trainControl(method = "CV", number = 5),
                   metric="Accuracy", ntree = 500, mtry=2, importance=TRUE)
print(modelo_RF)


##Plot comparando os modelos gerais para os acidentes
comparacao <- resamples(list(C4.5=modelo_c45, C5.0=modelo_c50, RF=modelo_RF))
# boxplots das validações
bwplot(comparacao)

summary(comparacao)


#Obtendo a importância das variáveis
#Para a opção varImp  type = 2 (default) é utilizadaa média do décrescimo médio de Gini 'MeanDecreaseGini' 
# que se baseia no índice de impureza de Gini utilizado para o cálculo dos nós das árvores de decisão. 
#Alternativamente, você pode definir o tipo = 1, então a medida calculada é a diminuição média da exatidão (mean decrease in accuracy) .
#Algoritmo RF

plot(varImp(modelo_RF,type=2))

#Importancia para as classes individualmente
importancia <- varImp(modelo_RF)
plot(importancia)



#Criando o modelo em arquivo raster
rf_pred <- predict(xvars, modelo_RF, filename="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RF_LULC.tif", type="raw", 
              progress="window", overwrite=TRUE)

c45_pred <- predict(xvars, modelo_c45, filename="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/c45_LULC.tif", type="raw", 
                   progress="window", overwrite=TRUE)

c50_pred <- predict(xvars, modelo_c50, filename="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/c50_LULC.tif", type="raw", 
                     index=1, na.rm=TRUE, progress="window", overwrite=TRUE)


#Plotar modelo no R
plot(rf_pred) 

plot(c45_pred)

plot(c50_pred)

#Matriz de confusao e estatisticas do modelo com as amostras de validação
#Primeiro se aplica o modelo no dado de validação
RFpred <- as.factor(predict(modelo_RF, validacao_class, type="raw"))

#Em seguida é criado uma matrix (table) entre 
#o dado de validação e o predito pelo modelo
RF_tabela <- table(validacao_class$Class, RFpred)

#Se calcula a matriz de confusão com as métricas estatísticas
confusionMatrix(RF_tabela)


