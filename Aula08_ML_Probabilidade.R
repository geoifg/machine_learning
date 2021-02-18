
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
amostras <- st_read("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RASTER/Ptos_agua_naoagua.gpkg")

     
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
amostras_xvars$Class_num <- as.factor(as.character(amostras_xvars$Class_num))
amostras_xvars$Class <- as.factor(as.character(amostras_xvars$Class))

str(amostras_xvars)

     
#Como o algoritmo não trabalho com raster ou vetor
#iremos converter o dado em data.frame
xvars_df <- as.data.frame(amostras_xvars)

#Excluir a coluna "geom" e Class_num
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
names(treinamento_class)

##Verificar quantas amostras de validao
nrow(validacao_class)

head(treinamento_class)
summary(treinamento_class)

plot(treinamento_class$Class)



#O comando trainContol determinará a abordagem do classificador
#o comando classProbs e summaryFunction só funciona com variáveis do tipo texto
control <- trainControl(method = "cv",
                        number = 5,
                        classProbs = TRUE,
                        savePredictions = TRUE,
                        summaryFunction = twoClassSummary)

##Modelo
modelo_RF_prob <- train(Class ~ ., data = treinamento_class, method = "rf", metric = 'ROC',
              trControl=control)

print(modelo_RF_prob)

plot(modelo_RF_prob)



#Obtendo a importância das variáveis
#Para a opção varImp  type = 2 (default) é utilizadaa média do décrescimo médio de Gini 'MeanDecreaseGini' 
# que se baseia no índice de impureza de Gini utilizado para o cálculo dos nós das árvores de decisão. 
#Alternativamente, você pode definir o tipo = 1, então a medida calculada é a diminuição média da exatidão (mean decrease in accuracy) .
#Algoritmo RF

plot(varImp(modelo_RF_prob,type=2))

#Importancia para as classes individualmente
importancia <- varImp(modelo_RF_prob)
plot(importancia)



#Criando o modelo em arquivo raster
rf_pred <- predict(xvars, modelo_RF_prob, filename="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RF_agua_prob.tif", type="prob", 
              progress="window", overwrite=TRUE)




#Plotar modelo no R
plot(rf_pred) 


#Validação

#Matriz de confusao e estatisticas do modelo com as amostras de validação
#Primeiro se aplica o modelo no dado de validação
RFpred <- as.factor(predict(modelo_RF_prob, validacao_class))

#Em seguida é criado uma matrix (table) entre 
#o dado de validação e o predito pelo modelo
RF_tabela <- table(as.factor(validacao_class$Class), RFpred)

#Se calcula a matriz de confusão com as métricas estatísticas
confusionMatrix(RF_tabela)



##Calcular ROC e AUC

## Obtendo a curva ROC a partir dos dados de validação
roc <- roc(validacao_class$Class,
            predict(modelo_RF_prob, validacao_class, type = "prob")[,1],
            levels = rev(levels(validacao_class$Class)))
roc


library(ROCR)
library(pROC) 
#Obtendo o valor de AUC
auc(roc)

#Obtendo o plot da curva ROC
plot(roc)



