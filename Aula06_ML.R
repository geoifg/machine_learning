
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


      
install.packages(c("rgdal","randomForest","raster","sp", "rpart", "rpart.plot", "sf", "caret"),dependencies=TRUE)
library(rgdal)
library(raster)
library(rpart)
library(rpart.plot)
library(sf)
library(caret)
      
#Comando para ler todos os arquivos raster em geotiff em um dado diretório
variaveis <- list.files(path="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RASTER", 
                                 pattern =".tif$", full.names=TRUE)
 
variaveis
     
# Comando stack faz o empilhamento dos dados raster em um arquivo só.
# Aqui chamaremos os raster empilhados de xvars
xvars <- stack(variaveis) 

xvars

# Com o comando "st_read" n�s leremos o arquivo vetorial em gpkg 
# de pontos das amostras de treinamento      
amostras <- st_read("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RASTER/Ptos_UsoeCob.gpkg")

     
#Para plotar os pontos
plot(amostras)     
      
      
# Para extrair o valor de pixel do arquivo raster empilhado para as amostras de treinamento
amostras_xvars <- raster::extract(xvars, amostras)

head(amostras_xvars)

# Extrair valores do raster para os pontos e 
# manter valores da tabela de atributos
# aqui usaremos as biliotecas tibble e dplyr para facilitar
amostras_xvars <- raster::extract(xvars, amostras) %>% 
  tibble::as_tibble() %>% 
  dplyr::bind_cols(amostras, .)
amostras_xvars

head(amostras_xvars)

#Declarando a vari�vel resposta como dado categ�rico
amostras_xvars$Class_num <- as.factor(amostras_xvars$Class_num)

str(amostras_xvars)

##Para salvar o shapefile 
#com os valores de pixel extraidos dos raster
st_write(amostras_xvars,"D:/Documentos/IFG/MACHINE_LEARNING/DADOS/Ptos_agua_naoagua_xvars.gpkg")      
      
#Como o algoritmo n�o trabalho com raster ou vetor
#iremos converter o dado em data.frame
xvars_df <- as.data.frame(amostras_xvars)

xvars_df$geom <- NULL
xvars_df$Class_num <- NULL

#Dividir as amostras de treinamento e valida��o
#No caso usaremos 70% de amostras para treinamento e 30% valida��o.
#A divis�o entre 70 e 30% ser� baseada com base no dado de classes
set.seed(100)
xvars_df2 <- as.vector(createDataPartition(xvars_df$Class, list=FALSE, p=0.7))
treinamento_agua <- xvars_df[xvars_df2,]
validacao_agua <- xvars_df[-xvars_df2,]

##Verificar quantas amostras de treinamento
nrow(treinamento_agua)
str(treinamento_agua)

##Verificar quantas amostras de validao
nrow(validacao_agua)

head(treinamento_agua)


## Criando um modelo de Arvore de decisao pelo algortimo CART
fitrpart <- rpart(treinamento_agua$Class ~ .,  data = treinamento_agua, method="class")

#Crit�rios para podar a �rvore (minsplit = 20, minbucket = 7)
      


#Apresentar a Arvore de Decisao
print(fitrpart)
      
printcp(fitrpart)
      
      
#Plot da Arvore de Decisão
rpart.plot::prp(fitrpart)
      
#Plot da Arvore de decisão em outro modelo
rpart.plot(fitrpart)
      
#Rankeamento das importâncias das variaveis
Importancia <- as.data.frame(fitrpart$variable.importance)

#Para listar as variáveis mais importantes dado pelo modelo CART      
Importancia
      

      
#Criando o modelo em arquivo raster
rpart_pred <- predict(xvars, fitrpart, filename="D:/Documentos/IFG/MACHINE_LEARNING/DADOS/CART_LULC.tif", type="class", 
              index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

#Plotar modelo no R
plot(rpart_pred)      



#Matriz de confusao e estatisticas do modelo
#Primeiro se aplica o modelo no dado de valida��o
rpartpred <- as.factor(predict(fitrpart, validacao_agua, type="class"))

#Em seguida � criado uma matrix (table) entre 
#o dado de valida��o e o predito pelo modelo
CART_tabela <- table(validacao_agua$Class, rpartpred)

#Se calcula a matriz de confus�o com as m�tricas estat�sticas
confusionMatrix(CART_tabela)


