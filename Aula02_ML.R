
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



# importar dados csv -------------------------------------------------------------

dados_imobiliarios <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/dados_imobiliarios.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a vari�vel como o denominamos, no caso "dados_imobiliarios"
dados_imobiliarios

#O comando para verificar as primeiras observa��es (linhas) do banco � dado por head():
head(dados_imobiliarios)



#Para se verificar a estrutura do dado, isto �, o formato que est�, o tipo de cada uma das suas vari�veis e as dimens�es, usamos str():
str(dados_imobiliarios)


#Para plotar uma estat�stica b�sica sobre os dados, podemos usar o comando summary()
summary(dados_imobiliarios)

#Para gerar uma gr�fico de boxplot de uma vari�vel esperc�fica do data frame, usamos $
boxplot(dados_imobiliarios$preco)

#Para gerar uma matriz de gr�ficos de dispers�o entre as vari�veis do data.frame podemos usar o comando pairs()
pairs(dados_imobiliarios)

#Melhorando essa matriz de gr�ficos de dispers�o
#fun��o retirada do help(pairs) para histograma
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

#fun��o retirada do help(pairs) para cor
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#Agora sim, chamando a matriz de gr�ficos de dispers�o
pairs(dados_imobiliarios, diag.panel = panel.hist, upper.panel = panel.cor)






##Trabalhando com pacotes
#Instalando o pacote "corrplot"

install.packages("corrplot")

#Chamando o pacote "corrplot"
library(corrplot)

# primeiro fazemos a matriz de correla��o
M <- cor(dados_imobiliarios)
corrplot(M, method = "ellipse")






#Trabalhando com dados geogr�ficos vetoriais e raster
#Primeiro, vamos instalar os pacotes raster, sf, rgdal e sp


install.packages("raster, sf, rgdal, sp")

library(sf)

# importar dados em geogr�fico usando o pacote "sf" e o comando "read_sf", no caso no formato geopackge
RM_Goiania_UDH <- read_sf("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RM_Goiania_UDH.gpkg")

#para plotar os dados espacializados
plot(RM_Goiania_UDH)

#Para plotar uma coluna espec�fica do nosso dado
plot(RM_Goiania_UDH["ivs"])

#Para plotar algumas colunas apenas do nosso dado
plot(RM_Goiania_UDH[10:15])



# Verificando as classes do data.frame usando o comanto "str()"---------------
str(RM_Goiania_UDH)




##Lendo arquivo raster

library(raster)
library(sp)
library(rgdal)
#Coloque o caminho do diret�rio onde est� seu arquivo raster, 
# aqui no caso � um MDE (Modelo Digital de Eleva��o) em formato tif

MDE <- raster("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/MDE.tif")


#Verificar os atributos do MDE
MDE

## Cria um histograma do raster
hist(MDE, main="Distribui��o dos valores de eleva��o", 
     col= "purple", 
     maxpixels=32000000)

#Criar uma imagem do MDE
image(MDE)

# Podemos especificar cores para essa imagem
col <- terrain.colors(7)
image(MDE, zlim=c(650,1100), main="Modelo Digital de Eleva��o (MDE)", col=col)
