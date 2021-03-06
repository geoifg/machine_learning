
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

install.packages("ggplot2")
install.packages("esquisse")

library(ggplot2)


# importar dados csv -------------------------------------------------------------

acid_2017 <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/Acid_tipo_2017.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a vari�vel como o denominamos, no caso "dados_imobiliarios"
acid_2017

#O comando para verificar as primeiras observa��es (linhas) do banco � dado por head():
head(acid_2017)



#Para se verificar a estrutura do dado, isto �, o formato que est�, o tipo de cada uma das suas vari�veis e as dimens�es, usamos str():
str(acid_2017)

acid_2017[,2:13]


##Vamos usar a A fun��o gather() que "empilha" o data.frame.
#Ela � utilizada principalmente quando as colunas da base 
#n�o representam nomes de vari�veis, mas sim seus valores.

library(tidyr)
acid_2017_ok <- acid_2017 %>% gather(key = BR, value = Value, 2:13)


library(randomcoloR)
n <- 16
palette <- distinctColorPalette(n)

ggplot(acid_2017_ok) +
  aes(x = BR, fill = TIPOS, weight = Value) +
  geom_bar() +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  coord_flip()





ggplot(acid_2017_ok) +
  aes(x = BR, fill = TIPOS, weight = Value) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()
