
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

install.packages("ggplot2")
install.packages("esquisse")

library(ggplot2)


# importar dados csv -------------------------------------------------------------

acid_2017 <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/Acid_tipo_2017.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a variável como o denominamos, no caso "dados_imobiliarios"
acid_2017

#O comando para verificar as primeiras observações (linhas) do banco é dado por head():
head(acid_2017)



#Para se verificar a estrutura do dado, isto é, o formato que está, o tipo de cada uma das suas variáveis e as dimensões, usamos str():
str(acid_2017)

acid_2017[,2:13]


##Vamos usar a A função gather() que "empilha" o data.frame.
#Ela é utilizada principalmente quando as colunas da base 
#não representam nomes de variáveis, mas sim seus valores.

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
