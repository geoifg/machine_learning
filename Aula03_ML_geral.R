
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

dados_imobiliarios <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/dados_imobiliarios.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a vari�vel como o denominamos, no caso "dados_imobiliarios"
dados_imobiliarios

#O comando para verificar as primeiras observa��es (linhas) do banco � dado por head():
head(dados_imobiliarios)



#Para se verificar a estrutura do dado, isto �, o formato que est�, o tipo de cada uma das suas vari�veis e as dimens�es, usamos str():
str(dados_imobiliarios)



##Embora a coluna "bairro" no data.frame seja um dado num�rico, essa informa��o
##corresponde a um dado categ�rico. Portanto devetor transformar esse dado
##para categ�rico que no R, se chama factor

dados_imobiliarios$bairro <- as.factor(dados_imobiliarios$bairro)


#Para se verificar a estrutura do dado, isto �, o formato que est�, o tipo de cada uma das suas vari�veis e as dimens�es, usamos str():
str(dados_imobiliarios)


library(ggplot2)
ggplot(dados_imobiliarios) +
  aes(x = pm2, y = preco) +
  geom_point() +
  labs(title = "Rela��o entre pre�o do im�vel e pre�o por m�") +
  theme_minimal()




ggplot(dados_imobiliarios) +
  aes(x = pm2, y = preco, size = distancia) +
  geom_point() +
  labs(title = "Rela��o entre pre�o do im�vel e pre�o por m�") +
  theme_minimal()


ggplot(dados_imobiliarios) +
  aes(x = pm2, y = preco, colour = bairro, size = area ) +
  geom_point() +
  scale_color_hue() +
  labs(title = "Rela��o entre pre�o do im�vel e pre�o por m�") +
  theme_minimal()

#Exemplos com gr�fico de box-plot
ggplot(dados_imobiliarios) +
  aes(x = bairro, y = pm2) +
  geom_boxplot() +
  theme_classic()


ggplot(dados_imobiliarios) +
  aes(x = bairro, y = pm2, fill = bairro) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_classic()


#Exemplos com gr�fico de violin-plot
ggplot(dados_imobiliarios) +
  aes(x = bairro, y = pm2, fill = bairro) +
  geom_violin() +
  scale_fill_hue() +
  theme_classic()


ggplot(dados_imobiliarios) +
  aes(x = bairro, y = pm2, fill = bairro) +
  geom_violin() +
  aes(x = bairro, y = pm2) +
  geom_boxplot(width=0.1, fill="gray", alpha=0.5) +
  scale_fill_hue() +
  theme_classic()


##Exemplo com dados de valida��o fotogrametrica

dados_foto <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/P_9_1_N.csv", header = TRUE, sep = ";",  dec = ".")

names(dados_foto)


colnames(dados_foto)<-c("Pontos","Erro E(m)","Erro N(m)","Erro h(m)")


ggplot(dados_foto) +
  aes(x = `Erro E(m)`, y = `Erro N(m)`, fill = `Erro h(m)`, colour = `Erro h(m)`) +
  geom_point() +
  scale_fill_distiller(palette = "RdYlGn") +
  scale_color_distiller(palette = "RdYlGn") +
  theme_classic() +
  theme(legend.position = "bottom")

library(ggforce)
library(tibble)


  
# Criando os par�metros dos circulos
circulos <- data.frame(x0 = 0, y0 = 0,r = seq (from=0 , to=0.4, by = 0.2 ))
  
 
ggplot(dados_foto) +
   aes(x = `Erro E(m)`, y = `Erro N(m)`, size = `Erro h(m)`) +
   geom_point(size = 3L, colour = "#0c4c8a") +
   theme_classic() +
   ggforce::geom_circle (data = circulos,
                         aes(x0=x0, y0=y0, r=r),
                         color = 'light gray',
                         inherit.aes=FALSE) +
   coord_fixed()         
               
#Outro exemplo
ggplot(dados_foto) +
  aes(x = `Erro E(m)`, y = `Erro N(m)`, fill = `Erro h(m)`, colour = `Erro h(m)`) +
  geom_point() +
  scale_fill_distiller(palette = "RdYlGn") +
  scale_color_distiller(palette = "RdYlGn") +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggforce::geom_circle (data = circulos,
                        aes(x0=x0, y0=y0, r=r),
                        color = 'light gray',
                        inherit.aes=FALSE) +
  coord_fixed() 
  