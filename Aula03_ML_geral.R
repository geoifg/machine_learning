
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

dados_imobiliarios <- read.csv("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/dados_imobiliarios.csv", header = TRUE, sep = ";",  dec = ".")

#Para mostrar os dados chamamos usamos a variável como o denominamos, no caso "dados_imobiliarios"
dados_imobiliarios

#O comando para verificar as primeiras observações (linhas) do banco é dado por head():
head(dados_imobiliarios)



#Para se verificar a estrutura do dado, isto é, o formato que está, o tipo de cada uma das suas variáveis e as dimensões, usamos str():
str(dados_imobiliarios)



##Embora a coluna "bairro" no data.frame seja um dado numérico, essa informação
##corresponde a um dado categórico. Portanto devetor transformar esse dado
##para categórico que no R, se chama factor

dados_imobiliarios$bairro <- as.factor(dados_imobiliarios$bairro)


#Para se verificar a estrutura do dado, isto é, o formato que está, o tipo de cada uma das suas variáveis e as dimensões, usamos str():
str(dados_imobiliarios)


library(ggplot2)
ggplot(dados_imobiliarios) +
  aes(x = pm2, y = preco) +
  geom_point() +
  labs(title = "Relação entre preço do imóvel e preço por m²") +
  theme_minimal()




ggplot(dados_imobiliarios) +
  aes(x = pm2, y = preco, size = distancia) +
  geom_point() +
  labs(title = "Relação entre preço do imóvel e preço por m²") +
  theme_minimal()


ggplot(dados_imobiliarios) +
  aes(x = pm2, y = preco, colour = bairro, size = area ) +
  geom_point() +
  scale_color_hue() +
  labs(title = "Relação entre preço do imóvel e preço por m²") +
  theme_minimal()

#Exemplos com gráfico de box-plot
ggplot(dados_imobiliarios) +
  aes(x = bairro, y = pm2) +
  geom_boxplot() +
  theme_classic()


ggplot(dados_imobiliarios) +
  aes(x = bairro, y = pm2, fill = bairro) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_classic()


#Exemplos com gráfico de violin-plot
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


##Exemplo com dados de validação fotogrametrica

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


  
# Criando os parâmetros dos circulos
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
  