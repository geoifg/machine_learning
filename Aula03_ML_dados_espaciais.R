
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

install.packages("ggplot2, ggplot")
install.packages("esquisse")
install.packages("raster, sf, rgdal, sp")
library(ggplot2)
library(sf)
library(raster)
library(ggspatial) # Elementos espaciais no ggplot2

#Trabalhando com dados geográficos vetoriais e raster
#Primeiro, vamos instalar os pacotes raster, sf, rgdal e sp




# importar dados em geográfico usando o pacote "sf" e o comando "read_sf", no caso no formato geopackge
RM_Goiania_UDH <- read_sf("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/RM_Goiania_UDH.gpkg")

names(RM_Goiania_UDH)
str(RM_Goiania_UDH)

#para plotar os dados espacializados
plot(RM_Goiania_UDH)

#Para plotar uma coluna específica do nosso dado
plot(RM_Goiania_UDH["ivs"])

#Para plotar algumas colunas apenas do nosso dado
plot(RM_Goiania_UDH[10:15])


# Elaborando representações espaciais (mapas*) -----------------------------
# Limite da Região Metropolitana de Goiânia
ggplot() +
  geom_sf(data = RM_Goiania_UDH)


# Limite da Região Metropolitana (RM) de Goiânia e preenchimento
ggplot() +
  geom_sf(data = RM_Goiania_UDH, color = "black", fill = NA)


# IVS em cores + preenchimento de limites + coords + themes com fundo branco
map <- ggplot() +
  geom_sf(data = RM_Goiania_UDH, aes(fill = ivs), color = NA) +
  geom_sf(data = RM_Goiania_UDH, color = "black", fill = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() 

map

# IVS em cores invertidas + preenchimento de limites + coords + themes com fundo branco
map <- ggplot() +
  geom_sf(data = RM_Goiania_UDH, aes(fill = ivs), color = NA) +
  geom_sf(data = RM_Goiania_UDH, color = "black", fill = NA) +
  scale_fill_viridis_c(option = "magma", trans = "reverse") +
  theme_bw() 

map

# IVS em cores + preenchimento de limites + coords + themes + barra de escala + norte
map <- ggplot() +
  geom_sf(data = RM_Goiania_UDH, aes(fill = ivs), color = NA) +
  geom_sf(data = RM_Goiania_UDH, color = "black", fill = NA) +
  scale_fill_viridis_c(option = "magma", trans = "reverse") +
  theme_bw() +
  annotation_scale(location = "bl", width_hint = .3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)

map



# Para exportar
ggsave(filename = "IVS_RM_Goiania.png",
       plot = map,
       path = "D:/Documentos/IFG/MACHINE_LEARNING/",
       width = 20, 
       height = 20, 
       units = "cm", 
       dpi = 300)

#Usando o ggplot2 com dado raster

#Coloque o caminho do diretório onde está seu arquivo raster, 
# aqui no caso é um MDE (Modelo Digital de Elevação) em formato tif

MDE <- raster("D:/Documentos/IFG/MACHINE_LEARNING/DADOS/MDE.tif")

#Usando o ggplot com dado raster e a função geom_raster

#Converte o MDE em data.frame
MDE.df <- as.data.frame(MDE, xy=TRUE)

head(MDE.df)

ggplot() +
  geom_raster(aes(x=x,y=y,fill=MDE),data=MDE.df)+
  labs(x='Longitude',y='Latitude',
       title="Modelo Digital de Elevação",
       subtitle='Ribeirão João Leite',
       caption='Fonte: MDE AW3D30')+
  scale_fill_gradientn(colours = terrain.colors(225))+
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed',
                                        size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,color = 'black'),
        panel.ontop = TRUE)





