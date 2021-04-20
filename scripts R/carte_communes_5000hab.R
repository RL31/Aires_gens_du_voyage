library(tidyverse)
library(sf)
library(extrafont)
loadfonts(device = "win")


# Carte des communes de plus de 5000 habitants
fond_com_pop <- st_read(paste0(here::here("voyageurs","data"),"/COMMUNE.shp"),options="ENCODING=UTF-8") %>%
  select(POPULATION) %>% 
  st_centroid() %>% 
  mutate(x=st_coordinates(.)[,1],
         y=st_coordinates(.)[,2]) %>% 
  filter(POPULATION>5000)

# Fond régional de l'IGN (ADMIN-EXPRESS)
fond_reg <- st_read(paste0(here::here("voyageurs","data"),"/REGION.shp"),options="ENCODING=UTF-8")


ggplot()+
  geom_sf(data=fond_reg,color="gray60",fill="seashell2")+
  geom_point(data=fond_com_pop,aes(x=x,y=y),color="gray30",shape=21,alpha=.7)+
  labs(title = "Communes de plus de 5000 habitants",
       caption = "Source : IGN, ADMIN-EXPRESS\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(text = element_text(family="Calibri"),
        plot.title = element_text(size=15,face="bold"),
        plot.caption=element_text(size=9, face = "italic", colour = "grey40"),
        plot.caption.position = "plot")+
  guides(size=guide_legend(override.aes = list(fill=NA)),
         color=guide_legend(override.aes = list(shape=15,size = 6)))


ggsave(paste0(here::here("voyageurs","output"),"/communes_5000.jpg"),width = 20,height =17,units = "cm",dpi=300 )
