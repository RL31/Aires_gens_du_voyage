library(tidyverse)
library(sf)
library(stringi)
library(extrafont)
loadfonts(device = "win")

############################################################
# Transformation des données de W Acker en sf à la commune #
############################################################
acker <- read.csv(paste0(here::here("Aires_gens_du_voyage","data"),"/aires-d-accueil-william-acker-visionscarto-2021-04-14.csv"),
                  encoding = "UTF-8",
                  colClasses = c("depno"="character") ) %>% 
  filter(!is.na(lon)) %>% # une donnée avec longitude manquante
  select(lon,lat,depno) %>% 
  st_as_sf(coords=c("lon","lat"),crs=4326) %>%  # conversion en objet géographique
  st_transform(crs=2154) # L93

# Fond communal de l'IGN (ADMIN-EXPRESS)
fond_com <- st_read(paste0(here::here("Aires_gens_du_voyage","data"),"/COMMUNE.shp"),options="ENCODING=UTF-8") %>% 
  mutate(ville_ok= str_to_upper(stri_trans_general(str_replace_all(NOM_COM,"[:punct:]|[:space:]|[:digit:]|-",""),"latin-ascii")) )

acker_communes <- st_intersection(fond_com,acker) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  count(INSEE_COM,NOM_COM,INSEE_DEP,ville_ok) %>% 
  mutate(source=1)

# donnees_acker_communes_ok <- st_intersection(fond_com %>% select(INSEE_COM,NOM_COM),acker) %>% 
#   st_drop_geometry() %>% 
#   as.data.frame()
# write.csv(donnees_acker_communes_ok,paste0(here::here(Aire_gens_duvoyage,"output"),"/aires-d-accueil-william-acker-communes-corr-2021-04-14.csv"))

####################################################################
# Travail sur la base finess pour avoir un sf de finess (code 218) #
####################################################################
finess <- read.csv(paste0(here::here("Aires_gens_du_voyage","data"),"/export_finess_20210415.csv"),sep=";") %>%
  mutate(across(everything(), ~str_remove_all(.x,pattern="=|CEDEX") ),
         X.Libelle.routage=str_replace(str_to_upper(X.Libelle.routage)," ST ","SAINT "),
         X.Libelle.routage=str_replace(str_to_upper(X.Libelle.routage),"^ST ","SAINT "),
         X.Libelle.routage=str_replace(str_to_upper(X.Libelle.routage),"^STE ","SAINTE "),
         X.Libelle.routage=str_replace(str_to_upper(X.Libelle.routage)," STE ","SAINTE "),
         ville_ok= str_to_upper(stri_trans_general(str_replace_all(X.Libelle.routage,"[:punct:]|[:space:]|[:digit:]|-",""),"latin-ascii")),
         ville_ok=case_when(ville_ok=="CHATEAUGONTIERSURMAYENN" ~ "CHATEAUGONTIERSURMAYENNE",
                            ville_ok=="FRANCONVILLELAGARENNE" ~ "FRANCONVILLE",
                            TRUE ~ ville_ok)) %>% 
  count(depno=substr(X.Code.postal,1,2),ville_ok) %>% 
  mutate(source=2)


finess_communes <- finess %>% 
  left_join(fond_com,by=c("depno"="INSEE_DEP","ville_ok")) %>% 
  mutate(INSEE_DEP=depno) %>% 
  select(INSEE_COM,NOM_COM,INSEE_DEP,ville_ok,n,source)

# Les deux sources dans une seule table
base_ok <- fond_com %>% 
  right_join(finess_communes %>% bind_rows(acker_communes) %>% select(INSEE_COM,n,source),by="INSEE_COM") %>% 
  st_centroid() %>% 
  mutate(x=st_coordinates(.)[,1],
         y=st_coordinates(.)[,2])

# Fond régional de l'IGN (ADMIN-EXPRESS)
fond_reg <- st_read(paste0(here::here("Aires_gens_du_voyage","data"),"/REGION.shp"),options="ENCODING=UTF-8")

ggplot()+
  geom_sf(data=fond_reg,color="gray60",fill="linen")+
  geom_point(data=base_ok,aes(x=x,y=y,size=n,color=as.factor(source),fill=as.factor(source)),shape=21,alpha=.5)+
  scale_color_manual(name="",
                     values=c("2"="#f13d6c","1"="#140c7c"),
                     labels=c("2"="Finess","1"="W. Acker"))+
  scale_fill_manual(name="",
                    values=c("2"="#f13d6c","1"="#140c7c"),
                    labels=c("2"="Finess","1"="W. Acker"))+
  scale_size_continuous(breaks=c(1,4),range=c(2,5))+
  facet_wrap(~source, labeller = as_labeller(c("2"="selon Finess","1"="selon W. Acker")), strip.position = "bottom")+
  labs(title = "Recensement des aires \"d'accueil\": Finess, le mal-nommé",
       subtitle = "Comparaison des aires recensées par William Acker avec celles présentes dans la base (Finess)\ndu ministère en charge des Affaires sociales",
       size="Nombre d'aires\ndans la commune",
       caption = "Source : W. Acker, \"Où sont les gens du voyage ?\"\nDrees, Finess au 15 avril 2021\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(text = element_text(family="Calibri"),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=8),
        plot.caption=element_text(size=7, face = "italic", colour = "grey40"),
        plot.caption.position = "plot")+
  guides(size=guide_legend(override.aes = list(fill=NA)),
         color="none",fill="none")#guide_legend(override.aes = list(shape=15,size = 6)))

ggsave(paste0(here::here("Aires_gens_du_voyage","output"),"/comparaison_finess_acker.jpg"),width = 20,height =10,units = "cm",dpi=300 )
