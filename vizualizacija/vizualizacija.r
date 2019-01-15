# 3. faza: Vizualizacija podatkov
source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")

library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)

# Uvozimo zemljevid Sveta
zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", mapa = "zemljevidi", pot.zemljevida = "", encoding = "UTF-8") %>% 
  fortify()


# Zemljevid sveta skrčimo na zemljevid Evrope
zemljevid <- filter(zemljevid, CONTINENT == "Europe" | SOVEREIGNT %in% c("Turkey", "Cyprus"), 
                 long < 45 & long > -45 & lat > 30 & lat < 75)

# Narišemo zemljevid Evrope
ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat, group=group, fill=id)) +
  guides(fill=FALSE) +labs(title="Evropa - osnovna slika")

#Narišemo grafe

#graf 6 držav- 3 z najvišjim BDP(Luxemburg,Norveška in Dansa) in 3 z najnižjim (Bolgarija, Romanija, Turčija)
drzave_BDP <- c("Bulgaria","Romania","Turkey","Luxembourg"," Norway", "Denmark", "Sweden")
graf_drzav_BDP <- ggplot(data= tabela_drzav %>% filter(drzave %in% drzave_BDP),aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP),aes(x=drzave,y= dnevni_kadilci),color="red", show.legend = TRUE) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= s_povisano_telesno_tezo),color="green", show.legend = TRUE)+
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= mesecno_prekomerno_pijancevanje),color="blue",show.legend = TRUE) +
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= niso_telesno_aktivni),color="orange",show.legend = TRUE) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple",show.legend = TRUE) 
print(graf_drzav_BDP)
graf_drzav_BDP <- graf_drzav_BDP+ theme(legend.position = "right") +
  scale_fill_continuous(guide = "colourbar") +
  scale_size(guide = "legend")

# graf glede na stopnnjo izobrazbe
graf_izobrazba <- ggplot(data= tabela_izobrazbe_spol %>% filter(spol == 'Total'),aes(x=izobrazba, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data=tabela_izobrazbe_spol %>% filter(spol == 'Total'),aes(x= izobrazba,y= dnevni_kadilci),color="red", show.legend = TRUE) + 
  geom_point(data= tabela_izobrazbe_spol %>% filter(spol == 'Total'), aes(x =izobrazba, y= s_povisano_telesno_tezo),color="green", show.legend = TRUE)+
  geom_point(data= tabela_izobrazbe_spol %>% filter(spol == 'Total'), aes(x =izobrazba, y= mesecno_prekomerno_pijancevanje),color="blue",show.legend = TRUE) +
  geom_point(data= tabela_izobrazbe_spol %>% filter(spol == 'Total'), aes(x =izobrazba, y= niso_telesno_aktivni),color="orange",show.legend = TRUE) + 
  geom_point(data= tabela_izobrazbe_spol %>% filter(spol == 'Total'), aes(x =izobrazba, y= nic_obrokov_sadja_in_zelenjave),color="purple",show.legend = TRUE) 
graf_izobrazba <- graf_izobrazba+ theme(legend.position = "right") +
  scale_fill_continuous(guide = "colourbar") +
  scale_size(guide = "legend")

print(graf_izobrazba)

#graf glede na spol
graf_spol <- ggplot(data= tabela_izobrazbe_spol %>% filter(izobrazba == 'All ISCED 2011 levels'),aes(x= spol, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data=tabela_izobrazbe_spol %>% filter(izobrazba == 'All ISCED 2011 levels'),aes(x= spol,y= dnevni_kadilci),color="red", show.legend = TRUE) + 
  geom_point(data= tabela_izobrazbe_spol %>% filter(izobrazba == 'All ISCED 2011 levels'), aes(x =spol, y= s_povisano_telesno_tezo),color="green", show.legend = TRUE)+
  geom_point(data= tabela_izobrazbe_spol %>% filter(izobrazba == 'All ISCED 2011 levels'), aes(x =spol, y= mesecno_prekomerno_pijancevanje),color="blue",show.legend = TRUE) +
  geom_point(data= tabela_izobrazbe_spol %>% filter(izobrazba == 'All ISCED 2011 levels'), aes(x =spol, y= niso_telesno_aktivni),color="orange",show.legend = TRUE) + 
  geom_point(data= tabela_izobrazbe_spol %>% filter(izobrazba == 'All ISCED 2011 levels'), aes(x =spol, y= nic_obrokov_sadja_in_zelenjave),color="purple",show.legend = TRUE) 
print(graf_spol)