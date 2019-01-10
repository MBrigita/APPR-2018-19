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

#Narišemo graf

graf<- ggplot(data= tabela_drzav,aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data=tabela_drzav,aes(x=drzave,y= dnevni_kadilci),color="red") + 
  geom_point(data=tabela_drzav, aes(x = drzave, y= s_povisano_telesno_tezo),color="green")+
  geom_point(data=tabela_drzav, aes(x = drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
  geom_point(data=tabela_drzav, aes(x = drzave, y= niso_telesno_aktivni),color="orange") + 
  geom_point(data=tabela_drzav, aes(x = drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") 

print(graf)

najvisji.BDP <- c("Luxembourg"," Norway", "Denmark", "Sweden","Ireland")
najnizji.BDP <- c("Bulgaria","Romania","Turkey","Croatia")
graf_najvisjih <- ggplot(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP),aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP),aes(x=drzave,y= dnevni_kadilci),color="red") + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= s_povisano_telesno_tezo),color="green")+
  geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
  geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= niso_telesno_aktivni),color="orange") + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") 

graf_najnizji<- ggplot(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP),aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP),aes(x=drzave,y= dnevni_kadilci),color="red") + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= s_povisano_telesno_tezo),color="green")+
  geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
  geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= niso_telesno_aktivni),color="orange") + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") 

print(graf_najvisjih)
print(graf_najnizji)
#graf_kadilcev <- ggplot(tabela_drzav %>% filter(dnevni_kadilci > 20), aes(x=drzave, y=dnevni_kadilci,color= drzave)) + geom_point() + theme_bw()
#print(graf_kadilcev)

