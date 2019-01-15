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


#graf ki prikazuje kako izobrazba in spol vplivata na kajenje
g_kajenja <- ggplot(data=tabela_izobrazbe_spol, aes(x=tabela_izobrazbe_spol$izobrazba, y=dnevni_kadilci, fill=spol)) 
g_kajenja<- g_kajenja +geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=dnevni_kadilci), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(limits=c("All ISCED 2011 levels", 
                            "Less than primary, primary and lower secondary education (levels 0-2)",
                            "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", 
                            "Tertiary education (levels 5-8)")) +
  theme_minimal()
print(g_kajenja)
# graf ki prikazuje kako izobrazna in spol vplivata na povišano telesno težo
g_debelosti <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=s_povisano_telesno_tezo, fill=spol)) 
g_debelosti<- g_debelosti +geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=s_povisano_telesno_tezo), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Greens")+
  scale_x_discrete(limits=c("All ISCED 2011 levels", 
                            "Less than primary, primary and lower secondary education (levels 0-2)",
                            "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", 
                            "Tertiary education (levels 5-8)")) +
  theme_minimal()
print(g_debelosti)

#graf, ki prikazuje kako izobrazba in spol vplivata na telesno aktivnost
g_niso_aktivni <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=niso_telesno_aktivni, fill=spol)) 
g_niso_aktivni<- g_niso_aktivni +geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=niso_telesno_aktivni), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Purples")+
  scale_x_discrete(limits=c("All ISCED 2011 levels", 
                            "Less than primary, primary and lower secondary education (levels 0-2)",
                            "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", 
                            "Tertiary education (levels 5-8)")) +
  theme_minimal() +  coord_flip()
print(g_niso_aktivni)

#graf, ki prikazuje kako izobrazba in spol vplivata na uživanje sadja in zelenjave
graf_nic_sz <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=nic_obrokov_sadja_in_zelenjave, fill=spol))

graf_nic_sz<- graf_nic_sz + geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=nic_obrokov_sadja_in_zelenjave), vjust=1.6, position = position_dodge(0.9),
            color="white", size=3.5)+
  scale_fill_brewer(palette="Blues")+ 
  scale_x_discrete(limits=c("All ISCED 2011 levels", 
                            "Less than primary, primary and lower secondary education (levels 0-2)",
                            "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", 
                            "Tertiary education (levels 5-8)")) +
  theme_minimal()
print(graf_nic_sz)
#graf, ki prikazuje kao izobbrazba in spol vplivata na mesečno pijančevanje
graf_pijancevanje <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=mesecno_prekomerno_pijancevanje, fill=spol))

graf_pijancevanje<- graf_pijancevanje + geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=mesecno_prekomerno_pijancevanje), vjust=1.6, position = position_dodge(0.9),
            color="white", size=3.5)+
  scale_fill_brewer(palette="Reds")+
  scale_x_discrete(limits=c("All ISCED 2011 levels", 
                            "Less than primary, primary and lower secondary education (levels 0-2)",
                            "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", 
                            "Tertiary education (levels 5-8)")) +
  theme_minimal() 
print(graf_pijancevanje)

