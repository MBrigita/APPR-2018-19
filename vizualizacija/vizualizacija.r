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
Evropa <- ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat, group=group, fill=id)) +
  guides(fill=FALSE) +labs(title="Evropa - osnovna slika")

#Narišemo grafe

#graf 6 držav- 3 z najvišjim BDP(Luxemburg,Norveška in Danska) in 3 z najnižjim (Bolgarija, Romanija, Turčija)
drzave_BDP <- c("Bulgaria","Romania","Turkey","Luxembourg","Norway", "Denmark")
graf_drzav_BDP <- ggplot(data= tabela_drzav %>% filter(drzave %in% drzave_BDP),aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP),aes(x=drzave,y= dnevni_kadilci),color="red", size=7,shape="*") + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= s_povisano_telesno_tezo),color="green", size=7,shape="*")+
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= mesecno_prekomerno_pijancevanje),color="blue",size=7,shape="*") +
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= niso_telesno_aktivni),color="orange",size=7,shape="*") + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% drzave_BDP), aes(x =drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple",size=7,shape="*") 
#print(graf_drzav_BDP)
graf_drzav_BDP <- graf_drzav_BDP+ theme(legend.position = "right") +
  scale_fill_continuous(guide = "colourbar") +
  scale_size(guide = "legend") + ylab("delez prebivalstva v procentih")

#pogledamo ali je mogoče kakšna korelacija med BDP per capita in drugimi meritvami, pri tem vzamemo proč vrstice, ki nimajo podatkov
corr <- cor((tabela_drzav[c(3,4,6,9,10)])[c(-2,-11,-20),])
#corrplot(podatki, method = "number")

#khm <- cor(tabela_izobrazbe_spol[c(-1,-2,-5,-7,-8,-11,-12)],)
#corrplot(khm, method = "number", title = "Korelacijamed  meritvami")

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

#print(graf_izobrazba)


#graf ki prikazuje kako izobrazba in spol vplivata na kajenje
g_kajenja <- ggplot(data=tabela_izobrazbe_spol, aes(x=tabela_izobrazbe_spol$izobrazba, y=dnevni_kadilci, fill=spol)) 
g_kajenja<- g_kajenja +geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=dnevni_kadilci), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Reds")+
  scale_x_discrete(limits=c("skupaj", 
                            "primarna",
                            "sekundarna", 
                            "terciarna")) +
  theme_minimal()
#print(g_kajenja)
# graf ki prikazuje kako izobrazna in spol vplivata na povišano telesno težo
g_debelosti <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=s_povisano_telesno_tezo, fill=spol)) 
g_debelosti<- g_debelosti +geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=s_povisano_telesno_tezo), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Greens")+
  scale_x_discrete(limits=c("skupaj", 
                            "primarna",
                            "sekundarna", 
                            "terciarna")) +
  theme_minimal()
#print(g_debelosti)

#graf, ki prikazuje kako izobrazba in spol vplivata na telesno aktivnost
g_niso_aktivni <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=niso_telesno_aktivni, fill=spol)) 
g_niso_aktivni<- g_niso_aktivni +geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=niso_telesno_aktivni), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Oranges")+
  scale_x_discrete(limits=c("skupaj", 
                            "primarna",
                            "sekundarna", 
                            "terciarna")) +
  theme_minimal()


#graf, ki prikazuje kako izobrazba in spol vplivata na uživanje sadja in zelenjave
g_nic_sz <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=nic_obrokov_sadja_in_zelenjave, fill=spol))

g_nic_sz<- g_nic_sz + geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=nic_obrokov_sadja_in_zelenjave), vjust=1.6, position = position_dodge(0.9),
            color="white", size=3.5)+
  scale_fill_brewer(palette="Purples")+ 
  scale_x_discrete(limits=c("skupaj", 
                            "primarna",
                            "sekundarna", 
                            "terciarna")) +
  theme_minimal()
#print(g_nic_sz)
#graf, ki prikazuje kako izobrazba in spol vplivata na mesečno pijančevanje
g_pijancevanje <- ggplot(data=tabela_izobrazbe_spol, aes(x=izobrazba, y=mesecno_prekomerno_pijancevanje, fill=spol))

g_pijancevanje<- g_pijancevanje + geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=mesecno_prekomerno_pijancevanje), vjust=1.6, position = position_dodge(0.9),
            color="white", size=3.5)+
  scale_fill_brewer(palette="Blues") +
  scale_x_discrete(limits=c("skupaj", 
                            "primarna",
                            "sekundarna", 
                            "terciarna"))+
  theme_minimal() 
#print(g_pijancevanje)
 #spodaj so že zemljevidi glede na države za posamezno kategorijo

#zemljevid dnevnih kadilcev
zemljevid_dk <- ggplot() + geom_polygon(data=left_join(zemljevid, tabela_drzav, by=c("NAME"="drzave")),
                                 aes(x=long, y=lat, group=group, fill=dnevni_kadilci)) +
  ggtitle("zemljevid dnevnih kadilcev") +  scale_fill_continuous(
    low= "yellow", high="red", name = "procent dnevnih kadilcev") + 
  geom_point(aes(x=6, y=50)) + geom_text(aes(x=6, y=50), label = "Luxembourg") +
  geom_point(aes(x=23, y=42)) + geom_text(aes(x=23, y=42), label = "Sofia") + 
  geom_point(aes(x=14.4, y=46)) + geom_text(aes(x=14, y=45), label = "Ljubljana")

#zemljevid ljudi s povišano telesno težo
zemljevid_ptt <- ggplot() + geom_polygon(data=left_join(zemljevid, tabela_drzav, by=c("NAME"="drzave")),
                                         aes(x=long, y=lat, group=group, fill=s_povisano_telesno_tezo)) +
  ggtitle("zemljevid ljudi s povišano telesno težo") +  scale_fill_continuous(
    low= "Green", high="black", name = "procent ljudi") + 
  geom_point(aes(x=6, y=50)) + geom_text(aes(x=6, y=50), label = "Luxembourg") +
  geom_point(aes(x=23, y=42)) + geom_text(aes(x=23, y=42), label = "Sofia") +
  geom_point(aes(x=14.4, y=46)) + geom_text(aes(x=14, y=45), label = "Ljubljana")


#zemljevid ljudi ki niso telesne aktivnosti
zemljevid_ta <-ggplot() + geom_polygon(data=left_join(zemljevid, tabela_drzav, by=c("NAME"="drzave")),
                                       aes(x=long, y=lat, group=group, fill=niso_telesno_aktivni)) +
  ggtitle("zemljevid deleža prebivalstva, ki niso telesno aktivni") + scale_fill_continuous(
    low= "white", high="orange", name = "procent ljudi") +
  geom_point(aes(x=6, y=50)) + geom_text(aes(x=6, y=50), label = "Luxembourg") +
  geom_point(aes(x=23, y=42)) + geom_text(aes(x=23, y=42), label = "Sofia") + 
  geom_point(aes(x=14.4, y=46)) + geom_text(aes(x=14, y=45), label = "Ljubljana")

#zemljevid ljudi ki ne zaužjejo nič sadja in zelenjave
zemljevid_nsz <-ggplot() + geom_polygon(data=left_join(zemljevid, tabela_drzav, by=c("NAME"="drzave")),
                                       aes(x=long, y=lat, group=group, fill=nic_obrokov_sadja_in_zelenjave)) +
  ggtitle("zemljevid deleža prebivalstva, ki ne je sadja in zelenjave") + scale_fill_continuous(
    low= "pink", high="purple", name = "procent ljudi") + geom_point(aes(x=6, y=50)) + geom_text(aes(x=6, y=50), label = "Luxembourg") +
  geom_point(aes(x=23, y=42)) + geom_text(aes(x=23, y=42), label = "Sofia") + 
  geom_point(aes(x=14.4, y=46)) + geom_text(aes(x=14, y=45), label = "Ljubljana")

#zemljevid ki prikazuje mesečno prekomerno pijančevanje
zemljevid_pp <- ggplot() + geom_polygon(data=left_join(zemljevid, tabela_drzav, by=c("NAME"="drzave")),
                                        aes(x=long, y=lat, group=group, fill=mesecno_prekomerno_pijancevanje)) +
  ggtitle("zemljevid prekomernega mesečnega pijančevanja")  + geom_point(aes(x=14.4, y=46)) + geom_text(aes(x=14, y=45), label = "Ljubljana") + 
  geom_point(aes(x=6, y=50)) + geom_text(aes(x=6, y=50), label = "Luxembourg") +
  geom_point(aes(x=23, y=42)) + geom_text(aes(x=23, y=42), label = "Sofia") 

                                          