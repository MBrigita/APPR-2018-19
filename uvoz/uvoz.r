# 2. faza: Uvoz podatkov

# Uvoz podatkov_za_tabelo_drzav
tabela_debelosti <- read_csv("podatki/drzave/hlth_ehis_bm1e_1_Data.csv",locale=locale(encoding="Windows-1252"))
tabela_kajenja <- read_csv("podatki/drzave/hlth_ehis_sk3e_1_Data.csv", locale=locale(encoding="Windows-1252"))
tabela_pijancevanja <- read_csv("podatki/drzave/hlth_ehis_al3e_1_Data.csv",locale=locale(encoding="Windows-1252"))
tabela_aktivnosti <- read_csv("podatki/drzave/hlth_ehis_pe2e_1_Data.csv",locale=locale(encoding="Windows-1252"))
tabela_hrane<- read_csv("podatki/drzave/hlth_ehis_fv3e_1_Data.csv", locale=locale(encoding="Windows-1252"))
tabela_BDP<- read_csv("podatki/drzave/a7df3683-554b-4911-9004-10584e4e2439_Data.csv",locale=locale(encoding="Windows-1252"))

#uvoz podatkov za tebelo_izobrazbe
tabela_pijancevanja_si <- read_csv("podatki/izobrazba,spol/alkohol.csv",locale=locale(encoding="Windows-1252"))
tabela_kajenja_si <-read_csv("podatki/izobrazba,spol/kajenje.csv",locale=locale(encoding="Windows-1252"))
tabela_aktivnosti_si<- read_csv("podatki/izobrazba,spol/telesna_aktivnost.csv", locale=locale(encoding="Windows-1252"))
tabela_hrane_si <- read_csv("podatki/izobrazba,spol/hrana.csv",locale=locale(encoding="Windows-1252"))
tabela_debelosti_si <- read_csv("podatki/izobrazba,spol/debelost.csv",locale=locale(encoding="Windows-1252"))

#precisti podatke za tabelo BDP
names(tabela_BDP)[5] <- "BDP_per_capita(US$)"
names(tabela_BDP)[3] <- "GEO"
tabela_BDP[1:2] <- NULL
tabela_BDP[2] <- NULL
tabela_BDP <- slice(tabela_BDP,1:32)
tabela_BDP$GEO <- gsub("^Slovak.*", "Slovakia", tabela_BDP$GEO)
tabela_BDP$GEO <- gsub("^Czech.*", "Czechia", tabela_BDP$GEO)

# funkcija ki cisti nepotrebne stolpce
brisanje <- function(tabela){
  tabela$`Flag and Footnotes` <- NULL
  tabela$TIME <- NULL
  tabela$UNIT <- NULL
  tabela$AGE <- NULL
  tabela$GEO <- gsub("^European.*", "European Union", tabela$GEO)
  if(i ==1){
    tabela$SEX <- NULL
    tabela$ISCED11 <- NULL
    tabela$GEO <- gsub("^Germany.*", "Germany", tabela$GEO)
  }
  else{
    tabela$GEO <- NULL
    tabela$SEX <- sub("Males", "moski",tabela$SEX)
    tabela$SEX <- sub("Females", "zenske",tabela$SEX)
  }
  return(tabela)
}

# zanke, ki uredijo tabele
i = 0
for(tabela in list(tabela_pijancevanja,tabela_pijancevanja_si)){
  i = i+1
  tabela <- split(tabela, tabela[1])
  names(tabela[[1]])[8] <- "tedensko_prekomerno_pijancevanje"
  names(tabela[[2]])[8] <- "mesecno_prekomerno_pijancevanje"
  tabela[[1]]$FREQUENC <- NULL
  tabela[[2]]$FREQUENC <- NULL
  tabela <-tabela <- full_join(tabela[[1]],tabela[[2]], by = NULL, copy=FALSE, suffix =c(".tabela[[1]]",".tabela[[2]]"))
  if(i == 1){
    tabela_pijancevanja <- brisanje(tabela)
  }
  else{
    tabela_pijancevanja_si <- brisanje(tabela)
    i = 0
  }
}

for(tabela in list(tabela_kajenja,tabela_kajenja_si)){
  i = i+1
  tabela$SMOKING <- NULL
  names(tabela)[7] <- "dnevni_kadilci"
  if( i ==1){
    tabela_kajenja <- brisanje(tabela)
  }
  else{
    tabela_kajenja_si <- brisanje(tabela)
    i = 0
  }
}

tabela_aktivnosti$Flag.and.Footnotes  <- NULL

for(tabela in list(tabela_aktivnosti, tabela_aktivnosti_si)){
  i = i+1
  tabela<- split(tabela,tabela$DURATION)
  names(tabela[[1]])[8] <- "vec_kot_150_min_telesno_aktivni"
  names(tabela[[2]])[8] <- "do 150min_telesno_aktivni"
  names(tabela[[3]])[8] <- "niso_telesno_aktivni"
  tabela[[1]]$DURATION <- NULL
  tabela[[2]]$DURATION <- NULL
  tabela[[3]]$DURATION <- NULL
  pomozna_t1 <-full_join(tabela[[1]],tabela[[2]], by = NULL, copy=FALSE, suffix =c(".tabela[[1]]",".tabela[[2]]"))
  tabela<- full_join(pomozna_t1, tabela[[3]], by = NULL, copy=FALSE, suffix =c(".pomozna_t1",".tabela[[3]]"))
  if(i == 1){
    tabela_aktivnosti <- brisanje(tabela)
  }
  else{
    tabela_aktivnosti_si <- brisanje(tabela)
    i =0
  }
} 

for(tabela in list(tabela_hrane,tabela_hrane_si)){
  i = i+1
  tabela<- split(tabela, tabela$N_PORTION)
  names(tabela[[3]])[8] <- " ena-stiri_obrokov_sadja_in_zelenjave"
  names(tabela[[2]])[8] <- "pet+_obrkov_sadja_in_zelenjave"
  names(tabela[[1]])[8] <- "nic_obrokov_sadja_in_zelenjave"
  tabela[[1]]$N_PORTION <- NULL
  tabela[[2]]$N_PORTION <- NULL
  tabela[[3]][1] <- NULL
  pomozna_t2 <-full_join(tabela[[1]],tabela[[2]], by = NULL, copy=FALSE, suffix =c(".tabela[[1]]",".tabela[[2]]"))
  tabela <- full_join(pomozna_t2, tabela[[3]], by = NULL, copy=FALSE, suffix =c(".pomozna_t2",".tabela[[3]]"))
  if(i == 1){
    tabela_hrane <- brisanje(tabela)
  }
  else{
    tabela_hrane_si <- brisanje(tabela)
    i = 0
  }
}

for(tabela in list(tabela_debelosti,tabela_debelosti_si)){
  i = i+1
  tabela$BMI <- NULL
  names(tabela)[7] <- "s_povisano_telesno_tezo"
  if(i ==1){
    tabela_debelosti<- brisanje(tabela)
  }
  else{
    tabela_debelosti_si <- brisanje(tabela)
    i = 0
  }
}

#naredi tabelo glede na drazve
for(i in list(tabela_debelosti,tabela_BDP, tabela_kajenja,tabela_pijancevanja,tabela_aktivnosti,tabela_hrane)){
  if(colnames(i[2]) == colnames(tabela_debelosti[2])){tabela_drzav= i}
  else{
    tabela_drzav <-full_join(tabela_drzav,i,by = NULL, copy=FALSE, suffix= c(".x", ".y"))
  }
}

#naredi tabelo glede na izobrazbo in spol
for(i in list(tabela_debelosti_si, tabela_kajenja_si,tabela_pijancevanja_si,tabela_aktivnosti_si,tabela_hrane_si)){
  if(colnames(i[3]) == colnames(tabela_debelosti_si[3])){tabela_izobrazbe_spol= i}
  else{
    tabela_izobrazbe_spol <-full_join(tabela_izobrazbe_spol,i,by = NULL, copy=FALSE, suffix= c(".x", ".y"))
  }
}
names(tabela_drzav)[1] <- "drzave"
names(tabela_izobrazbe_spol)[1] <- "izobrazba"
names(tabela_izobrazbe_spol)[2] <- "spol"

tabela_izobrazbe_spol$izobrazba[1:3] <- c("skupaj","skupaj","skupaj")
tabela_izobrazbe_spol$izobrazba[4:6] <-c("primarna","primarna","primarna")
tabela_izobrazbe_spol$izobrazba[7:9] <- c("sekundarna","sekundarna","sekundarna")
tabela_izobrazbe_spol$izobrazba[10:12] <- c("terciarna","terciarna","terciarna")

#izbrisemo nepotrebne elemente
remove(tabela_aktivnosti, tabela_aktivnosti_si, tabela_BDP,tabela_debelosti, tabela_debelosti_si,
       tabela_hrane, tabela_hrane_si, tabela_kajenja, tabela_kajenja_si, tabela_pijancevanja,
       tabela_pijancevanja_si, i, pomozna_t1, pomozna_t2, tabela)
