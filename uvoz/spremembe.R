tabela_debelosti$BMI <- NULL
names(tabela_debelosti)[7] <- "procent ljudi s povisano telesno tezo"
#tabela$BMI <- gsub("Normal", "Normalen", tabela$BMI)
#tabela$BMI <- gsub("Obese", "Debelost", tabela$BMI)
#tabela$BMI <- gsub("Underweight", "Podhranjen", tabela$BMI)

tabela_kajenja$SMOKING <- NULL
names(tabela_kajenja)[7] <- "procent dnevnih kadilcev"

tabela_pijancevanja <- split(tabela_pijancevanja, tabela_pijancevanja$FREQUENC)
names(tabela_pijancevanja[[1]])[8] <- "procent ljudi, ki tedensko prekomerno pijancujejo"
names(tabela_pijancevanja[[2]])[8] <- "procent ljudi, ki vsak mesec prekomerno pijancujejo"
tabela_pijancevanja[[1]]$FREQUENC <- NULL
tabela_pijancevanja[[2]]$FREQUENC <- NULL
tabela_pijancevanja <- full_join(tabela_pijancevanja[[1]],tabela_pijancevanja[[2]], by = NULL, copy=FALSE, suffix =c(".tabela_pijancevanja[[1]]",".tabela_pijancevanja[[2]]"))


tabela_aktivnosti$Flag.and.Footnotes  <- NULL
tabela_aktivnosti <- split(tabela_aktivnosti,tabela_aktivnosti$DURATION)
names(tabela_aktivnosti[[1]])[8] <- "procent ljudi,ki namenijo več kot 150 min na teden športu"
names(tabela_aktivnosti[[2]])[8] <- " procent ludi, ki so do 150min telesno aktivni na teden"
names(tabela_aktivnosti[[3]])[8] <- "procent ljudi, tedensko ne namenijo nič časa športnim aktivnostim"
tabela_aktivnosti[[1]]$DURATION <- NULL
tabela_aktivnosti[[2]]$DURATION <- NULL
tabela_aktivnosti[[3]]$DURATION <- NULL
pomozna_t1 <-full_join(tabela_aktivnosti[[1]],tabela_aktivnosti[[2]], by = NULL, copy=FALSE, suffix =c(".tabela_aktivnosti[[1]]",".tabela_aktivnosti[[2]]"))
tabela_aktivnosti <- full_join(pomozna_t1, tabela_aktivnosti[[3]], by = NULL, copy=FALSE, suffix =c(".pomozna_t1",".tabela_aktivnosti[[3]]"))

tabela_hrane <- split(tabela_hrane, tabela_hrane$N_PORTION)
names(tabela_hrane[[3]])[8] <- "procent ljudi, ki pojejo 1-4 obroke sadja in zelenjave na dan"
names(tabela_hrane[[2]])[8] <- "procent ljudi, ki pojejo 5+ sadja in zelenjave na dan"
names(tabela_hrane[[1]])[8] <- "procent ljudi, ki ne zaužijejo nič sadja in zelenjave na dan"
tabela_hrane[[1]]$N_PORTION <- NULL
tabela_hrane[[2]]$N_PORTION <- NULL
tabela_hrane[[3]][1] <- NULL
pomozna_t2 <-full_join(tabela_hrane[[1]],tabela_hrane[[2]], by = NULL, copy=FALSE, suffix =c(".tabela_hrane[[1]]",".tabela_hrane[[2]]"))
tabela_hrane <- full_join(pomozna_t2, tabela_hrane[[3]], by = NULL, copy=FALSE, suffix =c(".pomozna_t2",".tabela_hrane[[3]]"))

names(tabela_BDP)[5] <- "BDP per capita(US$)"
names(tabela_BDP)[3] <- "GEO"
tabela_BDP[1:2] <- NULL
tabela_BDP[2] <- NULL
tabela_BDP <- slice(tabela_BDP,1:32)
tabela_BDP$GEO <- gsub("^Slovak.*", "Slovakia", tabela_BDP$GEO)
tabela_BDP$GEO <- gsub("^Czech.*", "Czechia", tabela_BDP$GEO)

#zbriše samo za zadnjo podtabelo
brisanje_prvega_stolpca <- function(tabela){
  for(podtabela in length(tabela)){
    tabela[[podtabela]][1] <- NULL
  }
  return(tabela)
}

brisanje <- function(tabela){
  tabela$`Flag and Footnotes` <- NULL
  tabela$SEX <- NULL
  tabela$TIME <- NULL
  tabela$UNIT <- NULL
  tabela$ISCED11 <- NULL
  tabela$AGE <- NULL
  tabela$GEO <- gsub("^Germany.*", "Germany", tabela$GEO)
  tabela$GEO <- gsub("^European.*", "European Union", tabela$GEO)
  return(tabela)
}

tabela_debelosti <- brisanje(tabela_debelosti)
tabela_kajenja <- brisanje(tabela_kajenja)
tabela_pijancevanja <- brisanje(tabela_pijancevanja)
tabela_aktivnosti <- brisanje(tabela_aktivnosti)
tabela_hrane <- brisanje(tabela_hrane)


