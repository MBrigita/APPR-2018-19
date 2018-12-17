# 2. faza: Uvoz podatkov

# Uvoz podatkov
tabela_debelosti <- read_csv("podatki/hlth_ehis_bm1e_1_Data.csv")
tabela_kajenja <- read_csv("podatki/hlth_ehis_sk3e_1_Data.csv")
tabela_pijancevanja <- read_csv("podatki/hlth_ehis_al3e_1_Data.csv")
tabela_aktivnosti <- read.csv("podatki/hlth_ehis_pe2e_1_Data.csv")
tabela_hrane<- read_csv("podatki/hlth_ehis_fv3e_1_Data.csv")
tabela_BDP<- read_csv("podatki/a7df3683-554b-4911-9004-10584e4e2439_Data.csv")


for(i in list(tabela_debelosti,tabela_hrane, tabela_kajenja,tabela_pijancevanja,tabela_aktivnosti,tabela_BDP)){
  if(colnames(i[2]) == colnames(tabela_debelosti[2])){tabela_drzav= i}
  else{
    tabela_drzav <-full_join(tabela_drzav,i,by = NULL, copy=FALSE, suffix= c(".x", ".y"))
  }
}
