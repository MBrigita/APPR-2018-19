tabela$UNIT <- NULL
tabela$`TIME` <- NULL
tabela$`Flag and Footnotes` <- NULL
tabela$BMI <- gsub("Normal", "Normalen", tabela$BMI)
tabela$BMI <- gsub("Obese", "Debelost", tabela$BMI)
tabela$BMI <- gsub("Underweight", "Podhranjen", tabela$BMI)
tabela$GEO <- gsub("^Germany.*", "Germany", tabela$GEO)
tabela$GEO <- gsub("^European.*", "European Union", tabela$GEO)