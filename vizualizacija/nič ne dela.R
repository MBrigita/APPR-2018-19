

graf_kadilcev <- ggplot(tabela_drzav %>% filter(dnevni_kadilci > 20), aes(x=drzave, y=dnevni_kadilci,color= drzave)) + geom_point() + theme_bw()
print(graf_kadilcev)
graf<- ggplot(data= tabela_drzav,aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data=tabela_drzav,aes(x=drzave,y= dnevni_kadilci),color="red") + 
  geom_point(data=tabela_drzav, aes(x = drzave, y= s_povisano_telesno_tezo),color="green")+
  geom_point(data=tabela_drzav, aes(x = drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
  geom_point(data=tabela_drzav, aes(x = drzave, y= niso_telesno_aktivni),color="orange") + 
  geom_point(data=tabela_drzav, aes(x = drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") 
 
print(graf)
# graf6 <- graf6 + geom_point(data=hap_change_tbt, aes(x=rn, y=Algeria, group=1), color="#004d99") + geom_line(data=hap_change_tbt, aes(x=rn, y=Algeria, group=1), color="#004d99")
# graf6 <- graf6 + geom_point(data=hap_change_tbt, aes(x=rn, y=Venezuela, group=1), color="#ff8c1a") + geom_line(data=hap_change_tbt, aes(x=rn, y=Venezuela, group=1), color="#ff8c1a")
#


graf<- ggplot(data= tabela_drzav,aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data=tabela_drzav,aes(x=drzave,y= dnevni_kadilci),color="red") + 
  geom_point(data=tabela_drzav, aes(x = drzave, y= s_povisano_telesno_tezo),color="green")+
  geom_point(data=tabela_drzav, aes(x = drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
  geom_point(data=tabela_drzav, aes(x = drzave, y= niso_telesno_aktivni),color="orange") + 
  geom_point(data=tabela_drzav, aes(x = drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") 

print(graf)

najvisji.BDP <- c("Luxembourg"," Norway", "Denmark", "Sweden","Ireland")
najnizji.BDP <- c("Bulgaria","Romania","Turkey","Croatia")
# graf_najvisjih <- ggplot(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP),aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP),aes(x=drzave,y= dnevni_kadilci),color="red") + 
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= s_povisano_telesno_tezo),color="green")+
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= niso_telesno_aktivni),color="orange") + 
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najvisji.BDP), aes(x =drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") +
#    theme(legend.position = "right") + scale_color_manual(name = "Legenda",values = colors)
# 
# graf_najnizji<- ggplot(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP),aes(x=drzave, y = seq(from = 0, to = 100, by = 10))) + 
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP),aes(x=drzave,y= dnevni_kadilci),color="red") + 
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= s_povisano_telesno_tezo),color="green")+
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= mesecno_prekomerno_pijancevanje),color="blue") +
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= niso_telesno_aktivni),color="orange") + 
#   geom_point(data= tabela_drzav %>% filter(drzave %in% najnizji.BDP), aes(x =drzave, y= nic_obrokov_sadja_in_zelenjave),color="purple") +
#   scale_colour_brewer() + theme(legend.position = "right")
# 
# print(graf_najvisjih)
# print(graf_najnizji)
#graf_kadilcev <- ggplot(tabela_drzav %>% filter(dnevni_kadilci > 20), aes(x=drzave, y=dnevni_kadilci,color= drzave)) + geom_point() + theme_bw()
#print(graf_kadilcev)


***
  
  # Napredna analiza podatkov
  
  ```{r analiza, echo=FALSE, message=FALSE}
source("analiza/analiza.r", encoding="UTF-8")
```

Spodnji graf prikazuje povezavo med številom naselij in površino občine.

```{r graf, echo=FALSE, fig.align='center', fig.cap='Povezava med številom naselij in površino občine'}
ggplot(inner_join(obcine, data.frame(obcina=names(skupine),
                                     skupina=factor(skupine)), by="obcina")
       , aes(x=povrsina, y=naselja, color=skupina, size=prebivalci/1000)) + geom_point() +
  ggtitle("Število naselij glede na površino občine") +
  xlab(expression("Površina (km"^2 * ")")) + ylab("Št. naselij") +
  guides(color=guide_legend(title="Skupina"),
         size=guide_legend(title="Prebivalci (* 1000)"))
```


zgori zadnja vrstica v prvem odtavku
runtime: shiny
***
  
  ```{r shiny, echo=FALSE}
shinyAppDir("shiny", options=list(width="100%", height=600))



drzave_z <- unique(zemljevid$NAME)
drzave_z <- as.data.frame(drzave, stringsAsFactors=FALSE)
tt  <-left_join(drzave_z, tabela_drzav, by="drzave")

output$druzine <- DT::renderDataTable({
  dcast(druzine, obcina ~ velikost.druzine, value.var="stevilo.druzin") %>%
    rename(`Občina`=obcina)
})

output$pokrajine <- renderUI(
  selectInput("pokrajina", label="Izberi pokrajino",
              choices=c("Vse", levels(obcine$pokrajina)))
)
output$naselja <- renderPlot({
  main <- "Pogostost števila naselij"
  if (!is.null(input$pokrajina) && input$pokrajina %in% levels(obcine$pokrajina)) {
    t <- obcine %>% filter(pokrajina == input$pokrajina)
    main <- paste(main, "v regiji", input$pokrajina)
  } else {
    t <- obcine
  }
  ggplot(t, aes(x=naselja)) + geom_histogram() +
    ggtitle(main) + xlab("Število naselij") + ylab("Število občin")
})
})

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("zdravje ljudi v Evropi"),
  
  tabsetPanel(
    tabPanel("Velikost družine",
             DT::dataTableOutput("druzine")),
    
    tabPanel("Število naselij",
             sidebarPanel(
               uiOutput("pokrajine")
             ),
             mainPanel(plotOutput("naselja")))
    
    
    
    
    
kategorije <- c("Dnevni kadilci", "število ljudi s povišano telesno težo", "delež ljudi, ki niso telesno aktivni",
                "procent prebivalstva, ki ne je sadja in zelenjave", 
                "delež ljudi, ki mesečno prekomerno pijančuje")
graf_drzave <- ggplot(data= tabela_drzav %>% filter(drzave %in% "Slovenia"),aes(x=kategorije, y = seq(from = 0, to = 100, by = 10))) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% "Slovenia"), aes(x ="s_povisano_telesno_tezo", y= s_povisano_telesno_tezo),color="green", show.legend = TRUE)+
  geom_point(data= tabela_drzav %>% filter(drzave %in% "Slovenia"), aes(x ="dnevni_kadilci", y= dnevni_kadilci),color="red", show.legend = TRUE) +
  geom_point(data= tabela_drzav %>% filter(drzave %in% "Slovenia"), aes(x ="mesecno_prekomerno_pijancevanje", y= mesecno_prekomerno_pijancevanje),color="blue",show.legend = TRUE) +
  geom_point(data= tabela_drzav %>% filter(drzave %in% "Slovenia"), aes(x ="niso_telesno_aktivni", y= niso_telesno_aktivni),color="orange",show.legend = TRUE) + 
  geom_point(data= tabela_drzav %>% filter(drzave %in% "Slovenia"), aes(x ="nic_obrokov_sadja_in_zelenjave", y= nic_obrokov_sadja_in_zelenjave),color="purple",show.legend = TRUE)

```
