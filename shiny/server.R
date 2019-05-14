library(shiny)

shinyServer(function(input, output) {
  output$box <- renderPlot({
  if(input$type == "Dnevni kadilci"){
    print(zemljevid_dk)}
  else if (input$type == "število ljudi s povišano telesno težo"){
    print(zemljevid_ptt)}
  else if (input$type == "delež ljudi, ki niso telesno aktivni"){
    print(zemljevid_ta)}
  else if (input$type == "procent prebivalstva, ki ne je sadja in zelenjave"){
    print(zemljevid_nsz)}
  else if (input$type == "delež ljudi, ki mesečno prekomerno pijančuje"){
    print(zemljevid_pp)}
  else if(input$type == "graf za izbrane države"){
    print(graf_drzav_BDP)}
  })

  output$graf_sprem <- renderPlot({
    if(input$spol == "Dnevni kadilci"){
      print(g_kajenja)}
    else if (input$spol == "število ljudi s povišano telesno težo"){
      print(g_debelosti)}
    else if (input$spol == "delež ljudi, ki niso telesno aktivni"){
      print(g_niso_aktivni)}
    else if (input$spol == "procent prebivalstva, ki ne je sadja in zelenjave"){
      print(g_nic_sz)}
    else if (input$spol == "delež ljudi, ki mesečno prekomerno pijančuje"){
      print(g_pijancevanje)}
    
    else if(input$spol == "graf za izbrane države"){
      print(graf_drzav_BDP)}
  })
  
  # output$graf_drzav <- renderPlot({
  #   graf_bdp <- ggplot(data =tabela_drzav %>% filter(drzave %in% drzave_BDP),aes(x=drzave, y=seq(from = 0, to = 100, by = 20),fill= input$kategorija)) + geom_col()
  #   print(graf_bdp)})
  
  output$graf_drzave <- renderPlot({
    if(input$tabela == "Dnevni kadilci"){
      graf_drzave <- ggplot(data =tabela_kajenja %>% filter(GEO %in% drzave_BDP),aes(x=GEO, y=dnevni_kadilci)) + geom_col() + 
        geom_bar(stat="identity", fill="red", colour="black")
      print(graf_drzave)}
    else if (input$tabela == "število ljudi s povišano telesno težo"){
      graf_drzave <- ggplot(data =tabela_debelosti %>% filter(GEO %in% drzave_BDP),aes(x=GEO, y=s_povisano_telesno_tezo)) + 
        geom_col() + geom_bar(stat="identity", fill="green", colour="black")
      print(graf_drzave)}
    else if (input$tabela == "delež ljudi, ki niso telesno aktivni"){
      graf_drzave <- ggplot(data =tabela_aktivnosti %>% filter(GEO %in% drzave_BDP),aes(x=GEO, y=niso_telesno_aktivni)) +
        geom_col() + geom_bar(stat="identity", fill="orange", colour="black")
      print(graf_drzave)}
    else if (input$tabela == "procent prebivalstva, ki ne je sadja in zelenjave"){
      graf_drzave <- ggplot(data =tabela_hrane %>% filter(GEO %in% drzave_BDP),aes(x=GEO, y=nic_obrokov_sadja_in_zelenjave)) + 
        geom_col() + geom_bar(stat="identity", fill="purple", colour="black")
      print(graf_drzave)}
    else if (input$tabela == "delež ljudi, ki mesečno prekomerno pijančuje"){
      graf_drzave <- ggplot(data =tabela_pijancevanja %>% filter(GEO %in% drzave_BDP),aes(x=GEO, y=mesecno_prekomerno_pijancevanje)) +
        geom_col() + geom_bar(stat="identity", fill="blue", colour="black")
      print(graf_drzave)}
    
     else if(input$tabela == "BDP drzav"){
     graf_drzave <- ggplot(data =tabela_BDP %>% filter(GEO %in% drzave_BDP),aes(x=GEO, y=BDP_per_capita)) +
       geom_col() + geom_bar(stat="identity", fill="pink", colour="black")
     print(graf_drzave)}
    
    output$gizbrana_drzava <- renderPlot({
      gizbrana_drzava <- ggplot(data= tabela_drzav %>% filter(drzave == input$izbrana_drzava),aes(x=kategorije, y = seq(from = 0, to = 100, by = 10))) + 
        geom_point(data= tabela_drzav %>% filter(drzave == input$izbrana_drzava), aes(x ="s_povisano_telesno_tezo", y= s_povisano_telesno_tezo),color="green", show.legend = TRUE)+
        geom_point(data= tabela_drzav %>% filter(drzave == input$izbrana_drzava), aes(x ="dnevni_kadilci", y= dnevni_kadilci),color="red", show.legend = TRUE) +
        geom_point(data= tabela_drzav %>% filter(drzave == input$izbrana_drzava), aes(x ="mesecno_prekomerno_pijancevanje", y= mesecno_prekomerno_pijancevanje),color="blue",show.legend = TRUE) +
        geom_point(data= tabela_drzav %>% filter(drzave == input$izbrana_drzava), aes(x ="niso_telesno_aktivni", y= niso_telesno_aktivni),color="orange",show.legend = TRUE) + 
        geom_point(data= tabela_drzav %>% filter(drzave == input$izbrana_drzava), aes(x ="nic_obrokov_sadja_in_zelenjave", y= nic_obrokov_sadja_in_zelenjave),color="purple",show.legend = TRUE)
      
      print(gizbrana_drzava)
    })
     

})
})
