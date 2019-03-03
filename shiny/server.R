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
  


}
)
