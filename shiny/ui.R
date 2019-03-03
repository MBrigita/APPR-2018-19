library(shiny)

fluidPage(
  
  titlePanel("zdravje ljudi v Evropi"),
  
  tabPanel("Zemljevid",
    sidebarPanel(
      selectInput("type",label="Kategorija",
                  choice=c("Dnevni kadilci", "število ljudi s povišano telesno težo", "delež ljudi, ki niso telesno aktivni",
                           "procent prebivalstva, ki ne je sadja in zelenjave", 
                           "delež ljudi, ki mesečno prekomerno pijančuje","graf za izbrane države")
      )
    ),
  
  
               mainPanel(plotOutput("box"))),


  
  tabPanel("Graf",
           sidebarPanel(
             selectInput("spol", label = "Izberi kategorijo",
                         choice = c("Dnevni kadilci", "število ljudi s povišano telesno težo", "delež ljudi, ki niso telesno aktivni",
                                     "procent prebivalstva, ki ne je sadja in zelenjave", 
                                     "delež ljudi, ki mesečno prekomerno pijančuje","graf za izbrane države"))),
           mainPanel(plotOutput("graf_sprem"))),
  

)
