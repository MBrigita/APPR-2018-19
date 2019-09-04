library(shiny)

fluidPage(
  
  titlePanel("Analiza glede na države"),
  
  tabsetPanel(
    tabPanel("Zemljevid",
             sidebarPanel(
               selectInput("type",label="Kategorija",
                           choice=c("Dnevni kadilci", "število ljudi s povišano telesno težo", "delež ljudi, ki niso telesno aktivni",
                                    "procent prebivalstva, ki ne je sadja in zelenjave", 
                                    "delež ljudi, ki mesečno prekomerno pijančuje")
                           )
               ),
             
             mainPanel(plotOutput("box"))),
    # 
    # tabPanel("Graf glede na izobrazbo in spol",
    #          sidebarPanel(
    #            selectInput("spol", label = "Izberi kategorijo",
    #                        choice = c("Dnevni kadilci", "število ljudi s povišano telesno težo", "delež ljudi, ki niso telesno aktivni",
    #                                   "procent prebivalstva, ki ne je sadja in zelenjave", 
    #                                   "delež ljudi, ki mesečno prekomerno pijančuje","graf za izbrane države"))),
    #          mainPanel(plotOutput("graf_sprem"))),
    
    tabPanel("Graf izbranih drzav glede na BDP",
             sidebarPanel(
               selectInput("tabela", label= "Izberi kategorijo",
                           choices = c("Dnevni kadilci", "število ljudi s povišano telesno težo", "delež ljudi, ki niso telesno aktivni",
                                       "procent prebivalstva, ki ne je sadja in zelenjave", "delež ljudi, ki mesečno prekomerno pijančuje",
                                       "BDP drzav"))),
             mainPanel(plotOutput("graf_drzave"))),
  
    tabPanel("Graf izbrane drzave",
             sidebarPanel(
               selectInput("izbrana_drzava",label = "Izberi drzavo",
                           choices=  sort(unique(tabela_drzav$drzave))),
               tableOutput("table1")),
             mainPanel(plotOutput("gizbrana_drzava"),width = 12))
           
             
  

))
