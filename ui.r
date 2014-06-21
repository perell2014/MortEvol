library(shiny)
require(rCharts)

shinyUI(fluidPage(
  
  
  #CSS styling
  HTML('<style type="text/css">
       # .row-fluid { width: 50%; }  
       .well { background-color: #99CCFF; width: 150px;}
       .shiny-html-output { font-size: 8px; line-height:9px;}
       </style>'),
  
  # Application title
  fluidRow(column(3,img(src="logo_aspb.png")),column(8,h3("Mortality in Barcelona"))),
  
  fluidRow(
    column(3, #sidebarLayout(
           
           # Sidebar with combobox selectors  
           sidebarPanel(
             conditionalPanel(condition="input.conditionedPanels == 'Cause of Death'", 
                              selectInput("any", 
                                          label = "Year",
                                          choices = list("2011","2010","2009", "2008","2007","2006","2005", "2004","2003","2002","2001"),
                                          selected = "2011")),
             selectInput("sexe", 
                         label = "Sex",
                         choices = c("Both","Men","Women"),
                         # choices = list("Both"="T", "Men"="1",
                         #                 "Women"="2"),
                         selected = "T"),
             selectInput("districte", label = "District", 
                         choices = c("Ciutat Vella","Eixample","Sants-Montjuich","Les Corts","Sarria-Sant Gervasi",
                                     "Gracia","Horta-Guinardo","Nou Barris","Sant Andreu","Sant Marti","Barcelona"),
                         #choices = list("Ciutat Vella"="01","Eixample"="02","Sants-Montjuich"="03","Les Corts"="04","Sarrià-Sant Gervasi"="05",
                         #               "Gràcia"="06","Horta-Guinardó"="07","Nou Barris"="08","Sant Andreu"="09","Sant Martí"="10","Total Barcelona"="TOTAL"),
                         selected = "01"),
             #),
             #View Options combo box
             checkboxGroupInput("checkGroup", label = h5("View Options"), 
                                choices = list("Graphs" = 1, 
                                               "Tables" = 2, "See explanation" = 3),
                                selected = c(1,2,3))
           )
           #column
    ),column(9,tabsetPanel(
               tabPanel("Evolutive Data",htmlOutput("textEvol")
                        , showOutput("myChartEvol", "highcharts")
                        , tableOutput("mytableEvol")
                        #,verbatimTextOutput("explanationText1")
                        ,htmlOutput("explanationHtml1")
               ),
               tabPanel("Cause of Death",fluidRow(column(6,htmlOutput("textMort"), tableOutput("mytableMort")),column(5, plotOutput('newHist',width = "100%")
               ))
               ,htmlOutput("explanationHtml2")
               )
               #,tabPanel("Maps", h5("Not yet my friend!!"))
               ,id = "conditionedPanels"
             )
             
    ))
  
  ))