
#PREREQUISITES ON RSTUDIO
#install.packages: shiny 
#install.packages("devtools")
#install.packages('base64enc') 
#install_github("rCharts","ramnathv")
#NO install.packages("XLConnect") -> install_github("xlconnect", username = "miraisolutions",ref = "0.2-5")


#Windows environment
#if (Sys.getenv("JAVA_HOME")!="")
#+     Sys.setenv(JAVA_HOME="")

require(shiny)
require(rCharts)
options(java.parameters = "-Xmx512m")
require(XLConnect)
#Open Excel only once!
##excel.file <- file.path("data/FontA_B.xls")
excel.file <- file.path("FontA_B.xls")
elements <- readWorksheetFromFile(excel.file,sheet=1)
#Clean RAM 
xlcFreeMemory()
detach("package:XLConnect", unload=TRUE)

# Define server logic
shinyServer(function(input, output) {
  
  
  #OPS
  #will transpose: t()
  #will agregate; aggregate(x, by, FUN)
  #will filter data: subset(studentdata, Drink=='water')
  #SUBSET 2: my.data.frame <- subset(data , V1 > 2 | V2 < 4)
  #aggdata <-aggregate(mtcars, by=list(cyl,gear), FUN=mean, na.rm=TRUE)
  #output$mytable <- renderTable({ t(head(elements,100))})
  
  #control visualization TODO
  #if (!input$adjust) return(dataInput())
  
  #Makes run-once for equal parameters
  #Data set for Table Cease Cause
  dataInputTableMort<-reactive({   
    myset3<-{subset(elements,YEAR==input$any & UGEO==input$districte & SEXE==input$sexe)} 
    myset3[c('CauseOfDeath','IC95','RMC','PCTTOTAL','TXTOTAL','TSTDTOTAL')]
    
  })   
  
  output$mytableMort <- renderTable({
    if (any(input$checkGroup==2))  return(dataInputTableMort())  
  })
  
  #Data Set for Evolution Table
  dataInputTableEvol<-reactive({   
    myset<-{subset(elements,CAUSA=='99' & UGEO==input$districte & SEXE==input$sexe)}
    myset[c('YEAR','N01_14','N15_44','N45_74','N75_XX','TOTAL','TX01_14','TX15_44','TX45_74','TX75_XX','TXTOTAL','IC95','RMC','TSTDTOTAL')]
    #myset$YEAR<- round(myset$YEAR,0)
  })
  
  output$mytableEvol <- renderTable({
    if (any(input$checkGroup==2))  return(dataInputTableEvol())
  })
  
  
  #Data set for Graph
  dataInputGraph <-reactive({
    myset<-{subset(elements,YEAR==input$any & UGEO==input$districte & SEXE==input$sexe)}
    myset[c('CAUSA','TSTDTOTAL','TOTAL','IC95','RMC')]
  })
  
  #Evol graph 
  dataInputEvol <-reactive({
    myset2<-{subset(elements,CAUSA=='99' & UGEO==input$districte & SEXE==input$sexe)}
    myset2[c('YEAR','TSTDTOTAL')]
  })
  
  output$myChartEvol <- renderChart2({
    ##renderChart({
    DISTRICTE=input$districte
    SEXE=input$sexe
    if (any(input$checkGroup==1)) {
      #h1 <- renderLineChart()
      h1 <- Highcharts$new()
      dades<-dataInputEvol()
      #Replace rare chars for na
      dades$TSTDTOTAL[dades$TSTDTOTAL == "-"] <- NA
      dades$TSTDTOTAL<- as.numeric(dades$TSTDTOTAL)
      h1$chart(type = "line")
      h1$xAxis(categories=dades$YEAR)
      h1$series(data=dades$TSTDTOTAL,name=paste("District: ",DISTRICTE," Std Tax. Sex: ", SEXE,sep=""), dashStyle = "longdash")
      #h1$legend(symbolWidth = 80)    
      return(h1)
      # Set dom attribute otherwise chart will not appear on the web page
      ##h1$set(dom = 'myChartEvol')
      ##h1
    } else {
      tt <- Highcharts$new()
      #Min value 20
      tt$set(height ="20")
      return(tt)
    }
    
  }) 
  
  #Histogram Plot
  output$newHist <- renderPlot({
    if (any(input$checkGroup==1)) {
      dades <-dataInputGraph()
      #Replace rare chars for na
      dades$RMC[dades$RMC == "-"] <- NA
      dades$RMC<- as.numeric(dades$RMC)
      hist(dades$RMC, xlab='CMR Cause of Death', col = 'lightblue', main= 'Comparative Mortality Ratio for Cause of death',ylab='Frequency')
      #Calculation of mean of Comparative Mortality Ratio
      meanrmc <-mean(dades$RMC, na.rm = TRUE)
      abline(v = meanrmc, col = "red")
      text(30 + round(meanrmc,2),5,paste("mean(rmc) = ", round(meanrmc,2)),col = "red")
    } else {
      #hist(c(0))
    }
  }
  #, height = 400, width = 400
  ,width = 400
  )
  
  #OUTPUT 1st TAB
  output$textEvol <- renderText({ 
    if (any(input$checkGroup==1)) {
      HTML(
        paste("<h5>Evolution of Mortality standardized rates per year</h5>") 
      )
    } else  
      if (any(input$checkGroup==2)) {
        HTML(
          paste("<h5>Evolution of Mortality standardized rates in the District: <strong>", input$districte, "</strong> Sex: <strong>", input$sexe,"</strong></h5>", sep="")
        ) 
      }
    
  })
  
  #OUTPUT 2nd Tab
  output$textMort <- renderText({ 
    HTML(
      paste("<h5>Year <strong>", input$any,"</strong>. Mortality in the District: <strong>", input$districte,"</strong> Sex: <strong>", input$sexe,"</strong></h5>", sep="")
    )
  })
  
  #Explanatory HTML Tab1
  output$explanationHtml1 <-renderText({
    if (any(input$checkGroup==3)) {
      HTML("<h5>Explanation of Evolutive data TAB</h5>
           <br/>
           <h5>The Graph displays the evolution of standardized rates per year in Barcelona starting from year 2001 to 2011,
           and according to the selected District and Sex on the sidebar panel combo boxes.</h5>
           <br/>
           <h5>The Table shows per each year (rows), the following data from Barcelona, according to the selected District and Sex on the sidebar panel combo boxes; by columns:<br/>
           <ol>
           <li>The given year (YEAR)
           <li>Number of deaths by age group: from 0 to 14 years old (N01_14), from 15 to 44 (N15_44), from 45 to 74 (N45_44), 
           ,more than 75 years old (N75_XX), and from all ages (TOTAL)</li>
           <li><a href='http://en.wikipedia.org/wiki/Mortality_rate'>Rate of number of deaths</a> within the amount of population in Barcelona, corresponding to the same age groups (TX01_14,TX15_44,TX45_74,TX75_XX,TXTOTAL)</li>
           <li><a href='http://en.wikipedia.org/wiki/Confidence_interval'>Confidence Interval (IC95)</a></li>
           <li>Comparative Mortality Ratio (RMC)</li>
           <li><a href='http://en.wikipedia.org/wiki/Mortality_rate'>Standardized mortality rate </a>balanced with the population of Barcelona in the year 1996 (TSTDTOTAL)</li>
           </ol>
           </h5>
           ")
      #includeHTML("./www/explanation.html")
    } else {
      #HTML("<h5>Check See explanation for viewing the explanatory notes</h5>")
    }
  })
  
  #Explanatory HTML Tab2
  output$explanationHtml2 <-renderText({
    if (any(input$checkGroup==3)) {
      HTML("<h5>Explanation of Death Cause TAB</h5>
           <br/>
           <h5>The Graph displays an histogram or frequency distribution of values for Comparative Mortality Ratio (RMC), in Barcelona,
           according to the selected Year, District and Sex on the sidebar panel combo boxes. It also depicts in a red line the mean value for Comparative Mortality Ratio.</h5>
           <br/>
           <h5>The Table shows pear each main Cause of Death Group (17 groups and TOTAL in rows), the following data from Barcelona, according to the selected Year, District and Sex on the sidebar panel combo boxes; by columns:<br/>
           <ol>
           <li>The given <a href='http://en.wikipedia.org/wiki/Cause_of_death'>Cause of Death</a>(CauseOfDeath)
           <li><a href='http://en.wikipedia.org/wiki/Confidence_interval'>Confidence Interval (IC95)</a></li>
           <li>Comparative Mortality Ratio (RMC)</li>
           <li>Percent of number of deaths within the Cause of death (PCTTOTAL)</li>
           <li><a href='http://en.wikipedia.org/wiki/Mortality_rate'>Rate of number of deaths</a> within the amount of population in Barcelona (TXOTAL)</li>
           <li><a href='http://en.wikipedia.org/wiki/Mortality_rate'>Standardized mortality rate </a>balanced with the population of Barcelona in the year 1996 (TSTDTOTAL)    </li>
           </ol>
           </h5>
           ")
      #includeHTML("./www/explanation.html")
    } else {
      #HTML("<h5>Check See explanation for viewing the explanatory notes</h5>")
    }
  })
  
})