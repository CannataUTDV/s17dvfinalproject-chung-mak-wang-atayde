require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

online0 = TRUE

areas = query( 
  data.world(propsfile = "www/.data.world"), 
  dataset="ryanmak/s-17-dv-final-project", type="sql", 
  query="select distinct Subject_Race_Ethnicity as D, Subject_Race_Ethnicity as R 
  from OIS_Dataset_Subjects 
  order by 1" 
) # %>% View() 
area_list <- as.list(areas$D, areas$R) 
area_list <- append(list("All" = "All"), area_list) 

# Server.R structure:
#   Queries that don't need to be redone
#   shinyServer
#   widgets
#   tab specific queries and plotting

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
if(online0) {
  days = query(
    data.world(propsfile = "www/.data.world"),
    dataset="ryanmak/s-17-dv-final-project", type="sql",
    query="select distinct Day_of_Week as D, Day_of_Week as R
    from OIS_Dataset_Incidents
    order by 1"
  )
}
day_list <- as.list(days$D, days$R)
day_list <- append(list("All" = "All"), day_list)
day_list2 <- day_list

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  output$boxplotDays <- renderUI({selectInput("selectedBoxplotDays", "Choose Days:",
                                                 day_list2, multiple = TRUE, selected='All') })
  
  # These widgets are for the Histogram tab.
  online4 = reactive({input$rb4})
  
  # These widgets are for the Scatter Plots tab.
  online3 = reactive({input$rb3})
  
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2}) 
  output$regions2 <- renderUI({selectInput("selectedAreas", "Choose Race:", area_list, multiple = TRUE, selected='All') })   
 
   # Begin Box Plot Tab ------------------------------------------------------------------
  dfbp1 <- eventReactive(input$click5, {
    if(input$selectedBoxplotDays == 'All') day_list2 <- input$selectedBoxplotDays
    else day_list2 <- append(list("Skip" = "Skip"), input$selectedBoxplotDays)
    if(online5() == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="ryanmak/s-17-dv-final-project", type="sql",
        query="select OIS_Dataset_Subjects.Subject_Age,
        OIS_Dataset_Incidents.Call_Type_Categories,
        OIS_Dataset_Incidents.Day_of_Week, 
        OIS_Dataset_Incidents.Number_of_Hits
        
        from OIS_Dataset_Subjects 
        LEFT JOIN OIS_Dataset_Incidents 
        ON OIS_Dataset_Subjects.Case_Number=OIS_Dataset_Incidents.Case_Number
        where (? = 'All' or Day_of_Week in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))",
        queryParameters = day_list2) #%>% View()
    }
  })

  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
         extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$boxplotPlot1 <- renderPlotly({p <- ggplot(dfbp1()) +
      geom_boxplot(aes(x=Call_Type_Categories, y=Subject_Age)) +
      geom_point(aes(x=Call_Type_Categories, y=Subject_Age, color=Day_of_Week, size=Number_of_Hits)) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    ggplotly(p)
  })
  # # End Box Plot Tab ___________________________________________________________
  # 
  # # Begin Histgram Tab ------------------------------------------------------------------
  dfh1 <- eventReactive(input$click4, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="ryanmak/s-17-dv-final-project", type="sql",
        query="SELECT Number_of_Shots_Fired_by_Officer, Subject_Race_Ethnicity
        from OIS_Dataset_Officers INNER JOIN OIS_Dataset_Subjects ON OIS_Dataset_Officers.Case_Number = OIS_Dataset_Subjects.Case_Number"
      ) # %>% View()
    }
    })
  
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1(),
                                                          rownames = FALSE,
                                                          extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$histogramPlot1 <- renderPlotly({p <- ggplot(dfh1()) +
    geom_histogram(aes(x=Number_of_Shots_Fired_by_Officer, color=Subject_Race_Ethnicity, fill=Subject_Race_Ethnicity), binwidth = 1) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  ggplotly(p)
  })
  
  # # End Histogram Tab ___________________________________________________________
  # 
  # # Begin Scatter Plots Tab ------------------------------------------------------------------
  dfsc1 <- eventReactive(input$click3, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="ryanmak/s-17-dv-final-project", type="sql",
        query="SELECT year(OIS_Dataset_Incidents.Date) as year, sum(OIS_Dataset_Incidents.Number_of_Hits) as hits, sum(OIS_Dataset_Officers.Number_of_Shots_Fired_by_Officer) as shots
        from OIS_Dataset_Incidents INNER JOIN OIS_Dataset_Officers ON OIS_Dataset_Incidents.Case_Number = OIS_Dataset_Officers.Case_Number
        group by year(OIS_Dataset_Incidents.Date)
        order by year"
      ) # %>% View()
    }
    })
  
  output$scatterData1 <- renderDataTable({DT::datatable(dfsc1(),
                                                        rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  ggplotRegression <- function (fit) {
    
    #require(ggplot2)
    
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      #geom_point(size=5) +
      stat_smooth(method = "lm", col = "black") +
      labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                         "Intercept =",signif(fit$coef[[1]],5 ),
                         " Slope =",signif(fit$coef[[2]], 5),
                         " P =",signif(summary(fit)$coef[2,4], 5)))
  }
  output$scatterPlot1 <- renderPlotly({p <- ggplotRegression(lm(shots ~ year, data = dfsc1())) + geom_point(data = dfsc1(), aes(x=year, y=shots, color=hits), size = 5)
    #ggplot(dfsc1(), aes(x=year, y=shots, color=hits)) +
    #theme(axis.text.x=element_text(size=16, vjust=0.5)) +
    #theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    #geom_point(size=5) +
    #stat_smooth(method = "lm", col = "black")
    #geom_abline(aes(intercept=-6452.94, slope=3.22424), color = 'black')
  ggplotly(p)
  })
  
  # # End Scatter Plots Tab ___________________________________________________________
  # 
  # Begin Crosstab Tab ------------------------------------------------------------------
  
  dfct1 <- eventReactive(input$click1, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="ryanmak/s-17-dv-final-project", type="sql",
        query="select OIS_Dataset_Incidents.Subject_Weapon, 
        OIS_Dataset_Incidents.Premise_Category,
        sum(OIS_Dataset_Incidents.Number_of_Hits) as sumHits, 
        sum(OIS_Dataset_Officers.Number_of_Shots_Fired_by_Officer) as sumShots,

        case 
        when sum(OIS_Dataset_Incidents.Number_of_Hits)/sum(OIS_Dataset_Officers.Number_of_Shots_Fired_by_Officer) < ? then '03 Low'
        when sum(OIS_Dataset_Incidents.Number_of_Hits)/sum(OIS_Dataset_Officers.Number_of_Shots_Fired_by_Officer) < ? then '02 Medium'
        else '01 High'

        end as kpi
        
        FROM OIS_Dataset_Incidents
        LEFT JOIN OIS_Dataset_Officers ON OIS_Dataset_Incidents.Case_Number=OIS_Dataset_Officers.Case_Number
        
        group by Premise_Category, Subject_Weapon
        order by Premise_Category, Subject_Weapon",
        queryParameters = list(KPI_Low(), KPI_Medium())
      ) # %>% View()
    }
  })
  
  output$data1 <- renderDataTable({DT::datatable(dfct1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(dfct1()) +
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_text(aes(x=Premise_Category, y=Subject_Weapon, label=sumShots), size=6) +
      geom_tile(aes(x=Premise_Category, y=Subject_Weapon, fill=kpi), alpha=0.50)
    
  })
  
  # # End Crosstab Tab ___________________________________________________________
  # # Begin Barchart Tab ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, { 
    if(input$selectedAreas == 'All') area_list <- input$selectedAreas 
    else area_list <- append(list("Skip" = "Skip"), input$selectedAreas) 
    if(online2() == "SQL") { 
      print("Getting from data.world") 
      tdf = query( 
        data.world(propsfile = "www/.data.world"), 
        dataset="ryanmak/s-17-dv-final-project", type="sql", 
        query="select OIS_Dataset_Subjects.Subject_Age,
        OIS_Dataset_Subjects.Subject_Race_Ethnicity,
        OIS_Dataset_Subjects.Subject_Injuries, 
        OIS_Dataset_Subjects.Subject_Drug_or_Alcohol_Use,
        OIS_Dataset_Officers.Number_of_Shots_Fired_by_Officer
        
        from OIS_Dataset_Subjects 
        LEFT JOIN OIS_Dataset_Officers ON OIS_Dataset_Subjects.Case_Number=OIS_Dataset_Officers.Case_Number
        where ? = 'All' or Subject_Race_Ethnicity in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", 
        queryParameters = area_list 
      ) # %>% View() 
    }
  }) 
  output$barchartData1 <- renderDataTable({DT::datatable(df2(), 
         rownames = FALSE, 
         extensions = list(Responsive = TRUE, FixedHeader = TRUE) ) 
  }) 
  output$barchartPlot1 <- renderPlot({ggplot(df2(), aes(x=Subject_Drug_or_Alcohol_Use, y=Number_of_Shots_Fired_by_Officer, color=Subject_Injuries, fill=Subject_Injuries)) + 
      scale_y_continuous(labels = scales::comma) + # no scientific notation   
      geom_bar(stat = "identity")
  })
  
})

  # # End Barchart Tab ___________________________________________________________
  # 
