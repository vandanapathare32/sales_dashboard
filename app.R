library(ggplot2)
library(shiny)
library(shinydashboard)
library(MASS)
library(haven)
library(ggthemes)
library(zoo)
library(lubridate)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggrepel)
library(plotly)
library(DT)
library(readxl)
library(scales)

#changing the naming  format here to be easier on the eyes on the front end 
colName1 <- c("January" = "01", 
              "February" = "02",
              "March" = "03",
              "April" = "04",
              "May" = "05",
              "June" = "06",
              "July" = "07",
              "August" = "08",
              "September" = "09",
              "October" = "10",
              "November" = "11",
              "December" = "12")

#changing the naming  format here to be easier on the eyes on the front end 
colName2 <- c("Quarter 1" = "1",
              "Quarter 2" = "2",
              "Quarter 3" = "3",
              "Quarter 4" = "4")

#creating a list here so code below is not chunkcy 
teamNames <- c("m_store","us_store","hyd_store")

#formatting to display correctly on front end 
col_alias <- function(x) {switch(x,
                                 "01" = "January",
                                 "02" = "February",
                                 "03" = "March",
                                 "04" = "April",
                                 "05" = "May",
                                 "06" = "June",
                                 "07" = "July",
                                 "08" = "August",
                                 "09" = "September",
                                 "10" = "October",
                                 "11" = "November",
                                 "12" = "December")}
#formatting to display correctly on front end 
col_alias2 <- function(x) {switch(x,
                                  '1' = "Quarter 1",
                                  '2' = "Quarter 2",
                                  '3' = "Quarter 3",
                                  '4' = "Quarter 4")}


#start of dashboard 
ui = dashboardPage( skin = "blue",
                    dashboardHeader( title = "Sales Analytics"), 
                    dashboardSidebar(
                      sidebarMenu(
                        #upload file option
                        fileInput("myFile",
                                  "Upload Excel Data:",
                                  accept = ".xlsx"),
                        #time view selection
                        radioButtons("timeView", "Select View:", choices = 
                                       c("Monthly", "Quarterly", "Yearly")),
                        #year selection
                        radioButtons("yearOption1", "Select Year 1:", choices = 
                                       c("2011","2012","2013","2014","2015","2016","2017","2018"), selected = "2018"),
                        selectInput("team1", "Select Team 1:", choices = teamNames),
                        #month selection if that option is selcted in above radio buttons 
                        conditionalPanel(condition = 'input.timeView == "Monthly"',
                                         selectInput("month1", "Select Month 1:", choices = 
                                                       colName1, selected = "01")),
                        #quarterly selection if that option is selcted in above radio buttons 
                        conditionalPanel(condition = 'input.timeView == "Quarterly"',
                                         selectInput("quarter1", "Select Quarter 1:", choices = 
                                                       colName2, selected = "1")),
                        #option of years to select from 
                        conditionalPanel(condition = 'input.check1 == "1"',
                                         radioButtons("yearOption2", "Select Year 2:", choices = 
                                                        c("2011","2012","2013","2014","2015","2016","2017","2018"), selected = "2017")),
                        #team selection for comparison team
                        conditionalPanel(condition = 'input.check1 == "1"',
                                         selectInput("team2", "Select Team 2:", choices = teamNames)),
                        #month selection for comparison month
                        conditionalPanel(condition = 'input.check1 == "1" & input.timeView == "Monthly"',
                                         selectInput("month2", "Select Month 2:", choices = 
                                                       colName1,selected = "02")),
                        #quarterly selection for comparison quarter
                        conditionalPanel(condition = 'input.check1 == "1" & input.timeView == "Quarterly"',
                                         selectInput("quarter2", "Select Quarter 2:", choices = 
                                                       colName2, selected = "2")),
                        #make a comparison action checkbox
                        checkboxInput("check1","Make a comparison", value = FALSE)
                      )
                    ),
                    #body layout of dashboard 
                    dashboardBody(
                      fluidRow(
                        #histogram is being added here
                        box(width = 6, plotlyOutput("hist1")),
                        #donut chart is being added here
                        box(width = 6, plotlyOutput("donut1")),
                        #sum of 0-99% productivity box here
                        valueBoxOutput("productivityBox1"),
                        #average sales per rep box here
                        valueBoxOutput("productivityBox1.1"),
                        #sum of 100% productivity box here
                        valueBoxOutput("productivityBox1.2"),
                        #raw data table is being added here
                        box(width = 12,dataTableOutput("table1"))
                        
                      )
                    ))


#beginning of output portion/display
server = function(input, output) {
  
  #uploading the excel file
  a1 = reactive({
    req(input$myFile)
    read_excel(input$myFile$datapath)
  })
  
  
  
  #breakout the date to filter data for the select input and radio buttons input
  newdata <- reactive({
    a1 <- a1()
    x <- as.Date(a1$Month)
    
    a1$mo <- strftime(x, "%m")
    a1$yr <- strftime(x, "%Y")
    a1$qrt <- quarter(x, with_year = FALSE, fiscal_start = 01)
    
    #subsetting data to display sales reps that hold a quota
    newdata <- a1[grepl("Y", a1$`Quota Held Flag`),]
    
    #converting the participation column to  categorical for donut chart
    newdata$Participation[is.na(newdata$Participation)] <- 0
    newdata$Participation <- factor(newdata$Participation, labels =
                                      c("0-99%","100%"))
    
    #grouping data
    newdata %>%
      group_by(yr, mo, qrt)
    
    
  })
  
  #creating a subset of data for each filter when analyzing at one team 
  newdata2 <- reactive({
    newdata2 <- newdata ()
    
    
    if(input$timeView == 'Monthly'){
      return(newdata2 %>%
               filter(yr == input$yearOption1 & mo == input$month1 & Team == input$team1))
    }
    
    if(input$timeView == 'Quarterly'){
      return(newdata2 %>%
               filter(yr == input$yearOption1 & qrt == input$quarter1 & Team == input$team1))
    }
    else{
      return(newdata2 %>%
               filter(yr == input$yearOption1 & Team == input$team1))
    }
    
      
    })
  
  
  
  
  #creating a second subset of data for each filter for if you want to compare teams 
  newdata3 <- reactive({
    newdata3 <- newdata ()
    
    if(input$timeView == 'Monthly'){
      return(newdata3 %>%
               filter(yr == input$yearOption2 & mo == input$month2 & Team == input$team2))
    }
    
    if(input$timeView == 'Quarterly'){
      return(newdata3 %>%
               filter(yr == input$yearOption2 & qrt == input$quarter2 & Team == input$team2))
    }
    else{
      return(newdata3 %>%
               filter(yr == input$yearOption2 & Team == input$team2))
    }
  })
  
  
  #start of histogram
  p3 <- reactive({
    newdata2 <- newdata2()
    newdata3 <- newdata3()
    
    if(input$check1 == 'FALSE'){
      return(
        #for single view 
        ggplot() +
          geom_histogram(data = newdata2, aes(x =`Attainment Bucket`, 
                                              text = paste("Count:",..count..)), 
                         fill = "#3486f9", color = "#635f5f", stat = "count")+
          scale_x_discrete(limits=c("0-29%","30-69%","70-89%","90-99%",
                                    "100-200%","200-300%","+300%")) +
          geom_text(stat = "count", aes(label = ..count..,y = ..count..), 
                    vjust = 1.75,  
                    size = 5,
                    font = 3,
                    color = "white") +
          theme_bw() +
          labs(x="Attainment Buckets",
               title = paste(input$team1))
        
        
      ) 
    }
    else{
      return(
        #for comparing teams
        ggplot() +
          geom_histogram(data = newdata2, aes(x =`Attainment Bucket`,
                                              text = paste(input$team1,
                                                           "count:", ..count..)), 
                         fill = "#3486f9", stat = "count", position="identity", alpha = .8)+
          geom_histogram(data = newdata3, aes(x =`Attainment Bucket`, 
                                              text = paste(input$team2,
                                                           "count:", ..count..)), 
                         fill = "#635f5f", stat = "count", position="identity", alpha = .8)+
          scale_x_discrete(limits=c("0-29%","30-69%","70-89%","90-99%",
                                    "100-200%","200-300%","+300%"))+
          scale_fill_manual(name="Legend", values = c("#635f5f","#3486f9")) +   
          theme_bw() +
          labs(x="Attainment Buckets",
               title = paste(input$team1, "-",input$team2))
      )
    }
  })
  
  #key data output box 1 
  output$productivityBox1 <- renderValueBox({
    
    Sum <- sum(newdata2()$'Bookings'[which(newdata2()$'Participation' == '0-99%')], na.rm = T)
    
    valueBox(prettyNum(paste0("$", sprintf("%.2f",Sum)), big.mark = ","),
             subtitle = "Sum of 0-99% Participation:",
             icon = icon("dollar-sign", lib = "font-awesome"),
             color = "blue")
  })
  
  #key data output box 2
  output$productivityBox1.1 <- renderValueBox({
    
    avg <- mean(newdata2()$'Bookings', na.rm = T)
    
    valueBox(prettyNum(paste0("$", sprintf("%.2f",avg)), big.mark = ","),
             subtitle = "Average Productivity:",
             icon = icon("money-bill-wave", lib = "font-awesome"),
             color = "blue")
  })
  
  #key data output box 3
  output$productivityBox1.2 <- renderValueBox({
    
    Sum <- sum(newdata2()$'Bookings'[which(newdata2()$'Participation' == '100%')], na.rm = T)
    
    valueBox(prettyNum(paste0("$", sprintf("%.2f",Sum)), big.mark = ","),
             subtitle = "Sum of 100% Participation:",
             icon = icon("hand-holding-usd", lib = "font-awesome"),
             color = "blue")
  })
  
  
  output$hist1 <- renderPlotly({
    
    #friendly message to instruct user of how the app works
    text <- paste("Please  download the sales_data.xlsx file.\n",
                  "Make sure that it has the following variables: \n",
                  "Month, Employee, Quota, Bookings, Attainment, Participation, Quota Held Flag, Attainment Bucket, and Team.")
    
    validate(
      #only show above message when there is not a file uploaded 
      need(!is.null(input$myFile), text)
    )
    
    ggplotly(p3(), tooltip = "text") %>% config(displayModeBar = F) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(showlegend = T)
    
  })
  
  #start of donut plot
  output$donut1 <- renderPlotly ({
    
    text <- paste("Please  download the sales_data.xlsx file.\n",
                  "Make sure that it has the following variables: \n",
                  "Month, Employee, Quota, Bookings, Attainment, Participation, Quota Held Flag, Attainment Bucket, and Team.")
    
    validate(
      #only show above message when there is not a file uploaded 
      need(!is.null(input$myFile), text)
    )
    name_store <- c("us_store","m_store","hyd_store") # rows
    name_metric <- c("metric1","metric2","metric3") # columns
    
    # Generation of dataframe
    
    value_expression <- data.frame(store = name_store, 
                                   matrix(rnorm(360, 2, 1.8),nrow = 3, ncol = 3))
    names(value_expression)[2:4] <- name_metric
    
    # Melt dataframe
    
    df_heatmap <- melt(value_expression, id.vars = "store")
    names(df_heatmap)[2:3] <- c("metric", "performa")
    
    head(df_heatmap)
    
    ggplot(df_heatmap, aes(metric, store )) +
      geom_tile(aes(fill = performa), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("List of stores ") +
      xlab("List of metrics") +
      theme(legend.title = element_text(size = 10),
            legend.text = element_text(size = 12),
            plot.title = element_text(size=16),
            axis.title=element_text(size=14,face="bold"),
            axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(fill = "performa")
    
    
  })
  
  #raw data table start 
  output$table1 <- renderDataTable ({
    
    datatable(newdata2()[,c(2:5,8:9)], options = list(pageLength = 5)) %>% 
      formatCurrency(c(2:3),'$') %>%
      formatPercentage(4,2)
    
  })
  
  
}

shinyApp(ui = ui, server = server)
