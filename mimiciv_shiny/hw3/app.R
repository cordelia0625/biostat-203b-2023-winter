#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(tidyverse)
library(lubridate)



#load data
mydata <- read_rds("D:\\Desktop\\203b-hw\\mimiciv_shiny\\icu_cohort.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ICU Cohort Data Exploration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Choose a variable for the bar graph",
                    choice = list(
                      "Ethnicity" = "ethnicity",
                      "Language" = "language",
                      "Insurance" = "insurance",
                      "Marital status" = "marital_status",
                      "Gender" = "gender",
                      "First Care Unit" = "first_careunit",
                      "Creatinine" = "item_50912",
                      "Potassium" = "item_50971",
                      "Sodium" = "item_50983",
                      "Chloride" = "item_50902",
                      "Bicarbonate" = "item_50882",
                      "Hematocrit" = "item_51221",
                      "White Blood Cell" = "item_51301",
                      "Glucose" = "item_50931",
                      "Magnesium" = "item_50960",
                      "Calcium" = "item_50893",
                      "Heart Rate" = "item_220045",
                      "Mean Non-invasive Blood Pressure" = "item_220181",
                      "Systolic Non-invasive Blood Pressure" = "item_220179",
                      "Body Temperature in Fahrenheit" = "item_223761",
                      "Respiratory Rate" = "item_220210"
                    )),
        
      
      ),   
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Bar Graph for 30 day mortality", 
                             plotOutput("myplot1")),
                    tabPanel("Chart and Lab measurements", 
                             verbatimTextOutput("myplot2")),
                    tabPanel("Table", tableOutput("table"))
        )
      )
    )
  )
    




# Define server
server <- function(input, output) {
  output$myplot1 <- renderPlot({
    # Create a histogram using ggplot2
    mydata %>%
    ggplot() +
      geom_bar(mapping = 
                 aes_string(x = "thirty_day_mort", fill = input$var), 
               position = "fill") +
      labs( title = "Bar graph of thirty day mortality percentage")
  })
  
  
   output$myplot2 <- renderPlot ({
     
     
     # mydata %>%
     #   ggplot(aes_string(x = "item_50971" )) +
     #   geom_bar(aes(stat = "identity")) +
     #   labs(x = "item_50971", title = "My Bar Graph")
     
     select(input$var, "thirty_day_mort") %>%
       filter(.[, 1] > 0) %>%
       group_by_("thirty_day_mort") %>%
       na.omit() %>% 
       ggplot(mapping = aes_string(x = "thirty_day_mort", y = input$var)) +
       geom_boxplot() +
       labs(title = 
              "Distribution for selected variable by 30-day mortality") +
       labs(x = "30-day mortality") +
       coord_flip() 
   })
 
    
  
  
  
  output$table <- renderTable({
   
    m_col <- input$m
    
    mydata%>%
      filter(!!sym(m_col) > 0) %>%
      summarise(avg = mean(!!sym(m_col), na.rm = TRUE),
                total = sum(!!sym(m_col), na.rm = TRUE)) %>%
      mutate(min = min(!!sym(m_col), na.rm = TRUE),
             max = max(!!sym(m_col), na.rm = TRUE),
             Q1 = quantile(!!sym(m_col), 0.25),
             Median = median(!!sym(m_col), na.rm = TRUE),
             Q3 = quantile(!!sym(m_col), 0.75),
             Sd = sd(!!sym(m_col), na.rm = TRUE)) %>%
      select(avg, total, min, max, Q1, Median, Q3, Sd)
      
    
  })
  
}



shinyApp(ui = ui, server = server)