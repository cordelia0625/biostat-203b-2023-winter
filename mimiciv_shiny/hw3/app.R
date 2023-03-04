#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bigrquery)
library(dbplyr)
library(DBI)
library(lubridate)
library(tidyverse)
library(ggplot2)


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
                      "Gender" = "gender")
        ),
        selectInput("m",
                    "Choose a measurement for the histogram",
                    choice = list (
                      "Creatinine" = "lab50912",
                      "Potassium" = "lab50971",
                      "Sodium" = "lab50983",
                      "Chloride" = "lab50902",
                      "Bicarbonate" = "lab50882",
                      "Hematocrit" = "lab51221",
                      "White Blood Cell" = "lab51301",
                      "Glucose" = "lab50931",
                      "Magnesium" = "lab50960",
                      "Calcium" = "lab50893",
                      "Heart Rate" = "chart220045",
                      "Mean Non-invasive Blood Pressure" = "chart220181",
                      "Systolic Non-invasive Blood Pressure" = "chart220179",
                      "Body Temperature in Fahrenheit" = "chart223761",
                      "Respiratory Rate" = "chart220210"
                    )),
        
      
      ),   
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Bar Graphs", plotOutput("myplot1")),
                    tabPanel("Histograms", verbatimTextOutput("myplot2")),
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
    
    mydata %>%
      ggplot() +
      geom_bar(mapping = 
                 aes_string(x = input$m, fill=thirty_day_mort), 
               position = "fill") +
      labs(title="Histogram of chart / lab event")
  })
    
    
    

  
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
