# ---- Prepare Workspace ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(stringr)
library(rsconnect)

# Colour levels
colourLevels <- c("white", "black", "clear", "green", "blue", "red", "yellow", "orange", "other")

# Fill scale for plastic colour
myFillsPlastic <- c("white", "black", "grey80",
                    "green", "blue",
                    "red", "yellow", "orange",
                    "purple")
names(myFillsPlastic) <- colourLevels
fillScalePlastic <- scale_fill_manual(name = "", values = myFillsPlastic)

# ---- UI ----
ui <- dashboardPage(dashboardHeader(title = "Terranaut Club"),
                    dashboardSidebar(selectInput(inputId = "year", label = "Year", choices = 2021:2022)),
                    dashboardBody(uiOutput("content")))

# ---- Server ----
server <- function(input, output) {
  output$content <- renderUI({
    if(input$year == 2022) {
      fluidPage(
        fluidRow(column(width = 3, textInput(inputId = "name", label = "Observer:", value = "< Your Name >")),
                 column(width = 3, selectInput(inputId = "type", label = "Plastic Type:", choices = 1:6)),
                 column(width = 3, selectInput(inputId = "colour", label = "Plastic Colour:",
                                               choices = colourLevels)),
                 column(width = 2, actionButton(inputId = "submit", label = "Submit Observation"))),
        fluidRow(column(width = 4,
                        fluidRow(dataTableOutput("table1"))),
                 column(width = 8,
                        fluidRow(plotOutput("plot1")),
                        fluidRow(dataTableOutput("table2"))))
      )
    } else {
      fluidPage(
        fluidRow(column(width = 4,
                        fluidRow(dataTableOutput("table1"))),
                 column(width = 8,
                        fluidRow(plotOutput("plot1")),
                        fluidRow(dataTableOutput("table2"))))
      )
    }
  })
  
  myData <- reactiveValues(data = read.csv("myData2022.csv"))
  
  observeEvent(input$submit, {
    myData$data <- myData$data %>%
      bind_rows(data.frame(Observer = input$name,
                           Type = as.numeric(input$type),
                           Colour = input$colour))
    myData$data %>%
      write.csv("myData2022.csv", row.names = FALSE)
  })
  
  output$table1 <- DT::renderDataTable({
    if(input$year == 2021) {
      useData <- read.csv("myData2021.csv") %>%
        bind_rows(data.frame(Observer = input$name,
                             Type = as.numeric(input$type),
                             Colour = input$colour))
    }
    
    if(input$year == 2022) {
      useData <- myData$data
    }
    
    useData %>%
      slice(-1)},
    rownames = FALSE,
    options = list(pageLength = 16,
                   searching = FALSE,
                   ordering = FALSE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                     "}")))
  
  output$plot1 <- renderPlot({
    if(input$year == 2021) {
      useData <- read.csv("myData2021.csv") %>%
        bind_rows(data.frame(Observer = input$name,
                             Type = as.numeric(input$type),
                             Colour = input$colour))
    }
    
    if(input$year == 2022) {useData <- myData$data}
    
    ggplot(useData %>%
             slice(-1) %>%
             group_by(Type, Colour) %>%
             summarise(n = n()) %>%
             ungroup() %>%
             complete(Type = 1:6,
                      Colour = colourLevels,
                      fill = list(n = 0)) %>%
             mutate(Colour = factor(Colour, levels = colourLevels))) +
      geom_bar(aes(x = Type, y = n, fill = Colour),
               col = "black", stat = "identity") +
      theme_bw(14) +
      labs(x = "Plastic Type",
           y = "Frequency",
           fill = "Colour") +
      fillScalePlastic +
      scale_x_continuous(breaks = 1:6)
  })
  
  output$table2 <- renderDataTable({
    if(input$year == 2021) {
      useData <- read.csv("myData2021.csv") %>%
        bind_rows(data.frame(Observer = input$name,
                             Type = as.numeric(input$type),
                             Colour = input$colour))
    }
    
    if(input$year == 2022) {useData <- myData$data}
    
    useData %>%
      slice(-1) %>%
      group_by(Type, Colour) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      complete(Type = 1:6,
               Colour = colourLevels,
               fill = list(n = 0)) %>%
      pivot_wider(id_cols = Type, names_from = Colour, values_from = n) %>%
      select(Type, colourLevels) %>%
      rename_with(str_to_title)},
    rownames = FALSE,
    options = list(paging = FALSE,
                   dom = "ft",
                   ordering = FALSE,
                   searching = FALSE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                     "}")))
}


# ---- Run App ----
shinyApp(ui, server)