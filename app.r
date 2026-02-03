library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)

ui <- dashboardPage(
  dashboardHeader(title = "Statistical Analysis Tool"),
  
  dashboardSidebar(
    fileInput(
      "file",
      "Upload Excel or CSV File",
      accept = c(".xlsx", ".csv")
    ),
    hr(),
    uiOutput("var_select_x"),
    uiOutput("var_select_y")
  ),
  
  dashboardBody(
    tabsetPanel(
      
      tabPanel(
        "Data Preview",
        DTOutput("data_table")
      ),
      
      tabPanel(
        "Descriptive Statistics",
        tableOutput("desc_stats")
      ),
      
      tabPanel(
        "Correlation Matrix",
        tableOutput("cor_matrix")
      ),
      
      tabPanel(
        "Regression",
        verbatimTextOutput("regression_output")
      ),
      
      tabPanel(
        "Scatter Plot",
        plotlyOutput("scatter_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Read uploaded file
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "xlsx") {
      read_excel(input$file$datapath)
    } else {
      read.csv(input$file$datapath)
    }
  })
  
  # Dynamic variable selectors
  output$var_select_x <- renderUI({
    req(data())
    selectInput(
      "xvar",
      "Select X Variable",
      choices = names(data()),
      selected = names(data())[1]
    )
  })
  
  output$var_select_y <- renderUI({
    req(data())
    selectInput(
      "yvar",
      "Select Y Variable",
      choices = names(data()),
      selected = names(data())[2]
    )
  })
  
  # Data table
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Descriptive statistics
  output$desc_stats <- renderTable({
    req(data())
    num_data <- select_if(data(), is.numeric)
    if (ncol(num_data) == 0) return(NULL)
    describe(num_data)
  }, rownames = TRUE)
  
  # Correlation matrix
  output$cor_matrix <- renderTable({
    req(data())
    num_data <- select_if(data(), is.numeric)
    if (ncol(num_data) < 2) return(NULL)
    round(cor(num_data, use = "complete.obs"), 3)
  }, rownames = TRUE)
  
  # Regression output
  output$regression_output <- renderPrint({
    req(data(), input$xvar, input$yvar)
    df <- data()
    
    if (!is.numeric(df[[input$xvar]]) ||
        !is.numeric(df[[input$yvar]])) {
      cat("Both variables must be numeric.")
      return()
    }
    
    model <- lm(df[[input$yvar]] ~ df[[input$xvar]])
    summary(model)
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    req(data(), input$xvar, input$yvar)
    
    p <- ggplot(
      data(),
      aes_string(x = input$xvar, y = input$yvar)
    ) +
      geom_point(color = "steelblue", size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)