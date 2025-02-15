library(shiny)

ui <- fluidPage(
  titlePanel("CSV File Uploader and Visualizer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      uiOutput("scatterVarSelect")
    ),
    mainPanel(
      h3("Top 6 Rows of the File"),
      tableOutput("contents"),
      h3("Numeric Variable Histograms"),
      uiOutput("numericPlots"),
      h3("Categorical Variable Bar Charts"),
      uiOutput("categoricalPlots"),
      h3("Scatterplot of Selected Numeric Variables"),
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = TRUE)
  })
  
  output$contents <- renderTable({
    head(data())
  })
  
  output$numericPlots <- renderUI({
    df <- data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    plot_output_list <- lapply(num_vars, function(var) {
      plotname <- paste("plot", var, sep="_")
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    df <- data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    lapply(num_vars, function(var) {
      output[[paste("plot", var, sep="_")]] <- renderPlot({
        hist(df[[var]], main = paste("Histogram of", var), xlab = var, col = "skyblue", border = "white")
      })
    })
  })
  
  output$categoricalPlots <- renderUI({
    df <- data()
    cat_vars <- names(df)[sapply(df, is.factor)]
    plot_output_list <- lapply(cat_vars, function(var) {
      plotname <- paste("barplot", var, sep="_")
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    df <- data()
    cat_vars <- names(df)[sapply(df, is.factor)]
    lapply(cat_vars, function(var) {
      output[[paste("barplot", var, sep="_")]] <- renderPlot({
        barplot(table(df[[var]]), main = paste("Bar Chart of", var), col = "lightcoral", border = "white")
      })
    })
  })
  
  output$scatterVarSelect <- renderUI({
    df <- data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    if (length(num_vars) >= 2) {
      tagList(
        selectInput("xvar", "X-axis Variable", choices = num_vars),
        selectInput("yvar", "Y-axis Variable", choices = num_vars)
      )
    }
  })
  
  output$scatterPlot <- renderPlot({
    req(input$xvar, input$yvar)
    df <- data()
    plot(df[[input$xvar]], df[[input$yvar]],
         xlab = input$xvar, ylab = input$yvar,
         main = paste("Scatterplot of", input$xvar, "vs", input$yvar),
         col = "blue", pch = 19)
  })
}

shinyApp(ui, server)



