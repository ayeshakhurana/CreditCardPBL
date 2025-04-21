library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Load data
data <- read.csv("creditcard.csv")

# Convert 'Class' to factor
data$Class <- as.factor(data$Class)
levels(data$Class) <- c("Not Fraud", "Fraud")

ui <- dashboardPage(
  dashboardHeader(title = "Credit Card Fraud Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalTransactions"),
                valueBoxOutput("fraudCount"),
                valueBoxOutput("fraudPercent")
              ),
              fluidRow(
                box(title = "Class Distribution", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("classPlot")),
                box(title = "Amount Distribution", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("amountHist"))
              )
      ),
      
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Amount vs Time", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("scatterPlot"))
              ),
              fluidRow(
                box(title = "Correlation with Amount", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("correlationPlot"))
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Data Table", width = 12, status = "warning", solidHeader = TRUE,
                    DTOutput("dataTable"))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$totalTransactions <- renderValueBox({
    valueBox(nrow(data), "Total Transactions", icon = icon("list"), color = "blue")
  })
  
  output$fraudCount <- renderValueBox({
    frauds <- sum(data$Class == "Fraud")
    valueBox(frauds, "Fraudulent Transactions", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$fraudPercent <- renderValueBox({
    percent <- round(mean(data$Class == "Fraud") * 100, 3)
    valueBox(paste0(percent, "%"), "Fraud Percentage", icon = icon("percent"), color = "orange")
  })
  
  output$classPlot <- renderPlot({
    ggplot(data, aes(x = Class, fill = Class)) +
      geom_bar() +
      labs(title = "Fraud vs Non-Fraud Transactions", x = "", y = "Count") +
      scale_fill_manual(values = c("skyblue", "tomato"))
  })
  
  output$amountHist <- renderPlot({
    ggplot(data[data$Amount < 2000,], aes(x = Amount)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "white") +
      labs(title = "Transaction Amount Distribution", x = "Amount", y = "Frequency")
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(data[data$Amount < 2000,], aes(x = Time, y = Amount, color = Class)) +
      geom_point(alpha = 0.3) +
      labs(title = "Amount vs Time", x = "Time", y = "Amount") +
      scale_color_manual(values = c("skyblue", "tomato"))
  })
  
  output$correlationPlot <- renderPlot({
    corr_data <- data %>%
      group_by(Class) %>%
      summarise(across(starts_with("V"), ~cor(.x, Amount)))
    corr_df <- as.data.frame(t(corr_data[,-1]))
    names(corr_df) <- levels(data$Class)
    corr_df$Feature <- rownames(corr_df)
    
    ggplot(corr_df, aes(x = Feature)) +
      geom_line(aes(y = `Not Fraud`, color = "Not Fraud")) +
      geom_line(aes(y = Fraud, color = "Fraud")) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = "Correlation of Features with Amount", y = "Correlation", x = "Feature")
  })
  
  output$dataTable <- renderDT({
    datatable(head(data, 1000), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
