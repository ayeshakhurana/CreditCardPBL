library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(rmarkdown)

data <- read.csv("creditcard.csv")
data$Class <- as.factor(data$Class)
levels(data$Class) <- c("Not Fraud", "Fraud")
data$Hour <- floor(data$Time / 3600)

ui <- dashboardPage(
  dashboardHeader(title = "Credit Card Fraud Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-bar")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Report", tabName = "report", icon = icon("file-download"))
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
          box(title = "Class Distribution", width = 12,
              plotOutput("classPlot"),
              br(),
              uiOutput("classAnalysis")
          )
        ),
        fluidRow(
          box(title = "Transaction Amount Distribution (under $2000)", width = 12,
              plotOutput("amountHist"),
              br(),
              uiOutput("amountAnalysis")
          )
        )
      ),
      tabItem(tabName = "visuals",
        fluidRow(
          box(title = "Amount vs Time", width = 12,
              plotOutput("scatterPlot"),
              br(),
              uiOutput("scatterAnalysis")
          )
        ),
        fluidRow(
          box(title = "Boxplot of Amount by Class", width = 12,
              plotOutput("boxPlot"),
              br(),
              uiOutput("boxplotAnalysis")
          )
        ),
        fluidRow(
          box(title = "Transaction Frequency by Hour", width = 12,
              plotOutput("hourTrend"),
              br(),
              uiOutput("hourAnalysis")
          )
        ),
        fluidRow(
          box(title = "Fraud Rate by Hour", width = 12,
              plotOutput("fraudRateHour"),
              br(),
              uiOutput("fraudRateAnalysis")
          )
        )
      ),
      tabItem(tabName = "data",
        box(title = "Sample Data", width = 12, DTOutput("dataTable"))
      ),
      tabItem(tabName = "report",
        fluidRow(
          box(title = "Download Analysis Report", width = 6,
              downloadButton("downloadReport", "Download Report")
          )
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
    valueBox(sum(data$Class == "Fraud"), "Fraudulent Transactions", icon = icon("exclamation-triangle"), color = "red")
  })

  output$fraudPercent <- renderValueBox({
    percent <- round(mean(data$Class == "Fraud") * 100, 3)
    valueBox(paste0(percent, "%"), "Fraud Percentage", icon = icon("percent"), color = "orange")
  })

  output$classPlot <- renderPlot({
    ggplot(data, aes(x = Class, fill = Class)) +
      geom_bar() +
      scale_fill_manual(values = c("skyblue", "tomato")) +
      labs(x = "", y = "Count")
  })
  output$classAnalysis <- renderUI({
    HTML("<b>Analysis:</b> This plot shows a heavy class imbalance — most transactions are not fraudulent. This is common in fraud datasets and should be addressed in modeling.")
  })

  output$amountHist <- renderPlot({
    ggplot(data[data$Amount < 2000, ], aes(x = Amount)) +
      geom_histogram(bins = 50, fill = "steelblue") +
      labs(x = "Amount", y = "Frequency")
  })
  output$amountAnalysis <- renderUI({
    HTML("<b>Analysis:</b> Most transactions are low value. High-value transactions are rare but could represent more risk.")
  })

  output$scatterPlot <- renderPlot({
    ggplot(data[data$Amount < 2000, ], aes(x = Time, y = Amount, color = Class)) +
      geom_point(alpha = 0.4) +
      scale_color_manual(values = c("skyblue", "tomato")) +
      labs(x = "Time", y = "Amount")
  })
  output$scatterAnalysis <- renderUI({
    HTML("<b>Analysis:</b> Fraudulent transactions are scattered across time. No obvious clusters, which makes detection harder.")
  })

  output$boxPlot <- renderPlot({
    ggplot(data[data$Amount < 2000, ], aes(x = Class, y = Amount, fill = Class)) +
      geom_boxplot() +
      scale_fill_manual(values = c("skyblue", "tomato")) +
      labs(x = "Transaction Type", y = "Amount")
  })
  output$boxplotAnalysis <- renderUI({
    HTML("<b>Analysis:</b> Fraud transactions have wider variability and tend to include more high-value outliers.")
  })

  output$hourTrend <- renderPlot({
    data %>%
      group_by(Hour) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = Hour, y = count)) +
      geom_line(color = "darkgreen") +
      labs(x = "Hour of Day", y = "Transactions")
  })
  output$hourAnalysis <- renderUI({
    HTML("<b>Analysis:</b> Most transactions happen during the day. Fewer occur late at night or early morning.")
  })

  output$fraudRateHour <- renderPlot({
    data %>%
      group_by(Hour) %>%
      summarise(fraud_rate = mean(Class == "Fraud")) %>%
      ggplot(aes(x = Hour, y = fraud_rate)) +
      geom_line(color = "darkred") +
      labs(x = "Hour of Day", y = "Fraud Rate")
  })
  output$fraudRateAnalysis <- renderUI({
    HTML("<b>Analysis:</b> Slight peaks in fraud rate can be noticed during odd hours — possibly indicating less vigilance during those periods.")
  })

  output$dataTable <- renderDT({
    datatable(head(data, 1000), options = list(pageLength = 10))
  })

  # Dummy report download logic (You can customize to generate PDF with rmarkdown)
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("fraud_analysis_report", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      report_content <- paste(
        "<html><body><h1>Credit Card Fraud Report</h1>",
        "<p>This dataset contains", nrow(data), "transactions.</p>",
        "<p>Fraudulent transactions:", sum(data$Class == "Fraud"), "</p>",
        "<p>Fraud rate: ", round(mean(data$Class == "Fraud") * 100, 3), "%</p>",
        "<p><b>Insights:</b><ul>",
        "<li>Most transactions are non-fraud.</li>",
        "<li>Low-value transactions dominate.</li>",
        "<li>Fraud occurs across the timeline.</li>",
        "<li>Fraud is slightly more common at odd hours.</li>",
        "</ul></p></body></html>"
      )
      writeLines(report_content, file)
    }
  )
}

shinyApp(ui, server)
