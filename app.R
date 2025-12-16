library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  titlePanel("Experimental Design: Sample Size for Population Mean"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h5("Study Parameters"),
      
      numericInput("k", "Number of Subgroups", 3, min = 2, max = 5),
      
      hr(),
      
      h6("Subgroup Variance (Bias proxy)"),
      numericInput("v1", "Group 1", 6),
      numericInput("v2", "Group 2", 10),
      numericInput("v3", "Group 3", 8),
      numericInput("v4", "Group 4", 5),
      numericInput("v5", "Group 5", 4),
      
      hr(),
      
      h6("Cost per Observation"),
      numericInput("c1", "Group 1", 2),
      numericInput("c2", "Group 2", 4),
      numericInput("c3", "Group 3", 3),
      numericInput("c4", "Group 4", 5),
      numericInput("c5", "Group 5", 6),
      
      hr(),
      
      h6("Time per Observation"),
      numericInput("t1", "Group 1", 1),
      numericInput("t2", "Group 2", 2),
      numericInput("t3", "Group 3", 1.5),
      numericInput("t4", "Group 4", 2.5),
      numericInput("t5", "Group 5", 3)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Sample Allocation",
          br(),
          tableOutput("allocation")
        ),
        
        tabPanel(
          "Design Plot",
          br(),
          plotOutput("barplot", height = "350px")
        ),
        
        tabPanel(
          "Cost–Bias Tradeoff",
          br(),
          plotOutput("scatter", height = "350px")
        ),
        
        tabPanel(
          "Explanation",
          br(),
          p("This application demonstrates sample size determination for estimating a population mean under stratified sampling (Sampling–2)."),
          p("Sample sizes are allocated across subgroups based on variability (variance), expected cost, and time of survey."),
          p("Higher-variance subgroups receive larger sample sizes to reduce sampling bias, while cost and time constraints regulate feasibility.")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    k <- input$k
    
    variance <- c(input$v1, input$v2, input$v3, input$v4, input$v5)[1:k]
    cost <- c(input$c1, input$c2, input$c3, input$c4, input$c5)[1:k]
    time <- c(input$t1, input$t2, input$t3, input$t4, input$t5)[1:k]
    
    weight <- variance / (cost * time)
    sample_size <- round(200 * weight / sum(weight))
    
    data.frame(
      Subgroup = paste("Group", 1:k),
      Variance = variance,
      Cost = cost,
      Time = time,
      Sample_Size = sample_size
    )
  })
  
  output$allocation <- renderTable({
    data()
  })
  
  output$barplot <- renderPlot({
    ggplot(data(), aes(Subgroup, Sample_Size, fill = Subgroup)) +
      geom_col() +
      labs(
        title = "Experimental Design: Sample Size Allocation",
        x = "Subgroup",
        y = "Allocated Sample Size"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$scatter <- renderPlot({
    ggplot(data(), aes(Cost * Time, Variance, size = Sample_Size)) +
      geom_point(alpha = 0.7, color = "#2c3e50") +
      labs(
        title = "Cost–Bias–Sample Size Relationship",
        x = "Expected Survey Effort (Cost × Time)",
        y = "Variance (Bias Indicator)",
        size = "Sample Size"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
