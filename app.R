# ================================
# Sampling–2 Shiny Application
# Population Mean – Stratified Allocation
# Proportional | Neyman | Optimised
# ================================

library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)
library(tidyr)

# ----------------
# UI
# ----------------
ui <- fluidPage(

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  titlePanel("Sample Size Determination for Population Mean (Sampling–2)"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h5("Study Design Inputs"),

      numericInput("k", "Number of Strata (Subgroups)", 3, min = 2, max = 5),

      hr(),

      h6("Variance (Bias Proxy)"),
      numericInput("v1", "Stratum 1", 6),
      numericInput("v2", "Stratum 2", 10),
      numericInput("v3", "Stratum 3", 8),
      numericInput("v4", "Stratum 4", 5),
      numericInput("v5", "Stratum 5", 4),

      hr(),

      h6("Cost per Observation"),
      numericInput("c1", "Stratum 1", 2),
      numericInput("c2", "Stratum 2", 4),
      numericInput("c3", "Stratum 3", 3),
      numericInput("c4", "Stratum 4", 5),
      numericInput("c5", "Stratum 5", 6),

      hr(),

      h6("Time per Observation"),
      numericInput("t1", "Stratum 1", 1),
      numericInput("t2", "Stratum 2", 2),
      numericInput("t3", "Stratum 3", 1.5),
      numericInput("t4", "Stratum 4", 2.5),
      numericInput("t5", "Stratum 5", 3)
    ),

    mainPanel(
      tabsetPanel(

        tabPanel(
          "Optimised Allocation",
          br(),
          tableOutput("opt_table"),
          br(),
          plotOutput("opt_plot", height = "350px")
        ),

        tabPanel(
          "Cost–Bias Tradeoff",
          br(),
          plotOutput("tradeoff_plot", height = "350px")
        ),

        tabPanel(
          "Allocation Comparison",
          br(),
          tableOutput("compare_table"),
          br(),
          plotOutput("compare_plot", height = "350px")
        ),

        tabPanel(
          "Explanation",
          br(),
          p("This R Shiny application demonstrates sample size determination for estimating a population mean using stratified sampling (Sampling–2)."),
          p("Three allocation strategies are compared:"),
          tags$ul(
            tags$li("Proportional Allocation: Equal allocation across strata."),
            tags$li("Neyman Allocation: Allocation proportional to stratum variance."),
            tags$li("Optimised Allocation: Allocation proportional to variance and inversely proportional to cost and time.")
          ),
          p("The experimental design plots illustrate how statistical precision and operational constraints jointly influence sample size decisions.")
        )
      )
    )
  )
)

# ----------------
# SERVER
# ----------------
server <- function(input, output) {

  # Core data
  base_data <- reactive({
    k <- input$k

    variance <- c(input$v1, input$v2, input$v3, input$v4, input$v5)[1:k]
    cost <- c(input$c1, input$c2, input$c3, input$c4, input$c5)[1:k]
    time <- c(input$t1, input$t2, input$t3, input$t4, input$t5)[1:k]

    data.frame(
      Stratum = paste("Stratum", 1:k),
      Variance = variance,
      Cost = cost,
      Time = time
    )
  })

  # Optimised allocation
  optimised_data <- reactive({
    df <- base_data()
    Ntotal <- 200

    weight <- df$Variance / (df$Cost * df$Time)
    sample_size <- round(Ntotal * weight / sum(weight))

    df$Sample_Size <- sample_size
    df
  })

  # Optimised table
  output$opt_table <- renderTable({
    optimised_data()
  })

  # Optimised plot
  output$opt_plot <- renderPlot({
    ggplot(optimised_data(),
           aes(Stratum, Sample_Size, fill = Stratum)) +
      geom_col() +
      labs(
        title = "Experimental Design: Optimised Sample Allocation",
        x = "Stratum",
        y = "Sample Size"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Cost–Bias Tradeoff plot
  output$tradeoff_plot <- renderPlot({
    ggplot(optimised_data(),
           aes(Cost * Time, Variance, size = Sample_Size)) +
      geom_point(alpha = 0.7, color = "#2c3e50") +
      labs(
        title = "Cost–Bias–Sample Size Tradeoff",
        x = "Survey Effort (Cost × Time)",
        y = "Variance (Bias Indicator)",
        size = "Sample Size"
      ) +
      theme_minimal()
  })

  # Allocation comparison
  allocation_methods <- reactive({
    df <- base_data()
    k <- nrow(df)
    Ntotal <- 200

    # Proportional
    prop <- rep(round(Ntotal / k), k)

    # Neyman
    neyman <- round(Ntotal * df$Variance / sum(df$Variance))

    # Optimised
    opt <- optimised_data()$Sample_Size

    data.frame(
      Stratum = df$Stratum,
      Proportional = prop,
      Neyman = neyman,
      Optimised = opt
    )
  })

  output$compare_table <- renderTable({
    allocation_methods()
  })

  output$compare_plot <- renderPlot({
    allocation_methods() |>
      pivot_longer(-Stratum, names_to = "Method", values_to = "Sample_Size") |>
      ggplot(aes(Stratum, Sample_Size, fill = Method)) +
      geom_col(position = "dodge") +
      labs(
        title = "Comparison of Allocation Methods",
        x = "Stratum",
        y = "Sample Size"
      ) +
      theme_minimal()
  })
}

# ----------------
# RUN APP
# ----------------
shinyApp(ui = ui, server = server)
