library(shiny)
library(shinythemes)
library(tidyverse)
# Load the data
fn <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
data <- read.csv(file=fn)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("COVID-19 Active and Recovered Cases"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("selected_country", 
                     "Choose a country:", 
                     choices = unique(data$location), 
                     options = list(placeholder = 'Select a country', onInitialize = I('function() { this.setValue(""); }'))),
      checkboxInput("log_scale", "Logarithmic Scale", TRUE),
      br(),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("covidPlot")),
        tabPanel("Data", DT::dataTableOutput("covidData"))  # Placeholder for the data table
      )
    )
  )
)

server <- function(input, output) {
  
  # This reactive expression creates the subset d2 based on the selected country
  reactive_d2 <- reactive({
    TF <- data$location == input$selected_country
    d2 <- data[TF, ]
    d2$new_cases[is.na(d2$new_cases)] <- 0
    
    # Calculate active and recovered cases
    active_cases <- rep(0.0, times=nrow(d2))
    recovered_cases <- rep(0.0, times=nrow(d2))
    for (i in 1:nrow(d2)) {
      if (i > 14) {
        recovered_cases[i] = sum(d2$new_cases[1:(i-14)])
      }
      active_cases[i] = sum(d2$new_cases[max(1, i-13):i]) # Last 14 days
    }
    
    d2$active_cases <- active_cases
    d2$recovered_cases <- recovered_cases
    return(d2)
  })
  
  # Render plot
  output$covidPlot <- renderPlot({
    d2 <- reactive_d2()
      
    
    # Plot the data
    p <- ggplot(d2, aes(x = as.Date(date))) +
      geom_point(aes(y = active_cases, color = "Active"), size = 0.5) +
      geom_line(aes(y = active_cases, color = "Active"), size = 0.8) + 
      geom_point(aes(y = recovered_cases, color = "Recovered"), size = 0.5) + 
      geom_line(aes(y = recovered_cases, color = "Recovered"), size = 0.8) + 
      scale_color_manual(name = "Case Type", 
                         values = c(Active = "red", Recovered = "turquoise"), 
                         labels = c("Active", "Recovered")) + 
      xlab("Date") + 
      ylab("Number of Cases") + 
      labs(title = paste("Number of Active and Recovered Cases in", input$selected_country),
           caption = "Source: https://covid.ourworldindata.org/data/owid-covid-data.csv
                            This plot represents the active and recovered cases of COVID-19.") +
      theme_minimal() + 
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"),
        legend.position = "top",
        legend.box = "horizontal",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    output$covidData <- DT::renderDataTable({
      d2 <- reactive_d2()
      DT::datatable(d2[, c("date", "new_cases", "total_cases", "active_cases", "recovered_cases")], options = list(pageLength = 25))
    })
    
    if(input$log_scale) {
      p <- p + scale_y_log10()
    }
    
    print(p)
    
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

names(data)
