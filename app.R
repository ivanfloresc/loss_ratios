library(shiny)

# Define UI for the premium rate calculator app
ui <- fluidPage(
  
  # App title
  titlePanel("Crop Insurance Premium Rate Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Select Crop:", choices = c("Soybeans", "Corn")),
      selectInput("state", "Select State:", choices = c("Nebraska", "Iowa")),
      selectInput("county", "Select County:", choices = c("Lancaster", "Douglas")),
      
      numericInput("aph_yield", "APH Yield (bushels per acre):", value = 35, min = 0, step = 0.01),
      numericInput("reference_yield", "Reference Yield (bushels per acre):", value = 31.5, min = 0, step = 0.01),
      numericInput("exponent", "Exponent:", value = -1.924, step = 0.01),
      numericInput("reference_rate", "Reference Rate:", value = 0.128, step = 0.001),
      numericInput("fixed_rate_load", "Fixed Rate Load:", value = 0.023, step = 0.001),
      numericInput("previous_base_rate", "Previous Year's Base Rate:", value = 0.122, step = 0.001),
      numericInput("additional_coverage_rate", "Additional Coverage Rate:", value = 0.151, step = 0.001),
      numericInput("multiplicative_factor", "Multiplicative Factor:", value = 1.0, step = 0.01),
      numericInput("coverage_level_rate_differential", "Coverage Level Rate Differential:", value = 0.57, step = 0.01),
      
      actionButton("calculate", "Calculate Premium Rate")
    ),
    
    mainPanel(
      h3("Calculated Premium Rate:"),
      verbatimTextOutput("premium_rate")
    )
  )
)



# Define server logic for the premium rate calculator app
server <- function(input, output, session) {
  
  # Define mock data for different crops and counties
  crop_data <- reactive({
    if (input$crop == "Soybeans") {
      if (input$state == "Nebraska" && input$county == "Lancaster") {
        return(list(reference_yield = 31.5, exponent = -1.924, reference_rate = 0.128, fixed_rate_load = 0.023))
      } else if (input$state == "Iowa" && input$county == "Douglas") {
        return(list(reference_yield = 40.0, exponent = -1.800, reference_rate = 0.130, fixed_rate_load = 0.020))
      }
    } else if (input$crop == "Corn") {
      if (input$state == "Nebraska" && input$county == "Lancaster") {
        return(list(reference_yield = 50.0, exponent = -2.000, reference_rate = 0.140, fixed_rate_load = 0.025))
      } else if (input$state == "Iowa" && input$county == "Douglas") {
        return(list(reference_yield = 55.0, exponent = -1.950, reference_rate = 0.150, fixed_rate_load = 0.030))
      }
    }
  })
  
  # Function to calculate Yield Ratio
  calculate_yield_ratio <- function(aph_yield, reference_yield) {
    ratio <- aph_yield / reference_yield
    return(pmin(pmax(ratio, 0.50), 1.50))
  }
  
  # Function to calculate Continuous Rating Base Rate
  calculate_base_rate <- function(yield_ratio, exponent, reference_rate, fixed_rate_load) {
    base_rate <- (yield_ratio^exponent) * reference_rate + fixed_rate_load
    return(round(base_rate, 8))
  }
  
  # Function to cap the Base Rate
  cap_base_rate <- function(current_base_rate, previous_base_rate) {
    return(pmin(current_base_rate, 1.2 * previous_base_rate))
  }
  
  # Function to adjust for additional coverage
  adjust_base_rate <- function(preliminary_rate, additional_coverage_rate, multiplicative_factor) {
    adjusted_rate <- (preliminary_rate + additional_coverage_rate) * multiplicative_factor
    return(round(adjusted_rate, 8))
  }
  
  # Function to calculate the Final Premium Rate
  calculate_premium_rate <- function(adjusted_rate, coverage_level_rate_differential) {
    premium_rate <- adjusted_rate * coverage_level_rate_differential
    return(round(premium_rate, 8))
  }
  
  # Observe event when the 'Calculate' button is pressed
  observeEvent(input$calculate, {
    # Get the data based on crop, state, and county
    data <- crop_data()
    
    # Calculate each step
    yield_ratio <- calculate_yield_ratio(input$aph_yield, data$reference_yield)
    current_base_rate <- calculate_base_rate(yield_ratio, data$exponent, data$reference_rate, data$fixed_rate_load)
    capped_base_rate <- cap_base_rate(current_base_rate, input$previous_base_rate)
    adjusted_rate <- adjust_base_rate(capped_base_rate, input$additional_coverage_rate, input$multiplicative_factor)
    final_premium_rate <- calculate_premium_rate(adjusted_rate, input$coverage_level_rate_differential)
    
    # Display the calculated premium rate
    output$premium_rate <- renderText({
      paste("The final premium rate is:", final_premium_rate)
    })
  })
  
}

shinyApp(ui = ui, server = server)