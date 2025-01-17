library(shiny)
library(bslib)
library(dplyr)

source("helper_functions.R")

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title
  titlePanel(title = span(img(src = "UoB_CMYK_24.jpg", height = 35), "SIR (Susceptible - Infected - Recovered) dynamics")),
  
  # Enables MathJax notation
  withMathJax(),
  
  # Sidebar panel for inputs 
  sidebar = sidebar(
    #grid text is the slide numbers
    tags$head(
      tags$style(HTML("
    .irs-grid-text {
      font-size: 14px;
    }
    .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {
      font-size: 14px;
    }
    "))
    ),
    sliderInput(
      inputId = "N",
      label = "Number of people (N):",
      min = 1,
      max = 1000,
      value = 100
    ),
    sliderInput(
      inputId = "I_init",
      label = "Initial number of infected people (I_init):",
      min = 1,
      max = 100,
      value = 1
    ),
    sliderInput(
      inputId = "beta",
      label = 'Infection parameter (\\( \\beta \\)):',
      min = 0.1,
      max = 10,
      value = 2
  ),
  sliderInput(
    inputId = "sigma",
    label = "Recovery rate (\\( \\sigma \\)):",
    min = 0.0001,
    max = 1,
    value = 0.1
  ),
  sliderInput(
    inputId = "max_t",
    label = "Maximum time:",
    min = 20,
    max = 1000,
    value = 100
  )
  ),
  layout_columns(
    card(card_header("Odin model code"),
         uiOutput("show_code"),),
    
    card(card_header("Plot of dynamics"),
         # Output: Histogram ----
                  plotOutput(outputId = "distPlot"),
                  
                  checkboxGroupInput(inputId = "trend", 
                                     label = "Select which trends to plot:", 
                                     choices = c("S", "I", "R"),
                                     width = "100%",
                                     inline = TRUE)
    
  
  ))
  

)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    y = run_sir_model(N = input$N, 
                      I_init = input$I_init, 
                      beta = input$beta, 
                      sigma = input$sigma,
                      max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)

    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend)
    })
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), size = 2) + 
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) + 
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  output$show_code <- renderUI({
    raw_lines <- readLines("sir_model.R")
    # insert line breaks for HTML
    code_joined <- stringi::stri_join(raw_lines, collapse = "\n")
    
    tagList(
      tags$pre(
        tags$code(
          HTML(code_joined)
        )
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)