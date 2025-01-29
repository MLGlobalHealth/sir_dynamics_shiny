library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)

source("helper_functions.R")

ui <- page_navbar(
  title = span(img(src = "UoB_RGB_24.svg", height = 35), "  More complicated models explorer tool"),
  bg = "#FDEDEC",
  nav_panel(title = "Example 1",
            page_sidebar(# App title
              titlePanel(title = "Childhood vaccination"),
              
              # Enables MathJax notation
              withMathJax(),
              
              # Sidebar panel for inputs 
              sidebar = sidebar(
                #grid text is the slide numbers
                tags$head(
                  tags$style(HTML(".irs-grid-text { font-size: 14px;}
                                   .irs--shiny .irs-min,.irs--shiny .irs-max {font-size: 14px;}
                                   .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {font-size: 14px;}
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
                  inputId = "theta",
                  label = "Birth rate (\\( \\theta \\)):",
                  min = 0.0001,
                  max = 1,
                  value = 0.01
                ),
                sliderInput(
                  inputId = "mu",
                  label = "Death rate (\\( \\mu \\)):",
                  min = 0.0001,
                  max = 1,
                  value = 0.1
                ),
                sliderInput(
                  inputId = "u",
                  label = "Proportion of children vaccinated (\\( u \\)):",
                  min = 0,
                  max = 1,
                  value = 0.5
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
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "sir.png",
                         width = 500,
                         alt = "Childhood vaccination model"
                       ),
                       tags$figcaption("Childhood vaccination model diagram")
                     ),
                     uiOutput("show_code_childhood_vaccination")),
                
                card(card_header("Plot of dynamics"),
                     plotOutput(outputId = "distPlot_childhood_vaccination"),
                     
                     checkboxGroupInput(inputId = "trend_childhood_vaccination", 
                                        label = "Select which trends to plot:", 
                                        choices = c("V", "S", "I", "R"),
                                        width = "100%",
                                        inline = TRUE)
                     
                     
                )))),
  nav_panel(title = "Example 2",
            page_sidebar(# App title
              titlePanel(title =  "Emergency vaccination"),
              
              # Enables MathJax notation
              withMathJax(),
              
              # Sidebar panel for inputs 
              sidebar = sidebar(
                #grid text is the slide numbers
                tags$head(
                  tags$style(HTML(".irs-grid-text { font-size: 14px;}
                                   .irs--shiny .irs-min,.irs--shiny .irs-max {font-size: 14px;}
                                   .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {font-size: 14px;}
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
                  inputId = "gamma",
                  label = "Progression to infection rate (\\( \\gamma \\)):",
                  min = 0.0001,
                  max = 1,
                  value = 0.5
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
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "seir.png",
                         width = 500,
                         alt = "SEIR model flow diagram"
                       ),
                       tags$figcaption("SEIR model flow diagram")
                     ),
                     uiOutput("show_code_seir")),
                
                card(card_header("Plot of dynamics"),
                     # Output: Histogram ----
                     plotOutput(outputId = "distPlot_seir"),
                     
                     checkboxGroupInput(inputId = "trend_seir", 
                                        label = "Select which trends to plot:", 
                                        choices = c("S", "E", "I", "R"),
                                        width = "100%",
                                        inline = TRUE)
                     
                     
                )))),
  nav_panel(title = "Example 3",
            page_sidebar(# App title
              titlePanel(title =  "Example of two population heterogeneity"),
              
              # Enables MathJax notation
              withMathJax(),
              
              # Sidebar panel for inputs 
              sidebar = sidebar(
                #grid text is the slide numbers
                tags$head(
                  tags$style(HTML(".irs-grid-text { font-size: 14px;}
                                   .irs--shiny .irs-min,.irs--shiny .irs-max {font-size: 14px;}
                                   .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {font-size: 14px;}
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
                  inputId = "gamma",
                  label = "Progression to infection rate (\\( \\gamma \\)):",
                  min = 0.0001,
                  max = 1,
                  value = 0.5
                ),
                sliderInput(
                  inputId = "sigma",
                  label = "Recovery rate (\\( \\sigma \\)):",
                  min = 0.0001,
                  max = 1,
                  value = 0.1
                ),
                sliderInput(
                  inputId = "alpha",
                  label = "Waning of immunity rate (\\( \\alpha \\)):",
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
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "seirs.png",
                         width = 500,
                         alt = "SEIRS model flow diagram"
                       ),
                       tags$figcaption("SEIRS model flow diagram")
                     ),
                     uiOutput("show_code_seirs")),
                
                card(card_header("Plot of dynamics"),
                     # Output: Histogram ----
                     plotOutput(outputId = "distPlot_seirs"),
                     
                     checkboxGroupInput(inputId = "trend_seirs", 
                                        label = "Select which trends to plot:", 
                                        choices = c("S", "E", "I", "R"),
                                        width = "100%",
                                        inline = TRUE)
                     
                     
                )))),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("odin", href = "https://mrc-ide.github.io/odin/index.html")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
    )
)
  
  
  



server <- function(input, output) {
  # Code for childhood vaccination plot
  output$distPlot_childhood_vaccination <- renderPlot({
    
    y = run_child_vaccination(N = input$N, 
                              I_init = input$I_init,
                              beta = input$beta, 
                              sigma = input$sigma,
                              theta = input$theta,
                              mu = input$mu,
                              u = input$u,
                              max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)

    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend_childhood_vaccination)
    })
    
    my_colors = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    names(my_colors) <-  rev(unique(y_long$variable))  
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), linewidth = 2) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, input$N)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, input$max_t*1.1)) + 
      scale_color_manual(values = my_colors) +
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  # Code to show SIR code
  output$show_code_childhood_vaccination <- renderUI({
    raw_lines <- readLines("child_vaccination.R")
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
  
  # Code for SEIR plot
  output$distPlot_seir <- renderPlot({
    
    y = run_seir_model(N = input$N, 
                       I_init = input$I_init, 
                       beta = input$beta,
                       gamma = input$gamma,
                       sigma = input$sigma,
                       max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)
    
    my_colors_seir = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    names(my_colors_seir) <-  rev(unique(y_long$variable))
    
    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend_seir)
    })
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), linewidth = 2) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, input$N)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, input$max_t*1.1)) + 
      scale_color_manual(values = my_colors_seir) +
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  # Code to show SEIR code
  output$show_code_seir <- renderUI({
    raw_lines <- readLines("seir_model.R")
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
  
  # Code for SEIRS plot
  output$distPlot_seirs <- renderPlot({
    
    y = run_seirs_model(N = input$N, 
                       I_init = input$I_init, 
                       beta = input$beta,
                       gamma = input$gamma,
                       sigma = input$sigma,
                       alpha = input$alpha,
                       max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)
    
    my_colors_seirs = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    names(my_colors_seirs) <-  rev(unique(y_long$variable))
    
    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend_seirs)
    })
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), linewidth = 2) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, input$N)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, input$max_t*1.1)) + 
      scale_color_manual(values = my_colors_seirs) +
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  # Code to show SEIRS code
  output$show_code_seirs <- renderUI({
    raw_lines <- readLines("seirs_model.R")
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