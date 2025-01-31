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
              titlePanel(title = "Births and deaths"),
              
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
                  inputId = "N_init",
                  label = "Initial number of people (N_init):",
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
                  min = 0,
                  max = 1,
                  value = 0.3
                ),
                sliderInput(
                  inputId = "theta",
                  label = "Birth rate (\\( \\theta \\)):",
                  min = 0,
                  max = 0.5,
                  value = 0.05
                ),
                sliderInput(
                  inputId = "mu",
                  label = "Death rate (\\( \\mu \\)):",
                  min = 0,
                  max = 0.25,
                  value = 0.01
                ),
                sliderInput(
                  inputId = "max_t",
                  label = "Maximum time:",
                  min = 20,
                  max = 1000,
                  value = 200
                )
              ),
              layout_columns(
                card(card_header("Odin model code"),
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "births_deaths.png",
                         width = 500,
                         alt = "Births and deaths model"
                       ),
                       tags$figcaption("Births and deaths model diagram")
                     ),
                     uiOutput("show_code_births_deaths")),
                
                card(card_header("Plot of dynamics"),
                     plotOutput(outputId = "distPlot_births_deaths"),
                     
                     checkboxGroupInput(inputId = "trend_births_deaths", 
                                        label = "Select which trends to plot:", 
                                        choices = c("S", "I", "R", "N"),
                                        width = "100%",
                                        inline = TRUE)
                     
                     
                )))),
  nav_panel(title = "Example 2",
            page_sidebar(# App title
              titlePanel(title = "Infant vaccination"),
              
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
                  inputId = "N_init",
                  label = "Initial number of people (N_init):",
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
                  min = 0,
                  max = 1,
                  value = 0.3
                ),
                sliderInput(
                  inputId = "theta",
                  label = "Birth rate (\\( \\theta \\)):",
                  min = 0,
                  max = 0.5,
                  value = 0.05
                ),
                sliderInput(
                  inputId = "mu",
                  label = "Death rate (\\( \\mu \\)):",
                  min = 0,
                  max = 0.25,
                  value = 0.01
                ),
                sliderInput(
                  inputId = "u",
                  label = "Proportion of infants vaccinated (\\( u \\)):",
                  min = 0,
                  max = 1,
                  value = 0.25
                ),
                sliderInput(
                  inputId = "max_t",
                  label = "Maximum time:",
                  min = 20,
                  max = 1000,
                  value = 200
                )
              ),
              layout_columns(
                card(card_header("Odin model code"),
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "child_vaccination.png",
                         width = 500,
                         alt = "Infant vaccination model"
                       ),
                       tags$figcaption("Infant vaccination model diagram")
                     ),
                     uiOutput("show_code_childhood_vaccination")),
                
                card(card_header("Plot of dynamics"),
                     plotOutput(outputId = "distPlot_childhood_vaccination"),
                     
                     checkboxGroupInput(inputId = "trend_childhood_vaccination", 
                                        label = "Select which trends to plot:", 
                                        choices = c("S", "V", "I", "R", "N"),
                                        width = "100%",
                                        inline = TRUE)
                     
                     
                )))),
  nav_panel(title = "Example 3",
            page_sidebar(# App title
              titlePanel(title = "Emergency vaccination"),
              
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
                  inputId = "u",
                  label = "Vaccination rate (\\( u \\)):",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                sliderInput(
                  inputId = "sigma",
                  label = "Recovery rate (\\( \\sigma \\)):",
                  min = 0,
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
                         src = "emergency_vaccination.png",
                         width = 500,
                         alt = "Emergency vaccination flow diagram"
                       ),
                       tags$figcaption("Emergency vaccination model flow diagram")
                     ),
                     uiOutput("show_code_emergency_vaccination")),
                
                card(card_header("Plot of dynamics"),
    
                     plotOutput(outputId = "distPlot_emergency_vaccination"),
                     
                     checkboxGroupInput(inputId = "trend_emergency_vaccination", 
                                        label = "Select which trends to plot:", 
                                        choices = c("S", "V", "I", "R"),
                                        width = "100%",
                                        inline = TRUE)
                     
                     
                )))),
  nav_panel(title = "Example 4",
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
                  inputId = "N_A",
                  label = "Number of people in population A (N_A):",
                  min = 1,
                  max = 1000,
                  value = 100
                ),
                sliderInput(
                  inputId = "N_B",
                  label = "Number of people in population B (N_B):",
                  min = 1,
                  max = 1000,
                  value = 100
                ),
                sliderInput(
                  inputId = "I_init_A",
                  label = "Initial number of infected people in population A (I_init_A):",
                  min = 1,
                  max = 100,
                  value = 1
                ),
                sliderInput(
                  inputId = "I_init_B",
                  label = "Initial number of infected people in population B (I_init_B):",
                  min = 1,
                  max = 100,
                  value = 1
                ),
                sliderInput(
                  inputId = "beta_AA",
                  label = 'Infection parameter from group A to A (\\( \\beta_{AA} \\)):',
                  min = 0.1,
                  max = 10,
                  value = 2
                ),
                sliderInput(
                  inputId = "beta_BA",
                  label = 'Infection parameter from group B to A (\\( \\beta_{BA} \\)):',
                  min = 0.1,
                  max = 10,
                  value = 2
                ),
                sliderInput(
                  inputId = "beta_AB",
                  label = 'Infection parameter from group A to B (\\( \\beta_{AB} \\)):',
                  min = 0.1,
                  max = 10,
                  value = 2
                ),
                sliderInput(
                  inputId = "beta_BB",
                  label = 'Infection parameter from group B to B (\\( \\beta_{BB} \\)):',
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
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "heterogeneity.png",
                         width = 500,
                         alt = "Two population SIR flow diagram"
                       ),
                       tags$figcaption("Two population SIR model flow diagram")
                     ),
                     uiOutput("show_code_heterogeneity")),
                card(card_header("Plot of dynamics"),
                     plotOutput(outputId = "distPlot_heterogeneity"),
                     
                     checkboxGroupInput(inputId = "trend_heterogeneity", 
                                        label = "Select which trends to plot:", 
                                        choices = c("S_A", "S_B", "I_A", "I_B", "R_A", "R_B"),
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
  output$distPlot_births_deaths <- renderPlot({
    
    y = run_births_deaths(N_init = input$N_init,
                          I_init = input$I_init,
                          beta = input$beta,
                          sigma = input$sigma,
                          theta = input$theta,
                          mu = input$mu,
                          max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)
    
    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend_births_deaths)
    })
    
    my_colors_births_deaths = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    names(my_colors_births_deaths) <-  rev(unique(y_long$variable))  
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), linewidth = 2) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, input$N)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, input$max_t*1.1)) + 
      scale_color_manual(values = my_colors_births_deaths) +
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  # Code to show childhood vaccination code
  output$show_code_births_deaths <- renderUI({
    raw_lines <- readLines("births_deaths.R")
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
  
  # Code for childhood vaccination plot
  output$distPlot_childhood_vaccination <- renderPlot({
    
    y = run_child_vaccination(N_init = input$N_init, 
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
    
    my_colors = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#619CFF")
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
  
  # Code to show childhood vaccination code
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
  
  # Code for emergency vaccination plot
  output$distPlot_emergency_vaccination <- renderPlot({
    
    y = run_emergency_vaccination(N = input$N, 
                                  I_init = input$I_init, 
                                  beta = input$beta,
                                  u = input$u,
                                  sigma = input$sigma,
                                  max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)
    
    my_colors_emergency_vaccination = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    names(my_colors_emergency_vaccination) <-  rev(unique(y_long$variable))
    
    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend_emergency_vaccination)
    })
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), linewidth = 2) + 
      scale_y_continuous(expand = c(0, 0), limits = c(-0.1, input$N_init)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, input$max_t*1.1)) + 
      scale_color_manual(values = my_colors_emergency_vaccination) +
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  # Code to show Emergency Vaccination code
  output$show_code_emergency_vaccination <- renderUI({
    raw_lines <- readLines("emergency_vaccination.R")
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
  
  # Code for heterogeneity plot
  output$distPlot_heterogeneity <- renderPlot({
    
    y = run_heterogeneity(N_A = input$N_A,
                          N_B = input$N_B,
                          I_init_A = input$I_init_A, 
                          I_init_B = input$I_init_B, 
                          beta_AA = input$beta_AA,
                          beta_BA = input$beta_BA,
                          beta_AB = input$beta_AB,
                          beta_BB = input$beta_BB,
                          sigma = input$sigma,
                          max_t = input$max_t)
    
    y_long = gather(data.frame(y), 
                    key = variable, 
                    value = value, 
                    -t)
    
    my_colors_heterogeneity = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#619CFF", "#F564E3")
    names(my_colors_heterogeneity) <-  rev(unique(y_long$variable))
    
    ## filter the data
    filtered_data <- reactive({
      filter(y_long, variable %in% input$trend_heterogeneity)
    })
    
    # plot the selected trends
    ggplot(filtered_data()) +
      geom_line(aes(t, value, col = variable), linewidth = 2) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, input$N)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, input$max_t*1.1)) + 
      scale_color_manual(values = my_colors_heterogeneity) +
      xlab("Time") + ylab("Number of people") +
      theme_bw() +
      theme(legend.title=element_blank(), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            text = element_text(size = 16))
  })
  
  # Code to show heterogeneity code
  output$show_code_heterogeneity <- renderUI({
    raw_lines <- readLines("heterogeneity.R")
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