suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
source("utils.R")


ui <- fluidPage(

    
    titlePanel("Log Pots Approximation Tool"),

     
    sidebarLayout(
        sidebarPanel(
            radioButtons("taper", "Taper", choices = c("Log" = "log",
                                                       "Reverse Log" = "antilog")),
            textInput(inputId = "real_pot", label = "Log Pot (kohms)", 
                      value = "25"),
            textInput(inputId = "linear_pot", label = "Linear Pot (kohms)", 
                      value = "100"),
            actionButton("go", "GO"),
            br(),
            uiOutput("slider1")
            
        ),

        
        
        mainPanel(
            plotlyOutput("Plotly")
            
           
        )
    )
)


server <- function(input, output) {
    
    
    df_plot_data <- tibble(rotation=seq(0, 100, 0.1))
    max_res <- 50
    
    v <- reactiveValues(pot=25, real_pot=100, go_flag=FALSE, taper=NULL)
    
    
    
    load_resistor <- reactive({ input$load_resistor })

    
    observeEvent(input$go, {
        v$go_flag <- TRUE
        v$real_pot <- as.double(input$real_pot)
        v$pot <- as.double(input$linear_pot)
        v$taper <- input$taper

        
    })
    
    
    
    output$Plotly <- renderPlotly({

        if (!(v$go_flag)) return()

        
        if (v$taper =="antilog") {
        
            p1 <- df_plot_data %>%
                mutate(Real = round(anti_log_pot(rotation, pot=v$real_pot),2),
                       Approximation = round(anti_log_aprox(rotation,
                                                      pot=v$pot, 
                                                      load_res=load_resistor()),2)) %>%
                pivot_longer(c(Real, Approximation), names_to = "potentiometer",
                             values_to = "resistance") %>%
                ggplot(aes(rotation, resistance, colour=potentiometer)) + 
                geom_line() + 
                theme_minimal() +
                labs(title= ' Real vs Approx. Anti-log', x="Rotation %", 
                     y="Resistance (kohms)") +
                theme(legend.title = element_blank())
            
        } 
        
        if (v$taper == "log") {
            
            p1 <- df_plot_data %>%
                mutate(Real = round(log_pot(rotation, pot=v$real_pot),2),
                       Approximation = round(log_aprox(rotation,
                                                            pot=v$pot, 
                                                            load_res=load_resistor()),2)) %>%
                pivot_longer(c(Real, Approximation), names_to = "potentiometer",
                             values_to = "resistance") %>%
                ggplot(aes(rotation, resistance, colour=potentiometer)) + 
                geom_line() + 
                theme_minimal() +
                labs(title= ' Real vs Approx. Log', x="Rotation %", 
                     y="Resistance (kohms)") +
                theme(legend.title = element_blank())
        }
        
        ggplotly(p1) 
    })
    
    
    output$slider1 <- renderUI({
        if (!is.null(v$pot)){
            max_res <- v$pot
        }

        sliderInput("load_resistor",
                    "Loading Resistor (kohms)",
                    min = 0.1,
                    max = max_res,
                    value = max_res/2,
                    step= 0.1)
    })
    

}


shinyApp(ui = ui, server = server)
