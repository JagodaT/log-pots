suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
source("utils.R")


ui <- fluidPage(
    tags$head(
        tags$style(HTML("
        #image > img {
            display: block;
            margin: 0 auto;
            
        }"))
    ),
    

    titlePanel("Log Pots Approximation Tool"),
    br(),
     
    sidebarLayout(
        sidebarPanel(
            radioButtons("taper", "Taper", choices = c("Log" = "log",
                                                       "Anti-Log" = "antilog")),
            textInput(inputId = "real_pot", label = "Log Pot (kohms)", 
                      value = "25"),
            textInput(inputId = "linear_pot", label = "Linear Pot (kohms)", 
                      value = "100"),
            actionButton("go", "GO"),
            br(), br(),
            uiOutput("slider1"),
            width=3
            
        ),

        
        
        mainPanel(
                plotlyOutput("Plotly"),
                br(),br(),
                imageOutput("image")
            
        )
    )
)


server <- function(input, output) {
    
    
    df_plot_data <- tibble(rotation=seq(0, 100, 0.01))
    max_res <- 50
    
    v <- reactiveValues(pot=25, real_pot=100, go_flag=FALSE, taper=NULL)
    
    
    
    load_resistor <- reactive({ input$load_resistor })

    
    observeEvent(input$go, {

        v$go_flag <- TRUE
        v$real_pot <- input$real_pot
        v$pot <- input$linear_pot
        v$taper <- input$taper

        
    })
    
    
    
    output$Plotly <- renderPlotly({

        if (!(v$go_flag)) return()
        
        validate(
            need(!str_detect(v$real_pot, "\\D"), "only numbers!")
        )
        validate(
            need(!str_detect(v$pot, "\\D"), "only numbers!")
        )
        

        if (v$taper =="antilog") {
        
            p1 <- df_plot_data %>%
                mutate(Real = round(anti_log_pot(rotation, pot=as.double(v$real_pot)),1),
                       Approximation = round(anti_log_aprox(rotation,
                                                      pot=as.double(v$pot), 
                                                      load_res=load_resistor()),1)) %>%
                pivot_longer(c(Real, Approximation), names_to = "potentiometer",
                             values_to = "resistance") %>%
                ggplot(aes(rotation, resistance, colour=potentiometer)) + 
                geom_line() + 
                theme_minimal() +
                labs(title= ' Real vs Approx. Anti-Log', x="Rotation %", 
                     y="Resistance (kohms)") +
                theme(legend.title = element_blank())
            
        } else if (v$taper == "log") {
            
            p1 <- df_plot_data %>%
                mutate(Real = round(log_pot(rotation, pot=as.double(v$real_pot)),1),
                       Approximation = round(log_aprox(rotation,
                                                            pot=as.double(v$pot), 
                                                            load_res=load_resistor()),1)) %>%
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
    
    
    
    
    
    output$image <- renderImage({
        if (is.null(input$taper))       return()
        
        
        
        if (input$taper == "log") {
            return(list(
                src = "www/log.PNG",
                contentType = "image/png",
                width="60%",
                alt = "log pot"
            ))
        } else if (input$taper == "antilog") {
            return(list(
                src = "www/anti-log.PNG",
                width="60%",
                filetype = "image/png",
                alt = "anti-log pot"
            ))
        }
        
    }, deleteFile = FALSE)
    
    
    output$slider1 <- renderUI({
        
        validate(
            need(!str_detect(v$pot, "\\D"), "only numbers!")
        )

        if (!is.null(v$pot)){
            max_res <- as.double(v$pot)
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
