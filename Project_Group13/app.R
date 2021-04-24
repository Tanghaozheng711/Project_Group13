library(shinythemes)
library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- navbarPage("Group 13: The Impact of Lifestyle and Family Background on Grades of High School Students",
        theme = shinytheme("superhero"),
        tabPanel("EDA",
                 sidebarLayout(
                     sidebarPanel(
                         h3("This tab has a sidebar")
                     ),
                     mainPanel(
                         h2("EDA tab")
                     )
                 )
        ),
        tabPanel("Latent Clustering",
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "data_source",
                            label = "Choose school and subject:",
                            choices = c("GP & Math",
                                        "GP & Portuguese",
                                        "MS & Math",
                                        "MS & Portuguese"),
                            selected = "GP & Math")
            ),
                mainPanel(
                plotOutput("clusterplot")
                )
            )
        ),
        tabPanel("Regression and Decision Tree",
                 sidebarLayout(
                     sidebarPanel(
                         h3("This tab has a sidebar")
                     ),
                     mainPanel(
                         h2("regression and decision tree")
                     )
                 )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
    
        output$clusterplot <- renderPlot({
            if (input$data_source == "GP & Math") return(zp1)
            if (input$data_source == "GP & Portuguese") return(zp2)
            if (input$data_source == "MS & Math") return(zp3) 
            if (input$data_source == "MS & Portuguese") return(zp4)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
