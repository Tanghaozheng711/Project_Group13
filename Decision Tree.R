
library(shiny)



library(rpart)
library(visNetwork)
library(ggplot2)
library(dplyr)
library(ggpol)
library(sparkline)
library(stats)

df  <- data_all

data = df

data$Grade <- NA
data$Schoolsup <- NA
data$Famsup <- NA
data$Paid <- NA
data$Activities <- NA
data$Nursery <- NA
data$Higher <- NA
data$Internet <- NA
data$Romantic <- NA
data$Gender <- NA
data$Grade <- ifelse(data$G3>15,4,ifelse(data$G3>10,3,ifelse(data$G3>5,2,1)))
data$Grade <- factor(data$Grade)
data$Schoolsup <- ifelse(data$schoolsup == 'yes', 1, 0)
data$Schoolsup <- factor(data$Schoolsup)
data$Famsup <- ifelse(data$famsup == 'yes', 1 , 0)
data$Famsup <- factor(data$Famsup)
data$Paid <- ifelse(data$paid == 'yes',1,0)
data$Paid <- factor(data$Paid)
data$Activities <- ifelse(data$activities =='yes',1,0)
data$Activities <- factor(data$Activities)
data$Nursery <- ifelse(data$nursery =='yes',1,0)
data$Nursery <- factor(data$Nursery)
data$Higher <- ifelse(data$higher == 'yes',1,0)
data$Higher <- factor(data$Higher)
data$Internet <- ifelse(data$internet =='yes',1,0)
data$Internet <- factor(data$Internet)
data$Romantic <- ifelse(data$romantic == 'yes',1,0)
data$Romantic <- factor(data$Romantic)
data$Gender <- ifelse(data$sex == 'F',1,0)
data$Gender <- factor(data$Gender)
data = data[,-c(1:12)]
data= data[,-c(4:11)]
data =data[,-c(11:13)]


(var_list = sort(colnames(dplyr::select(data, -c("Grade")))))



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Decision Tree"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("GradeLevel",
                         "Number of Grades:",
                         value = 4,
                         min = 2,
                         max = 6,
                         step = 1),
            selectInput("Variables",
                        "Variables Selected:", 
                        choices = var_list,
                        selected = var_list,
                        selectize = FALSE,
                        multiple = TRUE,
                        size = length(var_list))
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            verticalLayout(
                visNetworkOutput("treeVis"),
                
                plotOutput("importance"),
                plotOutput("matrix")
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    mydata <- reactive({
        
        breaks <- floor(seq(min(df$G3), max(df$G3), length.out = input$GradeLevel + 1))
        
        data$Grade <- cut(df$G3, breaks = breaks, include.lowest = TRUE)
        
        cbind(Grade = data$Grade, dplyr::select(data, input$Variables))
    })
    
    
    mytree <- reactive({
        
        model2 <- rpart(Grade~., data = mydata())
        Tree <- prune(model2, cp= model2$cptable[which.min(model2$cptable[,"xerror"]),"CP"])
        
        Tree
        
    })
    
    
    output$treeVis <- renderVisNetwork({
        visTree(mytree(), main = 'Decision Tree', height = '400px', colorY = c('red','blue','green','black'))
        
    })
    
    
    output$importance <- renderPlot({
        
        Tree <- mytree()
        
        varimp <- Tree$variable.importance
        varimpdf <- data.frame(var = names(varimp),
                               impor = varimp)
        g <- ggplot(varimpdf,aes(x = reorder(var,impor), y = impor))+
            geom_col(colour = "lightblue",fill = "lightblue")+
            labs(x = "Variable", y = 'Importance') + 
            coord_flip()
        
        g
        
    })
    
    
    output$matrix <- renderPlot({
        data = mydata()
        Tree = mytree()
        
        pre <- predict(Tree,data,type = "class")
        
        ggplot() + geom_confmat(aes(x = data$Grade, y = pre),
                                normalize = TRUE, text.perc = TRUE)+
            labs(x = "Reference",y = "Prediction")+
            scale_fill_gradient2(low="darkblue", high="lightgreen")
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
