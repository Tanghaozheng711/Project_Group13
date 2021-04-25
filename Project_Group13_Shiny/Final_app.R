library(shinythemes)
library(shiny)
library(shinydashboard)

### -----------------------------------------------------------------------------------

packages = c("readr", "dplyr", "tidyverse", "plyr", "poLCA", "reshape2", "ggplot2",
             "ggparallel", "igraph", "knitr","rpart","visNetwork","ggpol","sparkline","stats")
for(p in packages){
    if(!require(p,character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

packages = c("readr", "dplyr", "tidyverse", "plyr", "ggstatsplot", "ggplot2", "ggpubr", "corrplot",  
             
             "plotly", "ggcorrplot", "rstantools", "fastDummies") 

for(p in packages){ 
    
    if(!require(p,character.only = T)){ 
        
        install.packages(p) 
        
    } 
    
    library(p,character.only = T) 
    
} 

#" 
## -----------------------------------------------------------------------------------
data_all <- list.files(path = "data",
                       pattern = "*.csv", full.names = TRUE) %>% 
    lapply(read_csv) %>%                                          
    bind_rows 

data_cleaned2 <- data_all


#" 
## -----------------------------------------------------------------------------------
data_cleaned2$school <- revalue(data_cleaned2$school, c("GP" = 1))
data_cleaned2$school <- revalue(data_cleaned2$school, c("MS" = 2))
data_cleaned2$school <- as.factor(data_cleaned2$school)

data_cleaned2$sex <- revalue(data_cleaned2$sex, c("F" = 1))#female
data_cleaned2$sex <- revalue(data_cleaned2$sex, c("M" = 2))#male
data_cleaned2$sex <- as.factor(data_cleaned2$sex)

data_cleaned2$famsize <- revalue(data_cleaned2$famsize, c("LE3" = 1))#less or equal to 3
data_cleaned2$famsize <- revalue(data_cleaned2$famsize, c("GT3" = 2))#greater than 3
data_cleaned2$famsize <- as.factor(data_cleaned2$famsize)

data_cleaned2$Pstatus <- revalue(data_cleaned2$Pstatus, c("T" = 1))#living together
data_cleaned2$Pstatus <- revalue(data_cleaned2$Pstatus, c("A" = 2))#apart
data_cleaned2$Pstatus <- as.factor(data_cleaned2$Pstatus)

data_cleaned2$reason <- revalue(data_cleaned2$reason, c("home" = 1))#close to home
data_cleaned2$reason <- revalue(data_cleaned2$reason, c("reputation" = 2))
data_cleaned2$reason <- revalue(data_cleaned2$reason, c("course" = 3))
data_cleaned2$reason <- revalue(data_cleaned2$reason, c("other" = 4))
data_cleaned2$reason <- as.factor(data_cleaned2$reason)

data_cleaned2$guardian <- revalue(data_cleaned2$guardian, c("mother" = 1))
data_cleaned2$guardian <- revalue(data_cleaned2$guardian, c("father" = 2))
data_cleaned2$guardian <- revalue(data_cleaned2$guardian, c("other" = 3))
data_cleaned2$guardian <- as.factor(data_cleaned2$guardian)

data_cleaned2$schoolsup <- revalue(data_cleaned2$schoolsup, c("no" = 1))
data_cleaned2$schoolsup <- revalue(data_cleaned2$schoolsup, c("yes" = 2))
data_cleaned2$schoolsup <- as.factor(data_cleaned2$schoolsup)

data_cleaned2$famsup <- revalue(data_cleaned2$famsup, c("no" = 1))
data_cleaned2$famsup <- revalue(data_cleaned2$famsup, c("yes" = 2))
data_cleaned2$famsup <- as.factor(data_cleaned2$famsup)

data_cleaned2$paid <- revalue(data_cleaned2$paid, c("no" = 1))
data_cleaned2$paid <- revalue(data_cleaned2$paid, c("yes" = 2))
data_cleaned2$paid <- as.factor(data_cleaned2$paid)

data_cleaned2$activities <- revalue(data_cleaned2$activities, c("no" = 1))
data_cleaned2$activities <- revalue(data_cleaned2$activities, c("yes" = 2))
data_cleaned2$activities <- as.factor(data_cleaned2$activities)

data_cleaned2$nursery <- revalue(data_cleaned2$nursery, c("no" = 1))
data_cleaned2$nursery <- revalue(data_cleaned2$nursery, c("yes" = 2))
data_cleaned2$nursery <- as.factor(data_cleaned2$nursery)

data_cleaned2$higher <- revalue(data_cleaned2$higher, c("no" = 1))
data_cleaned2$higher <- revalue(data_cleaned2$higher, c("yes" = 2))
data_cleaned2$higher <- as.factor(data_cleaned2$higher)

data_cleaned2$internet <- revalue(data_cleaned2$internet, c("no" = 1))
data_cleaned2$internet <- revalue(data_cleaned2$internet, c("yes" = 2))
data_cleaned2$internet <- as.factor(data_cleaned2$internet)

data_cleaned2$romantic <- revalue(data_cleaned2$romantic, c("no" = 1))
data_cleaned2$romantic <- revalue(data_cleaned2$romantic, c("yes" = 2))
data_cleaned2$romantic <- as.factor(data_cleaned2$romantic)

data_cleaned2$famrel <- factor(data_cleaned2$Medu, order = TRUE,
                               levels = c("1", "2", "3", "4", "5"))

data_cleaned2$freetime <- factor(data_cleaned2$freetime, order = TRUE,
                                 levels = c("1", "2", "3", "4", "5"))

data_cleaned2$goout <- factor(data_cleaned2$goout, order = TRUE,
                              levels = c("1", "2", "3", "4", "5"))

data_cleaned2$Dalc <- factor(data_cleaned2$Dalc, order = TRUE,
                             levels = c("1", "2", "3", "4", "5"))

data_cleaned2$Walc <- factor(data_cleaned2$Walc, order = TRUE,
                             levels = c("1", "2", "3", "4", "5"))

data_cleaned2$health <- factor(data_cleaned2$health, order = TRUE,
                               levels = c("1", "2", "3", "4", "5"))

data_cleaned2$absences[data_cleaned2$absences < 5] <- 1
data_cleaned2$absences[data_cleaned2$absences >= 5] <- 2

data_cleaned2$age[data_cleaned2$age <= 17] <- 1
data_cleaned2$age[data_cleaned2$age > 17] <- 2

data_cleaned2$G3[data_cleaned2$G3 <= 13] <- 1
data_cleaned2$G3[data_cleaned2$G3 > 13] <- 2

#" 
## -----------------------------------------------------------------------------------
GP_Math <- filter(data_cleaned2, school == 1 & Subject == "Math")
GP_Math <- dplyr::select(GP_Math,c(2:3, 11:14, 16:30, 33))

GP_Por <- filter(data_cleaned2, school == 1 & Subject == "Por")
GP_Por <- dplyr::select(GP_Por, c(2:3, 11:14, 16:30, 33))

MS_Math <- filter(data_cleaned2, school == 2 & Subject == "Math")
MS_Math <- dplyr::select(MS_Math, c(2:3, 11:14, 16:30, 33))

MS_Por <- filter(data_cleaned2, school == 2 & Subject == "Por")
MS_Por <- dplyr::select(MS_Por, c(2:3, 11:14, 16:30, 33))

#" ###GP_Math
## -----------------------------------------------------------------------------------
# define function
f_gpmath <- with(GP_Math,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC

GP_Math_LCA_best_model <- poLCA(f_gpmath, GP_Math, nclass=2, maxiter=3000, 
                                tol=1e-5, na.rm=FALSE,  
                                nrep=10, verbose=TRUE, calc.se=TRUE)

# Make a cleaner plot, first easily converting a list to a DF with melt():
GP_Math_lcModelProbs <- melt(GP_Math_LCA_best_model$probs)


# Suggested alternative, as a possible improvement:
zp1 <- ggplot(GP_Math_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ L1) +
    scale_x_discrete("Class", expand = c(0, 0)) +
    scale_y_continuous("Proportion", expand = c(0, 0)) +
    scale_fill_discrete("Factor Level") +
    theme_bw() +
    ggtitle("Latent Clusters of GP in Math")
zp2 <- ggplotly(zp1)

#" ###GP_Por
## -----------------------------------------------------------------------------------
# define function
f_gppor <- with(GP_Por,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC

GP_Por_LCA_best_model <- poLCA(f_gppor, GP_Por, nclass=2, maxiter=3000, 
                               tol=1e-5, na.rm=FALSE,  
                               nrep=10, verbose=TRUE, calc.se=TRUE)


# Make a cleaner plot, first easily converting a list to a DF with melt():
GP_Por_lcModelProbs <- melt(GP_Por_LCA_best_model$probs)


# Suggested alternative, as a possible improvement:
zp3 <- ggplot(GP_Por_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ L1) +
    scale_x_discrete("Class", expand = c(0, 0)) +
    scale_y_continuous("Proportion", expand = c(0, 0)) +
    scale_fill_discrete("Factor Level") +
    theme_bw() +
    ggtitle("Latent Clusters of GP in Portuguese")
zp4 <- ggplotly(zp3)

#" ###MS_Math
## -----------------------------------------------------------------------------------
# define function
f_msmath <- with(MS_Math,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #


MS_Math_LCA_best_model <- poLCA(f_msmath, MS_Math, nclass=2, maxiter=3000, 
                                tol=1e-5, na.rm=FALSE,  
                                nrep=10, verbose=TRUE, calc.se=TRUE)


# Make a cleaner plot, first easily converting a list to a DF with melt():
MS_Math_lcModelProbs <- melt(MS_Math_LCA_best_model$probs)


# Suggested alternative, as a possible improvement:
zp5 <- ggplot(MS_Math_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ L1) +
    scale_x_discrete("Class", expand = c(0, 0)) +
    scale_y_continuous("Proportion", expand = c(0, 0)) +
    scale_fill_discrete("Factor Level") +
    theme_bw() +
    ggtitle("Latent Clusters of MS in Math")
zp6 <- ggplotly(zp5)

#" ###MS_Por
## -----------------------------------------------------------------------------------
# define function
f_mspor <- with(MS_Por,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #


MS_Por_LCA_best_model <- poLCA(f_mspor, MS_Por, nclass=2, maxiter=3000, 
                               tol=1e-5, na.rm=FALSE,  
                               nrep=10, verbose=TRUE, calc.se=TRUE)


# Make a cleaner plot, first easily converting a list to a DF with melt():
MS_Por_lcModelProbs <- melt(MS_Por_LCA_best_model$probs)


# Suggested alternative, as a possible improvement:
zp7 <- ggplot(MS_Por_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ L1) +
    scale_x_discrete("Class", expand = c(0, 0)) +
    scale_y_continuous("Proportion", expand = c(0, 0)) +
    scale_fill_discrete("Factor Level") +
    theme_bw() +
    ggtitle("Latent Clusters of MS in Portuguese")
zp8 <- ggplotly(zp7)



### -----------------------------------------------------------------------------------
# Load packages 




#" 
#"  
#" 
#"  
#" 
#" Data Preparation and Cleaning 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Import the data and combine them 

data_all <- list.files(path = "data", 
                       
                       pattern = "*.csv", full.names = TRUE) %>%  
    
    lapply(read_csv) %>%                                           
    
    bind_rows 



# Test for any null values 

is.null(data_all) 


#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Duplicate data for further manipulation 

data_cleaned <- data_all 


#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Setting ordinal data levels 

data_cleaned$Medu <- factor(data_cleaned$Medu, order = TRUE, 
                            
                            levels = c("0", "1", "2", "3", "4")) 



data_cleaned$Fedu <- factor(data_cleaned$Fedu, order = TRUE, 
                            
                            levels = c("0", "1", "2", "3", "4")) 



data_cleaned$failures <- factor(data_cleaned$failures, order = TRUE, 
                                
                                levels = c("0", "1", "2", "3")) 



data_cleaned$famrel <- factor(data_cleaned$Medu, order = TRUE, 
                              
                              levels = c("1", "2", "3", "4", "5")) 



data_cleaned$freetime <- factor(data_cleaned$freetime, order = TRUE, 
                                
                                levels = c("1", "2", "3", "4", "5")) 



data_cleaned$goout <- factor(data_cleaned$goout, order = TRUE, 
                             
                             levels = c("1", "2", "3", "4", "5")) 



data_cleaned$Dalc <- factor(data_cleaned$Dalc, order = TRUE, 
                            
                            levels = c("1", "2", "3", "4", "5")) 



data_cleaned$Walc <- factor(data_cleaned$Walc, order = TRUE, 
                            
                            levels = c("1", "2", "3", "4", "5")) 



data_cleaned$health <- factor(data_cleaned$health, order = TRUE, 
                              
                              levels = c("1", "2", "3", "4", "5")) 




#" 
#"  
#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Creating new column for average score, then removing G1, G2 and G3 

data_cleaned$avgscore <- ((data_cleaned$G1 + data_cleaned$G2 + data_cleaned$G3)/3) 

data_cleaned = subset(data_cleaned, select = -c(G1, G2, G3)) 


#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Split by school 

data_GP <- subset(data_cleaned, school == "GP") 

data_MS <- subset(data_cleaned, school == "MS") 



# Then split by subject 

data_GP_math <- subset(data_GP, Subject == "Math") 

data_GP_por <- subset(data_GP, Subject == "Por") 

data_MS_math <- subset(data_MS, Subject == "Math") 

data_MS_por <- subset(data_MS, Subject == "Por") 



# Remove school and subject variable 

data_GP_math = subset(data_GP_math, select = -c(Subject, school)) 

data_GP_por = subset(data_GP_por, select = -c(Subject, school)) 

data_MS_math = subset(data_MS_math, select = -c(Subject, school)) 

data_MS_por = subset(data_MS_por, select = -c(Subject, school)) 




#" 
#"  
#" 
#"  
#" 
#" CHART 1 
#" 
#" **Chart 1 Option A (Histogram):** 
#" 
#"  
#" 
#" + For absences, grades and age 
#" 
#" + Allow user to select the following: 
#" 
#" - For data set = data_GP_math, data_GP_por, data_MS_math, data_MS_por, data_cleaned 
#" 
#" - For binwidth 
#" 
#" - For input x 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------




#" 
#"  
#" 
#" **Chart 1 Option B (Bar Chart):** 
#" 
#"  
#" 
#" + For all other variables except those in histogram 
#" 
#" + Allow user to select the following: 
#" 
#" - For data set = data_GP_math, data_GP_por, data_MS_math, data_MS_por, data_cleaned 
#" 
#" - For input x 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------



#" 
#"  
#" 
#"  
#" 
#"  
#" 
#" **Chart 2 Comparison of Results** 
#" 
#" + For all variables except those in histogram 
#" 
#" + Allow user to select the following: 
#" 
#" - For data set = data_GP_math, data_GP_por, data_MS_math, data_MS_por, data_cleaned 
#" 
#" - For input x 
#" 
#" - For statistical test  
#" 
#" • "parametric" as p 
#" 
#" • "nonparametric" as np 
#" 
#" • "robust" as r 
#" 
#" • "bayes" as bf 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Attempt using ggbetweenstats 




#" 
#"  
#" 
#"  
#" 
#" **Chart 3 Correlation** 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Creating dummy variables for categorical and non-numerical data 

data_for_corr_all <- data_all 

data_for_corr <- model.matrix(~0+., data=data_for_corr_all) 

data_for_corr <- as.data.frame(data_for_corr) 

data_for_corr = subset(data_for_corr, select = -c(G1, G2, G3)) 



data_for_corr <- fastDummies::dummy_cols(data_for_corr, select_columns = "Medu") 

data_for_corr <- fastDummies::dummy_cols(data_for_corr, select_columns = "Fedu") 

data_for_corr = subset(data_for_corr, select = -c(Medu, Fedu)) 



data_for_corr$avgscore <- ((data_all$G1 + data_all$G2 + data_all$G3)/3) 


#" 
#"  
#" 
## -----------------------------------------------------------------------------------

# Resplitting by school and subject 

data_cor_GP <- subset(data_for_corr, schoolGP == "1") 

data_cor_MS <- subset(data_for_corr, schoolMS == "1") 



data_cor_GP_math <- subset(data_cor_GP, SubjectPor == "0") 

data_cor_GP_por <- subset(data_cor_GP, SubjectPor == "1") 

data_cor_MS_math <- subset(data_cor_MS, SubjectPor == "0") 

data_cor_MS_por <- subset(data_cor_MS, SubjectPor == "1") 



data_cor_GP_math = subset(data_cor_GP_math, select = -c(SubjectPor, schoolGP, schoolMS)) 

data_cor_GP_por = subset(data_cor_GP_por, select = -c(SubjectPor, schoolGP, schoolMS)) 

data_cor_MS_math = subset(data_cor_MS_math, select = -c(SubjectPor, schoolGP, schoolMS)) 

data_cor_MS_por = subset(data_cor_MS_por, select = -c(SubjectPor, schoolGP, schoolMS)) 




#" 
#"  
#" 
#" Allow user to select which plot to choose, no further inputs required from user:  
#" 
#"  
#" 
#" **Static Plot** 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------




#" 
#"  
#" 
#" **Interactive Plot** 
#" 
#"  
#" 
## -----------------------------------------------------------------------------------
#Decision tree

df  <- list.files(path = "data", 
                 
                 pattern = "*.csv", full.names = TRUE) %>%  
    
    lapply(read_csv) %>%                                           
    
    bind_rows

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
data = data[,-c(4:11)]
data = data[,-c(11:13)]

(var_list = sort(colnames(dplyr::select(data, -c("Grade")))))



### -----------------------------------------------------------------------------------

ui <- navbarPage("Group 13: The Impact of Lifestyle and Family Background on Grades of High School Students",
        theme = shinytheme("flatly"),
        tabPanel("Introduction",
                     mainPanel(
                         HTML(
                             paste(
                         h2("Introduction"),
                         '<br/>',
                         h4("In the past many years, there has been an emphasis on education around the world because of the impact it a person, be it in terms of employment opportunities and quality of life. It is hence important to know what are factors that affect one’s academic performance. While there are many factors that can impact a person’s academic performance, family background and one’s lifestyle are two of the larger factors."),
                         '<br/>',
                         h4("Since there are many sub-factors in family background and lifestyle choices, the motivation of this study is to look deeper at these sub-factors to see which are the factors that have a greater correlation in the impact on a student’s grades. More specifically, this study aims to study the correlation between each factor and a student’s grades, as well as aiming to build a model that can accurately determine the academic performance of a student."),
                         '<br/>',
                         h4("From the findings, targeted help may be administered to students in these specific areas attributing to poor grades in school, therein helping them have a higher chance of a better future.")
                     )))
                 ),
        tabPanel("EDA",
                 tabsetPanel(
                     tabPanel("Histogram",
                              sidebarPanel(
                                  selectInput(inputId = "histo_data",
                                              label = "Choose school and subject:",
                                              choices = c("GP & Math",
                                                          "GP & Portuguese",
                                                          "MS & Math",
                                                          "MS & Portuguese",
                                                          "All"),
                                              selected = "All"),
                                  sliderInput(inputId = "binwidth",
                                              label = "Choose binwidth",
                                              min = 1,
                                              max = 20,
                                              value = 10),
                                  selectInput(inputId = "histo_variable",
                                              label = "Choose variable:",
                                              choices = c(
                                                  "Age" = "age",
                                                  "Absences" = "absences",
                                                  "Average Score" = "avgscore"
                                              ),
                                              selected = "absences")
                              ),
                              mainPanel(
                                  plotlyOutput("histogram")
                              )),
                     tabPanel("Bar chart",
                              sidebarPanel(
                                  selectInput(inputId = "bar_data",
                                              label = "Choose school and subject:",
                                              choices = c("GP & Math",
                                                          "GP & Portuguese",
                                                          "MS & Math",
                                                          "MS & Portuguese",
                                                          "All"),
                                              selected = "All"),
                                  selectInput(inputId = "barvariable",
                                              label = "Choose variable:",
                                              choices = c(
                                                  "Sex" = "sex",
                                                  "Address" = "address",
                                                  "Family Size" = "famsize",
                                                  "Parent Status" = "Pstatus",
                                                  "Mother Education" = "Medu",
                                                  "Father Education" = "Fedu",
                                                  "Mother Job" = "Mjob",
                                                  "Father Job" = "Fjob",
                                                  "Reason" = "reason",
                                                  "Guardian" = "guardian",
                                                  "Travel Time" = "traveltime",
                                                  "Study Time" = "studytime",
                                                  "Failures" = "failures",
                                                  "School Support" = "schoolsup",
                                                  "Family Support" = "famsup",
                                                  "Paid" = "paid",
                                                  "Activities" = "activities",
                                                  "Nursery" = "nursery",
                                                  "Higher Education" = "higher",
                                                  "Internet" = "internet",
                                                  "Romantic" = "romantic",
                                                  "Family Relation" = "famrel",
                                                  "Free Time" = "freetime",
                                                  "Go Out" = "goout",
                                                  "Daily Alcohol" = "Dalc",
                                                  "Weekend Alcohol" = "Walc",
                                                  "Health" = "health"
                                              ),
                                              selected = "Sex")
                                  
                              ),
                              mainPanel(
                                  plotOutput("barchart")
                              )),
                     tabPanel("Betweenstats",
                              sidebarPanel(
                                  selectInput(inputId = "bet_data",
                                              label = "Choose school and subject:",
                                              choices = c("GP & Math",
                                                          "GP & Portuguese",
                                                          "MS & Math",
                                                          "MS & Portuguese",
                                                          "All"),
                                              selected = "All"),
                                  selectInput(inputId = "betvariable",
                                              label = "Choose variable:",
                                              choices = c(
                                                  "Sex" = "sex",
                                                  "Address" = "address",
                                                  "Family Size" = "famsize",
                                                  "Parent Status" = "Pstatus",
                                                  "Mother Education" = "Medu",
                                                  "Father Education" = "Fedu",
                                                  "Mother Job" = "Mjob",
                                                  "Father Job" = "Fjob",
                                                  "Reason" = "reason",
                                                  "Guardian" = "guardian",
                                                  "Failures" = "failures",
                                                  "School Support" = "schoolsup",
                                                  "Family Support" = "famsup",
                                                  "Paid" = "paid",
                                                  "Activities" = "activities",
                                                  "Nursery" = "nursery",
                                                  "Higher Education" = "higher",
                                                  "Internet" = "internet",
                                                  "Romantic" = "romantic",
                                                  "Family Relation" = "famrel",
                                                  "Free Time" = "freetime",
                                                  "Go Out" = "goout",
                                                  "Daily Alcohol" = "Dalc",
                                                  "Weekend Alcohol" = "Walc",
                                                  "Health" = "health"
                                              ),
                                              selected = "Sex"),
                                  selectInput(inputId = "betmethod",
                                              label = "Choose calculation method:",
                                              choices = c(
                                                  "parametric" = "p",
                                                  "nonparametric" = "np",
                                                  "robust" = "r",
                                                  "bayes" = "bf"
                                              ),
                                              selected = "parametric")
                                  
                              ),
                              mainPanel(
                                  plotOutput("betchart")
                              )
                              ),
                     tabPanel("Correlation",
                              sidebarPanel(
                                  selectInput(inputId = "cordata",
                                              label = "Choose school and subject:",
                                              choices = c("GP & Math",
                                                          "GP & Portuguese",
                                                          "MS & Math",
                                                          "MS & Portuguese"),
                                              selected = "GP & Math")
                              ),
                              mainPanel(
                                  plotlyOutput("corplot")
                              )
                              ),
                              
                     tabPanel("Usage",
                              mainPanel(
                                  HTML(
                                      paste(
                                  h2("1. Histogram"),'<br/>',
                                  h4("A histogram is a graphical display of data using bars of different heights. In a histogram, each bar groups numbers into ranges. Taller bars show that more data falls in that range. A histogram displays the shape and spread of continuous sample data."),'<br/>',
                                  h4("Instruction:"),'<br/>',
                                  h4("1. Choose school and subject."),'<br/>',
                                  h4("2. Choose binwidth. A binwidth refers to the number of the bins you want to show."),'<br/>',
                                  h4("3. Choose which varibale you want to view."),'<br/>',
                                  h2("2. Bar chart"),'<br/>',
                                  h4("A bar chart or bar graph is a chart or graph that presents categorical data with rectangular bars with heights or lengths proportional to the values that they represent."),'<br/>',
                                  h4("Instruction:"),'<br/>',
                                  h4("1. Choose school and subject."),'<br/>',
                                  h4("2. Choose which variable you want to explore its distribution and propotion."),'<br/>',
                                  h2("3. Betweenstats"),'<br/>',
                                  h4("A combination of box and violin plots along with jittered data points for between-subjects designs with statistical details included in the plot as a subtitle."),'<br/>',
                                  h4("Instruction:"),'<br/>',
                                  h4("1. Choose school and subject."),'<br/>',
                                  h4("2. Choose varibales you want to compare under average score."),'<br/>',
                                  h4("3. Choose calculation method."),'<br/>',
                                  h2("4. Correaltion"),'<br/>',
                                  h4("In statistics, correlation or dependence is any statistical relationship, whether causal or not, between two random variables or bivariate data."),'<br/>',
                                  h4("Instruction:"),'<br/>',
                                  h4("The Interactive correlation mat allows viewers to select which two variables they are interested in by putting the mouse on the corresponding position."),'<br/>',
                                  h4("Also viewers can zoom in or out, and other functions provided by plotly."),'<br/>','<br/>'
                                      ))
                              ))
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
                            selected = "GP & Math"),
                    checkboxInput(inputId = "show_result",
                                  label = "Show cluster result",
                                  value = F)
            ),
                mainPanel(
                    tabsetPanel(
                    tabPanel(
                        title = "Plot",
                        plotlyOutput("clusterplot"),
                        verbatimTextOutput("clusterresult")
                    ),
                    tabPanel(
                        title = "Usage",
                        mainPanel(
                            HTML(
                                paste(
                                    h4("In statistics, a latent class model (LCM) relates a set of observed (usually discrete) multivariate variables to a set of latent variables. It is a type of latent variable model. It is called a latent class model because the latent variable is discrete. A class is characterized by a pattern of conditional probabilities that indicate the chance that variables take on certain values."),
                                    '<br/>',
                                    '<br/>',
                                    h4("In our project, LCM visualization reveals the shared pattern of lifestyle choices and family background of students that belong to the same cluster."),
                                    '<br/>',
                                    h4("Through this viewers are able to fetch the information about how these two major factors impact on students grade."),
                                    '<br/>',
                                    h4("Hint: By checking the 'Show cluster result', viewers are able to have a more specific information on cluster component proportion.")
                                )
                            )
                        )
                    )
                )
                
                )
            )
        ),
        tabPanel("Decision Tree",
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
                     mainPanel(
                         tabsetPanel(
                             tabPanel(
                                 title = "Decision Tree", 
                                    verticalLayout(
                                visNetworkOutput("treeVis"),
                                plotOutput("importance"),
                                plotOutput("matrix")
                             )
                             ),
                             tabPanel(
                                 title = "Usage",
                                 mainPanel(
                                     HTML(
                                         paste(
                                             h4("A decision tree is a decision support tool that uses a tree-like model of decisions and their possible consequences, including chance event outcomes, resource costs, and utility. It is one way to display an algorithm that only contains conditional control statements."),
                                             '<br/>',
                                             h4("Note:"),
                                             '<br/>',
                                             h4("Some combination of variables may result in error: 'unique() applies only to vectors.' And this is due to the data structure and algorithm of decision tree. ")
                                         )
                                     )
                                 )
                             )
                         )
                        
                     )
                 )
        )
)


server <- function(input, output) {
        output$histogram <- renderPlotly({
            if(input$histo_data == "GP & Math"){data_histogram<-data_GP_math}
            if(input$histo_data == "GP & Portuguese"){data_histogram<-data_GP_por}
            if(input$histo_data == "MS & Math"){data_histogram<-data_MS_math}
            if(input$histo_data == "MS & Portuguese"){data_histogram<-data_MS_por}
            if(input$histo_data == "All"){data_histogram<-data_cleaned}
            x <- unlist(data_histogram[,input$histo_variable])
            histo <- ggplot(data_histogram, aes(x)) + 
                        geom_histogram(bins=input$binwidth, color="black", fill="light blue") + 
                        ggtitle("Distribution and Proportion of Variables") + 
                        theme(axis.title.x = element_blank()) 
            
            ggplotly(histo)
        })
        
        output$barchart <- renderPlot({
            if(input$bar_data == "GP & Math"){data_bar<-data_GP_math}
            if(input$bar_data == "GP & Portuguese"){data_bar<-data_GP_por}
            if(input$bar_data == "MS & Math"){data_bar<-data_MS_math}
            if(input$bar_data == "MS & Portuguese"){data_bar<-data_MS_por}
            if(input$bar_data == "All"){data_bar<-data_cleaned}
            x <- unlist(data_bar[,input$barvariable])
            ggplot(data_bar) + 
                aes(x = x, fill = x) + 
                geom_bar(aes(x = x, fill = x), color = "black", alpha = 0.5) + 
                geom_text(aes(label = scales::percent(..prop..), group = 1, y= ..prop.. ), stat= "count", vjust = -1.5) + 
                ggtitle("Distribution and Proportion of Variables") + 
                theme(axis.title.x = element_blank()) 
        })
        
        output$betchart <- renderPlot({
            if(input$bet_data == "GP & Math"){data_bet<-data_GP_math}
            if(input$bet_data == "GP & Portuguese"){data_bet<-data_GP_por}
            if(input$bet_data == "MS & Math"){data_bet<-data_MS_math}
            if(input$bet_data == "MS & Portuguese"){data_bet<-data_MS_por}
            if(input$bet_data == "All"){data_bet<-data_cleaned}
            x <- unlist(data_bet[,input$betvariable])
            ggplot(data_bet, aes(x = x, y = avgscore, color = x)) +
                geom_violin(position = position_dodge(1)) +
                geom_boxplot(color = "black", alpha = 0.5)
        })
        
        output$corplot <- renderPlotly({
            if(input$cordata == "GP & Math"){data_cor<-data_cor_GP_math}
            if(input$cordata == "GP & Portuguese"){data_cor<-data_cor_GP_por}
            if(input$cordata == "MS & Math"){data_cor<-data_cor_MS_math}
            if(input$cordata == "MS & Portuguese"){data_cor<-data_cor_MS_por}
            
                data.cor <- cor(data_cor) 
                corr.plot <- ggcorrplot( 
                    
                    data.cor,  
                    
                    type = "upper",  
                    
                    outline.col = "black", 
                    
                    tl.cex=7, 
                    
                    colors = c("red", "white", "blue")) 
                
                interactive_cor <- ggplotly(corr.plot) 
                
                interactive_cor
            
        })
        
        output$clusterplot <- renderPlotly({
            if (input$data_source == "GP & Math") return(zp2)
            if (input$data_source == "GP & Portuguese") return(zp4)
            if (input$data_source == "MS & Math") return(zp6) 
            if (input$data_source == "MS & Portuguese") return(zp8)
        })
        
        output$clusterresult <-renderPrint({
            if(input$show_result){
            
            if (input$data_source == "GP & Math") return(GP_Math_LCA_best_model)
            if (input$data_source == "GP & Portuguese") return(GP_Por_LCA_best_model)
            if (input$data_source == "MS & Math") return(MS_Math_LCA_best_model) 
            if (input$data_source == "MS & Portuguese") return(MS_Por_LCA_best_model)
                
            }
        })
        
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
