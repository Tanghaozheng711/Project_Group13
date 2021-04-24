
#' 
## -----------------------------------------------------------------------------------

# Load packages 

packages = c('readr', 'dplyr', 'tidyverse', 'plyr', 'ggstatsplot', 'ggplot2', 'ggpubr', 'corrplot',  
             
             'plotly', 'ggcorrplot', 'rstantools', 'fastDummies') 

for(p in packages){ 
  
  if(!require(p,character.only = T)){ 
    
    install.packages(p) 
    
  } 
  
  library(p,character.only = T) 
  
} 


#' 
#'  
#' 
#'  
#' 
#' Data Preparation and Cleaning 
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

# Import the data and combine them 

data_all <- list.files(path = "data", 
                       
                       pattern = "*.csv", full.names = TRUE) %>%  
  
  lapply(read_csv) %>%                                           
  
  bind_rows 



# Test for any null values 

is.null(data_all) 


#' 
#'  
#' 
## -----------------------------------------------------------------------------------

# Duplicate data for further manipulation 

data_cleaned <- data_all 


#' 
#'  
#' 
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




#' 
#'  
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

# Creating new column for average score, then removing G1, G2 and G3 

data_cleaned$avgscore <- ((data_cleaned$G1 + data_cleaned$G2 + data_cleaned$G3)/3) 

data_cleaned = subset(data_cleaned, select = -c(G1, G2, G3)) 


#' 
#'  
#' 
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




#' 
#'  
#' 
#'  
#' 
#' CHART 1 
#' 
#' **Chart 1 Option A (Histogram):** 
#' 
#'  
#' 
#' + For absences, grades and age 
#' 
#' + Allow user to select the following: 
#' 
#' - For data set = data_GP_math, data_GP_por, data_MS_math, data_MS_por, data_cleaned 
#' 
#' - For binwidth 
#' 
#' - For input x 
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

histo <- ggplot(data_GP_math) + 
  
  aes(x = age) + 
  
  geom_histogram(binwidth=1, color="black", fill="light blue") + 
  
  ggtitle("Distribution and Proportion of Variables") + 
  
  theme(axis.title.x = element_blank()) 



ggplotly(histo) 


#' 
#'  
#' 
#' **Chart 1 Option B (Bar Chart):** 
#' 
#'  
#' 
#' + For all other variables except those in histogram 
#' 
#' + Allow user to select the following: 
#' 
#' - For data set = data_GP_math, data_GP_por, data_MS_math, data_MS_por, data_cleaned 
#' 
#' - For input x 
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

ggplot(data_GP_math) + 
  
  aes(x = sex, fill = sex) + 
  
  geom_bar(aes(x = sex, fill = sex), color = "black", alpha = 0.5) + 
  
  geom_text(aes(label = scales::percent(..prop..), group = 1, y= ..prop.. ), stat= "count", vjust = -1.5) + 
  
  ggtitle("Distribution and Proportion of Variables") + 
  
  theme(axis.title.x = element_blank()) 


#' 
#'  
#' 
#'  
#' 
#'  
#' 
#' **Chart 2 Comparison of Results** 
#' 
#' + For all variables except those in histogram 
#' 
#' + Allow user to select the following: 
#' 
#' - For data set = data_GP_math, data_GP_por, data_MS_math, data_MS_por, data_cleaned 
#' 
#' - For input x 
#' 
#' - For statistical test  
#' 
#' • "parametric" as p 
#' 
#' • "nonparametric" as np 
#' 
#' • "robust" as r 
#' 
#' • "bayes" as bf 
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

# Attempt using ggbetweenstats 

ggbetweenstats( 
  
  data = data_cleaned, 
  
  x = sex, 
  
  y = avgscore, 
  
  results.subtitle = TRUE, 
  
  bf.message = FALSE, 
  
  type = "p", 
  
  title = "Comparison of Variables Against Average Score") 


#' 
#'  
#' 
#'  
#' 
#' **Chart 3 Correlation** 
#' 
#'  
#' 
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


#' 
#'  
#' 
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




#' 
#'  
#' 
#' Allow user to select which plot to choose, no further inputs required from user:  
#' 
#'  
#' 
#' **Static Plot** 
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

data.cor = cor(data_cor_GP_math) 

static_corr <- corrplot(data.cor, 
                        
                        method = "ellipse",  
                        
                        type="lower", 
                        
                        diag = FALSE, 
                        
                        tl.col = "black", 
                        
                        tl.cex=0.7) 


#' 
#'  
#' 
#' **Interactive Plot** 
#' 
#'  
#' 
## -----------------------------------------------------------------------------------

p.mat <- cor_pmat(data_cor_GP_por) 



corr.plot <- ggcorrplot( 
  
  data.cor,  
  
  type = "upper",  
  
  outline.col = "black", 
  
  tl.cex=7, 
  
  colors = c("red", "white", "blue")) 

interactive_cor <- ggplotly(corr.plot) 



interactive_cor 


