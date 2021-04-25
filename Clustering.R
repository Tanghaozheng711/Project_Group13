
packages = c('readr', 'dplyr', 'tidyverse', 'plyr', 'poLCA', 'reshape2', 'ggplot2',
             'ggparallel', 'igraph', 'knitr')
for(p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

#' 
## -----------------------------------------------------------------------------------
data_all <- list.files(path = "data",
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                          
  bind_rows 

data_cleaned2 <- data_all


#' 
## -----------------------------------------------------------------------------------
data_cleaned2$school <- revalue(data_cleaned$school, c("GP" = 1))
data_cleaned2$school <- revalue(data_cleaned$school, c("MS" = 2))
data_cleaned2$school <- as.factor(data_cleaned$school)

data_cleaned2$sex <- revalue(data_cleaned$sex, c("F" = 1))#female
data_cleaned2$sex <- revalue(data_cleaned$sex, c("M" = 2))#male
data_cleaned2$sex <- as.factor(data_cleaned$sex)

data_cleaned2$famsize <- revalue(data_cleaned$famsize, c("LE3" = 1))#less or equal to 3
data_cleaned2$famsize <- revalue(data_cleaned$famsize, c("GT3" = 2))#greater than 3
data_cleaned2$famsize <- as.factor(data_cleaned$famsize)

data_cleaned2$Pstatus <- revalue(data_cleaned$Pstatus, c("T" = 1))#living together
data_cleaned2$Pstatus <- revalue(data_cleaned$Pstatus, c("A" = 2))#apart
data_cleaned2$Pstatus <- as.factor(data_cleaned$Pstatus)

data_cleaned2$reason <- revalue(data_cleaned$reason, c("home" = 1))#close to home
data_cleaned2$reason <- revalue(data_cleaned$reason, c("reputation" = 2))
data_cleaned2$reason <- revalue(data_cleaned$reason, c("course" = 3))
data_cleaned2$reason <- revalue(data_cleaned$reason, c("other" = 4))
data_cleaned2$reason <- as.factor(data_cleaned$reason)

data_cleaned2$guardian <- revalue(data_cleaned$guardian, c("mother" = 1))
data_cleaned2$guardian <- revalue(data_cleaned$guardian, c("father" = 2))
data_cleaned2$guardian <- revalue(data_cleaned$guardian, c("other" = 3))
data_cleaned2$guardian <- as.factor(data_cleaned$guardian)

data_cleaned2$schoolsup <- revalue(data_cleaned$schoolsup, c("no" = 1))
data_cleaned2$schoolsup <- revalue(data_cleaned$schoolsup, c("yes" = 2))
data_cleaned2$schoolsup <- as.factor(data_cleaned$schoolsup)

data_cleaned2$famsup <- revalue(data_cleaned$famsup, c("no" = 1))
data_cleaned2$famsup <- revalue(data_cleaned$famsup, c("yes" = 2))
data_cleaned2$famsup <- as.factor(data_cleaned$famsup)

data_cleaned2$paid <- revalue(data_cleaned$paid, c("no" = 1))
data_cleaned2$paid <- revalue(data_cleaned$paid, c("yes" = 2))
data_cleaned2$paid <- as.factor(data_cleaned$paid)

data_cleaned2$activities <- revalue(data_cleaned$activities, c("no" = 1))
data_cleaned2$activities <- revalue(data_cleaned$activities, c("yes" = 2))
data_cleaned2$activities <- as.factor(data_cleaned$activities)

data_cleaned2$nursery <- revalue(data_cleaned$nursery, c("no" = 1))
data_cleaned2$nursery <- revalue(data_cleaned$nursery, c("yes" = 2))
data_cleaned2$nursery <- as.factor(data_cleaned$nursery)

data_cleaned2$higher <- revalue(data_cleaned$higher, c("no" = 1))
data_cleaned2$higher <- revalue(data_cleaned$higher, c("yes" = 2))
data_cleaned2$higher <- as.factor(data_cleaned$higher)

data_cleaned2$internet <- revalue(data_cleaned$internet, c("no" = 1))
data_cleaned2$internet <- revalue(data_cleaned$internet, c("yes" = 2))
data_cleaned2$internet <- as.factor(data_cleaned$internet)

data_cleaned2$romantic <- revalue(data_cleaned$romantic, c("no" = 1))
data_cleaned2$romantic <- revalue(data_cleaned$romantic, c("yes" = 2))
data_cleaned2$romantic <- as.factor(data_cleaned$romantic)

data_cleaned2$famrel <- factor(data_cleaned$Medu, order = TRUE,
                              levels = c("1", "2", "3", "4", "5"))

data_cleaned2$freetime <- factor(data_cleaned$freetime, order = TRUE,
                                levels = c("1", "2", "3", "4", "5"))

data_cleaned2$goout <- factor(data_cleaned$goout, order = TRUE,
                             levels = c("1", "2", "3", "4", "5"))

data_cleaned2$Dalc <- factor(data_cleaned$Dalc, order = TRUE,
                            levels = c("1", "2", "3", "4", "5"))

data_cleaned2$Walc <- factor(data_cleaned$Walc, order = TRUE,
                            levels = c("1", "2", "3", "4", "5"))

data_cleaned2$health <- factor(data_cleaned$health, order = TRUE,
                              levels = c("1", "2", "3", "4", "5"))

data_cleaned2$absences[data_cleaned$absences < 5] <- 1
data_cleaned2$absences[data_cleaned$absences >= 5] <- 2

data_cleaned2$age[data_cleaned$age <= 17] <- 1
data_cleaned2$age[data_cleaned$age > 17] <- 2

data_cleaned2$G3[data_cleaned$G3 <= 13] <- 1
data_cleaned2$G3[data_cleaned$G3 > 13] <- 2

#' 
## -----------------------------------------------------------------------------------
GP_Math <- filter(data_cleaned2, school == 1 & Subject == "Math")
GP_Math <- dplyr::select(GP_Math,c(2:3, 11:14, 16:30, 33))

GP_Por <- filter(data_cleaned2, school == 1 & Subject == "Por")
GP_Por <- dplyr::select(GP_Por, c(2:3, 11:14, 16:30, 33))

MS_Math <- filter(data_cleaned2, school == 2 & Subject == "Math")
MS_Math <- dplyr::select(MS_Math, c(2:3, 11:14, 16:30, 33))

MS_Por <- filter(data_cleaned2, school == 2 & Subject == "Por")
MS_Por <- dplyr::select(MS_Por, c(2:3, 11:14, 16:30, 33))

#' ###GP_Math
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
ggplotly(zp1)
#' ###GP_Por
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
zp2 <- ggplot(GP_Por_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw() +
  ggtitle("Latent Clusters of GP in Portuguese")
ggplotly(zp2)

#' ###MS_Math
## -----------------------------------------------------------------------------------
# define function
f_msmath <- with(MS_Math,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

    	
MS_Math_LCA_best_model <- poLCA(f_msmath, MS_Math, nclass=2, maxiter=3000, 
                                tol=1e-5, na.rm=FALSE,  
                                nrep=10, verbose=TRUE, calc.se=TRUE)


# Make a cleaner plot, first easily converting a list to a DF with melt():
MS_Math_lcModelProbs <- melt(MS_Math_LCA_best_model$probs)


# Suggested alternative, as a possible improvement:
zp3 <- ggplot(MS_Math_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw() +
  ggtitle("Latent Clusters of MS in Math")
ggplotly(zp3)

#' ###MS_Por
## -----------------------------------------------------------------------------------
# define function
f_mspor <- with(MS_Por,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

    	
MS_Por_LCA_best_model <- poLCA(f_mspor, MS_Por, nclass=2, maxiter=3000, 
                               tol=1e-5, na.rm=FALSE,  
                               nrep=10, verbose=TRUE, calc.se=TRUE)


# Make a cleaner plot, first easily converting a list to a DF with melt():
MS_Por_lcModelProbs <- melt(MS_Por_LCA_best_model$probs)


# Suggested alternative, as a possible improvement:
zp4 <- ggplot(MS_Por_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw() +
  ggtitle("Latent Clusters of MS in Portuguese")
ggplotly(zp4)
 
