packages = c('readr', 'dplyr', 'tidyverse', 'plyr', 'poLCA', 'reshape2', 'ggplot2',
             'ggparallel', 'igraph', 'knitr')
for(p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}



data_all <- list.files(path = "data",
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                          
  bind_rows 

data_cleaned <- data_all
show(data_cleaned)



data_cleaned$school <- revalue(data_cleaned$school, c("GP" = 1))
data_cleaned$school <- revalue(data_cleaned$school, c("MS" = 2))
data_cleaned$school <- as.factor(data_cleaned$school)

data_cleaned$sex <- revalue(data_cleaned$sex, c("F" = 1))#female
data_cleaned$sex <- revalue(data_cleaned$sex, c("M" = 2))#male
data_cleaned$sex <- as.factor(data_cleaned$sex)

data_cleaned$famsize <- revalue(data_cleaned$famsize, c("LE3" = 1))#less or equal to 3
data_cleaned$famsize <- revalue(data_cleaned$famsize, c("GT3" = 2))#greater than 3
data_cleaned$famsize <- as.factor(data_cleaned$famsize)

data_cleaned$Pstatus <- revalue(data_cleaned$Pstatus, c("T" = 1))#living together
data_cleaned$Pstatus <- revalue(data_cleaned$Pstatus, c("A" = 2))#apart
data_cleaned$Pstatus <- as.factor(data_cleaned$Pstatus)

data_cleaned$reason <- revalue(data_cleaned$reason, c("home" = 1))#close to home
data_cleaned$reason <- revalue(data_cleaned$reason, c("reputation" = 2))
data_cleaned$reason <- revalue(data_cleaned$reason, c("course" = 3))
data_cleaned$reason <- revalue(data_cleaned$reason, c("other" = 4))
data_cleaned$reason <- as.factor(data_cleaned$reason)

data_cleaned$guardian <- revalue(data_cleaned$guardian, c("mother" = 1))
data_cleaned$guardian <- revalue(data_cleaned$guardian, c("father" = 2))
data_cleaned$guardian <- revalue(data_cleaned$guardian, c("other" = 3))
data_cleaned$guardian <- as.factor(data_cleaned$guardian)

data_cleaned$schoolsup <- revalue(data_cleaned$schoolsup, c("no" = 1))
data_cleaned$schoolsup <- revalue(data_cleaned$schoolsup, c("yes" = 2))
data_cleaned$schoolsup <- as.factor(data_cleaned$schoolsup)

data_cleaned$famsup <- revalue(data_cleaned$famsup, c("no" = 1))
data_cleaned$famsup <- revalue(data_cleaned$famsup, c("yes" = 2))
data_cleaned$famsup <- as.factor(data_cleaned$famsup)

data_cleaned$paid <- revalue(data_cleaned$paid, c("no" = 1))
data_cleaned$paid <- revalue(data_cleaned$paid, c("yes" = 2))
data_cleaned$paid <- as.factor(data_cleaned$paid)

data_cleaned$activities <- revalue(data_cleaned$activities, c("no" = 1))
data_cleaned$activities <- revalue(data_cleaned$activities, c("yes" = 2))
data_cleaned$activities <- as.factor(data_cleaned$activities)

data_cleaned$nursery <- revalue(data_cleaned$nursery, c("no" = 1))
data_cleaned$nursery <- revalue(data_cleaned$nursery, c("yes" = 2))
data_cleaned$nursery <- as.factor(data_cleaned$nursery)

data_cleaned$higher <- revalue(data_cleaned$higher, c("no" = 1))
data_cleaned$higher <- revalue(data_cleaned$higher, c("yes" = 2))
data_cleaned$higher <- as.factor(data_cleaned$higher)

data_cleaned$internet <- revalue(data_cleaned$internet, c("no" = 1))
data_cleaned$internet <- revalue(data_cleaned$internet, c("yes" = 2))
data_cleaned$internet <- as.factor(data_cleaned$internet)

data_cleaned$romantic <- revalue(data_cleaned$romantic, c("no" = 1))
data_cleaned$romantic <- revalue(data_cleaned$romantic, c("yes" = 2))
data_cleaned$romantic <- as.factor(data_cleaned$romantic)

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

data_cleaned$absences[data_cleaned$absences < 5] <- 1
data_cleaned$absences[data_cleaned$absences >= 5] <- 2

data_cleaned$age[data_cleaned$age <= 17] <- 1
data_cleaned$age[data_cleaned$age > 17] <- 2

data_cleaned$G3[data_cleaned$G3 <= 13] <- 1
data_cleaned$G3[data_cleaned$G3 > 13] <- 2



GP_Math <- filter(data_cleaned, school == 1 & Subject == "Math")
GP_Math <- dplyr::select(GP_Math,c(2:3, 11:14, 16:30, 33))

GP_Por <- filter(data_cleaned, school == 1 & Subject == "Por")
GP_Por <- dplyr::select(GP_Por, c(2:3, 11:14, 16:30, 33))

MS_Math <- filter(data_cleaned, school == 2 & Subject == "Math")
MS_Math <- dplyr::select(MS_Math, c(2:3, 11:14, 16:30, 33))

MS_Por <- filter(data_cleaned, school == 2 & Subject == "Por")
MS_Por <- dplyr::select(MS_Por, c(2:3, 11:14, 16:30, 33))

###GP_Math

# define function
f_gpmath <- with(GP_Math,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f_gpmath, GP_Math, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    GP_Math_LCA_best_model<-lc
  }
}    	
GP_Math_LCA_best_model

# Make a cleaner plot, first easily converting a list to a DF with melt():
GP_Math_lcModelProbs <- melt(GP_Math_LCA_best_model$probs)

# Replicating the poLCA 3-D plot, without the 3-D:
zp1 <- ggplot(GP_Math_lcModelProbs,
              aes(x = L1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Var1)
print(zp1)

# Suggested alternative, as a possible improvement:
zp2 <- ggplot(GP_Math_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw()
print(zp2)

###GP_Por

# define function
f_gppor <- with(GP_Por,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f_gppor, GP_Por, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    GP_Por_LCA_best_model<-lc
  }
}    	
GP_Por_LCA_best_model

plot(GP_Por_LCA_best_model)

# Make a cleaner plot, first easily converting a list to a DF with melt():
GP_Por_lcModelProbs <- melt(GP_Por_LCA_best_model$probs)

# Replicating the poLCA 3-D plot, without the 3-D:
zp3 <- ggplot(GP_Por_lcModelProbs,
              aes(x = L1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Var1)
print(zp3)

# Suggested alternative, as a possible improvement:
zp4 <- ggplot(GP_Por_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw()
print(zp4)

###MS_Math

# define function
f_msmath <- with(MS_Math,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f_msmath, MS_Math, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    MS_Math_LCA_best_model<-lc
  }
}    	
MS_Math_LCA_best_model

plot(MS_Math_LCA_best_model)

# Make a cleaner plot, first easily converting a list to a DF with melt():
MS_Math_lcModelProbs <- melt(MS_Math_LCA_best_model$probs)

# Replicating the poLCA 3-D plot, without the 3-D:
zp5 <- ggplot(MS_Math_lcModelProbs,
              aes(x = L1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Var1)
print(zp5)

# Suggested alternative, as a possible improvement:
zp6 <- ggplot(MS_Math_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw()
print(zp6)

###MS_Por

# define function
f_mspor <- with(MS_Por,cbind(sex,age,reason,guardian,traveltime,studytime,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G3) ~ 1) #

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f_mspor, MS_Por, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    MS_Por_LCA_best_model<-lc
  }
}    	
MS_Por_LCA_best_model

plot(MS_Por_LCA_best_model)

# Make a cleaner plot, first easily converting a list to a DF with melt():
MS_Por_lcModelProbs <- melt(MS_Por_LCA_best_model$probs)

# Replicating the poLCA 3-D plot, without the 3-D:
zp7 <- ggplot(MS_Por_lcModelProbs,
              aes(x = L1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Var1)
print(zp7)

# Suggested alternative, as a possible improvement:
zp8 <- ggplot(MS_Por_lcModelProbs,
              aes(x = Var1, y = value, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ L1) +
  scale_x_discrete("Class", expand = c(0, 0)) +
  scale_y_continuous("Proportion", expand = c(0, 0)) +
  scale_fill_discrete("Factor Level") +
  theme_bw()
print(zp8)
