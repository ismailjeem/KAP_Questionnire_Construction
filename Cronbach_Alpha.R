# load packages 
library(tidyverse)
library(readxl)
library(xlsx)
library(gt)
library(gtsummary)

# Install the 'psych' package if you haven't already
install.packages("psych")
install.packages("tidyverse")
install.packages("xlsx")
install.packages("gtsummary")                 
install.packages(gt)                 
install.packages(dplyr)                 
install.packages(xlsx)                 

                 
# Load the 'psych' package
library(psych)

library(dplyr)

# load data 
data <- read_excel("data/clean_KAP_data.xlsx")

# cheak columns name
names(data)

 combined_akd_kbd <- data |>
  select(8:20)

 colnames(combined_akd_kbd)<-paste0("Q", 1:13)

unique(combined_akd_kbd$Q1)

#knowladge of blood donation
knowledge_of_blood_donation <- combined_akd_kbd |>
  select(Q1:Q6)


#attitude_information of blood donation
attitude_of_blood_donation<-combined_akd_kbd|>
  select(Q7:Q13)
                                 
 #kjnowladge of blood donation

knowledge_of_blood_donation <- combined_akd_kbd |>
  select(Q1:Q6)|>
  mutate(across(Q1:Q6,~ case_when(
    .=="Yes"~1,
    .=="No"~0,
    TRUE ~ NA_real_
  )))
#attitude_information_of_blood-donation
attitude_of_blood_donation<-combined_akd_kbd|>
  select(Q7:Q13)|>
  mutate(across(Q7:Q13,~ case_when(
    .=="Yes"~1,
    .=="No"~0,
    TRUE ~ NA_real_
  )))

alpha(knowledge_of_blood_donation[,c("Q1","Q2","Q3","Q4","Q5","Q6")])

alpha(attitude_of_blood_donation[,c("Q7","Q8","Q9","Q10","Q11","Q12","Q13")])

alpha(knowledge_of_blood_donation[,c("Q1","Q2","Q3","Q4","Q5","Q6")], check.keys = TRUE)

alpha(attitude_of_blood_donation[,c("Q7","Q8","Q9","Q10","Q11","Q12","Q13")], check.keys = TRUE)

getwd()  # Check where R is working from

# Optional: Set it if needed
# setwd("C:/Users/IsmailHoseen/Documents/MyResearchProject")


library(psych)

# Run alpha analysis and store the result
alpha_result <- alpha(knowledge_of_blood_donation[, c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6")], check.keys = TRUE)


sink("result/cronbach_alpha_full_output.txt")
print(alpha_result)
sink()



















                                

kbd|>
  select(Q1:Q6)|>
  mutate(across(Q1:Q6,~case_when(
    .== "Yes"~1,
    .== "No"~0
  )))


kbd |>
  select(Q1:Q6)|>
  mutate(Q1 = case_when(
    Q1 == "yes~1",
    Q1 == "no~0"
  ))
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
      
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   [
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
















