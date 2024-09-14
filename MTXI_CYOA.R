library(DiagrammeR)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

#setwdhere

start_time <- Sys.time()

#Highlight and run all of the code below
#To view ALL users, execute render_graph(Total)
#To determine number of users, execute NumberOfUsers
#To view all users in data table, execute View(MTX_data1)

MTX <- read_excel("MTX CYOA Scenario I_levels (Responses)_TOTAL.xlsx")
MTXasDF <- as.data.frame(MTX)

MTX_COA <- MTXasDF[,c(2,4,3,6,5,9,8,12,11,15,14,17,16, 19, 18)]
names(MTX_COA) <- c("ID","Q1","Q1fluid","Q2","Q2fluid","Q3","Q3fluid","Q4","Q4fluid","Q5","Q5fluid","Q6","Q6fluid","Q7","Q7fluid")


MTX_COA2 <- MTX_COA[c(1:9, 15:41, 48:65),]
#SOC ONLY
  #MTX_COA2 <- MTX_COA[c(1:9, 15:32),]
#MTX ONLY
  #MTX_COA2<- MTX_COA[c(33:41, 48:65),]
number.unique.choices.MTX_COA2 <- sapply(MTX_COA2 , function(x) length(unique(na.omit(x))))



MTX_data1 <- MTX_COA

#USERS
MTX_data1 <- MTX_data1[c(1:9, 15:41, 48:65),]
  #SOC ONLY
  #MTX_data1 <- MTX_data1[c(1:9, 15:32),]
  #MTX ONLY
  #MTX_data1 <- MTX_data1[c(33:41, 48:65),]


#add End9, End10, End11, End12, and End13 ... since these are "ends" in the CYOA, gave them End designation
MTX_data1$End8 <- NA
MTX_data1$End9 <- NA
MTX_data1$End10 <- NA
MTX_data1$End11 <- NA
MTX_data1$End12 <- NA
MTX_data1$End13 <- NA
MTX_data1$End14 <- NA

#Setting Ends
#Q1
#Section2
#Hr24
MTX_data1$End8[MTX_data1$Q1=="Obtain next level in 24 hours (Hour 48)"] <- 8
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 48)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)"] <- 9
MTX_data1$End9[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 48)"] <- 9
MTX_data1$End10[MTX_data1$Q1=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End11[MTX_data1$Q1=="Discharge the patient home"] <- 11

#Setting Q2 to be filled in correctly
#Section3
#Hr30
MTX_data1$End8[MTX_data1$Q2=="Obtain next level in 24 hours (Hour 54)"] <- 8
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 48)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 54)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 48)"] <- 9
MTX_data1$End9[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 54)"] <- 9
MTX_data1$End10[MTX_data1$Q2=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End11[MTX_data1$Q2=="Discharge the patient home"] <- 11


#Setting Q3 to be filled in correctly
#Section4
#Hr36
MTX_data1$End8[MTX_data1$Q3=="Obtain next level in 24 hours (Hour 60)"] <- 8
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 48)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 54)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 48)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 54)"] <- 9
MTX_data1$End9[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)"] <- 9
MTX_data1$End10[MTX_data1$Q3=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End11[MTX_data1$Q3=="Discharge the patient home"] <- 11


#Setting Q4 to be filled in correctly
#Section5
#hr42
MTX_data1$End8[MTX_data1$Q4=="Obtain next level in 24 hours (Hour 66)"] <- 8
MTX_data1$End8[MTX_data1$Q4=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 66)"] <- 8
MTX_data1$End12[MTX_data1$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)"] <- 12
MTX_data1$End12[MTX_data1$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 54)"] <- 12
MTX_data1$End12[MTX_data1$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 60)"] <- 12
MTX_data1$End12[MTX_data1$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 66)"] <- 12
MTX_data1$End10[MTX_data1$Q4=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End11[MTX_data1$Q4=="Discharge the patient home"] <- 11

#Setting Q5 to be filled in correctly
#Section6
#hr48
MTX_data1$End8[MTX_data1$Q5=="Obtain next level in 18 hours (Hour 66)"] <- 8
MTX_data1$End8[MTX_data1$Q5=="Obtain next level in 24 hours (Hour 72)"] <- 8
MTX_data1$End8[MTX_data1$Q5=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 66)"] <- 8
MTX_data1$End8[MTX_data1$Q5=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 72)"] <- 8
MTX_data1$End12[MTX_data1$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)"] <- 12
MTX_data1$End12[MTX_data1$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 60)"] <- 12
MTX_data1$End12[MTX_data1$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 66)"] <- 12
MTX_data1$End12[MTX_data1$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 72)"] <- 12
MTX_data1$End10[MTX_data1$Q5=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End11[MTX_data1$Q5=="Discharge the patient home"] <- 11

#Setting Q6 to be filled in correctly
#Section7
#Hr54
MTX_data1$End8[MTX_data1$Q6=="Obtain next level in 12 hours (Hour 66)"] <- 8
MTX_data1$End8[MTX_data1$Q6=="Obtain next level in 18 hours (Hour 72)"] <- 8
MTX_data1$End8[MTX_data1$Q6=="Obtain next level in 24 hours (Hour 78)"] <- 8
MTX_data1$End8[MTX_data1$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 66)"] <- 8
MTX_data1$End8[MTX_data1$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 72)"] <- 8
MTX_data1$End8[MTX_data1$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 78)"] <- 8
MTX_data1$End12[MTX_data1$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)"] <- 12
MTX_data1$End12[MTX_data1$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 66)"] <- 12
MTX_data1$End12[MTX_data1$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 72)"] <- 12
MTX_data1$End12[MTX_data1$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 78)"] <- 12
MTX_data1$End10[MTX_data1$Q6=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End11[MTX_data1$Q6=="Discharge the patient home"] <- 11

#Setting Q7 to be filled in correctly
#Section8
#Hr60
MTX_data1$End13[MTX_data1$Q7=="Obtain next level in 6 hours (Hour 66)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Obtain next level in 12 hours (Hour 72)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Obtain next level in 18 hours (Hour 78)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Obtain next level in 24 hours (Hour 84)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 66)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 72)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 78)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 84)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 66)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 72)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 78)"] <- 13
MTX_data1$End13[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 84)"] <- 13
MTX_data1$End10[MTX_data1$Q7=="Give glucarpidase 50U/kg/dose IV once"] <- 10
MTX_data1$End14[MTX_data1$Q7=="Discharge the patient home"] <- 14 #CORRECT ENDING



#new section
#Setting Fluids to give choice output
MTX_data1$Q1fluid[MTX_data1$Q1fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q1fluid[MTX_data1$Q1fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"

MTX_data1$Q2fluid[MTX_data1$Q2fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q2fluid[MTX_data1$Q2fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"

MTX_data1$Q3fluid[MTX_data1$Q3fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q3fluid[MTX_data1$Q3fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"

MTX_data1$Q4fluid[MTX_data1$Q4fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q4fluid[MTX_data1$Q4fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"

MTX_data1$Q5fluid[MTX_data1$Q5fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q5fluid[MTX_data1$Q5fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"

MTX_data1$Q6fluid[MTX_data1$Q6fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q6fluid[MTX_data1$Q6fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"

MTX_data1$Q7fluid[MTX_data1$Q7fluid=="No fluid changes are necessary at this time"] <- "no"
MTX_data1$Q7fluid[MTX_data1$Q7fluid=="Increase fluids by 75 ml/m2/hr"] <- "yes"


MTX_data2 <- MTX_data1


#Setting Questions to give choice output
MTX_data1$Q1 <- ifelse(is.na(MTX_data1$Q1), NA, 1)
MTX_data1$Q2 <- ifelse(is.na(MTX_data1$Q2), NA, 2)
MTX_data1$Q3 <- ifelse(is.na(MTX_data1$Q3), NA, 3)
MTX_data1$Q4 <- ifelse(is.na(MTX_data1$Q4), NA, 4)
MTX_data1$Q5 <- ifelse(is.na(MTX_data1$Q5), NA, 5)
MTX_data1$Q6 <- ifelse(is.na(MTX_data1$Q6), NA, 6)
MTX_data1$Q7 <- ifelse(is.na(MTX_data1$Q7), NA, 7)
#View(MTX_data1)

number.unique.choices.MTX_data1 <- sapply(MTX_data1 , function(x) length(unique(na.omit(x))))





library(DiagrammeR)
setwd("/Users/caitlinhanlon/Desktop/Hopkins Related/mtx/Rfiles")
#note: excel file MUST be in numerical order for nodes
data <- read_excel("DatagenerationI_levels.xlsx")

#this tells it that if someone goes from Q1 to Q2 to Q3 to not put it in the visualization as Q1 to Q2 AND Q1 to Q3, etc
#telling it that if someone goes from Q1 to Q4, everything in between should be NA
#basically it's saying if there is something from Q1 to Q2, make everything past that NA; pick up Q2 later.
#consult DatagenerationI_levels.xlsx --> the numbers for each row should correspond to the numbers in each section below
#tried to make it IF/NOT type of statement but that just wasnt working for weird reasons
#for FLUIDS, have to add in yes or no question for each option

livecountX <- c(nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q2:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q2:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End8)) %>% select(Q2:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End9)) %>% select(Q2:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
      
                
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),
                
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q3:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q3:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End8)) %>% select(Q3:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End9)) %>% select(Q3:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),

                
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),
                
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q4:Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q4:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q4:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q4:Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End8)) %>% select(Q4:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End9)) %>% select(Q4:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
  
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q4))),
                
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q5:Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q5:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),     
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q5:Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(End8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(End10)) %>% select(Q5:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),
  
                
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(Q5))),
                
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q6:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q6:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End8)) %>% select(Q6:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End10)) %>% select(Q6:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                

                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(Q6))),  
                
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q7:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q7:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End10)) %>% select(Q7:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End11)) %>% select(Q7:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(Q7))),  
                
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(End10)) %>% select(End8:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(End13))  %>% select(End8:End12)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(End14))  %>% select(End8:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(End10)) %>% select(End8:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(End13))  %>% select(End8:End12)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(End14))  %>% select(End8:End13)  %>% filter_all(all_vars(is.na(.)))))
                
                
users <- length(which(MTX_data1$Q1 != "NA"))

data$LiveCount <- livecountX
data$LivePercent <- livecountX/users


MTX_data1$group <- NA
  MTX_data1$group[1:27] <- "SOC"
  MTX_data1$group[28:54] <- "MTX"
  
  livecountX_SOC <- c(nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q2:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q2:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End8)) %>% select(Q2:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End9)) %>% select(Q2:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                  
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q3:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q3:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End8)) %>% select(Q3:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End9)) %>% select(Q3:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q4))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q4:Q5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q4:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q4:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q4))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q4:Q5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End8)) %>% select(Q4:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End9)) %>% select(Q4:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q4))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q4))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q5))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q5:Q6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q5:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),     
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q5))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q5:Q6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End10)) %>% select(Q5:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q5))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q5))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q6))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q6:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q6:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q6))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End8)) %>% select(Q6:Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End10)) %>% select(Q6:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q6))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q6))),  
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q7))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q7:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q7:End10)%>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q7))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End10)) %>% select(Q7:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End11)) %>% select(Q7:End10)%>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(Q7))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(Q7))),  
                  
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End10)) %>% select(End8:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End13))  %>% select(End8:End12)  %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End14))  %>% select(End8:End13)  %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End10)) %>% select(End8:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End13))  %>% select(End8:End12)  %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End14))  %>% select(End8:End13)  %>% filter_all(all_vars(is.na(.)))))
  
 
  data$LiveCount_SOC <- livecountX_SOC
  data$LivePercent_SOC <- livecountX_SOC/27
  
  
   livecountX_MTX <- c(nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q2:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q2:End8) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End8)) %>% select(Q2:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End9)) %>% select(Q2:End8) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                      
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q3:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q3:End8) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End8)) %>% select(Q3:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End9)) %>% select(Q3:End8) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q4))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q4:Q5) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q4:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q4:End8) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q4))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q4:Q5) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End8)) %>% select(Q4:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End9)) %>% select(Q4:End8) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q4))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q4))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q5))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q5:Q6) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q5:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),     
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q5))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q5:Q6) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End10)) %>% select(Q5:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q5))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q5))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q6))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q6:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q6:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q6))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End8)) %>% select(Q6:Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End10)) %>% select(Q6:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q6))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q6))),  
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q7))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q7:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q7:End10)%>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q7))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End10)) %>% select(Q7:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End11)) %>% select(Q7:End10)%>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(Q7))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(Q7))),  
                      
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End10)) %>% select(End8:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End13))  %>% select(End8:End12)  %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End14))  %>% select(End8:End13)  %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End10)) %>% select(End8:End9) %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End13))  %>% select(End8:End12)  %>% filter_all(all_vars(is.na(.)))),
                      nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End14))  %>% select(End8:End13)  %>% filter_all(all_vars(is.na(.)))))
  
   data$LiveCount_MTX <- livecountX_MTX
   data$LivePercent_MTX <- livecountX_MTX/27
  

#EVERYTHING BELOW IS GOOD FOR MAKING MAP

node_list <- unique(data$node1); node_list2 <- unique(data$node2)
nl <- unique(c(node_list, node_list2))
nd <- as.data.frame(nl)
nd$id <- nd$nl
nd <- nd %>% select(-nl)
nd <- nd %>% arrange(id)
#nd$label <- c("START", "Yes (24)", "No (24)","30Hr","36Hr", 
#              "42Hr","Yes (30)", "No (30)", "Yes (36)", "No (36)",
#              "Yes (42)", "No (42)", "48Hr", "Yes (48)", "No (48)",
#              "54Hr", "Yes (54)", "No (54)", "60Hr", "Yes (60)",
#              "No (60)", "end1", "end2", "end3", "end4",
#               end5", "end6", "FINISH")

nd$label <- c("1", "2", "3","4","7", 
              "10","6", "5", "8", "9",
              "12", "11", "13", "14", "15",
              "16", "18", "17", "19", "20",
              "21", "end1", "end2", "end3", "end4",
              "end5", "end6", "FINISH")

# add colors, shapes, etc
nd$color <- NULL
nd$color <- ifelse(nd$id %in% c("22", "23", "24", "25", "26", "27"), "gray70", 
                   ifelse(nd$id %in% c("1", "3", "6", "11", "13", "15", "16", "18", "19", "21", "28"), "white", 
                          ifelse(nd$id %in% c("2", "4", "5", "7", "8", "9", "10", "12", "14", "17", "20"), "gray92",      
                          
                          "white"))) 

nd$shape <- NULL
nd$shape = ifelse(nd$id %in% c("22", "23", "24", "25", "26", "27", "28"), "square", 
                  ifelse(nd$id %in% c("2", "3", "7", "8", "9", "10", "11", "12", "14", "15", "17", "18", "20", "21") , "diamond",
                         "circle"))



colnames(data)[colnames(data)=="node1"] <- "from"
colnames(data)[colnames(data)=="node2"] <- "to"
colnames(data)[colnames(data)=="val2"] <- "LivePercent"
colnames(data)[colnames(data)=="val3"] <- "LivePercent_SOC"
colnames(data)[colnames(data)=="val4"] <- "LivePercent_MTX"


edge_list <- select(data, to, from, LivePercent) 
edge_list$style <- "solid"
edge_list$color <- NULL
#edge_list$color <- "black"
#edge_list$color <- ifelse(edge_list$LivePercent != 0, "black", "gray85")

edge_list$color <- ifelse((edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 2) |
                            (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 7) |
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 7) |
                            (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 8) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 8) |
                            (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 9) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 9) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 9) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 10) |
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 10) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 10) |
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 11) |
                            (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 11) |
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 12) |
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 12) |
                            (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 12) |
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 14) |
                            (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 14) |
                            (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 17) |
                            (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 20) 
                          , "turquoise4", 
                          ifelse((edge_list$LivePercent != 0 & edge_list$to == 2 & edge_list$from == 1) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 3) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 12 & edge_list$from == 6) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 14 & edge_list$from == 13) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 15) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 17 & edge_list$from == 16) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 20 & edge_list$from == 19) 
                           , "darkorange3", 
                           
                           ifelse((edge_list$LivePercent != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 2) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 7 & edge_list$from == 4) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 8 & edge_list$from == 4) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 9 & edge_list$from == 5) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 5) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 8) 
                                  , "dodgerblue4", 
                           
                           ifelse((edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 23 & edge_list$from == 2) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 2) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 2) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 3) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 23 & edge_list$from == 3) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 3) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 3) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 7) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 23 & edge_list$from == 7) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 7) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 7) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 8) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 23 & edge_list$from == 8) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 8) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 8) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 9) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 23 & edge_list$from == 9) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 9) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 9) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 10) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 23 & edge_list$from == 10) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 10) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 10) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 11) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 11) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 11) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 11) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 12) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 12) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 12) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 14) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 14) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 14) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 15) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 15) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 15) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 15) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 16) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 17) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 17) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 17) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 17) |
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 18) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 18) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 18) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 18) |
                                    
                                    
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 20) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 20) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 24 & edge_list$from == 21) |
                                    (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 21) 
                                  , "deeppink4", 
                                  
                                  ifelse((edge_list$LivePercent != 0), "black",   
                          
                          "gray89")))))


#edge_list$color <- "black" #for base map only

edge_list$penwidth <- 8*(edge_list$LivePercent+.15)

#edge_list$penwidth <- 1 #for base map only

edge_list_SOC <- select(data, to, from, LivePercent_SOC) 
edge_list_SOC$style <- "solid"
edge_list_SOC$color <- NULL
#edge_list_SOC$color <- ifelse(edge_list_SOC$LivePercent_SOC != 0, "black", "gray85")
edge_list_SOC$color <- ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 2) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 7) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 7) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 8) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 8) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 9) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 9) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 9) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 10) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 10) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 10) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 11) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 11) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 12) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 12) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 12) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 14) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 14) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 17) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 20) 
                          , "turquoise4", 
                          ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 2 & edge_list$from == 1) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 3) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 12 & edge_list$from == 6) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 14 & edge_list$from == 13) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 15) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 17 & edge_list$from == 16) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 20 & edge_list$from == 19) 
                                 , "darkorange3", 
                                 
                                 ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 2) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 7 & edge_list$from == 4) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 8 & edge_list$from == 4) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 9 & edge_list$from == 5) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 5) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 8) 
                                        , "dodgerblue4", 
                                        
                                        ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 23 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 23 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 3) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 23 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 7) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 23 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 8) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 23 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 9) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 10) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 23 & edge_list$from == 10) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 10) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 10) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 11) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 11) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 11) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 11) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 12) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 12) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 12) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 14) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 14) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 14) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 15) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 16) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 17) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 17) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 17) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 17) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 18) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 18) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 18) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 18) |
                                                 
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 20) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 20) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 24 & edge_list$from == 21) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 21) 
                                               , "deeppink4", 
                                               
                                               ifelse((edge_list_SOC$LivePercent_SOC != 0), "black",   
                                                      
                                                      "gray89")))))





edge_list_SOC$penwidth <- 8*(edge_list_SOC$LivePercent_SOC +.15)

edge_list_MTX <- select(data, to, from, LivePercent_MTX) 
edge_list_MTX$style <- "solid"
edge_list_MTX$color <- NULL
#edge_list_MTX$color <- ifelse(edge_list_MTX$LivePercent_MTX != 0, "black", "gray85")
edge_list_MTX$color <- ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 2) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 7) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 7) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 8) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 8) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 9) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 9) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 9) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 10) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 10) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 10) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 11) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 11) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 12) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 12) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 12) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 14) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 14) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 17) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 20) 
                          , "turquoise4", 
                          ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 2 & edge_list$from == 1) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 3) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 12 & edge_list$from == 6) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 14 & edge_list$from == 13) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 15) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 17 & edge_list$from == 16) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 20 & edge_list$from == 19) 
                                 , "darkorange3", 
                                 
                                 ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 2) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 7 & edge_list$from == 4) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 8 & edge_list$from == 4) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 9 & edge_list$from == 5) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 5) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 8) 
                                        , "dodgerblue4", 
                                        
                                        ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 23 & edge_list$from == 2) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 2) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 2) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 3) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 23 & edge_list$from == 3) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 3) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 3) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 7) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 23 & edge_list$from == 7) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 7) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 7) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 8) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 23 & edge_list$from == 8) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 8) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 8) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 9) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 23 & edge_list$from == 9) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 9) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 9) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 10) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 23 & edge_list$from == 10) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 10) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 10) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 11) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 11) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 11) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 11) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 12) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 12) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 12) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 14) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 14) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 14) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 15) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 16) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 17) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 17) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 17) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 17) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 18) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 18) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 18) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 18) |
                                                 
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 20) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 20) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 24 & edge_list$from == 21) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 21) 
                                               , "deeppink4", 
                                               
                                               ifelse((edge_list_MTX$LivePercent_MTX != 0), "black",   
                                                      
                                                      "gray89")))))


edge_list_MTX$penwidth <- 8*(edge_list_MTX$LivePercent_MTX +.15)


#setting nodes
#Scenario1  = Q1-Q4-Q5-Q6-Q7-End14
nodes_df <- create_node_df(n = nrow(nd),
                           type = nd$label,
                           label = nd$label,
                           shape = nd$shape,
                           fillcolor = nd$color,
                           penwidth = ifelse(nd$id %in% c("6", "13", "16"), 3,
                                             ifelse(nd$id %in% c("1", "3", "11","15", "18", "19", "21", "28"), 3, 1)),
                           style = "filled, solid",

                           fontsize = ifelse(nd$id %in% c("1", "3", "6", "11", "13", "15", "16", "18", "19", "21"), 18, #correct path
                                             ifelse(nd$id %in% c("2", "7", "8", "9", "10", "12", "14", "17", "20"), 12, 14)), #hydrations #endings

                           color = ifelse(nd$id %in% c("22", "23", "24", "25", "26", "27"), "gray22", 
                                          ifelse(nd$id %in% c("1", "3", "6", "11", "13", "15", "16", "18", "19", "21", "28"), "black", "gray37")),

                           fixedsize = FALSE,
                           fontcolor = ifelse(nd$id %in% c("22", "23", "24", "25", "26", "27"), "black", 
                                          ifelse(nd$id %in% c("1", "3", "6", "11", "13", "15", "16", "18", "19", "21", "28"), "black", "gray37")),

                           fontname = ifelse(nd$id %in% c("1", "3", "6", "11", "13", "15", "16", "18", "19", "21", "28"), "Helvetica-Bold", "Helvetica"),

                           rank = ifelse(nd$id %in% c("22", "23", "24", "25", "26", "27"), "1", 
                                            ifelse(nd$id %in% c("1", "3", "6", "11", "13", "15", "16", "18", "19", "21", "28"), "2", "3"))
                           
)

edges_df <- create_edge_df(to = edge_list$to,
                           from = edge_list$from,
                           style = edge_list$style,
                           color = edge_list$color,
                           penwidth = edge_list$penwidth,
                           rel = edge_list$to)
edges_df$style <- ifelse(edges_df$id %in% c("2", "12", "49", "51", "66", "73", "80", "86", "92", "98"), "solid", "solid")


Total <-
  create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df) %>%
  add_global_graph_attrs(
    attr = c("layout","ranksep", "rankdir", "splines"),
    value = c("dot","0.5", "TB", "ortho"),
    attr_type = c("graph","graph", "graph", "graph"))
#spline: ortho = subway, line = diagonal
  
Total %>% render_graph(title = "MTX Scenario I_TOTAL")




#MAKING VISUALIZATION FOR SOC ONLY
edges_df_SOC <- create_edge_df(to = edge_list_SOC$to,
                           from = edge_list_SOC$from,
                           style = edge_list_SOC$style,
                           color = edge_list_SOC$color,
                           penwidth = edge_list_SOC$penwidth,
                           rel = edge_list_SOC$to)
edges_df_SOC$style <- ifelse(edges_df_SOC$id %in% c("2", "12", "49", "51", "66", "73", "80", "86", "92", "98"), "solid", "solid")


SOC.USERS.GRAPH <-
  create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df_SOC) %>%
  add_global_graph_attrs(
    attr = c("layout","ranksep", "rankdir", "splines"),
    value = c("dot","0.5", "TB", "ortho"),
    attr_type = c("graph","graph", "graph", "graph"))

SOC.USERS.GRAPH %>% render_graph(title = "MTX Scenario I_SOC.USERS")


#MAKING VISUALIZATION FOR MTX ONLY
edges_df_MTX <- create_edge_df(to = edge_list_MTX$to,
                               from = edge_list_MTX$from,
                               style = edge_list_MTX$style,
                               color = edge_list_MTX$color,
                               penwidth = edge_list_MTX$penwidth,
                               rel = edge_list_MTX$to)
edges_df_MTX$style <- ifelse(edges_df_MTX$id %in% c("2", "12", "49", "51", "66", "73", "80", "86", "92", "98"), "solid", "solid")

MTX.USERS.GRAPH <-
  create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df_MTX) %>%
  add_global_graph_attrs(
    attr = c("layout","ranksep", "rankdir", "splines"),
    value = c("dot","0.5", "TB", "ortho"),
    attr_type = c("graph","graph", "graph", "graph"))

MTX.USERS.GRAPH %>% render_graph(title = "MTX Scenario I_MTX.USERS")



#export_graph(Total,file_name = "ScenI_MS_MTX.svg",file_type = "svg",title = NULL,width = NULL,height = NULL)








##################################
 ###########################################3
##################################

#Creating Tables
 
#EXPERT-ASSOCIATED PATHING   
    #Scenario1  = Q1-Q4-Q5-Q6-Q7-End14
    #Correct pathing means that the user "hit" the correct nodes; they may have also hit other nodes, but they touched the expert path
    #This shows how many ppl are touching the expert path at any given time
    #Formerly known as Correct Pathing
    
MTX_Correct <- MTX_data1
MTX_Correct[MTX_Correct == "no"] <- "0"
MTX_Correct[MTX_Correct == "yes"] <- "1"


x1 <- count(MTX_Correct %>% filter(Q1 == "1"))
x2 <- count(MTX_Correct %>% filter(Q1fluid == "0"))
x3 <- count(MTX_Correct %>% filter(Q4 == "4"))
x4 <- count(MTX_Correct %>% filter(Q4fluid == "1"))
x5 <- count(MTX_Correct %>% filter(Q5 == "5"))
x6 <- count(MTX_Correct %>% filter(Q5fluid == "0"))
x7 <- count(MTX_Correct %>% filter(Q6 == "6"))
x8 <- count(MTX_Correct %>% filter(Q6fluid == "0"))
x9 <- count(MTX_Correct %>% filter(Q7 == "7"))
x10 <- count(MTX_Correct %>% filter(Q7fluid == "0"))
x11 <- count(MTX_Correct %>% filter(End14 == "14"))

correct.table <-rbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
correct.table$ID <- c("START", "24hr Fluid","42Hr", 
                      "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                      "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
correct.table <- correct.table[,c(2,1)]
correct.table$percent <- correct.table$n/users*100
#View(correct.table)
correct.questions <- correct.table[c(1,3,5,7,9,11),]
correct.hydration <- correct.table[c(2,4,6,8,10),]

correct.id <- MTX_Correct %>% filter(!is.na(ID)) %>% filter(End14 == "14")


#TOLERABLE
  #This is showing the people who have not chosen incorrect endings by these times points
  #Also known as tolerable decision making

a1 <- count(MTX_Correct %>% filter(Q1 == "1"))
a2 <- count(MTX_Correct %>% filter(Q1fluid == "0")) + count(MTX_Correct %>% filter(Q1fluid == "1"))
a3 <- count(MTX_Correct %>% filter(Q4 == "4")) + count(MTX_Correct %>% filter(Q2 == "2") %>% filter(Q4 != "4")) + count(MTX_Correct %>% filter(Q3 == "3")%>% filter(Q4 != "4"))
a4 <- count(MTX_Correct %>% filter(Q4fluid == "1")) + count(MTX_Correct %>% filter(Q4fluid == "0"))
a5 <- count(MTX_Correct %>% filter(Q5 == "5")) + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)))
a6 <- count(MTX_Correct %>% filter(Q5fluid == "0")) + count(MTX_Correct %>% filter(Q5fluid == "1")) + + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)))
a7 <- count(MTX_Correct %>% filter(Q6 == "6")) + count(MTX_Correct %>% filter(Q5fluid == "1")) + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)))
a8 <- count(MTX_Correct %>% filter(Q6fluid == "0")) + count(MTX_Correct %>% filter(Q6fluid == "1")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q6fluid)))
a9 <- count(MTX_Correct %>% filter(Q7 == "7"))
a10 <- count(MTX_Correct %>% filter(Q7fluid == "0")) + count(MTX_Correct %>% filter(Q7fluid == "1"))
a11 <- count(MTX_Correct %>% filter(End14 == "14"))

tolerable.table <-rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
tolerable.table$ID <- c("START", "24hr Fluid","42Hr", 
                      "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                      "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
tolerable.table <- tolerable.table[,c(2,1)]
tolerable.table$percent <- tolerable.table$n/users*100


#EXPERT PATHING   
  #Expert pathing means that the user "hit" the best choices within the pathing
  #also known as expert decision making

expert.id <- MTX_data2 %>% filter(!is.na(ID)) %>% filter(End14 == "14") %>% 
  filter(Q7 == "Discharge the patient home") %>% 
  filter(Q7fluid == "no") %>% 
  filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% 
  filter(Q6fluid == "no") %>% 
  filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)") %>% 
  filter(Q5fluid == "no") %>% 
  filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% 
  filter(Q4fluid == "yes") %>% 
  filter(is.na(Q2)) %>% filter(is.na(Q3)) %>% 
  filter(!is.na(Q1)) %>% 
  filter(Q1fluid == "no")


z1 <- count(MTX_data2 %>% filter(Q1 != "NA"))
z2 <- count(MTX_data2 %>% filter(Q1 != "NA") %>% filter(Q1fluid == "no"))
z3 <- count(MTX_data2 %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)) %>% filter(Q1fluid == "no") %>% filter(Q1 != "NA"))
z4 <- count(MTX_data2 %>% filter(Q4fluid == "yes")%>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z5 <- count(MTX_data2 %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z6 <- count(MTX_data2 %>% filter(Q5fluid == "no")%>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z7 <- count(MTX_data2 %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z8 <- count(MTX_data2 %>% filter(Q6fluid == "no") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z9 <- count(MTX_data2 %>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z10 <- count(MTX_data2 %>% filter(Q7fluid == "no")%>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z11 <- count(MTX_data2 %>% filter(End14 == "14")%>% filter(Q7 == "Discharge the patient home")%>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))

expert.table <-rbind(z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11)
expert.table$ID <- c("START", "24hr Fluid","42Hr", 
                     "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                     "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
expert.table <- expert.table[,c(2,1)]
expert.table$percent <- expert.table$n/users*100


#INCORRECT

ic1 <- sum(MTX_data2$Q1=="Obtain next level in 24 hours (Hour 48)") + sum(MTX_data2$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)") +
sum(MTX_data2$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)") + sum(MTX_data2$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)") +
sum(MTX_data2$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 48)") + sum(MTX_data2$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)") +
sum(MTX_data2$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)") + sum(MTX_data2$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)") +
sum(MTX_data2$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 48)") + sum(MTX_data2$Q1=="Give glucarpidase 50U/kg/dose IV once") +
sum(MTX_data2$Q1=="Discharge the patient home")

ic2 <- sum(MTX_data2$Q2=="Obtain next level in 24 hours (Hour 54)", na.rm=TRUE) + sum(MTX_data2$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)", na.rm=TRUE) +
  sum(MTX_data2$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)", na.rm=TRUE) + sum(MTX_data2$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 48)", na.rm=TRUE) +
  sum(MTX_data2$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 54)", na.rm=TRUE) + sum(MTX_data2$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)", na.rm=TRUE) +
  sum(MTX_data2$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)", na.rm=TRUE) + sum(MTX_data2$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 48)", na.rm=TRUE) +
  sum(MTX_data2$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 54)", na.rm=TRUE) + sum(MTX_data2$Q2=="Give glucarpidase 50U/kg/dose IV once", na.rm=TRUE) +
  sum(MTX_data2$Q2=="Discharge the patient home", na.rm=TRUE)
  
  
ic3 <- sum(MTX_data2$Q3=="Obtain next level in 24 hours (Hour 60)", na.rm=TRUE) + sum(MTX_data2$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)", na.rm=TRUE) +
  sum(MTX_data2$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 48)", na.rm=TRUE) + sum(MTX_data2$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 54)", na.rm=TRUE) +
  sum(MTX_data2$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)", na.rm=TRUE) + sum(MTX_data2$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)", na.rm=TRUE) +
  sum(MTX_data2$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 48)", na.rm=TRUE) + sum(MTX_data2$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 54)", na.rm=TRUE) +
  sum(MTX_data2$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)", na.rm=TRUE) + sum(MTX_data2$Q3=="Give glucarpidase 50U/kg/dose IV once", na.rm=TRUE) +
  sum(MTX_data2$Q3=="Discharge the patient home", na.rm=TRUE)

ic4 <- sum(MTX_data2$Q4=="Obtain next level in 24 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q4=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 66)", na.rm=TRUE) +
  sum(MTX_data2$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)", na.rm=TRUE) + sum(MTX_data2$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 54)", na.rm=TRUE) +
  sum(MTX_data2$Q4=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)", na.rm=TRUE) + sum(MTX_data2$Q4=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 66)", na.rm=TRUE) +
  sum(MTX_data2$Q4=="Give glucarpidase 50U/kg/dose IV once", na.rm=TRUE) + sum(MTX_data2$Q4=="Discharge the patient home", na.rm=TRUE)


ic5 <- sum(MTX_data2$Q5=="Obtain next level in 18 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q5=="Obtain next level in 24 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q5=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q5=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)", na.rm=TRUE) + sum(MTX_data2$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 60)", na.rm=TRUE) +
  sum(MTX_data2$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q5=="Give glucarpidase 50U/kg/dose IV once", na.rm=TRUE) + sum(MTX_data2$Q5=="Discharge the patient home", na.rm=TRUE)


ic6 <- sum(MTX_data2$Q6=="Obtain next level in 12 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q6=="Obtain next level in 18 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q6=="Obtain next level in 24 hours (Hour 78)", na.rm=TRUE) + sum(MTX_data2$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 66)", na.rm=TRUE) +
  sum(MTX_data2$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 72)", na.rm=TRUE) + sum(MTX_data2$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 78)", na.rm=TRUE) +
  sum(MTX_data2$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)", na.rm=TRUE) + sum(MTX_data2$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 66)", na.rm=TRUE) +
  sum(MTX_data2$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 72)", na.rm=TRUE) + sum(MTX_data2$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 78)", na.rm=TRUE) +
  sum(MTX_data2$Q6=="Give glucarpidase 50U/kg/dose IV once", na.rm=TRUE) + sum(MTX_data2$Q6=="Discharge the patient home", na.rm=TRUE)

ic7 <- sum(MTX_data2$Q7=="Obtain next level in 6 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q7=="Obtain next level in 12 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q7=="Obtain next level in 18 hours (Hour 78)", na.rm=TRUE) + sum(MTX_data2$Q7=="Obtain next level in 24 hours (Hour 84)", na.rm=TRUE) +
  sum(MTX_data2$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 78)", na.rm=TRUE) + sum(MTX_data2$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 84)", na.rm=TRUE) +
  sum(MTX_data2$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 66)", na.rm=TRUE) + sum(MTX_data2$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 72)", na.rm=TRUE) +
  sum(MTX_data2$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 78)", na.rm=TRUE) + sum(MTX_data2$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 84)", na.rm=TRUE) +
  sum(MTX_data2$Q7=="Give glucarpidase 50U/kg/dose IV once", na.rm=TRUE)


ic.1 <- 0
ic.2 <- 0
ic.3 <- as.numeric(ic1)
ic.4 <- 0
ic.5 <- as.numeric(ic4)
ic.6 <- 0
ic.7 <- as.numeric(ic5)
ic.8 <- 0
ic.9 <- as.numeric(ic6)
ic.10 <- 0
ic.11 <- as.numeric(ic7)

incorrect.table <-as.data.frame(rbind(ic.1, ic.2, ic.3, ic.4, ic.5, ic.6, ic.7, ic.8, ic.9, ic.10, ic.11))
incorrect.table$ID <- c("START", "24hr Fluid","42Hr", 
                        "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                        "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
incorrect.table <- incorrect.table[,c(2,1)]
incorrect.table$percent <- incorrect.table$V1/users*100
incorrect.table$cumulative_incorrect <- c(0, 0, 11, 11, 13, 13, 13, 13, 21, 21, 28)
incorrect.table$total.percent <- incorrect.table$cumulative_incorrect/users*100
#View(correct.table)




#COMBINING EXPERT, EXPERT-ASSOCIATED, TOLERABLE, INCORRECT

Comparison.table <- cbind(correct.table$ID, tolerable.table$n, tolerable.table$percent, expert.table$n, expert.table$percent, correct.table$n, correct.table$percent, incorrect.table$cumulative_incorrect, incorrect.table$total.percent)
colnames(Comparison.table) <- c("ID", "n.Tolerable", "pct.Tolerable", "n.Expert", "pct.Expert", "n.ExpertAssociated", "pct.ExpertAssociated", "n.Incorrect", "pct.Incorrect")
#View(Comparison.table)
Comparison.table <- as.data.frame(Comparison.table)
#View(Comparison.table)
Comparison.table$pct.Tolerable <- as.numeric(Comparison.table$pct.Tolerable )
Comparison.table$pct.Expert <- as.numeric(Comparison.table$pct.Expert)
Comparison.table$pct.ExpertAssociated <- as.numeric(Comparison.table$pct.ExpertAssociated)
Comparison.table$pct.Incorrect <- as.numeric(Comparison.table$pct.Incorrect)



Comparison.table <- Comparison.table %>% mutate(across("pct.Tolerable", round, 1))
Comparison.table <- Comparison.table %>% mutate(across("pct.Expert", round, 1))
Comparison.table <- Comparison.table %>% mutate(across("pct.ExpertAssociated", round, 1))
Comparison.table <- Comparison.table %>% mutate(across("pct.Incorrect", round, 1))


Comparison.table <- Comparison.table[,c(1, 2, 3, 6, 7, 4, 5, 8, 9)]


#########################
#########################
#########################
#MAKING TABLE OF COMPARISON DATA BETWEEEN SOC AND MTX


#Expert-Associated pathing comparison
#Formerly known as Correct decision-making
MTX_Correct <- MTX_data1
MTX_Correct[MTX_Correct == "no"] <- "0"
MTX_Correct[MTX_Correct == "yes"] <- "1"

MTX_Correct$group <- NA
MTX_Correct$group[1:27] <- "SOC"
MTX_Correct$group[28:54] <- "MTX"

SOCUsers <- length(which(MTX_Correct$group == "SOC"))
MTXUsers <- length(which(MTX_Correct$group == "MTX"))

x1.soc <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "SOC"))
x2.soc <- count(MTX_Correct %>% filter(Q1fluid == "0") %>% filter(group == "SOC"))
x3.soc <- count(MTX_Correct %>% filter(Q4 == "4") %>% filter(group == "SOC"))
x4.soc <- count(MTX_Correct %>% filter(Q4fluid == "1") %>% filter(group == "SOC"))
x5.soc <- count(MTX_Correct %>% filter(Q5 == "5") %>% filter(group == "SOC"))
x6.soc <- count(MTX_Correct %>% filter(Q5fluid == "0") %>% filter(group == "SOC"))
x7.soc <- count(MTX_Correct %>% filter(Q6 == "6") %>% filter(group == "SOC"))
x8.soc <- count(MTX_Correct %>% filter(Q6fluid == "0") %>% filter(group == "SOC"))
x9.soc <- count(MTX_Correct %>% filter(Q7 == "7") %>% filter(group == "SOC"))
x10.soc <- count(MTX_Correct %>% filter(Q7fluid == "0") %>% filter(group == "SOC"))
x11.soc <- count(MTX_Correct %>% filter(End14 == "14") %>% filter(group == "SOC"))

x1.mtx <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "MTX"))
x2.mtx <- count(MTX_Correct %>% filter(Q1fluid == "0") %>% filter(group == "MTX"))
x3.mtx <- count(MTX_Correct %>% filter(Q4 == "4") %>% filter(group == "MTX"))
x4.mtx <- count(MTX_Correct %>% filter(Q4fluid == "1") %>% filter(group == "MTX"))
x5.mtx <- count(MTX_Correct %>% filter(Q5 == "5") %>% filter(group == "MTX"))
x6.mtx <- count(MTX_Correct %>% filter(Q5fluid == "0") %>% filter(group == "MTX"))
x7.mtx <- count(MTX_Correct %>% filter(Q6 == "6") %>% filter(group == "MTX"))
x8.mtx <- count(MTX_Correct %>% filter(Q6fluid == "0") %>% filter(group == "MTX"))
x9.mtx <- count(MTX_Correct %>% filter(Q7 == "7") %>% filter(group == "MTX"))
x10.mtx <- count(MTX_Correct %>% filter(Q7fluid == "0") %>% filter(group == "MTX"))
x11.mtx <- count(MTX_Correct %>% filter(End14 == "14") %>% filter(group == "MTX"))

correct.tableMTXvsSOC <-rbind(x1.soc, x2.soc, x3.soc, x4.soc, x5.soc, x6.soc, x7.soc, x8.soc, x9.soc, x10.soc, x11.soc)
correct.tableMTXvsSOC$ID <- c("START", "24hr Fluid","42Hr", 
                              "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                              "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
correct.tableMTXvsSOC <- correct.tableMTXvsSOC[,c(2,1)]
correct.tableMTXvsSOC$percent.SOC <- correct.tableMTXvsSOC$n/SOCUsers*100

correct.tableMTXvsSOC$n.mtx <-as.numeric(c(x1.mtx, x2.mtx, x3.mtx, x4.mtx, x5.mtx, x6.mtx, x7.mtx, x8.mtx, x9.mtx, x10.mtx, x11.mtx))

correct.tableMTXvsSOC$percent.MTX <- (correct.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(correct.tableMTXvsSOC) <- c("Node", "ExpertAssociated.SOC.n", "ExpertAssociated.SOC.pct", "ExpertAssociated.MTX.n", "ExpertAssociated.MTX.pct")

#TOLERABLE
#This is showing the people who have not chosen incorrect endings by these times points
#Also known as tolerable decision making

a1.soc <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "SOC")) 
a2.soc <- count(MTX_Correct %>% filter(Q1fluid == "0") %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q1fluid == "1") %>% filter(group == "SOC"))
a3.soc <- count(MTX_Correct %>% filter(Q4 == "4")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q2 == "2") %>% filter(Q4 != "4")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q3 == "3") %>% filter(Q4 != "4") %>% filter(group == "SOC"))
a4.soc <- count(MTX_Correct %>% filter(Q4fluid == "1")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q4fluid == "0") %>% filter(group == "SOC"))
a5.soc <- count(MTX_Correct %>% filter(Q5 == "5")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)) %>% filter(group == "SOC"))
a6.soc <- count(MTX_Correct %>% filter(Q5fluid == "0")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q5fluid == "1") %>% filter(group == "SOC")) + + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5)) %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)) %>% filter(group == "SOC"))
a7.soc <- count(MTX_Correct %>% filter(Q6 == "6")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q5fluid == "1") %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)) %>% filter(group == "SOC"))
a8.soc <- count(MTX_Correct %>% filter(Q6fluid == "0")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q6fluid == "1")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q6fluid)) %>% filter(group == "SOC"))
a9.soc <- count(MTX_Correct %>% filter(Q7 == "7") %>% filter(group == "SOC"))
a10.soc <- count(MTX_Correct %>% filter(Q7fluid == "0")  %>% filter(group == "SOC")) + count(MTX_Correct %>% filter(Q7fluid == "1") %>% filter(group == "SOC"))
a11.soc <- count(MTX_Correct %>% filter(End14 == "14") %>% filter(group == "SOC"))

a1.mtx <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "MTX"))
a2.mtx <- count(MTX_Correct %>% filter(Q1fluid == "0")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q1fluid == "1")%>% filter(group == "MTX"))
a3.mtx <- count(MTX_Correct %>% filter(Q4 == "4")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q2 == "2") %>% filter(Q4 != "4")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q3 == "3")%>% filter(Q4 != "4") %>% filter(group == "MTX"))
a4.mtx <- count(MTX_Correct %>% filter(Q4fluid == "1")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q4fluid == "0") %>% filter(group == "MTX"))
a5.mtx <- count(MTX_Correct %>% filter(Q5 == "5")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)) %>% filter(group == "MTX"))
a6.mtx <- count(MTX_Correct %>% filter(Q5fluid == "0")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q5fluid == "1")  %>% filter(group == "MTX")) + + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)) %>% filter(group == "MTX"))
a7.mtx <- count(MTX_Correct %>% filter(Q6 == "6")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q5fluid == "1")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q6 == "6") %>% filter(is.na(Q5))  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q5)) %>% filter(is.na(Q6)) %>% filter(group == "MTX"))
a8.mtx <- count(MTX_Correct %>% filter(Q6fluid == "0") %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q6fluid == "1")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q7 == "7") %>% filter(is.na(Q6fluid)) %>% filter(group == "MTX"))
a9.mtx <- count(MTX_Correct %>% filter(Q7 == "7") %>% filter(group == "MTX"))
a10.mtx <- count(MTX_Correct %>% filter(Q7fluid == "0")  %>% filter(group == "MTX")) + count(MTX_Correct %>% filter(Q7fluid == "1") %>% filter(group == "MTX"))
a11.mtx <- count(MTX_Correct %>% filter(End14 == "14") %>% filter(group == "MTX"))

tolerable.tableMTXvsSOC <-rbind(a1.soc, a2.soc, a3.soc, a4.soc, a5.soc, a6.soc, a7.soc, a8.soc, a9.soc, a10.soc, a11.soc)
tolerable.tableMTXvsSOC$ID <- c("START", "24hr Fluid","42Hr", 
                                "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                                "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
tolerable.tableMTXvsSOC <- tolerable.tableMTXvsSOC[,c(2,1)]
tolerable.tableMTXvsSOC$percent <- tolerable.tableMTXvsSOC$n/SOCUsers*100

tolerable.tableMTXvsSOC$n.mtx <-as.numeric(c(a1.mtx, a2.mtx, a3.mtx, a4.mtx, a5.mtx, a6.mtx, a7.mtx, a8.mtx, a9.mtx, a10.mtx, a11.mtx))
tolerable.tableMTXvsSOC$percent.MTX <- (tolerable.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(tolerable.tableMTXvsSOC) <- c("Node", "Tolerable.SOC.n", "Tolerable.SOC.pct", "Tolerable.MTX.n", "Tolerable.MTX.pct")



#Expert pathing
MTX_data2$group <- NA
MTX_data2$group[1:27] <- "SOC"
MTX_data2$group[28:54] <- "MTX"

z1.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q1 != "NA"))
z2.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q1 != "NA") %>% filter(Q1fluid == "no"))
z3.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)) %>% filter(Q1fluid == "no") %>% filter(Q1 != "NA"))
z4.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q4fluid == "yes")%>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z5.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z6.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q5fluid == "no")%>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z7.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z8.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q6fluid == "no") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z9.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z10.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(Q7fluid == "no")%>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z11.soc <- count(MTX_data2 %>% filter(group == "SOC") %>% filter(End14 == "14")%>% filter(Q7 == "Discharge the patient home")%>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))

z1.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q1 != "NA"))
z2.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q1 != "NA") %>% filter(Q1fluid == "no"))
z3.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)) %>% filter(Q1fluid == "no") %>% filter(Q1 != "NA"))
z4.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q4fluid == "yes")%>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z5.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z6.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q5fluid == "no")%>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z7.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z8.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q6fluid == "no") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z9.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z10.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(Q7fluid == "no")%>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))
z11.mtx <- count(MTX_data2 %>% filter(group == "MTX") %>% filter(End14 == "14")%>% filter(Q7 == "Discharge the patient home")%>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "yes") %>% filter(is.na(Q2)) %>% filter(is.na(Q3)))

expert.tableMTXvsSOC <- rbind(z1.soc, z2.soc, z3.soc, z4.soc, z5.soc, z6.soc, z7.soc, z8.soc, z9.soc, z10.soc, z11.soc)
expert.tableMTXvsSOC$ID <- c("START", "24hr Fluid","42Hr", 
                             "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                             "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
expert.tableMTXvsSOC <- expert.tableMTXvsSOC[,c(2,1)]
expert.tableMTXvsSOC$percent <- expert.tableMTXvsSOC$n/SOCUsers*100

expert.tableMTXvsSOC$n.mtx <-as.numeric(c(z1.mtx, z2.mtx, z3.mtx, z4.mtx, z5.mtx, z6.mtx, z7.mtx, z8.mtx, z9.mtx, z10.mtx, z11.mtx))
expert.tableMTXvsSOC$percent.MTX <- (expert.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(expert.tableMTXvsSOC) <- c("Node", "Expert.SOC.n", "Expert.SOC.pct", "Expert.MTX.n", "Expert.MTX.pct")

###Incorrect
# Number of users who are not on tolerable path
# Subract #of Tolerable from #of Total users

incorrect.tableMTXvsSOC <- NULL
incorrect.tableMTXvsSOC$ID <- c("START", "24hr Fluid","42Hr", 
                                "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                                "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
incorrect.tableMTXvsSOC$n.soc <- SOCUsers - tolerable.tableMTXvsSOC$Tolerable.SOC.n
  incorrect.tableMTXvsSOC <- as.data.frame(incorrect.tableMTXvsSOC)
  incorrect.tableMTXvsSOC$percent.soc <- incorrect.tableMTXvsSOC$n.soc/SOCUsers*100
incorrect.tableMTXvsSOC$n.mtx <- MTXUsers - tolerable.tableMTXvsSOC$Tolerable.MTX.n
  incorrect.tableMTXvsSOC$percent.mtx <- incorrect.tableMTXvsSOC$n.mtx/MTXUsers*100
colnames(incorrect.tableMTXvsSOC) <- c("Node", "Incorrect.SOC.n", "Incorrect.SOC.pct", "Incorrect.MTX.n", "Incorrect.MTX.pct")




#Table.Path.comparisons
Table.comparisonsMTXvsSOC <- cbind(tolerable.tableMTXvsSOC[,c(1,3,5)], correct.tableMTXvsSOC[,c(3,5)], expert.tableMTXvsSOC[,c(3,5)], incorrect.tableMTXvsSOC[,c(3,5)])

View(Table.comparisonsMTXvsSOC)




#END EVALAUTION
Endings <- as.data.frame(matrix(NA, 
                                   nrow = 7,
                                   ncol = 3))
names(Endings) <- c("End", "Freq", "Percent")
Endings$End <- c("end1", "end2", "end3", "end4",
                 "end5", "end6", "FINISH")
Endings$Freq <- c(sum(MTX_Correct$End8 == 8, na.rm = TRUE), 
                  sum(MTX_Correct$End9 == 9, na.rm = TRUE),
                  sum(MTX_Correct$End10 == 10, na.rm = TRUE),
                  sum(MTX_Correct$End11 == 11, na.rm = TRUE),
                  sum(MTX_Correct$End12 == 12, na.rm = TRUE),
                  sum(MTX_Correct$End13 == 13, na.rm = TRUE),
                  sum(MTX_Correct$End14 == 14, na.rm = TRUE))
Endings$Percent <- Endings$Freq/users*100
Endings$Meaning <- c("Waited too long for levels.",
                      "Leucovorin given too early.",
                       "Glucarpidase was not indicated in this situation.",
                       "Discharged inappropriately.",
                        "Leucovorin levels are too high and needed to be monitored more closely.",
                        "Patient should have been discharged.", 
                        "Congratulations! You successfully treated this patient.")
Endings$End <- c("end1", "end2", "end3", "end4",
                 "end5", "end6", "FINISH")



#Ending distribution SOC vs MTX
end.analysis2 <-as.data.frame(rbind(
  length(which(MTX_data1[1:27,] == "8")),
  length(which(MTX_data1[1:27,] == "9")), 
  length(which(MTX_data1[1:27,] == "10")), 
  length(which(MTX_data1[1:27,] == "11")), 
  length(which(MTX_data1[1:27,] == "12")), 
  length(which(MTX_data1[1:27,] == "13")), 
  length(which(MTX_data1[1:27,] == "14")), 
  length(which(MTX_data1[28:54,] == "8")),
  length(which(MTX_data1[28:54,] == "9")), 
  length(which(MTX_data1[28:54,] == "10")), 
  length(which(MTX_data1[28:54,] == "11")), 
  length(which(MTX_data1[28:54,] == "12")), 
  length(which(MTX_data1[28:54,] == "13")), 
  length(which(MTX_data1[28:54,] == "14"))))
end.analysis2$Group[1:7] <-  "SOC"
end.analysis2$Group[8:14] <-  "MTX"
end.analysis2$end <- c("End1", "End2", "End3", "End4", "End5", "End6", "FINISH", "End1", "End2", "End3", "End4", "End5", "End6", "FINISH")
end.analysis2 <- end.analysis2[,c(2,3,1)]
names(end.analysis2) <- c("Group", "End", "Count")
SOCUsers <- sum(end.analysis2$Count[which(end.analysis2$Group == "SOC")])
MTXUsers <- sum(end.analysis2$Count[which(end.analysis2$Group == "MTX")])
end.analysis2$Percent <- ifelse(end.analysis2$Group == "SOC", (end.analysis2$Count)/SOCUsers*100, (end.analysis2$Count)/MTXUsers*100)

end.analysis2$remain <- ifelse(end.analysis2$Group == "SOC", SOCUsers-end.analysis2$Count, MTXUsers-end.analysis2$Count)
#View(end.analysis2)

library(DescTools)

#GTEST
for (i in 0:6) {
  obs <- rbind(c(end.analysis2[8+i,3], end.analysis2[8+i,5]), 
               c(end.analysis2[1+i,3], end.analysis2[1+i,5]))
  G<-GTest(x = obs, correct = "none")
  end.analysis2$pvalue[i+1] <- G$p.value
}
end.analysis2$pvalue[8:14] <- NA

end.analysis2.5 <- end.analysis2[c(7,6,5,4,3,2,1, 14, 13, 12, 11, 10, 9, 8),]

end.analysis3 <- as.data.frame(matrix(NA, 
                                      nrow = 7,
                                      ncol = 6))
end.analysis3[,1] <- c("Waited too long for levels.",
                       "Leucovorin given too early.",
                       "Glucarpidase was not indicated in this situation.",
                       "Discharged inappropriately.",
                       "Leucovorin levels are too high and needed to be monitored more closely.",
                       "Patient should have been discharged.", 
                       "Congratulations! You successfully treated this patient.")
end.analysis3[,2] <- end.analysis2$Count[1:7]
end.analysis3[,3] <- end.analysis2$Percent[1:7]
end.analysis3[,4] <- end.analysis2$Count[8:14]
end.analysis3[,5] <- end.analysis2$Percent[8:14]
end.analysis3[,6] <- end.analysis2$pvalue[1:7]

names(end.analysis3) <- c("Endings", "SOC.n", "SOC.pct", "MTX.n", "MTX.pct", "p-value")

View(end.analysis3)







#Calculating the number of nodes touched
    MTX_nodes <- MTX_data1
      #MTX_nodes <- MTX_nodes[1:27,]  #SOC only
      #MTX_nodes <- MTX_nodes[28:54,]  #MTX only
      
    MTX_nodes$Q1fluid[MTX_nodes$Q1fluid == "no"] <- "Q1n"
    MTX_nodes$Q1fluid[MTX_nodes$Q1fluid == "yes"] <- "Q1y"
    MTX_nodes$Q2fluid[MTX_nodes$Q2fluid == "no"] <- "Q2n"
    MTX_nodes$Q2fluid[MTX_nodes$Q2fluid == "yes"] <- "Q2y"
    MTX_nodes$Q3fluid[MTX_nodes$Q3fluid == "no"] <- "Q3n"
    MTX_nodes$Q3fluid[MTX_nodes$Q3fluid == "yes"] <- "Q3y"
    MTX_nodes$Q4fluid[MTX_nodes$Q4fluid == "no"] <- "Q4n"
    MTX_nodes$Q4fluid[MTX_nodes$Q4fluid == "yes"] <- "Q4y"
    MTX_nodes$Q5fluid[MTX_nodes$Q5fluid == "no"] <- "Q5n"
    MTX_nodes$Q5fluid[MTX_nodes$Q5fluid == "yes"] <- "Q5y"
    MTX_nodes$Q6fluid[MTX_nodes$Q6fluid == "no"] <- "Q6n"
    MTX_nodes$Q6fluid[MTX_nodes$Q6fluid == "yes"] <- "Q6y"
    MTX_nodes$Q7fluid[MTX_nodes$Q7fluid == "no"] <- "Q7n"
    MTX_nodes$Q7fluid[MTX_nodes$Q7fluid == "yes"] <- "Q7y"

    nodes.touched<-nrow(table(unlist(MTX_nodes[,2:22])))
    nodes.touched.id<-as.data.frame(table(unlist(MTX_nodes[,2:22])))
    

MTX_nodes$touched <- NA
MTX_nodes$touched <- apply(MTX_nodes[,2:22], 1, function(x)length(unique(na.omit(x))))

MTX_nodes$group <- NA
  MTX_nodes$group[1:27] <- "SOC"
  MTX_nodes$group[28:54] <- "MTX"
  
ave.nodes.SOC <- mean(MTX_nodes$touched[1:27] )
ave.nodes.MTX <- mean(MTX_nodes$touched[28:54] )
ave.nodes.Total <- mean(MTX_nodes$touched[1:54] )
expert.nodes <- 11
total.nodes <- 28
fewest.nodees <- 3
median.nodes.Total <- median(MTX_nodes$touched[1:54] )
  median.nodes.SOC <- median(MTX_nodes$touched[1:27] )
  median.nodes.MTX <- median(MTX_nodes$touched[28:54] )

min.nodes.Total <- min(MTX_nodes$touched[1:54] )
  min.nodes.SOC <- min(MTX_nodes$touched[1:27] )
  min.nodes.MTX <- min(MTX_nodes$touched[28:54] )

max.nodes.Total <- max(MTX_nodes$touched[1:54] )
  max.nodes.SOC <- max(MTX_nodes$touched[1:27] )
  max.nodes.MTX <- max(MTX_nodes$touched[28:54] )



nodes.table <-rbind(ave.nodes.SOC, ave.nodes.MTX, expert.nodes,total.nodes, min.nodes.SOC, min.nodes.MTX, max.nodes.SOC, max.nodes.MTX)
nodes.table[,1] <- round(nodes.table[,1], digit=2)
colnames(nodes.table) <- "# nodes visited"


View(nodes.table)

#write.csv(MTX_nodes, "MTX_nodes_Scenario1.csv")


#######################
#######################
#Pathway frequency
MTX_nodes.1 <- MTX_nodes[,c(2:22)]  
  #SOC ONLY
  MTX_nodes.1.soc <- MTX_nodes[c(1:27),c(2:22)]  
  #MTX ONLY
  MTX_nodes.1.mtx <- MTX_nodes[c(28:54),c(2:22)]  
  
MTX_nodes.1 <- tidyr::unite(MTX_nodes.1, paths, dplyr::everything(), na.rm = TRUE, sep = ', ')
  pathway.frequency <- MTX_nodes.1 %>% group_by_all %>% count
  colnames(pathway.frequency) <- c("Pathway", "Frequency.TOTAL")
MTX_nodes.1.soc <- tidyr::unite(MTX_nodes.1.soc, paths, dplyr::everything(), na.rm = TRUE, sep = ', ')
  pathway.frequency.soc <- MTX_nodes.1.soc %>% group_by_all %>% count
  colnames(pathway.frequency.soc) <- c("Pathway", "Frequency.SOC")
MTX_nodes.1.mtx <- tidyr::unite(MTX_nodes.1.mtx, paths, dplyr::everything(), na.rm = TRUE, sep = ', ')
  pathway.frequency.mtx <- MTX_nodes.1.mtx %>% group_by_all %>% count
  colnames(pathway.frequency.mtx) <- c("Pathway", "Frequency.MTX")
  
pathways_merge <- merge(pathway.frequency, pathway.frequency.soc, by = "Pathway", all.x = TRUE)  
pathways_merge <- merge(pathways_merge, pathway.frequency.mtx, by = "Pathway", all.x = TRUE)  

pathways_merge <- pathways_merge[order(-pathways_merge$Frequency.TOTAL),]
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "1, ", "START, ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "14", "FINAL")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "13", "End 6")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "12", "End 5")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "11", "End 4")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "10", "End 3")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "9", "End 2")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "8", "End 1")
  
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "2, ", "30Hr, ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "3, ", "36Hr, ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "4, ", "42Hr, ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "5, ", "48Hr, ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "6, ", "54Hr, ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "7, ", "60Hr, ")
  
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q1n, ", "No (24), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q1y, ", "Yes (24), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q2n, ", "No (30), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q2y, ", "Yes (30), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q3n, ", "No (36), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q3y, ", "Yes (36), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q4n, ", "No (42), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q4y, ", "Yes (42), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q5n, ", "No (48), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q5y, ", "Yes (48), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q6n, ", "No (54), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q6y, ", "Yes (54), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q7n, ", "No (60), ")
  pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q7y, ", "Yes (60), ")

  pathways_merge[is.na(pathways_merge)] <- 0

  
  #define best path
  pathways_merge$Pathway[pathways_merge$Pathway == "START, No (24), 42Hr, Yes (42), 48Hr, No (48), 54Hr, No (54), 60Hr, No (60), FINAL"] <- "**START, No (24), 42Hr, Yes (42), 48Hr, No (48), 54Hr, No (54), 60Hr, No (60), FINAL"
  
View(pathways_merge)
#write.csv(pathways_merge, "pathways_merge_Scenario1.csv")


###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################







end_time <- Sys.time()
run_time <- end_time - start_time
