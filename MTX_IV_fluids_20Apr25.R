library(DiagrammeR)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

setwd("/Users/caitlinhanlon/Desktop/Hopkins Related/mtx/Rfiles")

start_time <- Sys.time()

#Highlight and run all of the code below
#To view ALL users, execute render_graph(Total)
#To determine number of users, execute NumberOfUsers
#To view all users in data table, execute View(MTX_data1)

data1 <- read_excel("MTX CYOA ScenarioIV_levels (Responses)_TOTAL.xlsx")
MTXasDF <- as.data.frame(data1)

MTX_COA <- MTXasDF[,c(2,4,3,6,5,9,8)]
names(MTX_COA) <- c("ID", "Q1","Q1fluid", "Q2","Q2fluid","Q3","Q3fluid")


MTX_COA2 <- MTX_COA[c(1:9, 15:41, 48:65),]
#SOC ONLY
#MTX_COA2 <- MTX_COA[c(1:9, 15:32),]
#MTX ONLY
#MTX_COA2<- MTX_COA[c(33:41, 48:65),]
number.unique.choices.MTX_COA2 <- sapply(MTX_COA2 , function(x) length(unique(na.omit(x))))



MTX_data1 <- MTX_COA

#USERS
  MTX_data1 <- MTX_data1[c(1:9, 15:41, 48:65),]
  #SOC
  #MTX_data1 <- MTX_data1[c(1:9, 15:32),]
  #MTX
  #MTX_data1 <- MTX_data1[c(33:41, 48:65),]


#add Q6, Q11, and Q20 ... since these are "ends" in the CYOA, gave them End designation
MTX_data1$End4 <- NA
MTX_data1$End5 <- NA
MTX_data1$End6 <- NA
MTX_data1$End7 <- NA
MTX_data1$End8 <- NA
MTX_data1$End9 <- NA
MTX_data1$End10 <- NA

#Setting Ends

#Q1
#24hr
#Section2
MTX_data1$End4[MTX_data1$Q1=="Obtain next level in 18 hours (Hour 42)"] <- 4
MTX_data1$End5[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)"] <- 5
MTX_data1$End5[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)"] <- 5
MTX_data1$End5[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)"] <- 5
MTX_data1$End5[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)"] <- 5
MTX_data1$End5[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)"] <- 5
MTX_data1$End5[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)"] <- 5
MTX_data1$End6[MTX_data1$Q1=="Give glucarpidase 50U/kg/dose IV once"] <- 6
MTX_data1$End7[MTX_data1$Q1=="Discharge the patient home"] <- 7

#Q2
#30hr
#Section3
MTX_data1$End4[MTX_data1$Q2=="Obtain next level in 12 hours (Hour 42)"] <- 4
MTX_data1$End4[MTX_data1$Q2=="Obtain next level in 24 hours (Hour 54)"] <- 4
MTX_data1$End5[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)"] <- 5
MTX_data1$End5[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)"] <- 5
MTX_data1$End5[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 54)"] <- 5
MTX_data1$End5[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)"] <- 5
MTX_data1$End5[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)"] <- 5
MTX_data1$End5[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 54)"] <- 5
MTX_data1$End6[MTX_data1$Q2=="Give glucarpidase 50U/kg/dose IV once"] <- 6
MTX_data1$End7[MTX_data1$Q2=="Discharge the patient home"] <- 7

#Q3
#36hr
#Section4
MTX_data1$End8[MTX_data1$Q3=="Obtain next level in 6 hours (Hour 42)"] <- 8
MTX_data1$End8[MTX_data1$Q3=="Obtain next level in 12 hours (Hour 48)"] <- 8
MTX_data1$End8[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 8
MTX_data1$End8[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 3 hours, obtain next level in 12 hours (Hour 48)"] <- 8
MTX_data1$End10[MTX_data1$Q3=="Give glucarpidase at 50U/kg/dose IV once, obtain next level in 3 hours (Hour 39)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give glucarpidase at 50U/kg/dose IV once, obtain next level in 6 hours (Hour 42)"] <- 10
MTX_data1$End9[MTX_data1$Q3=="Give glucarpidase at 50U/kg/dose IV once, leucovorin at 15mg/m2/dose every 3 hours, obtain next level in 6 hours (Hour 42)"] <- 9
MTX_data1$End10[MTX_data1$Q3=="Give glucarpidase at 50U/kg/dose IV once, leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give glucarpidase at 50U/kg/dose IV once, leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 10
MTX_data1$End4[MTX_data1$Q3=="Give glucarpidase at 50U/kg/dose IV once, obtain next level in 12 hours (Hour 48)"] <- 4
MTX_data1$End7[MTX_data1$Q3=="Discharge the patient home"] <- 7


#Setting Questions to give choice output
MTX_data1$Q1 <- ifelse(is.na(MTX_data1$Q1), NA, 1)
MTX_data1$Q2 <- ifelse(is.na(MTX_data1$Q2), NA, 2)
MTX_data1$Q3 <- ifelse(is.na(MTX_data1$Q3), NA, 3)

#new section
#Setting Fluids to give choice output
MTX_data1$Q1fluid[MTX_data1$Q1fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q1fluid[MTX_data1$Q1fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q2fluid[MTX_data1$Q2fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q2fluid[MTX_data1$Q2fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q3fluid[MTX_data1$Q3fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q3fluid[MTX_data1$Q3fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

#Adding in graph elements

library(DiagrammeR)
data <- read_excel("DatagenerationIV_levels.xlsx")

#this tells it that if someone goes from Q1 to Q2 to Q3 to not put it in the visualization as Q1 to Q2 AND Q1 to Q3, etc
#telling it that if someone goes from Q1 to Q4, everything in between should be NA
#basically it's saying if there is something from Q1 to Q2, make everything past that NA; pick up Q2 later.
#consult DatagenerationIV.xlsx --> the numbers for each row should correspond to the numbers in each section below
#tried to make it IF/NOT type of statement but that just wasnt working for weird reasons

livecountX <- c(nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
  
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End5)) %>% select(Q2:End4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End6)) %>% select(Q2:End5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End7)) %>% select(Q2:End6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End5)) %>% select(Q2:End4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End6)) %>% select(Q2:End5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End7)) %>% select(Q2:End6) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),                  
                
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End5)) %>% select(Q3:End4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End6)) %>% select(Q3:End5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End7)) %>% select(Q3:End6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End5)) %>% select(Q3:End4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End6)) %>% select(Q3:End5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End7)) %>% select(Q3:End6) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),                
                
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End4))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End7)) %>% select(End4:End6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End8)) %>% select(End4:End7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End9)) %>% select(End4:End8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(End4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End4))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End7)) %>% select(End4:End6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End8)) %>% select(End4:End7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End9)) %>% select(End4:End8) %>% filter_all(all_vars(is.na(.)))),               
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(End4:End9) %>% filter_all(all_vars(is.na(.)))))


users <- length(which(MTX_data1$Q1 != "NA"))

data$LiveCount <- livecountX
data$LivePercent <- livecountX/users


MTX_data1$group <- NA
  MTX_data1$group[1:27] <- "SOC"
  MTX_data1$group[28:54] <- "MTX"


livecountX_SOC <- c(nrow(MTX_data1  %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End5)) %>% select(Q2:End4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End6)) %>% select(Q2:End5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End7)) %>% select(Q2:End6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(End4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(End5)) %>% select(Q2:End4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(End6)) %>% select(Q2:End5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q1fluid=="no") %>% filter(!is.na(End7)) %>% select(Q2:End6) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),                  
                  
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End5)) %>% select(Q3:End4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End6)) %>% select(Q3:End5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End7)) %>% select(Q3:End6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="no") %>% filter(!is.na(End4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="no") %>% filter(!is.na(End5)) %>% select(Q3:End4) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="no") %>% filter(!is.na(End6)) %>% select(Q3:End5) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q2fluid=="no") %>% filter(!is.na(End7)) %>% select(Q3:End6) %>% filter_all(all_vars(is.na(.)))),
                  
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),                
                  
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End4))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End7)) %>% select(End4:End6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End8)) %>% select(End4:End7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End9)) %>% select(End4:End8) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(End4:End9) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="no") %>% filter(!is.na(End4))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="no") %>% filter(!is.na(End7)) %>% select(End4:End6) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="no") %>% filter(!is.na(End8)) %>% select(End4:End7) %>% filter_all(all_vars(is.na(.)))),
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="no") %>% filter(!is.na(End9)) %>% select(End4:End8) %>% filter_all(all_vars(is.na(.)))),               
                  nrow(MTX_data1 %>% filter(group == "SOC") %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(End4:End9) %>% filter_all(all_vars(is.na(.)))))
  
  
data$LiveCount_SOC <- livecountX_SOC
data$LivePercent_SOC <- livecountX_SOC/27


livecountX_MTX <- c(nrow(MTX_data1  %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End5)) %>% select(Q2:End4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End6)) %>% select(Q2:End5) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="yes") %>% filter(!is.na(End7)) %>% select(Q2:End6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(End4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(End5)) %>% select(Q2:End4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(End6)) %>% select(Q2:End5) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q1fluid=="no") %>% filter(!is.na(End7)) %>% select(Q2:End6) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),                  
                    
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End5)) %>% select(Q3:End4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End6)) %>% select(Q3:End5) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="yes") %>% filter(!is.na(End7)) %>% select(Q3:End6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="no") %>% filter(!is.na(End4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="no") %>% filter(!is.na(End5)) %>% select(Q3:End4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="no") %>% filter(!is.na(End6)) %>% select(Q3:End5) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q2fluid=="no") %>% filter(!is.na(End7)) %>% select(Q3:End6) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),                
                    
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End4))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End7)) %>% select(End4:End6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End8)) %>% select(End4:End7) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End9)) %>% select(End4:End8) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(End4:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="no") %>% filter(!is.na(End4))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="no") %>% filter(!is.na(End7)) %>% select(End4:End6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="no") %>% filter(!is.na(End8)) %>% select(End4:End7) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="no") %>% filter(!is.na(End9)) %>% select(End4:End8) %>% filter_all(all_vars(is.na(.)))),               
                    nrow(MTX_data1 %>% filter(group == "MTX") %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(End4:End9) %>% filter_all(all_vars(is.na(.)))))


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
#              "Yes (30)", "No (30)", "Yes (36)", "No (36)", "end1",
#              "end2", "end3", "end4", "end5", "FINAL", "end7")
nd$label <- c("1", "2", "3","4","7", 
              "6", "5", "8", "9", "end1",
              "end2", "end3", "end4", "end5", "FINAL", "end7")

# add colors, shapes, etc
nd$color <- NULL
nd$color <- ifelse(nd$id %in% c("10", "11", "12", "13", "14", "16"), "gray70", 
                   ifelse(nd$id %in% c("1", "2", "5", "9", "15"), "white", "gray92"))  

nd$shape <- NULL
nd$shape = ifelse(nd$id %in% c("1", "10", "11", "12", "13", "14","15", "16"), "square", 
                  ifelse(nd$id %in% c("2",  "9", "3", "7", "6", "8") , "diamond",
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


edge_list$color <- ifelse((edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 3) |
                            (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 6) |
                            (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 7) |
                            (edge_list$LivePercent != 0 & edge_list$to == 15 & edge_list$from == 8)
                          , "turquoise4", 
                          ifelse((edge_list$LivePercent != 0 & edge_list$to == 3 & edge_list$from == 1) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 8 & edge_list$from == 5) 
                                 , "darkorange3", 
                                 ifelse((edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 4) |
                                          (edge_list$LivePercent != 0 & edge_list$to == 7 & edge_list$from == 4) 
                                        , "dodgerblue4", 
                                        ifelse((edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 2) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 11 & edge_list$from == 2) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 12 & edge_list$from == 2) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 2) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 3) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 11 & edge_list$from == 3) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 12 & edge_list$from == 3) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 3) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 6) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 11 & edge_list$from == 6) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 12 & edge_list$from == 6) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 6) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 7) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 11 & edge_list$from == 7) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 12 & edge_list$from == 7) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 7) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 8) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 8) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 14 & edge_list$from == 8) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 8) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 10 & edge_list$from == 9) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 9) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 14 & edge_list$from == 9) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 9) 
                                               , "deeppink4", 
                                               
                                               ifelse((edge_list$LivePercent != 0), "black",   
                                                      
                                                      "gray89")))))



edge_list$color <- "black" #for base map only

edge_list$penwidth <- 8*(edge_list$LivePercent+.15)

edge_list$penwidth <- 1 #for base map only

edge_list_SOC <- select(data, to, from, LivePercent_SOC) 
edge_list_SOC$style <- "solid"
edge_list_SOC$color <- NULL
edge_list_SOC$color <- ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 3) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 6) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 7) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 15 & edge_list$from == 8)
                          , "turquoise4", 
                          ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 3 & edge_list$from == 1) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 8 & edge_list$from == 5) 
                                 , "darkorange3", 
                                 ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 4) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 7 & edge_list$from == 4) 
                                        , "dodgerblue4", 
                                        ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 11 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 12 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 2) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 11 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 12 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 3) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 6) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 11 & edge_list$from == 6) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 12 & edge_list$from == 6) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 6) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 11 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 12 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 7) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 14 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 8) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 10 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 14 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 9) 
                                               , "deeppink4", 
                                               
                                               ifelse((edge_list_SOC$LivePercent_SOC != 0), "black",   
                                                      
                                                      "gray89")))))



edge_list_SOC$penwidth <- 8*(edge_list_SOC$LivePercent_SOC +.33)

edge_list_MTX <- select(data, to, from, LivePercent_MTX) 
edge_list_MTX$style <- "solid"
edge_list_MTX$color <- NULL
edge_list_MTX$color <- ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 3) |
                                (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 6) |
                                (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 7) |
                                (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 15 & edge_list$from == 8)
                              , "turquoise4", 
                              ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 3 & edge_list$from == 1) |
                                       (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                       (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                       (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 8 & edge_list$from == 5) 
                                     , "darkorange3", 
                                     ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 4) |
                                              (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 7 & edge_list$from == 4) 
                                            , "dodgerblue4", 
                                            ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 2) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 11 & edge_list$from == 2) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 12 & edge_list$from == 2) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 2) |
                                                     
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 3) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 11 & edge_list$from == 3) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 12 & edge_list$from == 3) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 3) |
                                                     
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 6) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 11 & edge_list$from == 6) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 12 & edge_list$from == 6) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 6) |
                                                     
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 7) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 11 & edge_list$from == 7) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 12 & edge_list$from == 7) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 7) |
                                                     
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 8) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 8) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 14 & edge_list$from == 8) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 8) |
                                                     
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 10 & edge_list$from == 9) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 9) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 14 & edge_list$from == 9) |
                                                     (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 9) 
                                                   , "deeppink4", 
                                                   
                                                   ifelse((edge_list_MTX$LivePercent_MTX != 0), "black",   
                                                          
                                                          "gray89")))))
edge_list_MTX$penwidth <- 8*(edge_list_MTX$LivePercent_MTX +.33)


#setting nodes
#Perfect is Q1-Q3-End10
nodes_df <- create_node_df(n = nrow(nd),
                           type = nd$label,
                           label = nd$label,
                           shape = nd$shape,
                           fillcolor = nd$color,
                           penwidth = ifelse(nd$id %in% c("1", "2", "5", "9", "15"), 3, 1),
                           
                           style = "filled, solid",
                           
                           fontsize = ifelse(nd$id %in% c("1", "2", "5", "9"), 18, #correct path
                                             ifelse(nd$id %in% c("3", "7", "8", "6"), 12, 12)), #hydrations #endings
                           
                           color = ifelse(nd$id %in% c("10", "11", "12", "13", "14", "15", "16"), "gray22", 
                                          ifelse(nd$id %in% c("1", "2", "5", "9"), "black", "gray37")),
                       
                           fixedsize = FALSE,
                           fontcolor = ifelse(nd$id %in% c("10", "11", "12", "13", "14", "15", "16"), "black", 
                                                      ifelse(nd$id %in% c("1", "2", "5", "9"), "black", "gray37")),
                           fontname = ifelse(nd$id %in% c("1", "5"), "Helvetica-Bold", "Helvetica"),
                           rank = ifelse(nd$id %in% c("10", "11", "12", "13", "14", "16"), "1", 
                               ifelse(nd$id %in% c("1", "2", "5", "9", "15"), "2", "3"))
)
                           

edges_df <- create_edge_df(to = edge_list$to,
                           from = edge_list$from,
                           style = edge_list$style,
                           color = edge_list$color,
                           penwidth = edge_list$penwidth,
                           rel = edge_list$to)
edges_df$style <- ifelse(edges_df$id %in% c("1", "2", "5", "9", "15"), "solid", "solid")

Total <-
  create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df) %>%
  add_global_graph_attrs(
    attr = c("layout","ranksep", "rankdir", "splines"),
    value = c("dot","0.5", "TB", "ortho"),
    attr_type = c("graph","graph", "graph", "graph"))
#spline: ortho = subway, line = diagonal

Total %>% render_graph(title = "MTX Scenario IV_levels")

#MAKING VISUALIZATION FOR SOC ONLY
edges_df_SOC <- create_edge_df(to = edge_list_SOC$to,
                               from = edge_list_SOC$from,
                               style = edge_list_SOC$style,
                               color = edge_list_SOC$color,
                               penwidth = edge_list_SOC$penwidth,
                               rel = edge_list_SOC$to)
edges_df_SOC$style <- ifelse(edges_df_SOC$id %in% c("1", "2", "5", "9", "15"), "solid", "solid")

SOC.USERS.GRAPH <-
  create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df_SOC) %>%
  add_global_graph_attrs(
    attr = c("layout","ranksep", "rankdir", "splines"),
    value = c("dot","0.5", "TB", "ortho"),
    attr_type = c("graph","graph", "graph", "graph"))

SOC.USERS.GRAPH %>% render_graph(title = "MTX Scenario IV_SOC.USERS")


#MAKING VISUALIZATION FOR MTX ONLY
edges_df_MTX <- create_edge_df(to = edge_list_MTX$to,
                               from = edge_list_MTX$from,
                               style = edge_list_MTX$style,
                               color = edge_list_MTX$color,
                               penwidth = edge_list_MTX$penwidth,
                               rel = edge_list_MTX$to)
edges_df_MTX$style <- ifelse(edges_df_MTX$id %in% c("1", "2", "5", "9", "15"), "solid", "solid")

MTX.USERS.GRAPH <-
  create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df_MTX) %>%
  add_global_graph_attrs(
    attr = c("layout","ranksep", "rankdir", "splines"),
    value = c("dot","0.5", "TB", "ortho"),
    attr_type = c("graph","graph", "graph", "graph"))

MTX.USERS.GRAPH %>% render_graph(title = "MTX Scenario IV_MTX.USERS")




#export_graph(Total,file_name = "ScenIV_MS_MTX.svg",file_type = "svg")






##################################
  ###########################################3
##################################

#Creating Tables

#Perfect is Q1-Q3-End9

MTX_Correct <- MTX_data1
  MTX_Correct[MTX_Correct == "no"] <- "0"
  MTX_Correct[MTX_Correct == "yes"] <- "1"

#Correct users
x1 <- count(MTX_Correct %>% filter(Q1 == "1"))
x2 <- count(MTX_Correct %>% filter(Q1fluid == "1"))
x3 <- count(MTX_Correct %>% filter(Q3 == "3"))
x4 <- count(MTX_Correct %>% filter(Q3fluid == "0"))
x5 <- count(MTX_Correct %>% filter(End9 == "9"))

EA.table <-rbind(x1, x2,  x3, x4, x5)
EA.table$ID <- c("START", "Q1fluid","Q3", 
                      "Q3fluid", "FINAL")
EA.table <- EA.table[,c(2,1)]
EA.table$percent <- EA.table$n/users*100
#View(correct.table)


#TOLERABLE
#This is showing the people who have not chosen incorrect endings by these times points
#Also known as tolerable decision making


a1 <- data[1,5] + data[2,5] #24
a2 <- a1  #24fluid
a3 <- a1 - (data[6,5] + data[6,5] + data[7,5] + data[8,5] + data[11,5] + data[12,5] + data[13,5] + data[15,5] +
              data[23,5] + data[24,5] + data[25,5] + data[26,5] + data[18,5] + data[19,5] + data[20,5] + data[21,5]) #subtracting those that go to end before 36hr / Q3
a4 <- a3 #36hr fluid

a5 <- a3 - (data[29,5] + data[30,5] + data[31,5] + data[33,5] + data[34,5] + data[35,5] + data[36,5] + data[38,5]) #subtracting those that go to end before 42hr


tolerable.table <-rbind(a1, a2, a3, a4, a5)
tolerable.table$ID <- c("START", "24hr Fluid","36Hr", 
                        "36Hr fluid", "FINAL")
tolerable.table <- tolerable.table[,c(2,1)]
names(tolerable.table) <- c("ID", "n")
tolerable.table$percent <- tolerable.table$n/users*100



#EXPERT PATHING   
#Expert pathing means that the user "hit" the best choices within the pathing
#also known as expert decision making

z1 <- count(MTX_Correct %>% filter(Q1 == "1"))
z2 <- count(MTX_Correct %>% filter(Q1fluid == "1") %>% filter(Q1 == "1"))
z3 <- count(MTX_Correct %>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1"))
z4 <- count(MTX_Correct %>% filter(Q3fluid == "0")%>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1"))
z5 <- count(MTX_Correct %>% filter(End9 == "9")%>% filter(Q3fluid == "0")%>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1"))

expert.table <-rbind(z1, z2, z3, z4, z5)
expert.table$ID <- c("START", "Q1fluid","Q3", 
                      "Q3fluid", "FINAL")
expert.table <- expert.table[,c(2,1)]
expert.table$percent <- expert.table$n/users*100
#View(perfect.table)


#INCORRECT

ic.1 <- users - a1
ic.2 <- users - a2
ic.3 <- users - a3
ic.4 <- users - a4
ic.5 <- users - a5


incorrect.table <-as.data.frame(rbind(ic.1, ic.2, ic.3, ic.4, ic.5))
incorrect.table$ID <- c("START", "24hr Fluid", "36Hr", 
                        "36Hr fluid", "FINAL")
incorrect.table <- incorrect.table[,c(2,1)]
names(incorrect.table) <- c("ID", "n")
incorrect.table$percent <- incorrect.table$n/users*100


#COMBINING CORRECT AND PERFECT

Comparison.table <- cbind(tolerable.table$ID, tolerable.table$n, tolerable.table$percent, expert.table$n, expert.table$percent, EA.table$n, EA.table$percent, incorrect.table$n, incorrect.table$percent)
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


#################
#################
#################

#MAKING TABLE OF COMPARISON DATA BETWEEEN SOC AND MTX
MTX_Correct <- MTX_data1
MTX_Correct[MTX_Correct == "no"] <- "0"
MTX_Correct[MTX_Correct == "yes"] <- "1"

MTX_Correct$group <- NA
MTX_Correct$group[1:27] <- "SOC"
MTX_Correct$group[28:54] <- "MTX"

SOCUsers <- length(which(MTX_Correct$group == "SOC"))
MTXUsers <- length(which(MTX_Correct$group == "MTX"))

x1.soc <- count(MTX_Correct %>% filter(Q1 == "1")  %>% filter(group == "SOC"))
x2.soc <- count(MTX_Correct %>% filter(Q1fluid == "1")  %>% filter(group == "SOC"))
x3.soc <- count(MTX_Correct %>% filter(Q3 == "3")  %>% filter(group == "SOC"))
x4.soc <- count(MTX_Correct %>% filter(Q3fluid == "0")  %>% filter(group == "SOC"))
x5.soc <- count(MTX_Correct %>% filter(End9 == "9")  %>% filter(group == "SOC"))

x1.mtx <- count(MTX_Correct %>% filter(Q1 == "1")  %>% filter(group == "MTX"))
x2.mtx <- count(MTX_Correct %>% filter(Q1fluid == "1")  %>% filter(group == "MTX"))
x3.mtx <- count(MTX_Correct %>% filter(Q3 == "3")  %>% filter(group == "MTX"))
x4.mtx <- count(MTX_Correct %>% filter(Q3fluid == "0")  %>% filter(group == "MTX"))
x5.mtx <- count(MTX_Correct %>% filter(End9 == "9")  %>% filter(group == "MTX"))


EA.tableMTXvsSOC <-rbind(x1.soc, x2.soc, x3.soc, x4.soc, x5.soc)
EA.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36Hr", 
                         "36Hr fluid", "FINAL")
EA.tableMTXvsSOC <- EA.tableMTXvsSOC[,c(2,1)]
EA.tableMTXvsSOC$percent.SOC <- EA.tableMTXvsSOC$n/SOCUsers*100

EA.tableMTXvsSOC$n.mtx <-as.numeric(c(x1.mtx, x2.mtx, x3.mtx, x4.mtx, x5.mtx))

EA.tableMTXvsSOC$percent.MTX <- (EA.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(EA.tableMTXvsSOC) <- c("Node", "ExpertAssociated.SOC.n", "ExpertAssociated.SOC.pct", "ExpertAssociated.MTX.n", "ExpertAssociated.MTX.pct")


#View(correct.table)

#TOLERABLE
#This is showing the people who have not chosen incorrect endings by these times points
#Also known as tolerable decision making

a1.soc <- data[1,7] + data[2,7] #24
a2.soc <- a1.soc  #24fluid
a3.soc <- a1.soc - (data[6,7] + data[6,7] + data[7,7] + data[8,7] + data[11,7] + data[12,7] + data[13,7] + data[15,7] +
              data[23,7] + data[24,7] + data[25,7] + data[26,7] + data[18,7] + data[19,7] + data[20,7] + data[21,7]) #subtracting those that go to end before 36hr / Q3
a4.soc <- a3.soc #36hr fluid
a5.soc <- a3.soc - (data[29,7] + data[30,7] + data[31,7] + data[33,7] + data[34,7] + data[35,7] + data[36,7] + data[38,7]) #subtracting those that go to end before 42hr

a1.mtx <- data[1,9] + data[2,9] #24
a2.mtx <- a1.mtx  #24fluid
a3.mtx <- a1.mtx - (data[6,9] + data[6,9] + data[9,9] + data[8,9] + data[11,9] + data[12,9] + data[13,9] + data[15,9] +
                      data[23,9] + data[24,9] + data[25,9] + data[26,9] + data[18,9] + data[19,9] + data[20,9] + data[21,9]) #subtracting those that go to end before 36hr / Q3
a4.mtx <- a3.mtx #36hr fluid
a5.mtx <- a3.mtx - (data[29,9] + data[30,9] + data[31,9] + data[33,9] + data[34,9] + data[35,9] + data[36,9] + data[38,9]) #subtracting those that go to end before 42hr



tolerable.tableMTXvsSOC <-rbind(a1.soc, a2.soc, a3.soc, a4.soc, a5.soc)
tolerable.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36Hr", 
                                "36Hr fluid","FINAL")
tolerable.tableMTXvsSOC <- tolerable.tableMTXvsSOC[,c(2,1)]
tolerable.tableMTXvsSOC$percent <- tolerable.tableMTXvsSOC$LiveCount_SOC/SOCUsers*100

tolerable.tableMTXvsSOC$n.mtx <-as.numeric(c(a1.mtx, a2.mtx, a3.mtx, a4.mtx, a5.mtx))
tolerable.tableMTXvsSOC$percent.MTX <- (tolerable.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(tolerable.tableMTXvsSOC) <- c("Node", "Tolerable.SOC.n", "Tolerable.SOC.pct", "Tolerable.MTX.n", "Tolerable.MTX.pct")




#Expert.pathing
z1.soc <- count(MTX_Correct %>% filter(Q1 == "1")  %>% filter(group == "SOC"))
z2.soc <- count(MTX_Correct %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "SOC"))
z3.soc <- count(MTX_Correct %>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "SOC"))
z4.soc <- count(MTX_Correct %>% filter(Q3fluid == "0")%>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "SOC"))
z5.soc <- count(MTX_Correct %>% filter(End9 == "9")%>% filter(Q3fluid == "0")%>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "SOC"))

z1.mtx <- count(MTX_Correct %>% filter(Q1 == "1")  %>% filter(group == "MTX"))
z2.mtx <- count(MTX_Correct %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "MTX"))
z3.mtx <- count(MTX_Correct %>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "MTX"))
z4.mtx <- count(MTX_Correct %>% filter(Q3fluid == "0")%>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "MTX"))
z5.mtx <- count(MTX_Correct %>% filter(End9 == "9")%>% filter(Q3fluid == "0")%>% filter(Q3 == "3") %>% filter(is.na(Q2)) %>% filter(Q1fluid == "1") %>% filter(Q1 == "1")  %>% filter(group == "MTX"))

expert.tableMTXvsSOC <- rbind(z1.soc, z2.soc, z3.soc, z4.soc, z5.soc)
expert.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36Hr", 
                             "36Hr fluid", "FINAL")
expert.tableMTXvsSOC <- expert.tableMTXvsSOC[,c(2,1)]
expert.tableMTXvsSOC$percent <- expert.tableMTXvsSOC$n/SOCUsers*100

expert.tableMTXvsSOC$n.mtx <-as.numeric(c(z1.mtx, z2.mtx, z3.mtx, z4.mtx, z5.mtx))
expert.tableMTXvsSOC$percent.MTX <- (expert.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(expert.tableMTXvsSOC) <- c("Node", "Expert.SOC.n", "Expert.SOC.pct", "Expert.MTX.n", "Expert.MTX.pct")



### Incorrect 
incorrect.tableMTXvsSOC <- NULL
incorrect.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36hr", 
                                "36Hr fluid","FINAL")
incorrect.tableMTXvsSOC$n.soc <- SOCUsers - tolerable.tableMTXvsSOC$Tolerable.SOC.n
incorrect.tableMTXvsSOC <- as.data.frame(incorrect.tableMTXvsSOC)
incorrect.tableMTXvsSOC$percent.soc <- incorrect.tableMTXvsSOC$n.soc/SOCUsers*100
incorrect.tableMTXvsSOC$n.mtx <- MTXUsers - tolerable.tableMTXvsSOC$Tolerable.MTX.n
incorrect.tableMTXvsSOC$percent.mtx <- incorrect.tableMTXvsSOC$n.mtx/MTXUsers*100
colnames(incorrect.tableMTXvsSOC) <- c("Node", "Incorrect.SOC.n", "Incorrect.SOC.pct", "Incorrect.MTX.n", "Incorrect.MTX.pct")



#Table.Path.comparisons <- cbind(correct.table, perfect.table$n, perfect.table$percent, expert.table$n, expert.table$percent)

Table.comparisonsMTXvsSOC <- cbind(tolerable.tableMTXvsSOC[,c(1,3,5)], EA.tableMTXvsSOC[,c(3,5)], expert.tableMTXvsSOC[,c(3,5)], incorrect.tableMTXvsSOC[,c(3,5)])

View(Table.comparisonsMTXvsSOC)


end_time <- Sys.time()
run_time <- end_time - start_time















#END EVALAUTION
Endings <- as.data.frame(matrix(NA, 
                                nrow = 7,
                                ncol = 3))
names(Endings) <- c("End", "Freq", "Percent")
Endings$End <- c("end1", "end2", "end3", "end4",
                 "end5", "FINISH", "end7")
Endings$Freq <- c(sum(MTX_Correct$End4 == 4, na.rm = TRUE), 
                  sum(MTX_Correct$End5 == 5, na.rm = TRUE),
                  sum(MTX_Correct$End6 == 6, na.rm = TRUE),
                  sum(MTX_Correct$End7 == 7, na.rm = TRUE),
                  sum(MTX_Correct$End8 == 8, na.rm = TRUE),
                  sum(MTX_Correct$End9 == 9, na.rm = TRUE),
                  sum(MTX_Correct$End10 == 10, na.rm = TRUE))
Endings$Percent <- Endings$Freq/users*100
Endings$Meaning <- c("Waited too long for levels.",
                     "Leucovorin given too early.",
                     "Glucarpidase was not indicated in this situation.",
                     "Discharged inappropriately.",
                     "Glucarpidase was needed at an earlier timepoint.",
                     "Congratulations! You successfully treated this patient.", 
                     "Leucovorin levels incorrect.")
Endings$End <- c("end1", "end2", "end3", "end4",
                 "end5", "FINISH", "end7")




#Calculating the number of nodes touched
MTX_nodes <- MTX_data1
  #MTX_nodes <- MTX_nodes[1:27,]  #SOC only
  MTX_nodes <- MTX_nodes[28:54,]  #MTX only

  MTX_nodes$Q1fluid[MTX_nodes$Q1fluid == "no"] <- "Q1n"
  MTX_nodes$Q1fluid[MTX_nodes$Q1fluid == "yes"] <- "Q1y"
  MTX_nodes$Q2fluid[MTX_nodes$Q2fluid == "no"] <- "Q2n"
  MTX_nodes$Q2fluid[MTX_nodes$Q2fluid == "yes"] <- "Q2y"
  MTX_nodes$Q3fluid[MTX_nodes$Q3fluid == "no"] <- "Q3n"
  MTX_nodes$Q3fluid[MTX_nodes$Q3fluid == "yes"] <- "Q3y"

nodes.touched<-nrow(table(unlist(MTX_nodes[,2:14])))
nodes.touched.id<-as.data.frame(table(unlist(MTX_nodes[,2:14])))

MTX_nodes$touched <- NA
MTX_nodes$touched <- apply(MTX_nodes[,2:14], 1, function(x)length(unique(na.omit(x))))


MTX_nodes$group <- NA
MTX_nodes$group[1:27] <- "SOC"
MTX_nodes$group[28:54] <- "MTX"

ave.nodes.SOC <- mean(MTX_nodes$touched[1:27] )
ave.nodes.MTX <- mean(MTX_nodes$touched[28:54] )
ave.nodes.Total <- mean(MTX_nodes$touched[1:54] )
expert.nodes <- 5
total.nodes <- 16
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

write.csv(MTX_nodes, "MTX_nodes_Scenario4.csv")   


#######################
#######################
#Pathway frequency
MTX_nodes.1 <- MTX_nodes[,c(2:14)]  
#SOC ONLY
MTX_nodes.1.soc <- MTX_nodes[c(1:27),c(2:14)]  
#MTX ONLY
MTX_nodes.1.mtx <- MTX_nodes[c(28:54),c(2:14)]  

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
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "9", "FINAL")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "4", "End 1")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "5", "End 2")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "6", "End 3")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "7", "End 4")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "8", "End 5")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "10", "End 7")

pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "2, ", "30Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "3, ", "36Hr, ")


pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q1n, ", "No (24), ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q1y, ", "Yes (24), ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q2n, ", "No (30), ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q2y, ", "Yes (30), ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q3n, ", "No (36), ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q3y, ", "Yes (36), ")

pathways_merge[is.na(pathways_merge)] <- 0

#define best path
pathways_merge$Pathway[pathways_merge$Pathway == "START, Yes (24), 36Hr, No (36), FINAL"] <- "**START, Yes (24), 36Hr, No (36), FINAL"

View(pathways_merge)
#write.csv(pathways_merge, "pathways_merge_Scenario4.csv")







