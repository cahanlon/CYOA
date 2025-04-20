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
#To view pathways of individual, execute Individual("") where the JHEDID is written between the quotes

MTX <- read_excel("MTX CYOA Scenario II_levels (Responses)_TOTAL.xlsx")
MTXasDF <- as.data.frame(MTX)

MTX_COA <- MTXasDF[,c(2,4,3,6,5,9,8, 12, 11, 15, 14, 17, 16, 19, 18, 21, 20)]
names(MTX_COA) <- c("ID", "Q1", "Q1fluid", "Q2", "Q2fluid",  "Q3","Q3fluid",  "Q4","Q4fluid",  "Q5","Q5fluid", "Q6", "Q6fluid", "Q7","Q7fluid", "Q8","Q8fluid")


MTX_COA2 <- MTX_COA[c(1:9, 15:41, 48:65),]
#SOC ONLY
#MTX_COA2 <- MTX_COA[c(1:9, 15:32),]
#MTX ONLY
#MTX_COA2<- MTX_COA[c(33:41, 48:65),]
number.unique.choices.MTX_COA2 <- sapply(MTX_COA2 , function(x) length(unique(na.omit(x))))



MTX_data1 <- MTX_COA

#PNM
  MTX_data1 <- MTX_data1[c(1:9, 15:41, 48:65),]
  #SOC
  #MTX_data1 <- MTX_data1[c(1:9, 15:32),]
  #MTX
  #MTX_data1 <- MTX_data1[c(33:41, 48:65),]


#add End9, End10, End11, End12, and End13 ... since these are "ends" in the CYOA, gave them End designation
MTX_data1$End9 <- NA
MTX_data1$End10 <- NA
MTX_data1$End11 <- NA
MTX_data1$End12 <- NA
MTX_data1$End13 <- NA
MTX_data1$End14 <- NA
MTX_data1$End15 <- NA

#Setting Ends
#Q1
#Section2
#Hr24
MTX_data1$End10[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)"] <- 10
MTX_data1$End10[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)"] <- 10
MTX_data1$End10[MTX_data1$Q1=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)"] <- 10
MTX_data1$End10[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 30)"] <- 10
MTX_data1$End10[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 36)"] <- 10
MTX_data1$End10[MTX_data1$Q1=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 42)"] <- 10
MTX_data1$End11[MTX_data1$Q1=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End12[MTX_data1$Q1=="Discharge the patient home"] <- 12

#Setting Ends for Q2
#Section3
#Hr30
MTX_data1$End10[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)"] <- 10
MTX_data1$End10[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)"] <- 10
MTX_data1$End10[MTX_data1$Q2=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 48)"] <- 10
MTX_data1$End10[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 36)"] <- 10
MTX_data1$End10[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 42)"] <- 10
MTX_data1$End10[MTX_data1$Q2=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 18 hours (Hour 48)"] <- 10
MTX_data1$End11[MTX_data1$Q2=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End12[MTX_data1$Q2=="Discharge the patient home"] <- 12

#Setting Ends for Q3
#Section4
#Hr36
MTX_data1$End10[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 48)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 42)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 48)"] <- 10
MTX_data1$End10[MTX_data1$Q3=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 60)"] <- 10
MTX_data1$End11[MTX_data1$Q3=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End12[MTX_data1$Q3=="Discharge the patient home"] <- 12

#Setting Ends for Q4
#Section6
#Hr42
MTX_data1$End11[MTX_data1$Q4=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End12[MTX_data1$Q4=="Discharge the patient home"] <- 12

#Setting Ends for Q5
#Section6
#Hr48
MTX_data1$End9[MTX_data1$Q5=="Obtain next level in 24 hours (Hour 72)"] <- 9
MTX_data1$End11[MTX_data1$Q5=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End12[MTX_data1$Q5=="Discharge the patient home"] <- 12
MTX_data1$End13[MTX_data1$Q5=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 72)"] <- 13
MTX_data1$End13[MTX_data1$Q5=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 72)"] <- 13

#Setting Ends for Q6
#Section7
#Hr54
MTX_data1$End9[MTX_data1$Q6=="Obtain next level in 24 hours (Hour 78)"] <- 9
MTX_data1$End11[MTX_data1$Q6=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End12[MTX_data1$Q6=="Discharge the patient home"] <- 12
MTX_data1$End13[MTX_data1$Q6=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 78)"] <- 13
MTX_data1$End13[MTX_data1$Q6=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 78)"] <- 13

#Setting Ends for Q7
#Section8
#Hr60
MTX_data1$End11[MTX_data1$Q7=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End14[MTX_data1$Q7=="Obtain next level in 6 hours (Hour 66)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Obtain next level in 12 hours (Hour 72)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Obtain next level in 24 hours (Hour 84)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 66)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 72)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 84)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 66)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 72)"] <- 14
MTX_data1$End14[MTX_data1$Q7=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 84)"] <- 14
MTX_data1$End15[MTX_data1$Q7=="Discharge the patient home"] <- 15

#Setting Ends for Q8
#Section9
#Hr66
MTX_data1$End11[MTX_data1$Q8=="Give glucarpidase 50U/kg/dose IV once"] <- 11
MTX_data1$End14[MTX_data1$Q8=="Obtain next level in 6 hours (Hour 72)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Obtain next level in 12 hours (Hour 78)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Obtain next level in 24 hours (Hour 90)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 72)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 78)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="GGive leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 90)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 72)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 12 hours (Hour 78)"] <- 14
MTX_data1$End14[MTX_data1$Q8=="Give leucovorin at 100mg/m2/dose every 6 hours, obtain next level in 24 hours (Hour 90)"] <- 14
MTX_data1$End15[MTX_data1$Q8=="Discharge the patient home"] <- 15


#new section
#Setting Fluids to give choice output
MTX_data1$Q1fluid[MTX_data1$Q1fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q1fluid[MTX_data1$Q1fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q2fluid[MTX_data1$Q2fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q2fluid[MTX_data1$Q2fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q3fluid[MTX_data1$Q3fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q3fluid[MTX_data1$Q3fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q4fluid[MTX_data1$Q4fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q4fluid[MTX_data1$Q4fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q5fluid[MTX_data1$Q5fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q5fluid[MTX_data1$Q5fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q6fluid[MTX_data1$Q6fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q6fluid[MTX_data1$Q6fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q7fluid[MTX_data1$Q7fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q7fluid[MTX_data1$Q7fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data1$Q8fluid[MTX_data1$Q8fluid=="No fluid changes are necessary at this time."] <- "no"
MTX_data1$Q8fluid[MTX_data1$Q8fluid=="Increase fluids by 75 ml/m2/hr."] <- "yes"

MTX_data2 <- MTX_data1


#Setting Questions to give choice output
MTX_data1$Q1 <- ifelse(is.na(MTX_data1$Q1), NA, 1)
MTX_data1$Q2 <- ifelse(is.na(MTX_data1$Q2), NA, 2)
MTX_data1$Q3 <- ifelse(is.na(MTX_data1$Q3), NA, 3)
MTX_data1$Q4 <- ifelse(is.na(MTX_data1$Q4), NA, 4)
MTX_data1$Q5 <- ifelse(is.na(MTX_data1$Q5), NA, 5)
MTX_data1$Q6 <- ifelse(is.na(MTX_data1$Q6), NA, 6)
MTX_data1$Q7 <- ifelse(is.na(MTX_data1$Q7), NA, 7)
MTX_data1$Q8 <- ifelse(is.na(MTX_data1$Q8), NA, 8)

#View(MTX_data1)








#Adding in graph elements


library(DiagrammeR)
data <- read_excel("DatagenerationII_levels.xlsx")

livecountX <- c(nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
  
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q2:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q1fluid=="no") %>% filter(!is.na(End12)) %>% select(Q2:End11) %>% filter_all(all_vars(is.na(.)))),

                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),                
                
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q3:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q2fluid=="no") %>% filter(!is.na(End12)) %>% select(Q3:End11) %>% filter_all(all_vars(is.na(.)))),
    
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),
                
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q4:Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q4:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q4:Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q3fluid=="no") %>% filter(!is.na(End12)) %>% select(Q4:End11) %>% filter_all(all_vars(is.na(.)))),
 
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q4))),
                
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q5:End12) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(Q8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q4fluid=="no") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),
 
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(Q5))),

                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q6:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),                
                nrow(MTX_data1 %>% filter(Q5fluid=="yes") %>% filter(!is.na(End13)) %>% select(Q6:End12) %>% filter_all(all_vars(is.na(.)))),                
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End9)) %>% select(Q6:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q5fluid=="no") %>% filter(!is.na(End13)) %>% select(Q6:End12) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(Q6))), 
                
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q7:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q7:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="yes") %>% filter(!is.na(End13)) %>% select(Q7:End12) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(Q8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End9)) %>% select(Q7:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End11)) %>% select(Q7:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q6fluid=="no") %>% filter(!is.na(End13)) %>% select(Q7:End12) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(Q7))), 
                
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q8:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(End14))  %>% select(Q8:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="yes") %>% filter(!is.na(End15)) %>% select(Q8:End14) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(End11)) %>% select(Q8:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(End14))  %>% select(Q8:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q7fluid=="no") %>% filter(!is.na(End15)) %>% select(Q8:End14) %>% filter_all(all_vars(is.na(.)))),
 
                nrow(MTX_data1 %>% filter(Q8fluid=="yes") %>% filter(!is.na(Q8))),
                nrow(MTX_data1 %>% filter(Q8fluid=="no") %>% filter(!is.na(Q8))), 
                nrow(MTX_data1 %>% filter(Q8fluid=="yes") %>% filter(!is.na(End11)) %>% select(End9:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q8fluid=="yes") %>% filter(!is.na(End14))  %>% select(End9:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q8fluid=="yes") %>% filter(!is.na(End15)) %>% select(End9:End14) %>% filter_all(all_vars(is.na(.)))),               
                nrow(MTX_data1 %>% filter(Q8fluid=="no") %>% filter(!is.na(End11)) %>% select(End9:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q8fluid=="no") %>% filter(!is.na(End14))  %>% select(End9:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(Q8fluid=="no") %>% filter(!is.na(End15)) %>% select(End9:End14) %>% filter_all(all_vars(is.na(.)))))


users <- length(which(MTX_data1$Q1 != "NA"))


data$LiveCount <- livecountX
data$LivePercent <- livecountX/users




MTX_data1$group <- NA
MTX_data1$group[1:27] <- "SOC"
MTX_data1$group[28:54] <- "MTX"

livecountX_SOC <- c(nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q2:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End12)) %>% select(Q2:End11) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),                
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q3:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End12)) %>% select(Q3:End11) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q4:Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q4:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q4:Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End12)) %>% select(Q4:End11) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q4))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q4))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q5:End12) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q5))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q5))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q6:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End13)) %>% select(Q6:End12) %>% filter_all(all_vars(is.na(.)))),                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End9)) %>% select(Q6:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End13)) %>% select(Q6:End12) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q6))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q6))), 
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q7:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q7:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End13)) %>% select(Q7:End12) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End9)) %>% select(Q7:Q8) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End11)) %>% select(Q7:End10) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End13)) %>% select(Q7:End12) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(Q7))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(Q7))), 
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q8:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End14))  %>% select(Q8:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End15)) %>% select(Q8:End14) %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End11)) %>% select(Q8:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End14))  %>% select(Q8:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End15)) %>% select(Q8:End14) %>% filter_all(all_vars(is.na(.)))),
                
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(Q8))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="no") %>% filter(!is.na(Q8))), 
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(End11)) %>% select(End9:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(End14))  %>% select(End9:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(End15)) %>% select(End9:End14) %>% filter_all(all_vars(is.na(.)))),               
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="no") %>% filter(!is.na(End11)) %>% select(End9:End10)%>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>% filter(Q8fluid=="no") %>% filter(!is.na(End14))  %>% select(End9:End13)  %>% filter_all(all_vars(is.na(.)))),
                nrow(MTX_data1 %>% filter(group == "SOC")  %>%  filter(Q8fluid=="no") %>% filter(!is.na(End15)) %>% select(End9:End14) %>% filter_all(all_vars(is.na(.)))))



data$LiveCount_SOC <- livecountX_SOC
data$LivePercent_SOC <- livecountX_SOC/27

livecountX_MTX <- c(nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q1))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q1))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q2))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q2:End11) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q2))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q3)) %>% select(Q2) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q2:Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End10)) %>% select(Q2:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End11)) %>% select(Q2:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q1fluid=="no") %>% filter(!is.na(End12)) %>% select(Q2:End11) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q2))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q2))),                
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q3))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q3:End11) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q3))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q4)) %>% select(Q3) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q3:Q4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End10)) %>% select(Q3:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End11)) %>% select(Q3:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q2fluid=="no") %>% filter(!is.na(End12)) %>% select(Q3:End11) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q3))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q3))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q4))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q4:Q6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q4:End11) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q4))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q5)) %>% select(Q4) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q4:Q6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End10)) %>% select(Q4:End9) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End11)) %>% select(Q4:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q3fluid=="no") %>% filter(!is.na(End12)) %>% select(Q4:End11) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q4))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q4))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q5))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(Q8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q5:End12) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q5))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q6)) %>% select(Q5) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(Q8)) %>% select(Q5:Q7) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End11)) %>% select(Q5:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q4fluid=="no") %>% filter(!is.na(End12)) %>% select(Q5:End11) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q5))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q5))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q6))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q6:Q8) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),                
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="yes") %>% filter(!is.na(End13)) %>% select(Q6:End12) %>% filter_all(all_vars(is.na(.)))),                
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q6))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(Q7)) %>% select(Q6) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End9)) %>% select(Q6:Q8) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End11)) %>% select(Q6:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End12)) %>% select(Q6:End11) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q5fluid=="no") %>% filter(!is.na(End13)) %>% select(Q6:End12) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q6))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q6))), 
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q7))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(Q8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End9)) %>% select(Q7:Q8) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q7:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="yes") %>% filter(!is.na(End13)) %>% select(Q7:End12) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q7))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(Q8)) %>% select(Q7) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End9)) %>% select(Q7:Q8) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End11)) %>% select(Q7:End10) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End12)) %>% select(Q7:End11) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q6fluid=="no") %>% filter(!is.na(End13)) %>% select(Q7:End12) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(Q7))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(Q7))), 
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End11)) %>% select(Q8:End10)%>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End14))  %>% select(Q8:End13)  %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="yes") %>% filter(!is.na(End15)) %>% select(Q8:End14) %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End11)) %>% select(Q8:End10)%>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End14))  %>% select(Q8:End13)  %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q7fluid=="no") %>% filter(!is.na(End15)) %>% select(Q8:End14) %>% filter_all(all_vars(is.na(.)))),
                    
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(Q8))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="no") %>% filter(!is.na(Q8))), 
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(End11)) %>% select(End9:End10)%>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(End14))  %>% select(End9:End13)  %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="yes") %>% filter(!is.na(End15)) %>% select(End9:End14) %>% filter_all(all_vars(is.na(.)))),               
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="no") %>% filter(!is.na(End11)) %>% select(End9:End10)%>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>% filter(Q8fluid=="no") %>% filter(!is.na(End14))  %>% select(End9:End13)  %>% filter_all(all_vars(is.na(.)))),
                    nrow(MTX_data1 %>% filter(group == "MTX")  %>%  filter(Q8fluid=="no") %>% filter(!is.na(End15)) %>% select(End9:End14) %>% filter_all(all_vars(is.na(.)))))

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
#              "No (60)", "66Hr", "Yes (66)", "No (66)", "end1",
#              "end2", "end3", "end4", "end5", "end6", "FINISH")

#nd$label <- c("START", "2", "3","4","5", 
#              "6","7", "8", "9", "10",
#              "11", "12", "13", "14", "15",
#              "16", "17", "18", "19", "20",
#              "21", "22", "23", "24", "end1",
#              "end2", "end3", "end4", "end5", "end6", "FINISH")

nd$label <- c("1", "2", "3","4","7", 
              "10","6", "5", "8", "9",
              "12", "11", "13", "14", "15",
              "16", "18", "17", "19", "20",
              "21", "22", "24", "23", "end1",
              "end2", "end3", "end4", "end5", "end6", "FINISH")



# add colors, shapes, etc
nd$color <- NULL
nd$color <- ifelse(nd$id %in% c("25", "26", "27", "28", "29", "30"), "gray70", #ends
                   ifelse(nd$id %in% c("3", "4", "7", "8", "9", "11", "14", "17", "20", "22", "23", "24"), "gray92", "white"))  #middle, top


nd$shape <- NULL
nd$shape = ifelse(nd$id %in% c("26", "28", "27", "29", "25", "30", "31"), "square", 
                  ifelse(nd$id %in% c("3", "7", "8", "9", "11",  "17", "14", "20", "23", "24", "2", "10", "12", "15", "18", "21") , "diamond",
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
                            (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 3) |
                            
                            (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 7) |
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 7) |
                            
                            (edge_list$LivePercent != 0 & edge_list$to == 5 & edge_list$from == 8) |
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 8) | 
                            
                            (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 9) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 9) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 9) |
                            
                            (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 11) | 
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 11) |
                            
                            (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 14) |
                            (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 14) |
                            
                            (edge_list$LivePercent != 0 & edge_list$to == 31 & edge_list$from == 20) |

                            (edge_list$LivePercent != 0 & edge_list$to == 31 & edge_list$from == 23) |
                            (edge_list$LivePercent != 0 & edge_list$to == 31 & edge_list$from == 24) 
                          , "turquoise4", 
                          ifelse((edge_list$LivePercent != 0 & edge_list$to == 3 & edge_list$from == 1) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 6 & edge_list$from == 2) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 9 & edge_list$from == 5) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 13 & edge_list$from == 10) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 10) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 11 & edge_list$from == 6) |
                                   
                                   (edge_list$LivePercent != 0 & edge_list$to == 16 & edge_list$from == 12) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                   
                                   (edge_list$LivePercent != 0 & edge_list$to == 19 & edge_list$from == 15) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 17 & edge_list$from == 16) |
                                   
                                   (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 18) |
                                   (edge_list$LivePercent != 0 & edge_list$to == 20 & edge_list$from == 19) 
                                 , "darkorange3", 
                                 
                                 ifelse((edge_list$LivePercent != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                          (edge_list$LivePercent != 0 & edge_list$to == 7 & edge_list$from == 4) |
                                          (edge_list$LivePercent != 0 & edge_list$to == 8 & edge_list$from == 4) |
                                          (edge_list$LivePercent != 0 & edge_list$to == 7 & edge_list$from == 6) |
                                          (edge_list$LivePercent != 0 & edge_list$to == 8 & edge_list$from == 6) |
                                          
                                          (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 11) |
                                          (edge_list$LivePercent != 0 & edge_list$to == 22 & edge_list$from == 17) 
                                        , "dodgerblue4", 
                                        
                                        ifelse((edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 2) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 2) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 2) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 3) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 3) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 3) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 7) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 7) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 7) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 8) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 8) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 8) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 9) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 9) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 9) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 10) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 10) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 10) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 11) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 11) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 12) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 12) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 14) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 14) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 14) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 29 & edge_list$from == 14) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 15) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 15) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 15) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 29 & edge_list$from == 15) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 26 & edge_list$from == 15) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 17) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 17) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 17) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 29 & edge_list$from == 17) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 25 & edge_list$from == 18) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 18) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 28 & edge_list$from == 18) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 29 & edge_list$from == 18) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 20) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 30 & edge_list$from == 20) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 21) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 30 & edge_list$from == 21) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 23) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 30 & edge_list$from == 23) |
                                                 
                                                 (edge_list$LivePercent != 0 & edge_list$to == 27 & edge_list$from == 24) |
                                                 (edge_list$LivePercent != 0 & edge_list$to == 30 & edge_list$from == 24) 

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
edge_list_SOC$color <- ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 3) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 3) |
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 7) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 7) |
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 5 & edge_list$from == 8) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 8) | 
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 9) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 9) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 9) |
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 11) | 
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 11) |
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 14) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 14) |
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 31 & edge_list$from == 20) |
                            
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 31 & edge_list$from == 23) |
                            (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 31 & edge_list$from == 24) 
                          , "turquoise4", 
                          ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 3 & edge_list$from == 1) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 6 & edge_list$from == 2) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 9 & edge_list$from == 5) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 13 & edge_list$from == 10) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 10) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 11 & edge_list$from == 6) |
                                   
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 16 & edge_list$from == 12) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                   
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 19 & edge_list$from == 15) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 17 & edge_list$from == 16) |
                                   
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 18) |
                                   (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 20 & edge_list$from == 19) 
                                 , "darkorange3", 
                                 
                                 ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 7 & edge_list$from == 4) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 8 & edge_list$from == 4) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 7 & edge_list$from == 6) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 8 & edge_list$from == 6) |
                                          
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 11) |
                                          (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 22 & edge_list$from == 17) 
                                        , "dodgerblue4", 
                                        
                                        ifelse((edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 2) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 2) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 3) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 3) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 7) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 7) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 8) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 8) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 9) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 9) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 10) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 10) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 10) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 11) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 11) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 12) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 12) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 14) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 14) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 14) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 29 & edge_list$from == 14) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 29 & edge_list$from == 15) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 26 & edge_list$from == 15) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 17) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 17) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 17) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 29 & edge_list$from == 17) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 25 & edge_list$from == 18) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 18) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 28 & edge_list$from == 18) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 29 & edge_list$from == 18) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 20) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 30 & edge_list$from == 20) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 21) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 30 & edge_list$from == 21) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 23) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 30 & edge_list$from == 23) |
                                                 
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 27 & edge_list$from == 24) |
                                                 (edge_list_SOC$LivePercent_SOC != 0 & edge_list$to == 30 & edge_list$from == 24) 
                                               
                                               , "deeppink4", 
                                               
                                               ifelse((edge_list_SOC$LivePercent_SOC != 0), "black",   
                                                      
                                                      "gray89")))))
edge_list_SOC$penwidth <- 8*(edge_list_SOC$LivePercent_SOC +.15)


edge_list_MTX <- select(data, to, from, LivePercent_MTX) 
edge_list_MTX$style <- "solid"
edge_list_MTX$color <- NULL
#edge_list_MTX$color <- ifelse(edge_list_MTX$LivePercent_MTX != 0, "black", "gray85")
edge_list_MTX$color <- ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 3) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 3) |
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 7) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 7) |
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 5 & edge_list$from == 8) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 8) | 
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 9) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 9) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 9) |
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 11) | 
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 11) |
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 14) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 14) |
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 31 & edge_list$from == 20) |
                            
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 31 & edge_list$from == 23) |
                            (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 31 & edge_list$from == 24) 
                          , "turquoise4", 
                          ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 3 & edge_list$from == 1) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 4 & edge_list$from == 2) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 6 & edge_list$from == 2) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 9 & edge_list$from == 5) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 13 & edge_list$from == 10) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 10) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 11 & edge_list$from == 6) |
                                   
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 16 & edge_list$from == 12) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 12) |
                                   
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 19 & edge_list$from == 15) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 17 & edge_list$from == 16) |
                                   
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 18) |
                                   (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 20 & edge_list$from == 19) 
                                 , "darkorange3", 
                                 
                                 ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 4 & edge_list$from == 3) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 7 & edge_list$from == 4) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 8 & edge_list$from == 4) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 7 & edge_list$from == 6) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 8 & edge_list$from == 6) |
                                          
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 11) |
                                          (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 22 & edge_list$from == 17) 
                                        , "dodgerblue4", 
                                        
                                        ifelse((edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 2) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 2) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 2) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 3) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 3) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 3) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 7) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 7) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 7) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 8) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 8) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 8) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 9) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 9) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 9) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 10) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 10) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 10) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 11) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 11) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 12) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 12) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 14) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 14) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 14) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 29 & edge_list$from == 14) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 29 & edge_list$from == 15) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 26 & edge_list$from == 15) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 17) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 17) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 17) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 29 & edge_list$from == 17) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 25 & edge_list$from == 18) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 18) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 28 & edge_list$from == 18) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 29 & edge_list$from == 18) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 20) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 30 & edge_list$from == 20) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 21) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 30 & edge_list$from == 21) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 23) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 30 & edge_list$from == 23) |
                                                 
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 27 & edge_list$from == 24) |
                                                 (edge_list_MTX$LivePercent_MTX != 0 & edge_list$to == 30 & edge_list$from == 24) 
                                               
                                               , "deeppink4", 
                                               
                                               ifelse((edge_list_MTX$LivePercent_MTX != 0), "black",   
                                                      
                                                      "gray89")))))

edge_list_MTX$penwidth <- 8*(edge_list_MTX$LivePercent_MTX +.15)


#setting nodes
#Scenario2  = Q1-Q3-Q4-Q5-Q6-Q7-End16
nodes_df <- create_node_df(n = nrow(nd),
                           type = nd$label,
                           label = nd$label,
                           shape = nd$shape,
                           fillcolor = nd$color,
                           penwidth = ifelse(nd$id %in% c("6", "13", "16"), 3,
                                 ifelse(nd$id %in% c("1", "2", "5", "10", "12", "15", "18", "19", "21", "31"), 3, 1)),
                           style = "filled, solid",
                           
                           fontsize = ifelse(nd$id %in% c("1", "2", "5", "10", "6", "12", "13", "15", "16","18", "19", "21"), 18, #correct path
                                             ifelse(nd$id %in% c("3","4", "7", "8", "9", "11","14", "17", "20","22", "23", "24"), 12, 14)), #hydrations #endings
                           

                            color = ifelse(nd$id %in% c("25", "26", "27", "28", "29", "30"), "gray22",
                                          ifelse(nd$id %in% c("1", "2", "5", "10", "6", "12", "13", "15", "16", "18", "19", "21", "31"), "black", "gray37")),
                          
                            fixedsize = FALSE,
                           fontcolor = ifelse(nd$id %in% c("25", "26", "27", "28", "29", "30"), "black",
                                          ifelse(nd$id %in% c("1", "2", "5", "10", "6", "12", "13", "15", "16", "18", "19", "21", "31"), "black", "gray37")),
                           
                           fontname = ifelse(nd$id %in% c("1", "2", "5", "10", "6", "12", "13", "15", "16","18", "19", "21", "31"), "Helvetica-Bold", "Helvetica"),
                           rank = ifelse(nd$id %in% c("25", "26", "27", "28", "29", "30"), "1", 
                              ifelse(nd$id %in% c("1", "2", "5", "10", "6", "12", "13", "15", "16", "18", "19", "21", "31"), "2", "3"))
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

Total %>% render_graph(title = "MTX Scenario II_levels")




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

SOC.USERS.GRAPH %>% render_graph(title = "MTX Scenario II_SOC.USERS")


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

MTX.USERS.GRAPH %>% render_graph(title = "MTX Scenario II_MTX.USERS")



#export_graph(Total,file_name = "ScenII_MS_MTX.svg",file_type = "svg",title = NULL,width = NULL,height = NULL)















##################################
###########################################3
##################################

#Creating Tables

#EXPERT-ASSOCIATED PATHING   
#Scenario1  = Q1-Q3-Q4-Q5-Q6-Q7-End14
#Correct pathing means that the user "hit" the correct nodes; they may have also hit other nodes, but they touched the expert path
#This shows how many ppl are touching the expert path at any given time
#Formerly known as Correct Pathing

MTX_Correct <- MTX_data1
MTX_Correct[MTX_Correct == "no"] <- "0"
MTX_Correct[MTX_Correct == "yes"] <- "1"


x1 <- count(MTX_Correct %>% filter(Q1 == "1"))
x2 <- count(MTX_Correct %>% filter(Q1fluid == "1"))
x1.1 <- count(MTX_Correct %>% filter(Q3 == "3"))
x2.1 <- count(MTX_Correct %>% filter(Q3fluid == "0"))
x3 <- count(MTX_Correct %>% filter(Q4 == "4"))
x4 <- count(MTX_Correct %>% filter(Q4fluid == "0"))
x5 <- count(MTX_Correct %>% filter(Q5 == "5"))
x6 <- count(MTX_Correct %>% filter(Q5fluid == "0"))
x7 <- count(MTX_Correct %>% filter(Q6 == "6"))
x8 <- count(MTX_Correct %>% filter(Q6fluid == "0"))
x9 <- count(MTX_Correct %>% filter(Q7 == "7"))
x10 <- count(MTX_Correct %>% filter(Q7fluid == "0"))
x11 <- count(MTX_Correct %>% filter(End15 == "15"))

EA.table <-rbind(x1, x2, x1.1, x2.1, x3, x4, x5, x6, x7, x8, x9, x10, x11)
EA.table$ID <- c("START", "Q1fluid","Q3", "Q3fluid","Q4",  "Q4fluid","Q5", "Q5fluid", "Q6", 
                      "Q6fluid", "Q7", "Q7fluid", "FINAL")
EA.table <- EA.table[,c(2,1)]
EA.table$percent <- EA.table$n/users*100



#TOLERABLE
#This is showing the people who have not chosen incorrect endings by these times points
#Also known as tolerable decision making

a1 <- count(MTX_Correct %>% filter(Q1 == "1")) #24
a2 <- a1  #24fluid
a1.1 <- a1 - (data[6,5] + data[7,5] + data[8,5] + data[12,5] + data[13,5] + data[14,5] + data[20,5] + data[21,5] + data[22,5] + data[26,5] + data[27,5] + data[28,5]) #subtracting those that go to end before 36hr / Q3
a2.1 <- a1.1 #36hrfluid
a3 <- a1.1 - (data[34,5] + data[35,5] + data[36,5] + data[40,5] + data[41,5] + data[42,5] ) #subtracting those that go to end before 42hr / Q4
a4 <- a3 #42hr fluid
a5 <- a3 - (data[48,5] + data[49,5] + data[53,5] + data[54,5]) #subtracting those that go to end before 48hr
a6 <- a5 #48hr fluid
a7 <- a5 - (data[59,5] + data[60,5] + data[61,5] + data[62,5] + data[65,5] + data[66,5] + data[67,5] + data[68,5]) #subtracting those that go to end before 54hr
a8 <- a7 #54hr fluid
a9 <- a7 - (data[73,5] + data[74,5] + data[75,5] + data[76,5] + data[79,5] + data[80,5] + data[81,5] + data[82,5]) #subtracting those that go to end before 60hr
a10 <- a9
a11 <- a9 - (data[85,5] + data[86,5] + data[88,5] + data[89,5] + data[93,5] + data[94,5] + data[96,5] + data[97,5]) #subtracting those that go to end before Finish

tolerable.table <-rbind(a1, a2, a1.1, a2.1, a3, a4, a5, a6, a7, a8, a9, a10, a11)
tolerable.table$ID <- c("START", "24hr Fluid","36Hr", "36Hr Fluid", "42Hr", 
                        "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                        "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
tolerable.table <- tolerable.table[,c(2,1)]
tolerable.table$percent <- tolerable.table$n/users*100

#EXPERT PATHING   
#Expert pathing means that the user "hit" the best choices within the pathing
#also known as expert decision making

expert.id <- MTX_data2 %>% filter(!is.na(ID)) %>% filter(End15 == "15") %>% 
  filter(Q7 == "Discharge the patient home") %>% 
  filter(Q7fluid == "no") %>% 
  filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% 
  filter(Q6fluid == "no") %>% 
  filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)") %>% 
  filter(Q5fluid == "no") %>% 
  filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% 
  filter(Q4fluid == "no") %>% 
  filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% 
  filter(Q3fluid == "no") %>% 
  filter(is.na(Q2)) %>% 
  filter(!is.na(Q1)) %>% 
  filter(Q1fluid == "yes")

z1 <- count(MTX_data2 %>% filter(Q1 != "NA"))
z2 <- count(MTX_data2 %>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z3 <- count(MTX_data2 %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z4 <- count(MTX_data2 %>% filter(Q3fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z4.1 <- count(MTX_data2 %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)")  %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA")) 
z4.2 <- count(MTX_data2  %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA")) 
z5 <- count(MTX_data2 %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA")) 
z6 <- count(MTX_data2 %>% filter(Q5fluid == "no")%>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z7 <- count(MTX_data2 %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z8 <- count(MTX_data2 %>% filter(Q6fluid == "no") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z9 <- count(MTX_data2 %>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z10 <- count(MTX_data2 %>% filter(Q7fluid == "no")%>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))
z11 <- count(MTX_data2 %>% filter(End15 == "15")%>% filter(Q7 == "Discharge the patient home")%>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA"))

expert.table <-rbind(z1, z2, z3, z4, z4.1, z4.2, z5, z6, z7, z8, z9, z10, z11)
expert.table$ID <- c("START", "24hr Fluid","36Hr", "36Hr Fluid", "42Hr", 
                     "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                     "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
expert.table <- expert.table[,c(2,1)]
expert.table$percent <- expert.table$n/users*100

#INCORRECT

ic.1 <- users - a1
ic.2 <- users - a2
ic.3 <- users - a1.1
ic.4 <- users - a2.1
ic.5 <- users - a3
ic.6 <- users - a4
ic.7 <- users - a5
ic.8 <- users - a6
ic.9 <- users - a7
ic.10 <- users - a8
ic.11 <- users - a9
ic.12 <- users - a10
ic.13 <- users - a11

incorrect.table <-as.data.frame(rbind(ic.1, ic.2, ic.3, ic.4, ic.5, ic.6, ic.7, ic.8, ic.9, ic.10, ic.11, ic.12, ic.13))
incorrect.table$ID <- c("START", "24hr Fluid","36Hr", "36Hr Fluid", "42Hr", 
                         "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                         "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
incorrect.table <- incorrect.table[,c(2,1)]

incorrect.table$percent <- incorrect.table$n/users*100


#COMBINING CORRECT, PERFECT, AND EXPERT

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
x2.soc <- count(MTX_Correct %>% filter(Q1fluid == "1") %>% filter(group == "SOC"))
x1.1.soc <- count(MTX_Correct %>% filter(Q3 == "3") %>% filter(group == "SOC"))
x2.1.soc <- count(MTX_Correct %>% filter(Q3fluid == "0") %>% filter(group == "SOC"))
x3.soc <- count(MTX_Correct %>% filter(Q4 == "4") %>% filter(group == "SOC"))
x4.soc <- count(MTX_Correct %>% filter(Q4fluid == "0") %>% filter(group == "SOC"))
x5.soc <- count(MTX_Correct %>% filter(Q5 == "5") %>% filter(group == "SOC"))
x6.soc <- count(MTX_Correct %>% filter(Q5fluid == "0") %>% filter(group == "SOC"))
x7.soc <- count(MTX_Correct %>% filter(Q6 == "6") %>% filter(group == "SOC"))
x8.soc <- count(MTX_Correct %>% filter(Q6fluid == "0") %>% filter(group == "SOC"))
x9.soc <- count(MTX_Correct %>% filter(Q7 == "7") %>% filter(group == "SOC"))
x10.soc <- count(MTX_Correct %>% filter(Q7fluid == "0") %>% filter(group == "SOC"))
x11.soc <- count(MTX_Correct %>% filter(End15 == "15") %>% filter(group == "SOC"))

x1.mtx <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "MTX"))
x2.mtx <- count(MTX_Correct %>% filter(Q1fluid == "1") %>% filter(group == "MTX"))
x1.1.mtx <- count(MTX_Correct %>% filter(Q3 == "3") %>% filter(group == "MTX"))
x2.1.mtx <- count(MTX_Correct %>% filter(Q3fluid == "0") %>% filter(group == "MTX"))
x3.mtx <- count(MTX_Correct %>% filter(Q4 == "4") %>% filter(group == "MTX"))
x4.mtx <- count(MTX_Correct %>% filter(Q4fluid == "0") %>% filter(group == "MTX"))
x5.mtx <- count(MTX_Correct %>% filter(Q5 == "5") %>% filter(group == "MTX"))
x6.mtx <- count(MTX_Correct %>% filter(Q5fluid == "0") %>% filter(group == "MTX"))
x7.mtx <- count(MTX_Correct %>% filter(Q6 == "6") %>% filter(group == "MTX"))
x8.mtx <- count(MTX_Correct %>% filter(Q6fluid == "0") %>% filter(group == "MTX"))
x9.mtx <- count(MTX_Correct %>% filter(Q7 == "7") %>% filter(group == "MTX"))
x10.mtx <- count(MTX_Correct %>% filter(Q7fluid == "0") %>% filter(group == "MTX"))
x11.mtx <- count(MTX_Correct %>% filter(End15 == "15") %>% filter(group == "MTX"))



EA.tableMTXvsSOC <-rbind(x1.soc, x2.soc,  x1.1.soc, x2.1.soc, x3.soc, x4.soc, x5.soc, x6.soc, x7.soc, x8.soc, x9.soc, x10.soc, x11.soc)
EA.tableMTXvsSOC$ID <- c("START", "24hr Fluid", "36Hr", "36Hr fluid", "42Hr",
                              "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                              "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
EA.tableMTXvsSOC <- EA.tableMTXvsSOC[,c(2,1)]
EA.tableMTXvsSOC$percent.SOC <- EA.tableMTXvsSOC$n/SOCUsers*100

EA.tableMTXvsSOC$n.mtx <-as.numeric(c(x1.mtx, x2.mtx, x1.1.mtx, x2.1.mtx, x3.mtx, x4.mtx, x5.mtx, x6.mtx, x7.mtx, x8.mtx, x9.mtx, x10.mtx, x11.mtx))

EA.tableMTXvsSOC$percent.MTX <- (EA.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(EA.tableMTXvsSOC) <- c("Node", "ExpertAssociated.SOC.n", "ExpertAssociated.SOC.pct", "ExpertAssociated.MTX.n", "ExpertAssociated.MTX.pct")


#TOLERABLE
#This is showing the people who have not chosen incorrect endings by these times points
#Also known as tolerable decision making

a1.soc <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "SOC")) #24
a2.soc <- a1.soc  #24fluid
a1.1.soc  <- a1.soc  - (data[6,7] + data[7,7] + data[8,7] + data[12,7] + data[13,7] + data[14,7] + data[20,7] + data[21,7] + data[22,7] + data[26,7] + data[27,7] + data[28,7]) #subtracting those that go to end before 36hr / Q3
a2.1.soc  <- a1.1.soc  #36hrfluid
a3.soc  <- a1.1.soc  - (data[34,7] + data[35,7] + data[36,7] + data[40,7] + data[41,7] + data[42,7] ) #subtracting those that go to end before 42hr / Q4
a4.soc <- a3.soc  #42hr fluid
a5.soc  <- a3.soc  - (data[48,7] + data[49,7] + data[53,7] + data[54,7]) #subtracting those that go to end before 48hr
a6.soc  <- a5.soc #48hr fluid
a7.soc  <- a5.soc  - (data[59,7] + data[60,7] + data[61,7] + data[62,7] + data[65,7] + data[66,7] + data[67,7] + data[68,7]) #subtracting those that go to end before 54hr
a8.soc  <- a7.soc  #54hr fluid
a9.soc  <- a7.soc  - (data[73,7] + data[74,7] + data[75,7] + data[76,7] + data[79,7] + data[80,7] + data[81,7] + data[82,7]) #subtracting those that go to end before 60hr
a10.soc  <- a9.soc 
a11.soc  <- a9.soc  - (data[85,7] + data[86,7] + data[88,7] + data[89,7] + data[93,7] + data[94,7] + data[96,7] + data[97,7]) #subtracting those that go to end before Finish

a1.mtx <- count(MTX_Correct %>% filter(Q1 == "1") %>% filter(group == "MTX")) #24
a2.mtx <- a1.mtx  #24fluid
a1.1.mtx  <- a1.mtx  - (data[6,9] + data[7,9] + data[8,9] + data[12,9] + data[13,9] + data[14,9] + data[20,9] + data[21,9] + data[22,9] + data[26,9] + data[27,9] + data[28,9]) #subtracting those that go to end before 36hr / Q3
a2.1.mtx  <- a1.1.mtx  #36hrfluid
a3.mtx  <- a1.1.mtx  - (data[34,9] + data[35,9] + data[36,9] + data[40,9] + data[41,9] + data[42,9] ) #subtracting those that go to end before 42hr / Q4
a4.mtx <- a3.mtx  #42hr fluid
a5.mtx  <- a3.mtx  - (data[48,9] + data[49,9] + data[53,9] + data[54,9]) #subtracting those that go to end before 48hr
a6.mtx  <- a5.mtx #48hr fluid
a7.mtx  <- a5.mtx  - (data[59,9] + data[60,9] + data[61,9] + data[62,9] + data[65,9] + data[66,9] + data[67,9] + data[68,9]) #subtracting those that go to end before 54hr
a8.mtx  <- a7.mtx  #54hr fluid
a9.mtx  <- a7.mtx  - (data[73,9] + data[74,9] + data[75,9] + data[76,9] + data[79,9] + data[80,9] + data[81,9] + data[82,9]) #subtracting those that go to end before 60hr
a10.mtx  <- a9.mtx 
a11.mtx  <- a9.mtx  - (data[85,9] + data[86,9] + data[88,9] + data[89,9] + data[93,9] + data[94,9] + data[96,9] + data[97,9]) #subtracting those that go to end before Finish


tolerable.tableMTXvsSOC <-rbind(a1.soc , a2.soc , a1.1.soc , a2.1.soc , a3.soc , a4.soc , a5.soc , a6.soc , a7.soc , a8.soc , a9.soc , a10.soc , a11.soc )
tolerable.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36Hr", "36Hr Fluid", "42Hr", 
                        "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                        "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
tolerable.tableMTXvsSOC <- tolerable.tableMTXvsSOC[,c(2,1)]
tolerable.tableMTXvsSOC$percent <- tolerable.tableMTXvsSOC$n/SOCUsers*100

tolerable.tableMTXvsSOC$n.mtx <-as.numeric(c(a1.mtx , a2.mtx , a1.1.mtx , a2.1.mtx , a3.mtx , a4.mtx , a5.mtx , a6.mtx , a7.mtx , a8.mtx , a9.mtx , a10.mtx , a11.mtx))
tolerable.tableMTXvsSOC$percent.MTX <- (tolerable.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(tolerable.tableMTXvsSOC) <- c("Node", "Tolerable.SOC.n", "Tolerable.SOC.pct", "Tolerable.MTX.n", "Tolerable.MTX.pct")






#EXPERT PATHING   
#Expert pathing means that the user "hit" the best choices within the pathing
#also known as expert decision making
MTX_data2$group <- NA
MTX_data2$group[1:27] <- "SOC"
MTX_data2$group[28:54] <- "MTX"

z1.soc <- count(MTX_data2 %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z2.soc <- count(MTX_data2 %>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z3.soc <- count(MTX_data2 %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z4.soc <- count(MTX_data2 %>% filter(Q3fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z4.1.soc <- count(MTX_data2 %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)")  %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") ) 
z4.2.soc <- count(MTX_data2  %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") ) 
z5.soc <- count(MTX_data2 %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") ) 
z6.soc <- count(MTX_data2 %>% filter(Q5fluid == "no")%>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z7.soc <- count(MTX_data2 %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z8.soc <- count(MTX_data2 %>% filter(Q6fluid == "no") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z9.soc <- count(MTX_data2 %>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z10.soc <- count(MTX_data2 %>% filter(Q7fluid == "no")%>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )
z11.soc <- count(MTX_data2 %>% filter(End15 == "15")%>% filter(Q7 == "Discharge the patient home")%>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "SOC") )

z1.mtx <- count(MTX_data2 %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z2.mtx <- count(MTX_data2 %>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z3.mtx <- count(MTX_data2 %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z4.mtx <- count(MTX_data2 %>% filter(Q3fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z4.1.mtx <- count(MTX_data2 %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)")  %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") ) 
z4.2.mtx <- count(MTX_data2  %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") ) 
z5.mtx <- count(MTX_data2 %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") ) 
z6.mtx <- count(MTX_data2 %>% filter(Q5fluid == "no")%>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z7.mtx <- count(MTX_data2 %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z8.mtx <- count(MTX_data2 %>% filter(Q6fluid == "no") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z9.mtx <- count(MTX_data2 %>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z10.mtx <- count(MTX_data2 %>% filter(Q7fluid == "no")%>% filter(Q7 == "Discharge the patient home") %>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no") %>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no")%>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )
z11.mtx <- count(MTX_data2 %>% filter(End15 == "15")%>% filter(Q7 == "Discharge the patient home")%>% filter(Q6 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 60)") %>% filter(Q6fluid == "no") %>% filter(Q5 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 54)")%>% filter(Q5fluid == "no") %>% filter(Q4 == "Give leucovorin at 15mg/m2/dose every 6 hours, obtain next level in 6 hours (Hour 48)") %>% filter(Q4fluid == "no")%>% filter(Q3 == "Obtain next level in 6 hours (Hour 42)") %>% filter(Q3fluid == "no") %>% filter(is.na(Q2))%>% filter(Q1fluid == "yes") %>% filter(Q1 != "NA") %>% filter(group == "MTX") )



expert.tableMTXvsSOC <-rbind(z1.soc, z2.soc, z3.soc, z4.soc, z4.1.soc, z4.2.soc, z5.soc, z6.soc, z7.soc, z8.soc, z9.soc, z10.soc, z11.soc)
expert.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36Hr", "36Hr Fluid", "42Hr", 
                     "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                     "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
expert.tableMTXvsSOC <- expert.tableMTXvsSOC[,c(2,1)]
expert.tableMTXvsSOC$percent <- expert.tableMTXvsSOC$n/SOCUsers*100

expert.tableMTXvsSOC$n.mtx <-as.numeric(c(z1.mtx, z2.mtx, z3.mtx, z4.mtx, z4.1.mtx, z4.2.mtx, z5.mtx, z6.mtx, z7.mtx, z8.mtx, z9.mtx, z10.mtx, z11.mtx))
expert.tableMTXvsSOC$percent.MTX <- (expert.tableMTXvsSOC$n.mtx)/MTXUsers*100

colnames(expert.tableMTXvsSOC) <- c("Node", "Expert.SOC.n", "Expert.SOC.pct", "Expert.MTX.n", "Expert.MTX.pct")



###Incorrect
# Number of users who are not on tolerable path
# Subract #of Tolerable from #of Total users

incorrect.tableMTXvsSOC <- NULL
incorrect.tableMTXvsSOC$ID <- c("START", "24hr Fluid","36Hr", "36Hr Fluid", "42Hr", 
                                "42Hr fluid","48Hr", "48Hr Fluid", "54Hr", 
                                "54Hr Fluid", "60Hr", "60Hr Fluid", "FINAL")
incorrect.tableMTXvsSOC$n.soc <- SOCUsers - tolerable.tableMTXvsSOC$Tolerable.SOC.n
incorrect.tableMTXvsSOC <- as.data.frame(incorrect.tableMTXvsSOC)
incorrect.tableMTXvsSOC$percent.soc <- incorrect.tableMTXvsSOC$n.soc/SOCUsers*100
incorrect.tableMTXvsSOC$n.mtx <- MTXUsers - tolerable.tableMTXvsSOC$Tolerable.MTX.n
incorrect.tableMTXvsSOC$percent.mtx <- incorrect.tableMTXvsSOC$n.mtx/MTXUsers*100
colnames(incorrect.tableMTXvsSOC) <- c("Node", "Incorrect.SOC.n", "Incorrect.SOC.pct", "Incorrect.MTX.n", "Incorrect.MTX.pct")




#Table.Path.comparisons
Table.comparisonsMTXvsSOC <- cbind(tolerable.tableMTXvsSOC[,c(1,3,5)], EA.tableMTXvsSOC[,c(3,5)], expert.tableMTXvsSOC[,c(3,5)], incorrect.tableMTXvsSOC[,c(3,5)])

View(Table.comparisonsMTXvsSOC)







#END EVALAUTION
Endings <- as.data.frame(matrix(NA, 
                                nrow = 7,
                                ncol = 3))
names(Endings) <- c("End", "Freq", "Percent")
Endings$End <- c("end1", "end2", "end3", "end4",
                 "end5", "end6", "FINISH")
Endings$Freq <- c(sum(MTX_Correct$End9 == 9, na.rm = TRUE),
                  sum(MTX_Correct$End10 == 10, na.rm = TRUE),
                  sum(MTX_Correct$End11 == 11, na.rm = TRUE),
                  sum(MTX_Correct$End12 == 12, na.rm = TRUE),
                  sum(MTX_Correct$End13 == 13, na.rm = TRUE),
                  sum(MTX_Correct$End14 == 14, na.rm = TRUE),
                  sum(MTX_Correct$End15 == 15, na.rm = TRUE))
Endings$Percent <- Endings$Freq/users*100
Endings$Meaning <- c("Waited too long for levels.",
                      "Leucovorin given too early.",
                      "Glucarpidase was not indicated in this situation.",
                      "Discharged inappropriately.",
                      "Leucovorin levels are too high and needed to be monitored more closely.",
                      "Patient should have been discharged.", 
                      "Congratulations! You successfully treated this patient.")
Endings$End <- c("end1", "end2", "end3", "end4","end5", "end6", "FINISH")




#NodesTouched  
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
  MTX_nodes$Q5fluid[MTX_nodes$Q6fluid == "yes"] <- "Q6y"
  MTX_nodes$Q7fluid[MTX_nodes$Q7fluid == "no"] <- "Q7n"
  MTX_nodes$Q7fluid[MTX_nodes$Q7fluid == "yes"] <- "Q7y"
  MTX_nodes$Q8fluid[MTX_nodes$Q8fluid == "no"] <- "Q8n"
  MTX_nodes$Q8fluid[MTX_nodes$Q8fluid == "yes"] <- "Q8y"

nodes.touched<-nrow(table(unlist(MTX_nodes[,2:24])))
nodes.touched.id<-as.data.frame(table(unlist(MTX_nodes[,2:24])))

MTX_nodes$touched <- NA
MTX_nodes$touched <- apply(MTX_nodes[,2:24], 1, function(x)length(unique(na.omit(x))))


MTX_nodes$group <- NA
MTX_nodes$group[1:27] <- "SOC"
MTX_nodes$group[28:54] <- "MTX"

ave.nodes.SOC <- mean(MTX_nodes$touched[1:27] )
ave.nodes.MTX <- mean(MTX_nodes$touched[28:54] )
ave.nodes.Total <- mean(MTX_nodes$touched[1:54] )
expert.nodes <- 13
total.nodes <- 31
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

#write.csv(MTX_nodes, "MTX_nodes_Scenario3.csv")




#######################
#######################
#Pathway frequency
MTX_nodes.1 <- MTX_nodes[,c(2:24)]  
#SOC ONLY
MTX_nodes.1.soc <- MTX_nodes[c(1:27),c(2:24)]  
#MTX ONLY
MTX_nodes.1.mtx <- MTX_nodes[c(28:54),c(2:24)]  

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
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "15", "FINAL")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "14", "End 6")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "13", "End 5")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "12", "End 4")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "11", "End 3")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "10", "End 2")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "9", "End 1")

pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "2, ", "30Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "3, ", "36Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "4, ", "42Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "5, ", "48Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "6, ", "54Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "7, ", "60Hr, ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "8, ", "66Hr, ")

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
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q8n, ", "No (66), ")
pathways_merge$Pathway <-str_replace_all(pathways_merge$Pathway, "Q8y, ", "Yes (66), ")

pathways_merge[is.na(pathways_merge)] <- 0


#define best path
pathways_merge$Pathway[pathways_merge$Pathway == "START, Yes (24), 36Hr, No (36), 42Hr, No (42), 48Hr, No (48), 54Hr, No (54), 60Hr, No (60), FINAL"] <- "** START, Yes (24), 36Hr, No (36), 42Hr, No (42), 48Hr, No (48), 54Hr, No (54), 60Hr, No (60), FINAL"

View(pathways_merge)
#write.csv(pathways_merge, "pathways_merge_Scenario2.csv")




end_time <- Sys.time()
run_time <- end_time - start_time
