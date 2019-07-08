install.packages("ggplot2")
library(ggplot2)



#ANALYSIS!
CEDA=read.csv("Data/CEDAclean.csv", na.strings=c("","NA"))
# str(CEDA)

# Examining columns X & X.1

# Note columns X.1 and X are duplicates:
boolean_test <- c(CEDA$X.1 == CEDA$X)
str(boolean_test)


# Oops, the columns are duplicates! Remove column X.1 (Updated clean data doc version.)
CEDA$X.1 <- NULL


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# OCCUPATION GROUPS TO BE STUDIED 

# OCCUPATIONS: Occupations_Sum is a pre-processed list of respondents occupations, broadly grouped by sector and by their level of interaction with other people who they may provide services to in a professional capacity:


#Summary:

# Boolean_occ_alliedhealth: Health and allied health services for this analysis include:
# Note: Technicians in laboratories are excluded, because in survey respondent data it was not always possible to separate non-medical lab technicians from medical lab technicians based on open-ended occupation responses.
# Medical, Nurse practitioner, Nurse, Occupational therapist, Psychologist, Psychiatrist, Psychotherapist, Physical therapist, General practitioner (family doctor), Paramedic, Social worker, Nutritionist or dietitian, 

# Boolean_occ_health: Interaction with health service users: 
# Medical, Nurse practitioner, Nurse, General practitioner (family doctor)

# Boolean_occ_support: Counselling, therapy, guidance, support, and social service work, social worker
# Counselling (other), Occupational therapist, psychologist, social service worker, Psychiatrist, Psychotherapist, Social worker, Therapy (other)

# Boolean_occ_firstresp: First responders include:
# First responder (other), Firefighter, Paramedic, Police officer, Police (other)

# Boolean_occ_edu: Educator responders include:
# College/University course instructor/professor, School teacher, Education (other), School guidance counsellor

# Boolean_occ_fitness: Fitness professional include:
# Athletic team coach, Sports or fitness instructor, Personal fitness trainer

# Boolean_occu_other: Work environments not related to health, fitness, education.
# Admin, HR, Service/Retail/Sales, Management


# Analysis set-up:
# Boolean occupational category columns for occupation groups:

# Boolean_occ_alliedhealth
# Boolean_occ_health
# Boolean_occ_support
# Boolean_occ_firstresp
# Boolean_occ_edu
# Boolean_occ_fitness
# Boolean_occu_other

# Boolean_occ_alliedhealth: Health and allied health services for this analysis include:
# Note: Technicians in laboratories are excluded, because in survey respondent data it was not always possible to separate non-medical lab technicians from medical lab technicians based on open-ended occupation responses.
# Medical, Nurse practitioner, Nurse, Occupational therapist, Psychologist, Psychiatrist, Psychotherapist, Physical therapist, General practitioner (family doctor), Paramedic, Social worker, Nutritionist or dietitian, 


Boolean_occ_alliedhealth <- c(CEDA$Occupations_Sum == "Medical"|
                                CEDA$Occupations_Sum == "Nurse practitioner"|
                                CEDA$Occupations_Sum == "Nurse"|
                                CEDA$Occupations_Sum == "Occupational therapist"|
                                CEDA$Occupations_Sum == "Psychologist"|
                                CEDA$Occupations_Sum == "Psychiatrist"|
                                CEDA$Occupations_Sum == "Psychotherapist"|
                                CEDA$Occupations_Sum == "Physical therapist"|
                                CEDA$Occupations_Sum == "General practitioner (family doctor)"|
                                CEDA$Occupations_Sum == "Paramedic"|
                                CEDA$Occupations_Sum == "Social worker"|
                                CEDA$Occupations_Sum == "Nutritionist or dietitian"
)

# Boolean_occ_health: Interaction with health service users: 
# Medical, Nurse practitioner, Nurse, General practitioner (family doctor)


Boolean_occ_health <- c(CEDA$Occupations_Sum == "Medical"|
                          CEDA$Occupations_Sum == "Nurse practitioner"|
                          CEDA$Occupations_Sum == "Nurse"|
                          CEDA$Occupations_Sum == "General practitioner (family doctor)"
)

# Boolean_occ_support: Counselling, therapy, guidance, support, and social service work, social worker
# Counselling (other), School guidance counsellor, Occupational therapist, Psychologist, Social service worker, Psychiatrist, Psychotherapist, Social worker, Therapy (other)

Boolean_occ_support <- c(CEDA$Occupations_Sum == "Counselling (other)"|
                           CEDA$Occupations_Sum == "School guidance counsellor"|
                           CEDA$Occupations_Sum == "Occupational therapist"|
                           CEDA$Occupations_Sum == "Psychologist"|
                           CEDA$Occupations_Sum == "Social service worker"|
                           CEDA$Occupations_Sum == "Psychiatrist"|
                           CEDA$Occupations_Sum == "Psychotherapist"|
                           CEDA$Occupations_Sum == "Social worker"|
                           CEDA$Occupations_Sum == "Therapy (other)"
) 

# Boolean_occ_firstresp: First responders include:
# First responder (other), Firefighter, Paramedic, Police officer, Police (other)

Boolean_occ_firstresp <- c(CEDA$Occupations_Sum == "First responder (other)"|
                             CEDA$Occupations_Sum == "Firefighter"|
                             CEDA$Occupations_Sum == "Paramedic"|
                             CEDA$Occupations_Sum == "Police officer"|
                             CEDA$Occupations_Sum == "Police (other)"
) 

# Boolean_occ_edu: Educator responders include:
# College/University course instructor/professor, School teacher, Education (other), School guidance counsellor

Boolean_occ_edu <- c(CEDA$Occupations_Sum == "College/University course instructor/professor"|
                       CEDA$Occupations_Sum == "School teacher"|
                       CEDA$Occupations_Sum == "Education (other)"|
                       CEDA$Occupations_Sum == "School guidance counsellor"
) 

# Boolean_occ_fitness: Fitness professional include:
# Athletic team coach, Sports or fitness instructor, Personal fitness trainer

Boolean_occ_fitness <- c(CEDA$Occupations_Sum == "Athletic team coach"|
                           CEDA$Occupations_Sum == "Sports or fitness instructor"|
                           CEDA$Occupations_Sum == "Personal fitness trainer"
) 

# Boolean_occu_other: Work environments not related to health, fitness, education.
# Admin, HR, Service/Retail/Sales, Management

Boolean_occ_other <- c(CEDA$Occupations_Sum == "Admin"|
                         CEDA$Occupations_Sum == "HR"|
                         CEDA$Occupations_Sum == "Service/Retail/Sales"|
                         CEDA$Occupations_Sum == "Management"
) 

####################
# Checking the size of each group:

summary(Boolean_occ_alliedhealth)
summary(Boolean_occ_health)
summary(Boolean_occ_support)
summary(Boolean_occ_firstresp)
summary(Boolean_occ_edu)
summary(Boolean_occ_fitness)
summary(Boolean_occ_other)




# Subsetting groups for ease of use:

Group_alliedhealth <- subset(CEDA,Boolean_occ_alliedhealth==TRUE)
Group_health <- subset(CEDA,Boolean_occ_health==TRUE)  
Group_support <- subset(CEDA,Boolean_occ_support==TRUE) 
Group_firstresp <- subset(CEDA,Boolean_occ_firstresp==TRUE)   
Group_edu <- subset(CEDA,Boolean_occ_edu==TRUE)
Group_fitness <- subset(CEDA,Boolean_occ_fitness==TRUE)  
Group_other <- subset(CEDA,Boolean_occ_other==TRUE)   


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Creating grade score columns based on responses to questions about ED literacy & stigma

# Steps: Complete Literacy Grades columns per literacy question, then 2. add them.


##########
# Create graph of responses on most frequent ED on literacy quiz
# CEDA=read.csv("CEDAclean.csv", na.strings=c("","NA"))



CEDA$LitGrade

str(CEDA$LitGrade)

# This is is so we can make the graph using different subsets of the data in the future 
# Replace subgroup_name with the name of the subgroup to examine. Else, uncomment LitPlaceholder <- CEDA$LitGrade.

# LitPlaceholder <- subgroup_name$LitGrade
# LitPlaceholder <- CEDA$LitGrade


Literacy_Grade_num <-
  ifelse(LitPlaceholder ==100,Literacy_Grade_num <-"High",
         ifelse(LitPlaceholder ==50,Literacy_Grade_num <-"Medium", 
                ifelse(LitPlaceholder ==0,Literacy_Grade_num <-"Low",NA
                )))

Literacy_Grade <- na.omit(Literacy_Grade_num)

str(Literacy_Grade)
table(Literacy_Grade)

x<- Literacy_Grade

#x <-revalue(yna, c("Strongly disagree"= "SD", "Somewhat disagree"= "SWD", "Neither disagree nor agree"="N", "Somewhat agree"= "SWA", "Strongly agree"= "SA"))
#levels(x)
ggplot(data.frame(x), aes(x=x, fill=x, na.rm= TRUE)) + geom_bar(na.rm = TRUE) 
q <- ggplot(data.frame(x), aes(x=x, fill=Literacy_Grade, na.rm= TRUE)) + geom_bar(na.rm = TRUE) 
q + labs(x = "Score", y = "Count", title = "Scores in question on most common eating disorder", fill = "Score")







# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # LITERACY SCORE FUNCTION  # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

LiteracyScore <- function(CEDA_subgroup) {
  
  ## NOTE: If working with full data, use:
  # CEDA_subgroup <- CEDA
  
  # 1. Creating literacy grade columns
  
  
  # Lit_EDChoice 
  
  levels(CEDA_subgroup$Lit_EDChoice)
  LG_EDChoice <-          # Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDChoice == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDChoice == "Somewhat disagree",1,0))
  
  # Lit_EDIllness
  
  levels(CEDA_subgroup$Lit_EDIllness)
  LG_EDIllness <-          # Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDIllness == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDIllness == "Somewhat agree",1,0))
  
  # Lit_EDImmigrant n/a
  
  # Lit_EDPOC n/a
  
  # Lit_EDGenetic 
  
  levels(CEDA_subgroup$Lit_EDGenetic)
  LG_EDGenetic <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDGenetic == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDGenetic == "Somewhat agree",1,0))
  
  # Lit_EDLGBTQ n/a
  
  # Lit_EDThreatening 
  
  levels(CEDA_subgroup$Lit_EDThreatening)
  LG_EDThreatening <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDThreatening == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDThreatening == "Somewhat agree",1,0))
  
  # Lit_EDMen n/a 
  
  # Lit_EDTreatment	n/a
  
  #Lit_EDWeight	A, SA
  
  levels(CEDA_subgroup$Lit_EDWeight)
  LG_EDWeight <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDWeight == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDWeight == "Somewhat agree",1,0))
  
  # Lit_EDRecover	n/a
  
  # Lit_EDDiet	SD, D
  
  levels(CEDA_subgroup$Lit_EDDiet)
  LG_EDDiet <- #Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDDiet == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDDiet == "Somewhat disagree",1,0))
  
  # 2. Adding literacy grade colums and CEDA_subgroup$LitGrade
  
  LiteracyScore <- CEDA_subgroup$LitGrade/50 + LG_EDChoice + LG_EDDiet + LG_EDGenetic + LG_EDIllness + LG_EDThreatening + LG_EDWeight
  
  LiteracyScoreGr <- LiteracyScore*100/8 # Score as grade out of 100 points
  
  LiteracyScoreGr # Returns LiteracyScoreGR
  
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # STIGMA SCORE FUNCTION # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1. Creating stigma grade columns

# Lit_EDChoice	A, SA

StigmaScore <- function(CEDA_subgroup) {
  
  # levels(CEDA_subgroup$Lit_EDChoice)
  StigS_EDChoice <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDChoice == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDChoice == "Somewhat disagree",1,0
           ))
  
  # Lit_EDIllness	SD, D, N
  
  # levels(CEDA_subgroup$Lit_EDIllness)
  
  StigS_EDIllness <- #Answers accepted SD, D, N
    ifelse(CEDA_subgroup$Lit_EDIllness == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDIllness == "Somewhat disagree",1,
                  ifelse(CEDA_subgroup$Lit_EDIllness == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDImmigrant 	SA, A
  
  # levels(CEDA_subgroup$Lit_EDImmigrant)
  
  StigS_EDImmigrant <- #Answers accepted SA, A
    ifelse(CEDA_subgroup$Lit_EDImmigrant == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDImmigrant == "Somewhat agree",1,0
           ))
  
  # Lit_EDPOC 	A, SA
  
  # levels(CEDA_subgroup$Lit_EDPOC )
  
  StigS_EDPOC <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDPOC  == "Strongly sagree", 1,
           ifelse(CEDA_subgroup$Lit_EDPOC  == "Somewhat agree",1,0
           ))
  
  # Lit_EDGenetic 	n/a
  
  # Lit_EDLGBTQ 	SD, D
  
  # levels(CEDA_subgroup$Lit_EDLGBTQ)
  
  StigS_EDLGBTQ <- #Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDLGBTQ == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDLGBTQ == "Somewhat disagree",1,0))
  
  # Lit_EDThreatening	SD, D, N
  
  # levels(CEDA_subgroup$Lit_EDThreatening)
  
  StigS_EDThreatening <- #Answers accepted SD, D, N
    ifelse(CEDA_subgroup$Lit_EDThreatening == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDThreatening == "Somewhat disagree",1,
                  ifelse(CEDA_subgroup$Lit_EDThreatening == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDMen 	SD, D
  
  # levels(CEDA_subgroup$Lit_EDMen)
  
  StigS_EDMen <- #Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDMen == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDMen == "Somewhat disagree",1,0))
  
  # Lit_EDTreatment	A, SA
  
  # levels(CEDA_subgroup$Lit_EDTreatment)
  
  StigS_EDTreatment <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDTreatment == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDTreatment == "Somewhat agree",1,0
           ))
  
  # Lit_EDWeight	SD, D ,N
  
  # levels(CEDA_subgroup$EDWeight)
  
  StigS_EDWeight <- #Answers accepted SD, D, N
    ifelse(CEDA_subgroup$Lit_EDWeight == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDWeight == "Somewhat disagree",1,
                  ifelse(CEDA_subgroup$Lit_EDWeight == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDRecover k)	N, A, SA
  
  # levels(CEDA_subgroup$Lit_EDRecover)
  
  StigS_EDRecover <- #Answers accepted N, A, SA
    ifelse(CEDA_subgroup$Lit_EDRecover == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDRecover == "Somewhat agree",1,
                  ifelse(CEDA_subgroup$Lit_EDRecover == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDDiet	l)	N, A, SA
  
  
  
  # levels(CEDA_subgroup$Lit_EDDiet)
  
  StigS_EDDiet <- # Answers accepted N, A, SA
    ifelse(CEDA_subgroup$Lit_EDDiet == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDDiet == "Somewhat agree",1,
                  ifelse(CEDA_subgroup$Lit_EDDiet == "Neither disagree nor agree",1,0
                  )))
  
  # Step 2 Adding all stigma grade colums together
  
  StigmaScore <- StigS_EDDiet + StigS_EDRecover + StigS_EDWeight + StigS_EDMen + StigS_EDLGBTQ + StigS_EDPOC + StigS_EDImmigrant + StigS_EDIllness + StigS_EDChoice + StigS_EDThreatening + StigS_EDTreatment
  
  StigmaScoreGr <- StigmaScore*100/9 # Score as grade out of 100 points
  
  StigmaScoreGr # Returns StigmaScoreGR
}




summary(StigmaScoreGr)
summary(LiteracyScoreGr)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # EVALUATING STIGMA & LITERACY SCORES FOR SUBGROUPS # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# Occupation subgroups are: 
# Group_alliedhealth
# Group_health
# Group_support
# Group_firstresp   
# Group_edu
# Group_fitness 
# Group_other 

# Group_alliedhealth

StigmaScore_AlliedHealth <- StigmaScore(Group_alliedhealth)
LiteracyScore_AlliedHealth <- LiteracyScore(Group_alliedhealth)
summary(StigmaScore_AlliedHealth)
mean(StigmaScore_AlliedHealth)
summary(LiteracyScore_AlliedHealth)
mean(LiteracyScore_AlliedHealth)

# Group_health

StigmaScore_Health <- StigmaScore(Group_health)
LiteracyScore_Health <- LiteracyScore(Group_health)
summary(StigmaScore_Health)
mean(StigmaScore_Health)
summary(LiteracyScore_Health)
mean(LiteracyScore_Health)

# Group_support

StigmaScore_Support <- StigmaScore(Group_support)
LiteracyScore_Support <- LiteracyScore(Group_support)
summary(StigmaScore_Support)
mean(StigmaScore_Support)
summary(LiteracyScore_Support)
mean(LiteracyScore_Support)

# Group_firstresp   

StigmaScore_Firstresp <- StigmaScore(Group_firstresp)
LiteracyScore_Firstresp <- LiteracyScore(Group_firstresp)
summary(StigmaScore_Firstresp)
mean(StigmaScore_Firstresp)
summary(LiteracyScore_Firstresp)
mean(LiteracyScore_Firstresp)

# Group_edu

StigmaScore_Edu <- StigmaScore(Group_edu)
LiteracyScore_Edu <- LiteracyScore(Group_edu)
summary(StigmaScore_Edu)
mean(StigmaScore_Edu)
summary(LiteracyScore_Edu)
mean(LiteracyScore_Edu)

# Group_fitness 

StigmaScore_Fitness <- StigmaScore(Group_fitness)
LiteracyScore_Fitness <- LiteracyScore(Group_fitness)
summary(StigmaScore_Fitness)
mean(StigmaScore_Fitness)
summary(LiteracyScore_Fitness)
mean(LiteracyScore_Fitness)

# Group_other 

StigmaScore_Other <- StigmaScore(Group_other)
LiteracyScore_Other <- LiteracyScore(Group_other)
summary(StigmaScore_Other)
mean(StigmaScore_Other)
summary(LiteracyScore_Other)
mean(LiteracyScore_Other)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Literacy Scores:

# LiteracyScore_AlliedHealth
# LiteracyScore_Health
# LiteracyScore_Support
# LiteracyScore_Firstresp   
# LiteracyScore_Edu
# LiteracyScore_Fitness 
# LiteracyScore_Other 

# Stigma Scores:

# StigmaScore_AlliedHealth
# StigmaScore_Health
# StigmaScore_Support
# StigmaScore_Firstresp   
# StigmaScore_Edu
# StigmaScore_Fitness 
# Stigmacore_Other 
  
#IN PROGRESS
