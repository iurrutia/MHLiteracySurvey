#### OLD VERSION, OUTATED - Please see CEDAnalysis2019.R for live version ####



install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

library(plyr)

#ANALYSIS!
CEDA=read.csv("Data/CEDAClean.csv", na.strings=c("","NA"))
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
# # # RUN A FUNCTION OVER ALL SUBGROUPS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# For functions that are passed full subgroups:

all_subgroups_na <- list(Group_alliedhealth, Group_health, Group_support, Group_firstresp, Group_edu, Group_fitness, Group_other)
all_subgroups <- na.omit(all_subgroups_na)
str(all_subgroups)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# For funcitons that are passed a column of each subgroup:

# lapply(all_subgroups,select_,"columnname")

# or

# all_subgroups_columnname <- list (
#   Group_alliedhealth$columnname, 
#   Group_health$columnname, 
#   Group_support$columnname, 
#   Group_firstresp$columnname,
#   Group_edu$columnname,
#   Group_fitness$columnname,
#   Group_other$columnname)


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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

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
  
<<<<<<< HEAD
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # ANALYSIS: ANOVA for subgroups  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

StigmaScore_All <- StigmaScore(CEDA)
LiteracyScore_All <- LiteracyScore(CEDA)











































mu0_LS <- mean(LiteracyScore_All,  na.rm=TRUE)
mu0_SS <- mean(StigmaScore_All, na.rm=TRUE)


# Notes from t tests (1 sample, distributions for stigma and literacy scores look close to normal)
# t.test(dataset$sample1, mu=mu0)

LiteracyScore_subgroups <- lapply(all_subgroups, LiteracyScore) # Obtains Literacy Score of all subgroups
StigmaScore_subgroups <-lapply(all_subgroups, StigmaScore) # Obtains Stigma Score of all subgroups

# Alpha = 0.05

ttestoneLiteracyScore_subgroups <- lapply(LiteracyScore_subgroups, t.test, mu=mu0_LS) # t-test of Literacy Score of all subgroups
ttestoneStigmaScore_subgroups <-lapply(StigmaScore_subgroups, t.test, mu=mu0_SS) # t-test of Stigma Score of all subgroups

# Alpha = 0.01

ttest2LiteracyScore_subgroups <- lapply(LiteracyScore_subgroups, t.test, conf.level=0.99, mu=mu0_LS) # t-test of Literacy Score of all subgroups
ttest2StigmaScore_subgroups <-lapply(StigmaScore_subgroups, t.test, conf.level=0.99, mu=mu0_SS) # t-test of Stigma Score of all subgroups


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Question 4
# Question 4 responses are recorded in: 
# CEDA$Identify_Recognize
# CEDA$Identify_Support
# CEDA$Identify_Resources

factrecognize <- factor(CEDA$Identify_Recognize, levels = c("Strongly disagree", "Disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"))
factrecognize
y <- revalue(factrecognize, c("Disagree"= "Somewhat disagree"))
summary(y)
yna <- na.omit(y)
yna
summary(yna)
x <- revalue(yna, c("Strongly disagree"= "SD", "Somewhat disagree"= "SWD", "Neither disagree nor agree"="N", "Somewhat agree"= "SWA", "Strongly agree"= "SA"))
levels(x)
ggplot(data.frame(x), aes(x=x, fill=x, na.rm= TRUE)) + geom_bar(na.rm = TRUE) 
q <- ggplot(data.frame(x), aes(x=x, fill=yna, na.rm= TRUE)) + geom_bar(na.rm = TRUE) 
q + labs(x = "Response", y = "Count", title = "I believe I can recognize the signs of an eating disorder in others", fill = "Response")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # CATEGORICAL TO NUMERICAL # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Input needs to be a column with responses: 
# strongly disagree, somewhat disagree, neither disagree or agree, somewhat agree, strongly agree 

# This question should be measured against every occupation group. Task: Function by subgroup.
# Synthesizing data into categories: SA, A, N, D, SD are changed to 0, 2.5, 5, 7.5, 10

ConvertCategoriesNum <- function(CEDA_subgroup_col){
  
  Category_to_number_na <- 
    ifelse(CEDA_subgroup_col == "Strongly agree", 10,
           ifelse(CEDA_subgroup_col == "Somewhat agree", 7.5,
                  ifelse(CEDA_subgroup_col == "Neither disagree or agree", 5,
                         ifelse(CEDA_subgroup_col == "Somewhat disagree", 2.5,
                                ifelse(CEDA_subgroup_col == "Disagree", 2.5,
                                       ifelse(CEDA_subgroup_col == "Strongly disagree", 0, NA
                                       ))))))
  
  Category_to_number <- na.omit(Category_to_number_na)
  return(Category_to_number)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Q4 Preparedness: Recognize, support, resources # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Setup information:



# Occupation subgroups are: 
# Group_alliedhealth
# Group_health
# Group_support
# Group_firstresp   
# Group_edu
# Group_fitness 
# Group_other 

# Variables of interest:

# "I believe I can recognize the signs of an eating disorder in others"
# Identify_Recognize

Q4_Recognize <- lapply(all_subgroups,select_,"Identify_Recognize") # Selects column correstponding to Identify_Recognize from each subgroup
Q4_Recognize_num <- lapply(Q4_Recognize,ConvertCategoriesNum) # Converts Identify_Recognize responses to numerical data for each subgroup

mu_Identify_Recognize <- mean(ConvertCategoriesNum(CEDA$Identify_Recognize)) # Global mean for t.test

ttest_Identify_Recognize <- lapply(Q4_Recognize_num, t.test, mu=mu_Identify_Recognize) # t-test of Literacy Score of all subgroups, alpha=0.05
ttest2_Identify_Recognize <- lapply(Q4_Recognize_num, t.test, conf.level=0.99, mu=mu_Identify_Recognize) # t-test of Literacy Score of all subgroups, alpha=0.01


# "I feel prepared to support someone who is experiencing an eating disorder"
# Identify_Support

Q4_Support <- lapply(all_subgroups,select_,"Identify_Support") # Selects column correstponding to Identify_Support from each subgroup
Q4_Support_num <- lapply(Q4_Support,ConvertCategoriesNum) # Converts Identify_Support responses to numerical data for each subgroup

mu_Identify_Support <- mean(ConvertCategoriesNum(CEDA$Identify_Support)) # Global mean for t.test

ttest_Identify_Support <- lapply(Q4_Support_num, t.test, mu=mu_Identify_Support) # t-test of Literacy Score of all subgroups, alpha=0.05
ttest2_Identify_Support <- lapply(Q4_Support_num, t.test, conf.level=0.99, mu=mu_Identify_Support) # t-test of Literacy Score of all subgroups, alpha=0.01

# "I know about the resources available to support people experiencing eating disorders"
# Identify_Resources

Q4_Resources <- lapply(all_subgroups,select_,"Identify_Resources") # Selects column correstponding to Identify_Resources from each subgroup
Q4_Resources_num <- lapply(Q4_Resources,ConvertCategoriesNum) # Converts Identify_Resources responses to numerical data for each subgroup

mu_Identify_Resources <- mean(ConvertCategoriesNum(CEDA$Identify_Resources)) # Global mean for t.test

ttest_Identify_Resources <- lapply(Q4_Resources_num, t.test, mu=mu_Identify_Resources) # t-test of Literacy Score of all subgroups, alpha=0.05
ttest2_Identify_Resources <- lapply(Q4_Resources_num, t.test, conf.level=0.99, mu=mu_Identify_Resources) # t-test of Literacy Score of all subgroups, alpha=0.01

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Q5 Who would you approach (clean version) # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Q5 Who would you approach (clean version) # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Where have you received information about eating disorders in the past?
# Info_PastEdu





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Q8 Education received: Adequate, Active, Open  # # # # # # # # # # # # # # # # # # # #
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

# Variables of interest:

# I have received an adequate amount of public education about eating disorders
# PublicEdu_Adequate

Q8_Adequate <- lapply(all_subgroups,select_,"PublicEdu_Adequate") # Selects column correstponding to PublicEdu_Adequate from each subgroup
Q8_Adequate_num <- lapply(Q8_Adequate,ConvertCategoriesNum) # Converts PublicEdu_Adequate responses to numerical data for each subgroup

mu_PublicEdu_Adequate <- mean(ConvertCategoriesNum(CEDA$PublicEdu_Adequate)) # Global mean for t.test

ttest_PublicEdu_Adequate <- lapply(Q8_Adequate_num, t.test, mu=mu_PublicEdu_Adequate) # t-test of Literacy Score of all subgroups, alpha=0.05
ttest2_PublicEdu_Adequate <- lapply(Q8_Adequate_num, t.test, conf.level=0.99, mu=mu_PublicEdu_Adequate) # t-test of Literacy Score of all subgroups, alpha=0.01

# I am actively seeking more public education about eating disorders"
# PublicEdu_Active

Q8_Active <- lapply(all_subgroups,select_,"PublicEdu_Active") # Selects column correstponding to PublicEdu_Active from each subgroup
Q8_Active_num <- lapply(Q8_Active,ConvertCategoriesNum) # Converts PublicEdu_Active responses to numerical data for each subgroup

mu_PublicEdu_Active <- mean(ConvertCategoriesNum(CEDA$PublicEdu_Active)) # Global mean for t.test

ttest_PublicEdu_Active <- lapply(Q8_Active_num, t.test, mu=mu_PublicEdu_Active) # t-test of Literacy Score of all subgroups, alpha=0.05
ttest2_PublicEdu_Active <- lapply(Q8_Active_num, t.test, conf.level=0.99, mu=mu_PublicEdu_Active) # t-test of Literacy Score of all subgroups, alpha=0.01

# I am open to learning more about eating disorders
# PublicEdu_Open

Q8_Open <- lapply(all_subgroups,select_,"PublicEdu_Open") # Selects column correstponding to PublicEdu_Open from each subgroup
Q8_Open_num <- lapply(Q8_Open,ConvertCategoriesNum) # Converts PublicEdu_Open responses to numerical data for each subgroup

mu_PublicEdu_Open <- mean(ConvertCategoriesNum(CEDA$PublicEdu_Open)) # Global mean for t.test

ttest_PublicEdu_Open <- lapply(Q8_Open_num, t.test, mu=mu_PublicEdu_Open) # t-test of Literacy Score of all subgroups, alpha=0.05
ttest2_PublicEdu_Open <- lapply(Q8_Open_num, t.test, conf.level=0.99, mu=mu_PublicEdu_Open) # t-test of Literacy Score of all subgroups, alpha=0.01

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # Q3 (Most frequent ED) converted to Categorical # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Create graph of responses on most frequent ED on literacy quiz
# CEDA=read.csv("CEDAclean.csv", na.strings=c("","NA"))

# This is is so we can make the graph using different subsets of the data in the future 
# Replace subgroup_name with the name of the subgroup to examine. Else, uncomment LitPlaceholder <- CEDA$LitGrade.

# LitPlaceholder <- subgroup_name$LitGrade
# LitPlaceholder <- CEDA$LitGrade # Comment this line, and uncomment line above to run on subset


LiteracyGradeCategorical <- function(CEDA_subgroup){
  Literacy_Grade_num <- 
    
    ifelse(CEDA_subgroup$LitGrade == 100,"High",
           ifelse(CEDA_subgroup$LitGrade == 50,"Medium", 
                  ifelse(CEDA_subgroup$LitGrade ==0,"Low",NA
                  )))
  
  Literacy_Grade <- na.omit(Literacy_Grade_num)
  return(Literacy_Grade)
}

LiteracyGradeCategorical(CEDA) #Test

LitGradeCatGraph <- function(CEDA_subgroup){
  
  x<- LiteracyGradeCategorical(CEDA_subgroup)
  #  x <-revalue(yna, c("Strongly disagree"= "SD", "Somewhat disagree"= "SWD", "Neither disagree nor agree"="N", "Somewhat agree"= "SWA", "Strongly agree"= "SA"))
  #levels(x)
  ggplot(data.frame(x), aes(x=x, fill=x, na.rm= TRUE)) + geom_bar(na.rm = TRUE) 
  q <- ggplot(data.frame(x), aes(x=x, fill=Literacy_Grade, na.rm= TRUE)) + geom_bar(na.rm = TRUE) 
  q + labs(x = "Score", y = "Count", title = "Scores in question on most common eating disorder", fill = "Score")
}

LitGradeCatGraph(CEDA) #Test


IN PROGRESS
=======
#IN PROGRESS
>>>>>>> c96b696a3ed289a057d44d4f4128835fa639b6c4
