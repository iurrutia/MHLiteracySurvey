# This document uses CEDAclean.csv data to examine the following:
# The stigma and education scores (detailed in document LiteracyAndStigmaScores2019)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Analysis setup and package installation
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("plyr")
library(plyr)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We begin with CEDAclean.csv - this file has already been processed and cleaned (joint work with
# Jacqueline Baker and Isabel Urrutia)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# We read in the csv file:

CEDA=read.csv("CEDAClean.csv", na.strings=c("","NA"))
str(CEDA)

# Preliminary exploration:

# Note columns X, X.1 and X.2 are duplicates:
boolean_test <- c(CEDA$X.1 == CEDA$X)
str(boolean_test)
# Remove column X.1 and X.2:
CEDA$X.1 <- NULL
CEDA$X.2 <- NULL
str(CEDA)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We are interested in examining differences between different occupation groups, so we categorize
# respondents based on their occupation type.
#
# Occupations_Sum is a pre-processed list of respondents occupations, broadly grouped by sector and by their level of interaction with other people who they may provide services to in a professional capacity.
#
# This section shows the processing of respondents' reported occupation and classifies it into the categories:
# Allied health, health, other care (non-medical including counselling, psychologists, etc) and support-related professions, first responders, education, fitness, and other

# We classify respondent's occupations by creating boolean variables for each of these occupations:
# Boolean_occ_alliedhealth
# Boolean_occ_health
# Boolean_occ_support
# Boolean_occ_firstresp
# Boolean_occ_edu
# Boolean_occ_fitness
# Boolean_occu_other

# Below are the detailed break-downs of the occupations included in each category.

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
# Medical, Nutritionist or dietitian, 

# The boolean variables are created below:

Boolean_occ_alliedhealth <- c(CEDA$Occupations_Sum == "Medical"|
                          CEDA$Occupations_Sum == "Nutritionist or dietitian"
)

# Boolean_occ_health: Interaction with health service users: 
# Medical, Nurse practitioner, Nurse, General practitioner (family doctor)


Boolean_occ_health <- c(CEDA$Occupations_Sum == "Nurse practitioner"|
                          CEDA$Occupations_Sum == "Nurse"|
                          CEDA$Occupations_Sum == "General practitioner (family doctor)"
)

# Boolean_occ_support: Counselling, therapy, guidance, support, and social service work, social worker
# Counselling (other), School guidance counsellor, Occupational therapist, Psychologist, Social service worker, Psychiatrist, Psychotherapist, Social worker, Therapy (other)

Boolean_occ_support <- c(CEDA$Occupations_Sum == "Counselling (other)"|
                           CEDA$Occupations_Sum == "Occupational therapist"|
                           CEDA$Occupations_Sum == "Psychologist"|
                           CEDA$Occupations_Sum == "Social service worker"|
                           CEDA$Occupations_Sum == "Psychiatrist"|
                           CEDA$Occupations_Sum == "Psychotherapist"|
                           CEDA$Occupations_Sum == "Social worker"|
                           CEDA$Occupations_Sum == "Therapy (other)"|
                           CEDA$Occupations_Sum == "School guidance counsellor"
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
                       CEDA$Occupations_Sum == "Education (other)"
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We note the size of each occupational category group:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

summary(Boolean_occ_alliedhealth)
summary(Boolean_occ_health)
summary(Boolean_occ_support)
summary(Boolean_occ_firstresp)
summary(Boolean_occ_edu)
summary(Boolean_occ_fitness)
summary(Boolean_occ_other)





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# To work more easily with each occupational we write functions to calculate 'grade' the stigma and 
# literacy scores of a group of survey respondents.
# We create a subgroup for each occupational category, and then create a function that allows us
# to grade how a subgroup scored on their attitudes and knowledge of EDs (a stigma and a literacy score)
# We also create a function that applies the scoring function to all subgroups, to automate the
# process of scoring all of the occupational categories.
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# First create a subset for each occupational category, with the help of the boolean variables set up above. We can use these
# subsets to calculate literacy and stigma scores, as well as descriptive statistics (e.g. mean, sd, etc) for each ocupational category.

Group_alliedhealth <- subset(CEDA,Boolean_occ_alliedhealth==TRUE)
Group_health <- subset(CEDA,Boolean_occ_health==TRUE)  
Group_support <- subset(CEDA,Boolean_occ_support==TRUE) 
Group_firstresp <- subset(CEDA,Boolean_occ_firstresp==TRUE)   
Group_edu <- subset(CEDA,Boolean_occ_edu==TRUE)
Group_fitness <- subset(CEDA,Boolean_occ_fitness==TRUE)  
Group_other <- subset(CEDA,Boolean_occ_other==TRUE)   




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # LITERACY SCORE FUNCTION  # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# This function evaluates the literacy score of a group (or subgroup) of survey respondents:
# Input a subgroup of respondents using the format:
# LiteracyScore(name_of_subgroup)
# Note that we use this function over ALL survey respondents in the ANOVA below
  

LiteracyScore <- function(CEDA_subgroup) {
  

  # 1. Creating literacy grade columns: We create a column that gives 1 point if the respondent answerede the question demonstrating literacy, and zero points if the answer did not demonstrate literacy.
  
  
  # Lit_EDChoice (Survey question 2.a - points awarded for strongly/somewhat disagree)
  
  levels(CEDA_subgroup$Lit_EDChoice)
  LG_EDChoice <-          # Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDChoice == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDChoice == "Somewhat disagree",1,0))
  
  # Lit_EDIllness (Survey question 2.b - points awarded for strongly/somewhat agree)
  
  levels(CEDA_subgroup$Lit_EDIllness)
  LG_EDIllness <-          # Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDIllness == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDIllness == "Somewhat agree",1,0))
  
  # Lit_EDImmigrant (Survey question 2.c - n/a to literacy score)
  
  # Lit_EDPOC (Survey question 2.d - n/a to literacy score)
  
  # Lit_EDGenetic (Survey question 2.e - points awarded for strongly/somewhat agree)
  
  levels(CEDA_subgroup$Lit_EDGenetic)
  LG_EDGenetic <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDGenetic == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDGenetic == "Somewhat agree",1,0))
  
  # Lit_EDLGBTQ (Survey question 2.f - n/a to literacy score)
  
  # Lit_EDThreatening (Survey question 2.g - points awarded for strongly/somewhat agree)
  
  levels(CEDA_subgroup$Lit_EDThreatening)
  LG_EDThreatening <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDThreatening == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDThreatening == "Somewhat agree",1,0))
  
  # Lit_EDMen (Survey question 2.h - n/a to literacy score)
  
  # Lit_EDTreatment	(Survey question 2.i - n/a to literacy score)
  
  #Lit_EDWeight (Survey question 2.j - points awarded for strongly/somewhat agree)
  
  levels(CEDA_subgroup$Lit_EDWeight)
  LG_EDWeight <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDWeight == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDWeight == "Somewhat agree",1,0))
  
  # Lit_EDRecover (Survey question 2.k - points awarded for strongly/somewhat disagree)
  
  levels(CEDA_subgroup$Lit_EDRecover)
  LG_EDRecover <- #Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDRecover == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDRecover == "Somewhat disagree",1,0))  
  
  # Lit_EDDiet	(Survey question 2.l - points awarded for strongly/somewhat disagree)
  
  levels(CEDA_subgroup$Lit_EDDiet)
  LG_EDDiet <- #Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDDiet == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDDiet == "Somewhat disagree",1,0))

# Step 2. Grading question on the most common ED (See grading scheme in appendix.)

CEDA_subgroup$LitGrade <- ifelse(CEDA_subgroup$Rank_BED=="1", 50,0)
CEDA_subgroup$LitGrade <- ifelse(CEDA_subgroup$Rank_Anorexia=="3" & CEDA_subgroup$Rank_BED=="1" & CEDA_subgroup$Rank_Bulimia=="2" & CEDA_subgroup$Rank_OSFED=="4", 100,CEDA_subgroup$LitGrade)  
  
# Step 3. Adding literacy grade colums and CEDA_subgroup$LitGrade
  
  LiteracyScore <- CEDA_subgroup$LitGrade/50 + LG_EDChoice + LG_EDIllness + LG_EDGenetic  + LG_EDThreatening + LG_EDWeight + LG_EDRecover + LG_EDDiet
  
  LiteracyScoreGr <- LiteracyScore*100/9 # Score as grade out of 100 points
  
  LiteracyScoreGr # Returns LiteracyScoreGR
  
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # STIGMA SCORE FUNCTION # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Step 1. Creating stigma grade columns

# Lit_EDChoice	(Survey question 2.a - points awarded for strongly/somewhat agree)

  StigmaScore <- function(CEDA_subgroup){
  
  # levels(CEDA_subgroup$Lit_EDChoice)
  StigS_EDChoice <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDChoice == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDChoice == "Somewhat agree",1,
             ifelse(CEDA_subgroup$Lit_EDChoice == "Neither disagree nor agree",1,0
                  )))
  
# Lit_EDIllness	SD, D, N (Survey question 2.b)
  
  # levels(CEDA_subgroup$Lit_EDIllness)
  
  StigS_EDIllness <- #Answers accepted SD, D, N
    ifelse(CEDA_subgroup$Lit_EDIllness == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDIllness == "Somewhat disagree",1,
                  ifelse(CEDA_subgroup$Lit_EDIllness == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDImmigrant 	SA, A (Survey question 2.c)
  
  # levels(CEDA_subgroup$Lit_EDImmigrant)
  
  StigS_EDImmigrant <- #Answers accepted SA, A
    ifelse(CEDA_subgroup$Lit_EDImmigrant == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDImmigrant == "Somewhat agree",1,0
           ))
  
  # Lit_EDPOC 	A, SA (Survey question 2.d)
  
  # levels(CEDA_subgroup$Lit_EDPOC )
  
  StigS_EDPOC <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDPOC  == "Strongly sagree", 1,
           ifelse(CEDA_subgroup$Lit_EDPOC  == "Somewhat agree",1,0
           ))
  
  # Lit_EDGenetic 	n/a (Survey question 2.e)
  
  # Lit_EDLGBTQ 	SD, D (Survey question 2.f)
  
  # levels(CEDA_subgroup$Lit_EDLGBTQ)
  
  StigS_EDLGBTQ <- #Answers accepted SD, D
    ifelse(CEDA_subgroup$Lit_EDLGBTQ == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDLGBTQ == "Somewhat disagree",1,0))
  
  # Lit_EDThreatening	SD, D, N (Survey question 2.g)
  
  # levels(CEDA_subgroup$Lit_EDThreatening)
  
  StigS_EDThreatening <- #Answers accepted SD, D, N
    ifelse(CEDA_subgroup$Lit_EDThreatening == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDThreatening == "Somewhat disagree",1,
                  ifelse(CEDA_subgroup$Lit_EDThreatening == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDMen 	(Survey question 2.h)
  
  # levels(CEDA_subgroup$Lit_EDMen)
  
  StigS_EDMen <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDMen == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDMen == "Somewhat agree",1,0))
  
  # Lit_EDTreatment	A, SA (Survey question 2.i)
  
  # levels(CEDA_subgroup$Lit_EDTreatment)
  
  StigS_EDTreatment <- #Answers accepted A, SA
    ifelse(CEDA_subgroup$Lit_EDTreatment == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDTreatment == "Somewhat agree",1,0
           ))
  
  # Lit_EDWeight	SD, D ,N  (Survey question 2.j)
  
  # levels(CEDA_subgroup$EDWeight)
  
  StigS_EDWeight <- #Answers accepted SD, D, 
    ifelse(CEDA_subgroup$Lit_EDWeight == "Strongly disagree", 1,
           ifelse(CEDA_subgroup$Lit_EDWeight == "Somewhat disagree",1,0
                  ))
  
  # Lit_EDRecover k)	N, A, SA (Survey question 2.k)
  
  # levels(CEDA_subgroup$Lit_EDRecover)
  
  StigS_EDRecover <- #Answers accepted N, A, SA
    ifelse(CEDA_subgroup$Lit_EDRecover == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDRecover == "Somewhat agree",1,
                  ifelse(CEDA_subgroup$Lit_EDRecover == "Neither disagree nor agree",1,0
                  )))
  
  # Lit_EDDiet	l)	N, A, SA (Survey question 2.l)
  
  
  
  # levels(CEDA_subgroup$Lit_EDDiet)
  
  StigS_EDDiet <- # Answers accepted N, A, SA
    ifelse(CEDA_subgroup$Lit_EDDiet == "Strongly agree", 1,
           ifelse(CEDA_subgroup$Lit_EDDiet == "Somewhat agree",1,
                  ifelse(CEDA_subgroup$Lit_EDDiet == "Neither disagree nor agree",1,0
                  )))

  
# Step 3. Adding all stigma grade colums together
  
  StigmaScore <- StigS_EDDiet + StigS_EDRecover + StigS_EDWeight + StigS_EDMen + StigS_EDLGBTQ + StigS_EDPOC + StigS_EDImmigrant + StigS_EDIllness + StigS_EDChoice + StigS_EDThreatening + StigS_EDTreatment
  
  StigmaScoreGr <- StigmaScore*100/11 # Score as grade out of 100 points
  
  StigmaScoreGr # Returns StigmaScoreGR
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # CREATE A DATAFRAME FOR ANOVA  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

  
  
  
# We create a new dataframe (CEDA_analysis_LS) to conduct our tests which just has the variables we are interested in: Categories and stigma/literacy score values:

CEDA$Literacy_Score <- LiteracyScore(CEDA)
CEDA$Stigma_Score <- StigmaScore(CEDA) 



# And create a column of zeros/ones for each class

CEDA$Num_occ_alliedhealth  <- as.numeric(Boolean_occ_alliedhealth)
CEDA$Num_occ_health  <- as.numeric(Boolean_occ_health)
CEDA$Num_occ_support  <- as.numeric(Boolean_occ_support)
CEDA$Num_occ_firstresp  <- as.numeric(Boolean_occ_firstresp)
CEDA$Num_occ_edu  <- as.numeric(Boolean_occ_edu)
CEDA$Num_occ_fitness  <- as.numeric(Boolean_occ_fitness)

# We assign eaach category a numerical in the vector 'Occupation_Cat':

CEDA$Occupation_Cat <- CEDA$Num_occ_alliedhealth + 2*CEDA$Num_occ_health + 3*CEDA$Num_occ_support + 4*CEDA$Num_occ_firstresp + 5*CEDA$Num_occ_edu + 6*CEDA$Num_occ_fitness 
table(CEDA$Occupation_Cat)

# Allied health = Category 1
# Health = Category 2
# Support = Category 3
# First responders = Category 4
# Education = Category 5
# Fitness = Category 6
# Other = Category 0

# Check for no repeating categories (to satisfy analysis conditions that categories are independent):

CEDA$Occupation_Conditions <- CEDA$Num_occ_alliedhealth + CEDA$Num_occ_health + CEDA$Num_occ_support + CEDA$Num_occ_firstresp + CEDA$Num_occ_edu + CEDA$Num_occ_fitness 

# The test bellow checks that every respondend is assigned to exactly one category (i.e. has exactly one value of 1 among all boolean categorical variables)
max(CEDA$Occupation_Conditions)

CEDA$Occupation_Cat_Lab <- ifelse(CEDA$Occupation_Cat == 1, "Allied health","Other")
CEDA$Occupation_Cat_Lab <- ifelse(CEDA$Occupation_Cat == 2, "Health",CEDA$Occupation_Cat_Lab)
CEDA$Occupation_Cat_Lab <- ifelse(CEDA$Occupation_Cat == 3, "Support",CEDA$Occupation_Cat_Lab)
CEDA$Occupation_Cat_Lab <- ifelse(CEDA$Occupation_Cat == 4, "First responder",CEDA$Occupation_Cat_Lab)
CEDA$Occupation_Cat_Lab <- ifelse(CEDA$Occupation_Cat == 5, "Education",CEDA$Occupation_Cat_Lab)
CEDA$Occupation_Cat_Lab <- ifelse(CEDA$Occupation_Cat == 6, "Fitness",CEDA$Occupation_Cat_Lab)

# We can now create a new dataframe with the variables for the anova analysis:

CEDA_analysis_LS <- data.frame("Occupation" = CEDA$Occupation_Cat_Lab,"Occupation_num" = CEDA$Occupation_Cat, "Literacy" = CEDA$Literacy_Score, "Stigma" = CEDA$Stigma_Score)
str(CEDA_analysis_LS)


#######################################################################
# We will use the aov function:
help(aov)
# Broadly speaking, we are interested in comparing the literacy and stigma variables accross occupations, so before
# we conduct the ANOVA, let's look at the boxplots:

par(mfrow=c(2,1))
boxplot(CEDA_analysis_LS$Literacy~CEDA_analysis_LS$Occupation, ylab="Literacy Score") # Open graphs in a separate window for legibility
boxplot(CEDA_analysis_LS$Stigma~CEDA_analysis_LS$Occupation, ylab="Stigma Score") 
dev.off()

# Observing the boxplots above we note that the Literacy scores are all similar, but the stigma scores
# seem to vary a bit more by occupation, however it is not clear from visual inspection if there is any
# strong variation between groups.
table(CEDA_analysis_LS$Occupation)

# ANOVA:
# Ho: The mean Literacy score is the same accross occupational categories
# Ho: The mean Stigma score is the same accross occupational categories

tapply(CEDA_analysis_LS$Literacy,CEDA_analysis_LS$Occupation,mean, na.rm=TRUE)
tapply(CEDA_analysis_LS$Stigma,CEDA_analysis_LS$Occupation,mean, na.rm=TRUE)

anova_lit <- aov(Literacy ~ Occupation, data=CEDA_analysis_LS)

summary(anova_lit)

anova_stig <- aov(Stigma ~ Occupation, data=CEDA_analysis_LS)

summary(anova_stig)



# More visually communicative boxplots can be created as shown below:

# boxplots:
library(ggplot2)
library(readr)
p <- ggplot(CEDA_analysis_LS,aes(x=reorder(Occupation, Literacy, mean), y=Literacy, fill=Occupation)) +
      geom_violin() +
      geom_jitter(width=0.25, alpha=0.2)
   p   + labs(x = "Occupation Category") + labs(y = "Literacy Score")

# boxplots:
library(ggplot2)
library(readr)
p <- ggplot(CEDA_analysis_LS,aes(x=reorder(Occupation, Stigma, mean), y=Stigma, fill=Occupation)) +
      geom_violin() +
      geom_jitter(width=0.25, alpha=0.2)
   p   + labs(x = "Occupation Category") + labs(y = "Stigma Score")

   
# We use TukeyHSD to find differences between subgroups:
TukeyHSD(anova_lit)
plot(TukeyHSD(anova_lit))

TukeyHSD(anova_stig)
plot(TukeyHSD(anova_stig))

# Note in the plots that all p vals are quite high, and that the confidence intetrvals for the pairwise comparisons (TukeyHSD)
# all contain zero, so we see no significant differences between any of the groups.

# Note that we should test with MANOVA because we have >1 dependent variables, but since we fail to reject the null for
# ANOVA, we know we would also fail to reject for MANOVA

# We go ahead and confirm, and note the large p value, as expected:

#manova(dependent.vars ~ indep.vars)

litstig.man <- manova(cbind(Stigma, Literacy) ~ Occupation, data = CEDA_analysis_LS)
summary(litstig.man)
summary.aov(litstig.man)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Q4 Preparedness: Recognize, support, resources  # # # # # # # # # # # # # # # # # # # #
# # # # # # Q8 Education received: Adequate, Active, Open # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # Q8 Types of educations vs RSR score # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
   
# # # # # # # # # # # # # # # # # # #
# # PART 1: Creating a dataframe  # #
# # # # # # # # # # # # # # # # # # #

# Setup information:
# Similarly to above, we create a score for participants 'comfort and preparadeness' (RSR_score) to
# recognize and support others experiencing eating disorders (compared between occupations)
# We create this score using survey question 4.
# We create variables for Education received variables (survey question 8) (compared between occupations)
# We create a variable that indicates whether or not respondents have received ED education (of the types offered by NEDIC) and compare that to their
# 'comfort and preparedness' score
# These variables are stored in a new dataframe, CEDA_analysis_b, for analysis

# Occupation subgroups are: 
# Group_alliedhealth
# Group_health
# Group_support
# Group_firstresp   
# Group_edu
# Group_fitness 
# Group_other 
   
# We will be working with three variables that all have answers ranging from 'Strongly agree' to 'Strongly disagree'
# We create a function to convert these variables to numbers:
   
# A function that convers categorical data to numerical (Strongly agree = 5, ..., Strongly disagree = 0)
   
ConvertCategoriesNum <- function(column){
  
  numerical <- 
    ifelse(column == "Strongly agree", 5,
           ifelse(column == "Somewhat agree",4,
                  ifelse(column == "Neither disagree nor agree"|column == "Neither disagree or agree",3,
                         ifelse(column == "Disagree"|column == "Somewhat disagree",2,
                                ifelse(column == "Strongly disagree",1,0
                                )))))
  
  numerical
}

# A function that convers categorical data to numerical (Strongly DISAGREE = 5, ..., Strongly AGREE = 0)
# (This is the 'reverse function from the one above)

ConvertCategoriesNum_Rev <- function(column){
  
  numerical <- 
    ifelse(column == "Strongly agree", 1,
           ifelse(column == "Somewhat agree",2,
                  ifelse(column == "Neither disagree nor agree"|column == "Neither disagree or agree",3,
                         ifelse(column == "Disagree"|column == "Somewhat disagree",4,
                                ifelse(column == "Strongly disagree",5,0
                                )))))
  
  numerical
}

# A function that aggregates respondents' responses regarding their preparedness to recognize, support, and provide resources 
# individuals who may be experiencing EDs. # We check answers for three survey questions:
# Identify_Recognize: ‘I believe I can recognize the signs of an eating disorder in others’
# Identify_Support: ‘I feel prepared to support someone who is experience an eating disorder’
# Identify_Resources: 'I know about the resources available to support people experiencing eating disorders’


RSR_scoring <- function(df){

Identify_Recognize_num <- ConvertCategoriesNum(df$Identify_Recognize)  
Identify_Support <- ConvertCategoriesNum(df$Identify_Support)  
Identify_Resources <- ConvertCategoriesNum(df$Identify_Resources)

RSR_score <- Identify_Recognize_num + Identify_Support + Identify_Resources
RSR_score
}


RSR_score <- RSR_scoring(CEDA)

str(RSR_score)
table(RSR_score)
hist(RSR_score)

# Note that the RSR score looks Gaussian upon visual inspection

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Q8 Education received: Adequate, Active, Open  # # # # # # # # # # # # # # # # # # # #

# We wish to examine respondents' experience with ED education, as well as their 
# openness to and work towards receiving more ED-related education


# PublicEdu_Adequate: ‘I have received and adequate amount of public education about eating disorders’
# PublicEdu_Active: ‘I am actively seeking more public education about eating disorders’
# PublicEdu_Open: ‘I am open to learning more about eating disorders’

Q8_Adequate <- ConvertCategoriesNum(CEDA$PublicEdu_Adequate) 
table(Q8_Adequate)
hist(Q8_Adequate)

Q8_Active <- ConvertCategoriesNum(CEDA$PublicEdu_Active) 
table(Q8_Active)
hist(Q8_Active)

Q8_Open <- ConvertCategoriesNum(CEDA$PublicEdu_Open) 
table(Q8_Open)
hist(Q8_Open)

# Note all histograms look roughly Gaussian 

###################################################################


# Note, we use the grepl function to find which respondents indicated they had received education on EDs in a form that is provided 
# by NEDIC. We then compare respondents' comfort providing resources, support and recognizing signs of EDs based on whether or not they've 
# received these typeos of education/information regarding EDs.

# We have NEDIC_services value 1 if respondent has had access to the types of services offered by NEDIC, and 0 otherwise

NEDIC_services_vector <- c("(during grade K-12)", 
                           "Conference", 
                           "Community panel", 
                           "Health/mental health fair or exhibit", 
                           "organization website", 
                           "Pamphlet", 
                           "Webinar", 
                           "Social media")

# table(grepl(NEDIC_services,CEDA$Info_PastEdu))
# grepl(NEDIC_services,CEDA$Info_PastEdu)

# However grepl doesn't allow for multiple arguments, so we need to paste the string patterns directly into the grepl args:
# To observe our results: table(grepl(NEDIC_services,CEDA$Info_PastEdu))

NEDIC_services_string <- paste(NEDIC_services_vector, collapse = "|") # Creates a string with entries of NEDIC_services_vector

NEDIC_services_bool <- grepl(NEDIC_services_string,CEDA$Info_PastEdu)

table(NEDIC_services_bool)

NEDIC_services <- as.numeric(NEDIC_services_bool)

str(NEDIC_services_bool)
str(NEDIC_services)


######################################################################################

# Now we create a dataframe (CEDA_analysis_b) that will be used for analyses regarding questions 4, 8, and respondents' RSR scores


CEDA_analysis_b <- data.frame("Occupation" = CEDA$Occupation_Cat_Lab,
                              "Occupation_num" = CEDA$Occupation_Cat,
                              "RSR_Score" = RSR_score, 
                              "NEDIC_services" = NEDIC_services,
                              "Q8_Adequate" = Q8_Adequate,
                              "Q8_Active" = Q8_Active,
                              "Q8_Open" = Q8_Open
                              )
str(CEDA_analysis_b)



# # # # # # # # # # # # # # # # # # #
# # PART 2: Tests # # # # # # # # # #
# # # # # # # # # # # # # # # # # # #

# Summary:
# Test A: Differences in RSR score by occupation
# Test B: Differences in Q8 responses by occupation
# Test C: Difference in RSR score by NEDIC services
  

# TEST A & B
######################################################################

# We will use the aov function for exploration, and then conduct MANOVA to account for multiple comparisons.
help(aov)

# TEST A
######################################################################

# Broadly speaking, we are interested in comparing RSR scores accross occupations, so before
# we conduct the ANOVA, let's look at the boxplots:
boxplot(CEDA_analysis_b$RSR_Score~CEDA_analysis_b$Occupation) # Open graphs in a separate window for legibility


# Upon visual inspection, note that the boxplots above we note that the RSR score mean is higher in 
# 'Support' and 'Allied health' occupations categories.


# ANOVA:
# Ho: The mean RSR score is the same accross occupational categories
# Ho: The mean RSR score is the same accross occupational categories

tapply(CEDA_analysis_b$RSR_Score,CEDA_analysis_b$Occupation,mean, na.rm=TRUE)

anova_RSR <- aov(RSR_Score ~ Occupation, data=CEDA_analysis_b)

summary(anova_RSR)

# We note that we have a few low p-vals: Education-Allied health, Other-Allied health, Support-Education, and Support-Other
TukeyHSD(anova_RSR)
plot(TukeyHSD(anova_RSR))
# Examining the differences, we see that Support and Allied Health scored significantly higher, particularly relative to Education and Other
# Other occupation groups scored somewhere in between, and where not statistically distinguishable



# boxplots:
library(ggplot2)
library(readr)
p <- ggplot(CEDA_analysis_b,aes(x=reorder(Occupation, RSR_Score, mean), y=RSR_Score, fill=Occupation)) +
  geom_violin() +
  geom_jitter(width=0.25, alpha=0.2)
p   + labs(x = "Occupation Category") + labs(y = "RSR_Score")

  
  

# TEST B
######################################################################

q8.man <- manova(cbind(Q8_Adequate, Q8_Active, Q8_Open) ~ Occupation, data = CEDA_analysis_b)
summary(q8.man)
summary.aov(q8.man)

summary(q8.man, test=c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))
summary(q8.man, test = "Wilks")
summary(q8.man, test = "Hotelling-Lawley")
summary(q8.man, test = "Roy")


# Note that sig. dif. was found for Q8_Active & Q8_Open, but for Q8_Adequate no significant distinction btw groups was found.
# We run all dep vars separately to examine the differences.

# Q8_Adequate
anova_Q8_Adequate <- aov(Q8_Adequate ~ Occupation, data=CEDA_analysis_b)
TukeyHSD(anova_Q8_Adequate)
plot(TukeyHSD(anova_Q8_Adequate))

# We find no significant differences for Q8_Adequate, as expected from the MANOVA failing to reject

# Q8_Active
anova_Q8_Active <- aov(Q8_Active ~ Occupation, data=CEDA_analysis_b)
TukeyHSD(anova_Q8_Active)
plot(TukeyHSD(anova_Q8_Active))
tapply(CEDA_analysis_b$Q8_Active,CEDA_analysis_b$Occupation,mean, na.rm=TRUE)

# We find stat. sig. dif. for the difference in means btw: Support-First responder, Support-Other, 
# and almost 0.05 for Other-Allied health 
# Note that Support scored highest in Q8_Active (3.094340), First responder scored lowest in Q8_Open (2.000000),
# and Other scored second lowest in Q8_Active (2.315162)
# This is consistent with the results found in MANOVA (rejecting null H)


# Q8_Open
anova_Q8_Open <- aov(Q8_Open ~ Occupation, data=CEDA_analysis_b)
TukeyHSD(anova_Q8_Open)
plot(TukeyHSD(anova_Q8_Open))
tapply(CEDA_analysis_b$Q8_Open,CEDA_analysis_b$Occupation,mean, na.rm=TRUE)


# We find stat. sig. dif. for the difference in means btw. Support-Other (and no other dif.)
# Note that Support scored highest in Q8_Open (4.547170) and Other scored lowest in Q8_Open (4.052901)
# This is consistent with the results found in MANOVA (rejecting null H)


# TEST C
######################################################################

# We compare RSR scores accross exposure to types of ED educational resources offered by NEDIC


# Additional observations:
# We note that the effect the variation in RSR scores by NEDIC_services varies accross occupations:

NS_1 <- subset(CEDA_analysis_b,NEDIC_services == 1)
NS_0 <- subset(CEDA_analysis_b,NEDIC_services == 0)
str(NS_1)
NSm_1 <- tapply(NS_1$RSR_Score,NS_1$Occupation,mean,na.rm=TRUE)
NSm_0 <-tapply(NS_0$RSR_Score,NS_0$Occupation,mean,na.rm=TRUE)

NScount_1 <- table(NS_1$Occupation)
NScount_0 <- table(NS_0$Occupation)

NS_Occ_means <- data.frame("NEDIC_Services = 0" = NSm_0,"NEDIC_Services = 1" = NSm_1,"Count_0"=NScount_0, "Count_1"=NScount_1)

ggplot(data = NS_Occ_means) +
  geom_point(mapping = aes(x=row.names(NS_Occ_means),y=NSm_0, size = Count_0.Freq, color = "red" )) +
  geom_point(mapping = aes(x=row.names(NS_Occ_means),y=NSm_1, size = Count_1.Freq, color = "blue" )) 


####################################################

boxplot(CEDA_analysis_b$RSR_Score~CEDA_analysis_b$NEDIC_services) # Open graphs in a separate window for legibility


# Recall that NEDIC_services has value 1 if respondent has had access to the types of services offered by NEDIC, and 0 otherwise

# Since we only have two means to compare (NEDIC_services = 0 or 1), it is appropriate to use a t-test:

# t-test:
# Ho: The mean RSR score is the same between respondents receiving NEDIC-type educational services and those who haven't
# Ho: The mean RSR score is not the between respondents receiving NEDIC-type educational services and those who haven't


# Check assumptions:

res.ftest <- var.test(RSR_Score ~ NEDIC_services, data = CEDA_analysis_b)
res.ftest

shapiro.test(RSR_score[NEDIC_services == "0"])
shapiro.test(RSR_score[NEDIC_services == "1"]) 

shapiro.test(NS_0$RSR_Score)
shapiro.test(NS_1$RSR_Score)

plot(density(na.omit(NS_0$RSR_Score)))
plot(density(NS_1$RSR_Score))

qqnorm( CEDA_analysis_b$RSR_Score[CEDA_analysis_b$NEDIC_services==0], main='No NEDIC Services')
qqline( CEDA_analysis_b$RSR_Score[CEDA_analysis_b$NEDIC_services==0] )

qqnorm( CEDA_analysis_b$RSR_Score[CEDA_analysis_b$NEDIC_services==1], main='NEDIC Services')
qqline( CEDA_analysis_b$RSR_Score[CEDA_analysis_b$NEDIC_services==1] )

ggplot(data = CEDA_analysis_b, aes(x=RSR_score)) + 
  geom_histogram()+facet_grid(~NEDIC_services)+theme_bw()

with(CEDA_analysis_b, shapiro.test(RSR_score[NEDIC_services == "0"]))
with(CEDA_analysis_b, shapiro.test(RSR_score[NEDIC_services == "1"])) 

# T TEST:


res <- t.test(RSR_score ~ NEDIC_services, data = CEDA_analysis_b, var.equal = TRUE)
res


# We conclude that, though similar, respondents who have received the types of Educational services offered by NEDIC tended
# to score higher on RSR_Score than those who hadn't, to a statistically significant level. In other words, these types of 
# services produce a measurable effec on 'comfort and preparadeness' to
# recognize and support others experiencing eating disorders (compared between occupations)

####################################################


# boxplots:
library(ggplot2)
library(readr)
p <- ggplot(CEDA_analysis_b,aes(x=reorder(Occupation, RSR_Score, mean), y=RSR_Score, fill=Occupation)) +
  geom_violin() +
  geom_jitter(width=0.25, alpha=0.2)
p   + labs(x = "Occupation Category") + labs(y = "RSR_Score")


#####################################################
# Helpful function to examin variables

mymax <- function(x) {
  ifelse( !all(is.na(x)), max(x, na.rm=T), NA)}


######################################################

# Extras:


#manova(dependent.vars ~ indep.var)

RSR_ON.an <- aov(RSR_score ~ Occupation + NEDIC_services + Occupation:NEDIC_services, data = CEDA_analysis_b)
summary(RSR_ON.an)
plot(RSR_ON.an)

TukeyHSD(RSR_ON.an)

# Two-way Interaction Plot

with(CEDA_analysis_b,{
interaction.plot(NEDIC_services, Occupation, RSR_score, type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Occupation",
                 ylab="NEDIC_services",
                 main="Interaction Plot")
  
})

