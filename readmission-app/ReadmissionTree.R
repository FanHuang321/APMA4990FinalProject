# APMA 4990_001 Final Project - Excessive Hospital Readmission Analysis
# AUTHOR: CHENHUI HUANG, BING HAN
# Load Necessary Packages



##############################################################################################
## Data Input ##
Readmission_Tree_HF <- function(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15){
  
dataframe <- data.frame(Equipment = f1/103, HomeHealthAgency = f2/816, Hospice = f3/122, SkilledNursing = f4/3319, 
                          ED_1b = f5, ED_2b = f6, CareTransition = f7, Cleanliness = f8, 
                        DischargeInfo = f9, MedCommunication = f10, DoctorCommunication = f11, 
                          NurseCommunication = f12, PainManagement = f13, Quietness = f14, 
                          StaffResponsiveness= f15)

# Readmission Reduction (Readmission Outcome) #
# Heart Failure
Readmission_Reduction <- read_csv('readmission-app/data/READMISSION REDUCTION.csv')
HF_Readmission <- Readmission_Reduction %>%
  filter(`Measure Name` == 'READM-30-HF-HRRP') %>%
  select(`Provider Number`, `Excess Readmission Ratio`) %>%
  rename(`ProviderID` = `Provider Number`) %>%
  mutate(`ExcessReadmission` = `Excess Readmission Ratio`>1)
HF_Readmission$ExcessReadmission <- as.factor(HF_Readmission$ExcessReadmission)

# Hospital Consumer Assessment of Healthcare Providers and Systems survey #
HCAHPS <- read_csv('readmission-app/data/HCAHPS - Hospital.csv')
HCAHPS <- HCAHPS %>% 
  select(c(`Provider ID`, `HCAHPS Question`, `Patient Survey Star Rating`)) %>%
  filter(`Patient Survey Star Rating` != 'Not Applicable') %>% # Filter NA Values
  filter(`Patient Survey Star Rating` != 'Not Available') %>% 
  spread(`HCAHPS Question`,`Patient Survey Star Rating`) # Make rows become columns
colnames(HCAHPS)<- c("ProviderID","CareTransition","Cleanliness","MedCommunication","DischargeInfo",
                     "DoctorCommunication","NurseCommunication", "OverallRating","PainManagement",
                     "Quietness","RecommendHospital","StaffResponsiveness","Summary")
HCAHPS$CareTransition <- as.integer(HCAHPS$CareTransition)
HCAHPS$Cleanliness <- as.integer(HCAHPS$Cleanliness)
HCAHPS$DischargeInfo <- as.integer(HCAHPS$DischargeInfo)
HCAHPS$MedCommunication <- as.integer(HCAHPS$MedCommunication)
HCAHPS$DoctorCommunication <- as.integer(HCAHPS$DoctorCommunication)
HCAHPS$NurseCommunication <- as.integer(HCAHPS$NurseCommunication)
HCAHPS$PainManagement <- as.integer(HCAHPS$PainManagement)
HCAHPS$Quietness <- as.integer(HCAHPS$Quietness)
HCAHPS$StaffResponsiveness <- as.integer(HCAHPS$StaffResponsiveness)

# Timely and Effectively Care #
EffectiveCare <- read_csv('readmission-app/data/Timely and Effective Care - Hospital.csv')
EffectiveCare <- EffectiveCare %>%
  filter(`Measure ID` == 'ED_1b' | `Measure ID` == 'ED_2b' | `Measure ID` == 'IMM_2' |
           `Measure ID` == 'OP_20') %>%
  select(`Provider ID`, `Measure ID`, Score) %>%
  spread(`Measure ID`, Score) %>%
  rename(ProviderID = `Provider ID`)
EffectiveCare$ED_1b <- as.integer(EffectiveCare$ED_1b)
EffectiveCare$ED_2b <- as.integer(EffectiveCare$ED_2b)
EffectiveCare$IMM_2 <- as.integer(EffectiveCare$IMM_2)
EffectiveCare$OP_20 <- as.integer(EffectiveCare$OP_20)

# Spending Claim #
SpendingClaim <- read_csv('readmission-app/data/Medicare Hospital Spending by Claim.csv')
SpendingClaim$Avg_Spending_Per_Episode_Hospital <- as.integer(SpendingClaim$Avg_Spending_Per_Episode_Hospital)
SpendingClaim$Avg_Spending_Per_Episode_Nation <- as.integer(SpendingClaim$Avg_Spending_Per_Episode_Nation)
# Post Discharge
DischargeSpending <- SpendingClaim %>%
  filter(Period == "1 through 30 days After Discharge from Index Hospital Admission") %>%
  mutate(diff = Avg_Spending_Per_Episode_Hospital / Avg_Spending_Per_Episode_Nation) %>%
  select(Provider_ID, Claim_Type, diff) %>%
  spread(Claim_Type, diff)
colnames(DischargeSpending)<- c("ProviderID","Carrier","Equipment","HomeHealthAgency","Hospice",
                                "Inpatient","Outpatient", "SkilledNursing")
DischargeSpending$ProviderID <- as.character(DischargeSpending$ProviderID)

#########################################################################
## Joining Data Set ##

# HCAHPS and Heart Failure Readmission
HCAHPS_HF_Readmission <- full_join(HF_Readmission, HCAHPS, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 

# Post Discharge Spending and HF Readmission
DischargeSpending_HF_Readmission <- full_join(HF_Readmission, DischargeSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
DischargeSpending_HF_Readmission$`Excess Readmission Ratio` <- as.numeric(DischargeSpending_HF_Readmission$`Excess Readmission Ratio`)

# Effective Care and HF Readmission
EffectiveCare_HF_Readmission <- full_join(HF_Readmission, EffectiveCare, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()
EffectiveCare_HF_Readmission$`Excess Readmission Ratio` <- as.numeric(EffectiveCare_HF_Readmission$`Excess Readmission Ratio`)

# All Feature Join HF Readmission
All_HF_Readmission <- full_join(DischargeSpending_HF_Readmission,EffectiveCare_HF_Readmission, 
                                by = "ProviderID") %>% 
  full_join(HCAHPS, by = "ProviderID" ) %>%
  na.omit() %>%
  select(-c( Inpatient, Outpatient, Carrier, `Excess Readmission Ratio.y`, ExcessReadmission.y, 
             OverallRating, RecommendHospital, Summary, IMM_2, OP_20))

#########################################################################
## Decision Tree Modeling ##
# Heart Failure

# Use the optimal tree depth to build model
fitHF <- rpart(ExcessReadmission.x ~ ED_1b + ED_2b  +
               CareTransition + Cleanliness + MedCommunication +
               DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
               StaffResponsiveness + Equipment + HomeHealthAgency + Hospice +
               SkilledNursing,
             data = All_HF_Readmission,
             method = "class",
             control = list(cp = 0, maxdepth = 2))

if (predict(fitHF, dataframe, type = "class")==TRUE){
  output="Readmission rate will be excessive!"
}else{output="Congratulations! Based on our prediction, your readmission rate will be non-excessive."}
#output <- predict(fitHF, dataframe, type = "class")
return(output)
}

Readmission_Tree_COPD <- function(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15){
  
  dataframe <- data.frame(Equipment = f1/103, HomeHealthAgency = f2/816, Hospice = f3/122, SkilledNursing = f4/3319, 
                          ED_1b = f5, ED_2b = f6, CareTransition = f7, Cleanliness = f8, 
                          DischargeInfo = f9, MedCommunication = f10, DoctorCommunication = f11, 
                          NurseCommunication = f12, PainManagement = f13, Quietness = f14, 
                          StaffResponsiveness= f15)
  
  # Readmission Reduction (Readmission Outcome) #
  Readmission_Reduction <- read_csv('readmission-app/data/READMISSION REDUCTION.csv')
  # Chronic Obstructive Pulmonary Disease (COPD)
  COPD_Readmission <- Readmission_Reduction %>%
    filter(`Measure Name` == 'READM-30-COPD-HRRP') %>%
    select(`Provider Number`, `Excess Readmission Ratio`) %>%
    rename(`ProviderID` = `Provider Number`) %>%
    mutate(`ExcessReadmission` = `Excess Readmission Ratio`>1)
  COPD_Readmission$ExcessReadmission <- as.factor(COPD_Readmission$ExcessReadmission)

  # Hospital Consumer Assessment of Healthcare Providers and Systems survey #
  HCAHPS <- read_csv('readmission-app/data/HCAHPS - Hospital.csv')
  HCAHPS <- HCAHPS %>% 
    select(c(`Provider ID`, `HCAHPS Question`, `Patient Survey Star Rating`)) %>%
    filter(`Patient Survey Star Rating` != 'Not Applicable') %>% # Filter NA Values
    filter(`Patient Survey Star Rating` != 'Not Available') %>% 
    spread(`HCAHPS Question`,`Patient Survey Star Rating`) # Make rows become columns
  colnames(HCAHPS)<- c("ProviderID","CareTransition","Cleanliness","MedCommunication","DischargeInfo",
                       "DoctorCommunication","NurseCommunication", "OverallRating","PainManagement",
                       "Quietness","RecommendHospital","StaffResponsiveness","Summary")
  HCAHPS$CareTransition <- as.integer(HCAHPS$CareTransition)
  HCAHPS$Cleanliness <- as.integer(HCAHPS$Cleanliness)
  HCAHPS$DischargeInfo <- as.integer(HCAHPS$DischargeInfo)
  HCAHPS$MedCommunication <- as.integer(HCAHPS$MedCommunication)
  HCAHPS$DoctorCommunication <- as.integer(HCAHPS$DoctorCommunication)
  HCAHPS$NurseCommunication <- as.integer(HCAHPS$NurseCommunication)
  HCAHPS$PainManagement <- as.integer(HCAHPS$PainManagement)
  HCAHPS$Quietness <- as.integer(HCAHPS$Quietness)
  HCAHPS$StaffResponsiveness <- as.integer(HCAHPS$StaffResponsiveness)
  
  # Timely and Effectively Care #
  EffectiveCare <- read_csv('readmission-app/data/Timely and Effective Care - Hospital.csv')
  EffectiveCare <- EffectiveCare %>%
    filter(`Measure ID` == 'ED_1b' | `Measure ID` == 'ED_2b' | `Measure ID` == 'IMM_2' |
             `Measure ID` == 'OP_20') %>%
    select(`Provider ID`, `Measure ID`, Score) %>%
    spread(`Measure ID`, Score) %>%
    rename(ProviderID = `Provider ID`)
  EffectiveCare$ED_1b <- as.integer(EffectiveCare$ED_1b)
  EffectiveCare$ED_2b <- as.integer(EffectiveCare$ED_2b)
  EffectiveCare$IMM_2 <- as.integer(EffectiveCare$IMM_2)
  EffectiveCare$OP_20 <- as.integer(EffectiveCare$OP_20)
  
  # Spending Claim #
  SpendingClaim <- read_csv('readmission-app/data/Medicare Hospital Spending by Claim.csv')
  SpendingClaim$Avg_Spending_Per_Episode_Hospital <- as.integer(SpendingClaim$Avg_Spending_Per_Episode_Hospital)
  SpendingClaim$Avg_Spending_Per_Episode_Nation <- as.integer(SpendingClaim$Avg_Spending_Per_Episode_Nation)
  # Post Discharge
  DischargeSpending <- SpendingClaim %>%
    filter(Period == "1 through 30 days After Discharge from Index Hospital Admission") %>%
    mutate(diff = Avg_Spending_Per_Episode_Hospital / Avg_Spending_Per_Episode_Nation) %>%
    select(Provider_ID, Claim_Type, diff) %>%
    spread(Claim_Type, diff)
  colnames(DischargeSpending)<- c("ProviderID","Carrier","Equipment","HomeHealthAgency","Hospice",
                                  "Inpatient","Outpatient", "SkilledNursing")
  DischargeSpending$ProviderID <- as.character(DischargeSpending$ProviderID)
  
  #########################################################################
  ## Joining Data Set ##
  

  # HCAHPS and COPD Readmission
  HCAHPS_COPD_Readmission <- full_join(COPD_Readmission, HCAHPS, by = "ProviderID") %>%
    filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
    na.omit() 

    # Post Discharge Spending and COPD Readmission
  DischargeSpending_COPD_Readmission <- full_join(COPD_Readmission, DischargeSpending, by = "ProviderID") %>%
    filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
    na.omit() 

 
  # Effective Care and COPD Readmission
  EffectiveCare_COPD_Readmission <- full_join(COPD_Readmission, EffectiveCare, by = "ProviderID") %>%
    filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
    na.omit()
  EffectiveCare_COPD_Readmission$`Excess Readmission Ratio` <- as.numeric(EffectiveCare_COPD_Readmission$`Excess Readmission Ratio`)
  
  # All Feature Join COPD Readmission
  All_COPD_Readmission <- full_join(DischargeSpending_COPD_Readmission,EffectiveCare_COPD_Readmission, 
                                    by = "ProviderID") %>% 
    full_join(HCAHPS, by = "ProviderID" ) %>%
    na.omit() %>%
    select(-c( Inpatient, Outpatient, Carrier, `Excess Readmission Ratio.y`, ExcessReadmission.y, 
               OverallRating, RecommendHospital, Summary, IMM_2, OP_20))
  
  #########################################################################
  ## Decision Tree Modeling COPD##
  # shuffle the data and assign each row to one of 5 different folds
  fitCOPD <- rpart(ExcessReadmission.x ~ ED_1b + ED_2b +
                 CareTransition + Cleanliness + MedCommunication +
                 DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
                 StaffResponsiveness + Equipment + HomeHealthAgency + Hospice +
                 SkilledNursing,
               data = All_COPD_Readmission,
               method = "class",
               control = list(cp = 0, maxdepth = 3))
  
 
  if (predict(fitCOPD, dataframe, type = "class")==TRUE){
    output="Readmission rate will be excessive!"
  }else{output="Congratulations! Based on our prediction, your readmission rate will be non-excessive."}
  #output <- predict(fitHF, dataframe, type = "class")
  return(output)
}

Readmission_Tree_Pneumonia <- function(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15){
  
  dataframe <- data.frame(Equipment = f1/103, HomeHealthAgency = f2/816, Hospice = f3/122, SkilledNursing = f4/3319, 
                          ED_1b = f5, ED_2b = f6, CareTransition = f7, Cleanliness = f8, 
                          DischargeInfo = f9, MedCommunication = f10, DoctorCommunication = f11, 
                          NurseCommunication = f12, PainManagement = f13, Quietness = f14, 
                          StaffResponsiveness= f15)
  
  # Readmission Reduction (Readmission Outcome) #
  Readmission_Reduction <- read_csv('readmission-app/data/READMISSION REDUCTION.csv')
  # Pneumonia
  PN_Readmission <- Readmission_Reduction %>%
    filter(`Measure Name` == 'READM-30-PN-HRRP') %>%
    select(`Provider Number`, `Excess Readmission Ratio`) %>%
    rename(`ProviderID` = `Provider Number`) %>%
    mutate(`ExcessReadmission` = `Excess Readmission Ratio`>1)
  PN_Readmission$ExcessReadmission <- as.factor(PN_Readmission$ExcessReadmission)
  
  # Hospital Consumer Assessment of Healthcare Providers and Systems survey #
  HCAHPS <- read_csv('readmission-app/data/HCAHPS - Hospital.csv')
  HCAHPS <- HCAHPS %>% 
    select(c(`Provider ID`, `HCAHPS Question`, `Patient Survey Star Rating`)) %>%
    filter(`Patient Survey Star Rating` != 'Not Applicable') %>% # Filter NA Values
    filter(`Patient Survey Star Rating` != 'Not Available') %>% 
    spread(`HCAHPS Question`,`Patient Survey Star Rating`) # Make rows become columns
  colnames(HCAHPS)<- c("ProviderID","CareTransition","Cleanliness","MedCommunication","DischargeInfo",
                       "DoctorCommunication","NurseCommunication", "OverallRating","PainManagement",
                       "Quietness","RecommendHospital","StaffResponsiveness","Summary")
  HCAHPS$CareTransition <- as.integer(HCAHPS$CareTransition)
  HCAHPS$Cleanliness <- as.integer(HCAHPS$Cleanliness)
  HCAHPS$DischargeInfo <- as.integer(HCAHPS$DischargeInfo)
  HCAHPS$MedCommunication <- as.integer(HCAHPS$MedCommunication)
  HCAHPS$DoctorCommunication <- as.integer(HCAHPS$DoctorCommunication)
  HCAHPS$NurseCommunication <- as.integer(HCAHPS$NurseCommunication)
  HCAHPS$PainManagement <- as.integer(HCAHPS$PainManagement)
  HCAHPS$Quietness <- as.integer(HCAHPS$Quietness)
  HCAHPS$StaffResponsiveness <- as.integer(HCAHPS$StaffResponsiveness)
  
  # Timely and Effectively Care #
  EffectiveCare <- read_csv('readmission-app/data/Timely and Effective Care - Hospital.csv')
  EffectiveCare <- EffectiveCare %>%
    filter(`Measure ID` == 'ED_1b' | `Measure ID` == 'ED_2b' | `Measure ID` == 'IMM_2' |
             `Measure ID` == 'OP_20') %>%
    select(`Provider ID`, `Measure ID`, Score) %>%
    spread(`Measure ID`, Score) %>%
    rename(ProviderID = `Provider ID`)
  EffectiveCare$ED_1b <- as.integer(EffectiveCare$ED_1b)
  EffectiveCare$ED_2b <- as.integer(EffectiveCare$ED_2b)
  EffectiveCare$IMM_2 <- as.integer(EffectiveCare$IMM_2)
  EffectiveCare$OP_20 <- as.integer(EffectiveCare$OP_20)
  
  # Spending Claim #
  SpendingClaim <- read_csv('readmission-app/data/Medicare Hospital Spending by Claim.csv')
  SpendingClaim$Avg_Spending_Per_Episode_Hospital <- as.integer(SpendingClaim$Avg_Spending_Per_Episode_Hospital)
  SpendingClaim$Avg_Spending_Per_Episode_Nation <- as.integer(SpendingClaim$Avg_Spending_Per_Episode_Nation)
  # Post Discharge
  DischargeSpending <- SpendingClaim %>%
    filter(Period == "1 through 30 days After Discharge from Index Hospital Admission") %>%
    mutate(diff = Avg_Spending_Per_Episode_Hospital / Avg_Spending_Per_Episode_Nation) %>%
    select(Provider_ID, Claim_Type, diff) %>%
    spread(Claim_Type, diff)
  colnames(DischargeSpending)<- c("ProviderID","Carrier","Equipment","HomeHealthAgency","Hospice",
                                  "Inpatient","Outpatient", "SkilledNursing")
  DischargeSpending$ProviderID <- as.character(DischargeSpending$ProviderID)
  
  #########################################################################
  ## Joining Data Set ##
  
  # HCAHPS and Pneumonia Readmission
  HCAHPS_PN_Readmission <- full_join(PN_Readmission, HCAHPS, by = "ProviderID") %>%
    filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
    na.omit() 
  
   # Post Discharge Spending and PN Readmission
  DischargeSpending_PN_Readmission <- full_join(PN_Readmission, DischargeSpending, by = "ProviderID") %>%
    filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
    na.omit() 
  

  # Effective Care and PN Readmission
  EffectiveCare_PN_Readmission <- full_join(PN_Readmission, EffectiveCare, by = "ProviderID") %>%
    filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
    na.omit()
  EffectiveCare_PN_Readmission$`Excess Readmission Ratio` <- as.numeric(EffectiveCare_PN_Readmission$`Excess Readmission Ratio`)
  
  
  # All Feature Join PN Readmission
  All_PN_Readmission <- full_join(DischargeSpending_PN_Readmission,EffectiveCare_PN_Readmission, 
                                  by = "ProviderID") %>% 
    full_join(HCAHPS, by = "ProviderID" ) %>%
    na.omit() %>%
    select(-c( Inpatient, Outpatient, Carrier, `Excess Readmission Ratio.y`, ExcessReadmission.y, 
               OverallRating, RecommendHospital, Summary, IMM_2, OP_20))
  
  
  #########################################################################
  ## Decision Tree Modeling Pneumonia##
  
  fitPN <- rpart(ExcessReadmission.x ~ ED_1b + ED_2b +
                 CareTransition + Cleanliness + MedCommunication +
                 DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
                 StaffResponsiveness + Equipment + HomeHealthAgency + Hospice +
                 SkilledNursing,
               data = All_PN_Readmission,
               method = "class",
               control = list(cp = 0, maxdepth = 4))
  if (predict(fitPN, dataframe, type = "class")==TRUE){
    output="Readmission rate will be excessive!"
  }else{output="Congratulations! Based on our prediction, your readmission rate will be non-excessive."}
  #output <- predict(fitHF, dataframe, type = "class")
  return(output)
}
