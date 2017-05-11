# APMA 4990_001 Final Project - Excessive Hospital Readmission Analysis
# AUTHOR: CHENHUI HUANG, BING HAN
# Load Necessary Packages
library(Matrix)
library(glmnet)
library(ROCR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(maptools)
library(mapdata)
library(ggthemes)
library(tibble)
library(viridis)
library(mapproj)
library(party)
library(rpart)
library(stringr)
library(rpart.plot)
library(randomForest)
library(modelr)

##############################################################################################
## Data Input ##
# Hospital General Information #
Hospital_General_Info <- read_csv('Hospital General Information.csv')
Hospital_General_Info <- Hospital_General_Info %>%
  filter(`Hospital overall rating` != 'Not Available', !State %in% c('AK','HI','GU','PR')) %>%
  select (-c(`County Name`, `Address`, `ZIP Code`, `Phone Number`, `Emergency Services`, `Meets criteria for meaningful use of EHRs`, `Hospital overall rating footnote`, `Mortality national comparison footnote`, `Safety of care national comparison footnote`, `Readmission national comparison footnote`, `Patient experience national comparison footnote`, `Effectiveness of care national comparison footnote`, `Efficient use of medical imaging national comparison footnote`, `Timeliness of care national comparison footnote`))
Hospital_General_Rating <- Hospital_General_Info %>%
  select(`Provider ID`, `Hospital overall rating`)
colnames(Hospital_General_Rating) <- c("ProviderID", "RatingScore")
Hospital_General_Rating$RatingScore <- as.integer(Hospital_General_Rating$RatingScore)

# Readmission Reduction (Readmission Outcome) #
Readmission_Reduction <- read_csv('READMISSION REDUCTION.csv')
# Heart Failure
HF_Readmission <- Readmission_Reduction %>%
  filter(`Measure Name` == 'READM-30-HF-HRRP') %>%
  select(`Provider Number`, `Excess Readmission Ratio`) %>%
  rename(`ProviderID` = `Provider Number`) %>%
  mutate(`ExcessReadmission` = `Excess Readmission Ratio`>1)
HF_Readmission$ExcessReadmission <- as.factor(HF_Readmission$ExcessReadmission)
# Chronic Obstructive Pulmonary Disease (COPD)
COPD_Readmission <- Readmission_Reduction %>%
  filter(`Measure Name` == 'READM-30-COPD-HRRP') %>%
  select(`Provider Number`, `Excess Readmission Ratio`) %>%
  rename(`ProviderID` = `Provider Number`) %>%
  mutate(`ExcessReadmission` = `Excess Readmission Ratio`>1)
COPD_Readmission$ExcessReadmission <- as.factor(COPD_Readmission$ExcessReadmission)
# Pneumonia
PN_Readmission <- Readmission_Reduction %>%
  filter(`Measure Name` == 'READM-30-PN-HRRP') %>%
  select(`Provider Number`, `Excess Readmission Ratio`) %>%
  rename(`ProviderID` = `Provider Number`) %>%
  mutate(`ExcessReadmission` = `Excess Readmission Ratio`>1)
PN_Readmission$ExcessReadmission <- as.factor(PN_Readmission$ExcessReadmission)

# Hospital Consumer Assessment of Healthcare Providers and Systems survey #
HCAHPS <- read_csv('HCAHPS - Hospital.csv')
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
EffectiveCare <- read_csv('Timely and Effective Care - Hospital.csv')
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
SpendingClaim <- read_csv('Medicare Hospital Spending by Claim.csv')
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
# During Hospital Admission Index
InHospitalSpending <- SpendingClaim %>%
  filter(Period == "During Index Hospital Admission") %>%
  mutate(diff = Avg_Spending_Per_Episode_Hospital / Avg_Spending_Per_Episode_Nation) %>%
  select(Provider_ID, Claim_Type, diff) %>%
  spread(Claim_Type, diff)
colnames(InHospitalSpending)<- c("ProviderID","Carrier_In","Equipment_In","HomeHealthAgency","Hospice",
                                 "Inpatient_In","Outpatient", "SkilledNursing")
InHospitalSpending <- InHospitalSpending %>%
  select(ProviderID, Carrier_In, Equipment_In, Inpatient_In)
InHospitalSpending$ProviderID <- as.character(InHospitalSpending$ProviderID)

# Payment #
Payment <- read_csv('Payment and Value of Care - Hospital.csv')
# Heart Failure
HF_Pay <- Payment %>%
  filter(`Payment measure name` == 'Payment for heart failure patients') %>%
  select(`Provider ID`, `Payment`) %>%
  rename(ProviderID = `Provider ID`)
HF_Pay$Payment <- gsub('$', '', HF_Pay$Payment, fixed = TRUE)
HF_Pay$Payment <- gsub(',', '', HF_Pay$Payment, fixed = TRUE)
HF_Pay$Payment <- as.integer(HF_Pay$Payment)
# Pneumonia
PN_Pay <- Payment %>%
  filter(`Payment measure name` == 'Payment for pneumonia patients') %>%
  select(`Provider ID`, `Payment`) %>%
  rename(ProviderID = `Provider ID`)
PN_Pay$Payment <- gsub('$', '', PN_Pay$Payment, fixed = TRUE)
PN_Pay$Payment <- gsub(',', '', PN_Pay$Payment, fixed = TRUE)
PN_Pay$Payment <- as.integer(PN_Pay$Payment)

# Mortality #
Mortality <- read_csv('Readmissions and Deaths - Hospital.csv')
# Heart Failure
HF_Mortality <- Mortality %>%
  filter(`Measure ID` == 'MORT_30_HF' | `Measure ID` == 'READM_30_HF') %>%
  select(`Provider ID`, `Measure ID`, Score) %>%
  spread(`Measure ID`, Score) %>%
  rename(ProviderID = `Provider ID`)
HF_Mortality$MORT_30_HF <- as.numeric(HF_Mortality$MORT_30_HF)
HF_Mortality$READM_30_HF <- as.numeric(HF_Mortality$READM_30_HF)
HF_Mortality <- HF_Mortality %>% na.omit()
# COPD
COPD_Mortality <- Mortality %>%
  filter(`Measure ID` == 'MORT_30_COPD' | `Measure ID` == 'READM_30_COPD') %>%
  select(`Provider ID`, `Measure ID`, Score) %>%
  spread(`Measure ID`, Score) %>%
  rename(ProviderID = `Provider ID`)
COPD_Mortality$MORT_30_COPD <- as.numeric(COPD_Mortality$MORT_30_COPD)
COPD_Mortality$READM_30_COPD <- as.numeric(COPD_Mortality$READM_30_COPD)
COPD_Mortality <- COPD_Mortality %>% na.omit()
# Pneumonia
PN_Mortality <- Mortality %>%
  filter(`Measure ID` == 'MORT_30_PN' | `Measure ID` == 'READM_30_PN') %>%
  select(`Provider ID`, `Measure ID`, Score) %>%
  spread(`Measure ID`, Score) %>%
  rename(ProviderID = `Provider ID`)
PN_Mortality$MORT_30_PN <- as.numeric(PN_Mortality$MORT_30_PN)
PN_Mortality$READM_30_PN <- as.numeric(PN_Mortality$READM_30_PN)
PN_Mortality <- PN_Mortality %>% na.omit()

# Structural Measures #
Structural_Measure <- read_csv('Structural Measures - Hospital.csv')
Structural_Measure <- Structural_Measure %>%
  select(`Provider ID`, `Measure Name`, `Measure Response`) %>%
  filter(`Measure Response` != 'Not Available') %>%
  spread(`Measure Name`, `Measure Response`)
Structural_Measure <- Structural_Measure %>% select(-`Multispecialty surgical registry`) %>%
  mutate(`Electronic Lab Report` = `Able to receive lab results electronically`=='Yes',
         `Electronic Tracking` = `able to track patients  lab results, tests, and referrals electronically between visits` == 'Yes',
         `Safe Inpatient Surgery Checklist` = `Safe surgery checklist use (inpatient)`=='Y',
         `Safe Outpatient Surgery Checklist` = `Safe surgery checklist use (outpatient)`=='Yes',
         `General Surgery Registry` = `General Surgery Registry`=='Y',
         `Nursing Care Registry` = `Nursing care registry` == 'Y') %>%
  select(c(`Provider ID`,`Electronic Lab Report`,`Electronic Tracking`,`Safe Inpatient Surgery Checklist`,`Safe Outpatient Surgery Checklist`,
           `General Surgery Registry`, `Nursing Care Registry`))
colnames(Structural_Measure) <- c("ProviderID", "ElectronicLabReport", "ElectronicTracking",
                                  "SafeInpatientChecklist", "SafeOutpatientChecklist",
                                  "GeneralSurgeryRegistry", "NursingCareRegistry")
Structural_Measure$`ProviderID` <- as.character(Structural_Measure$`ProviderID`)

#########################################################################
## Joining Data Set ##

# HCAHPS and Heart Failure Readmission
HCAHPS_HF_Readmission <- full_join(HF_Readmission, HCAHPS, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
# HCAHPS and COPD Readmission
HCAHPS_COPD_Readmission <- full_join(COPD_Readmission, HCAHPS, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
# HCAHPS and Pneumonia Readmission
HCAHPS_PN_Readmission <- full_join(PN_Readmission, HCAHPS, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 

# Post Discharge Spending and HF Readmission
DischargeSpending_HF_Readmission <- full_join(HF_Readmission, DischargeSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
DischargeSpending_HF_Readmission$`Excess Readmission Ratio` <- as.numeric(DischargeSpending_HF_Readmission$`Excess Readmission Ratio`)
# Post Discharge Spending and COPD Readmission
DischargeSpending_COPD_Readmission <- full_join(COPD_Readmission, DischargeSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
# Post Discharge Spending and PN Readmission
DischargeSpending_PN_Readmission <- full_join(PN_Readmission, DischargeSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 

# In Hospital Spending and HF Readmission
InHospitalSpending_HF_Readmission <- full_join(HF_Readmission, InHospitalSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
# In Hospital Spending and COPD Readmission
InHospitalSpending_COPD_Readmission <- full_join(COPD_Readmission, InHospitalSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 
# In Hospital Spending and PN Readmission
InHospitalSpending_PN_Readmission <- full_join(PN_Readmission, InHospitalSpending, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit() 

# Payment and HF Readmission
Payment_HF_Readmission <- full_join(HF_Readmission, HF_Pay, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()
Payment_HF_Readmission$`Excess Readmission Ratio` <- as.numeric(Payment_HF_Readmission$`Excess Readmission Ratio`)
# Payment and PN Readmission
Payment_PN_Readmission <- full_join(PN_Readmission, PN_Pay, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()
Payment_PN_Readmission$`Excess Readmission Ratio` <- as.numeric(Payment_PN_Readmission$`Excess Readmission Ratio`)

# Effective Care and HF Readmission
EffectiveCare_HF_Readmission <- full_join(HF_Readmission, EffectiveCare, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()
EffectiveCare_HF_Readmission$`Excess Readmission Ratio` <- as.numeric(EffectiveCare_HF_Readmission$`Excess Readmission Ratio`)
# Effective Care and COPD Readmission
EffectiveCare_COPD_Readmission <- full_join(COPD_Readmission, EffectiveCare, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()
EffectiveCare_COPD_Readmission$`Excess Readmission Ratio` <- as.numeric(EffectiveCare_COPD_Readmission$`Excess Readmission Ratio`)
# Effective Care and PN Readmission
EffectiveCare_PN_Readmission <- full_join(PN_Readmission, EffectiveCare, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available' & `Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()
EffectiveCare_PN_Readmission$`Excess Readmission Ratio` <- as.numeric(EffectiveCare_PN_Readmission$`Excess Readmission Ratio`)

# Structural Meausres
Structural_HF_Readmission <- full_join(HF_Readmission, Structural_Measure, by = "ProviderID") %>%
  filter(`Excess Readmission Ratio` != 'Not Available') %>%
  na.omit()

# All Feature Join HF Readmission
All_HF_Readmission <- full_join(DischargeSpending_HF_Readmission,InHospitalSpending_HF_Readmission, 
                                by = "ProviderID") %>% 
  full_join(HCAHPS, by = "ProviderID" ) %>%
  full_join(EffectiveCare_HF_Readmission, by = "ProviderID") %>%
  na.omit() %>%
  select(-c(`Excess Readmission Ratio.x`, ExcessReadmission.x,`Excess Readmission Ratio.y`, ExcessReadmission.y))

# All Feature Join COPD Readmission
All_COPD_Readmission <- full_join(DischargeSpending_COPD_Readmission,InHospitalSpending_COPD_Readmission, 
                                  by = "ProviderID") %>% 
  full_join(HCAHPS, by = "ProviderID" ) %>%
  full_join(EffectiveCare_COPD_Readmission, by = "ProviderID") %>%
  na.omit() %>%
  select(-c(`Excess Readmission Ratio.x`, ExcessReadmission.x,`Excess Readmission Ratio.y`, ExcessReadmission.y))

# All Feature Join PN Readmission
All_PN_Readmission <- full_join(DischargeSpending_PN_Readmission,InHospitalSpending_PN_Readmission, 
                                by = "ProviderID") %>% 
  full_join(HCAHPS, by = "ProviderID" ) %>%
  full_join(EffectiveCare_PN_Readmission, by = "ProviderID") %>%
  na.omit() %>%
  select(-c(`Excess Readmission Ratio.x`, ExcessReadmission.x,`Excess Readmission Ratio.y`, ExcessReadmission.y))

# Hospital Genreal Rating
Rating_HF_Readmission <- full_join(HF_Readmission, Hospital_General_Rating, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available') %>%
  na.omit()

Rating_COPD_Readmission <- full_join(COPD_Readmission, Hospital_General_Rating, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available') %>%
  na.omit()

Rating_PN_Readmission <- full_join(PN_Readmission, Hospital_General_Rating, by = "ProviderID") %>%
  filter(ExcessReadmission != 'Not Available') %>%
  na.omit()

#########################################################################
## Exploratory Data Analysis ##

# Heart Failure
# HCAHPAS
ggplot(HCAHPS_HF_Readmission, aes(CareTransition)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Care Transition')
ggplot(HCAHPS_HF_Readmission, aes(MedCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Communication about Medication')
ggplot(HCAHPS_HF_Readmission, aes(StaffResponsiveness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Staff Responsiveness')
ggplot(HCAHPS_HF_Readmission, aes(Quietness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') 
ggplot(HCAHPS_HF_Readmission, aes(NurseCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_HF_Readmission, aes(DischargeInfo)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Discharge Information')
ggplot(HCAHPS_HF_Readmission, aes(PainManagement)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_HF_Readmission, aes(DoctorCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_HF_Readmission, aes(Cleanliness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')

# Structural Measures
ggplot(Structural_HF_Readmission, aes(`ElectronicLabReport`)) +
  geom_bar(aes(fill = ExcessReadmission), position = "dodge")
ggplot(Structural_HF_Readmission, aes(`ElectronicTracking`)) +
  geom_bar(aes(fill = ExcessReadmission), position = "dodge")
ggplot(Structural_HF_Readmission, aes(`SafeInpatientChecklist`)) +
  geom_bar(aes(fill = ExcessReadmission), position = "dodge")

# Payment
ggplot(Payment_HF_Readmission, aes(x=Payment, fill=ExcessReadmission)) +
  geom_histogram(binwidth=1000, position="dodge")
ggplot(Payment_HF_Readmission, aes(x = Payment, y = `Excess Readmission Ratio`)) +
  geom_point()+
  geom_smooth(method = "lm")
lmfit <- lm(`Excess Readmission Ratio` ~ Payment,
            data = Payment_HF_Readmission)
pred_actual <- Payment_HF_Readmission %>%
  add_predictions(lmfit) %>%
  mutate(actual = `Excess Readmission Ratio`)
pred_actual %>%
  summarize(rmse = sqrt(mean((pred - actual)^2)),
            cor = cor(pred, actual),
            cor_sq = cor^2)

# Effective Care
ggplot(EffectiveCare_HF_Readmission, aes(x = ED_1b, y = `Excess Readmission Ratio`)) +
  geom_bin2d(bins = 40) +
  coord_cartesian(ylim = c(0.7, 0.8, 1, 1.2, 1.5))
ggplot(EffectiveCare_HF_Readmission, aes(x = OP_20, y = `Excess Readmission Ratio`)) +
  geom_bin2d(bins = 40) +
  coord_cartesian(ylim = c(0.7, 0.8, 1, 1.2, 1.5))

# Discharge Spending
ggplot(DischargeSpending_HF_Readmission, aes(x = ExcessReadmission, y = SkilledNursing)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab('Excess Readmission') +
  ylab('Skilled Nursing')
ggplot(DischargeSpending_HF_Readmission, aes(x = ExcessReadmission, y = Hospice)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab('Excess Readmission')

# Mortality
HF_Mortality %>% na.omit()%>%
  ggplot(aes(x = READM_30_HF, y = MORT_30_HF)) +
  geom_bin2d(bins = 40) +
  xlab('30 Day Readmission Rates') +
  ylab('30 Day Mortality Rates')
mean(HF_Mortality$READM_30_HF)
mean(HF_Mortality$MORT_30_HF)

# General Rating
ggplot(Rating_HF_Readmission, aes(RatingScore)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  ggtitle('HF Excessive Readmission vs. General Hospital Rating')

# COPD #
# HCAHPAS
ggplot(HCAHPS_COPD_Readmission, aes(CareTransition)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Care Transition')
ggplot(HCAHPS_COPD_Readmission, aes(MedCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Communication about Medication')
ggplot(HCAHPS_COPD_Readmission, aes(StaffResponsiveness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Staff Responsiveness')
ggplot(HCAHPS_COPD_Readmission, aes(Quietness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') 
ggplot(HCAHPS_COPD_Readmission, aes(NurseCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_COPD_Readmission, aes(DischargeInfo)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Discharge Information')
ggplot(HCAHPS_COPD_Readmission, aes(PainManagement)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_COPD_Readmission, aes(DoctorCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_COPD_Readmission, aes(Cleanliness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')

# Effective Care
ggplot(EffectiveCare_COPD_Readmission, aes(x = ED_1b, y = `Excess Readmission Ratio`)) +
  geom_bin2d(bins = 40) +
  coord_cartesian(ylim = c(0.7, 0.8, 1, 1.2, 1.5))
ggplot(EffectiveCare_COPD_Readmission, aes(x = OP_20, y = `Excess Readmission Ratio`)) +
  geom_bin2d(bins = 40)+
  coord_cartesian(ylim = c(0.7, 0.8, 1, 1.2, 1.5))

# Discharge Spending
ggplot(DischargeSpending_COPD_Readmission, aes(x = ExcessReadmission, y = SkilledNursing)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab('Excess Readmission') +
  ylab('Skilled Nursing')
ggplot(DischargeSpending_COPD_Readmission, aes(x = ExcessReadmission, y = Hospice)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab('Excess Readmission')

# Mortality
COPD_Mortality %>% na.omit()%>%
  ggplot(aes(x = READM_30_COPD, y = MORT_30_COPD)) +
  geom_bin2d(bins = 40) +
  xlab('30 Day Readmission Rates') +
  ylab('30 Day Mortality Rates')
mean(COPD_Mortality$READM_30_COPD)
mean(COPD_Mortality$MORT_30_COPD)

# General Rating
ggplot(Rating_COPD_Readmission, aes(RatingScore)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  ggtitle('COPD Excessive Readmission vs. General Hospital Rating')


# Pneumonia #
# HCAHPAS
ggplot(HCAHPS_PN_Readmission, aes(CareTransition)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Care Transition')
ggplot(HCAHPS_PN_Readmission, aes(MedCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Communication about Medication')
ggplot(HCAHPS_PN_Readmission, aes(StaffResponsiveness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Staff Responsiveness')
ggplot(HCAHPS_PN_Readmission, aes(Quietness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') 
ggplot(HCAHPS_PN_Readmission, aes(NurseCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_PN_Readmission, aes(DischargeInfo)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  xlab('Discharge Information')
ggplot(HCAHPS_PN_Readmission, aes(PainManagement)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_PN_Readmission, aes(DoctorCommunication)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')
ggplot(HCAHPS_PN_Readmission, aes(Cleanliness)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts')

# Effective Care
ggplot(EffectiveCare_PN_Readmission, aes(x = ED_1b, y = `Excess Readmission Ratio`)) +
  geom_bin2d(bins = 40) +
  coord_cartesian(ylim = c(0.7, 0.8, 1, 1.2, 1.5))
ggplot(EffectiveCare_PN_Readmission, aes(x = OP_20, y = `Excess Readmission Ratio`)) +
  geom_bin2d(bins = 40)+
  coord_cartesian(ylim = c(0.7, 0.8, 1, 1.2, 1.5))

# Discharge Spending
ggplot(DischargeSpending_PN_Readmission, aes(x = ExcessReadmission, y = SkilledNursing)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab('Excess Readmission') +
  ylab('Skilled Nursing')
ggplot(DischargeSpending_PN_Readmission, aes(x = ExcessReadmission, y = Hospice)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab('Excess Readmission')

# Mortality
PN_Mortality %>% 
  ggplot(aes(x = READM_30_PN, y = MORT_30_PN)) +
  geom_bin2d(bins = 40) +
  xlab('30 Day Readmission Rates') +
  ylab('30 Day Mortality Rates')
mean(PN_Mortality$READM_30_PN)
mean(PN_Mortality$MORT_30_PN)

# General Rating
ggplot(Rating_PN_Readmission, aes(RatingScore)) +
  geom_bar(aes(fill = ExcessReadmission),position = "dodge") +
  ylab('Hospital Counts') +
  ggtitle('Pneumonia Excessive Readmission vs. General Hospital Rating')

#########################################################################
## Decision Tree Modeling ##
# Heart Failure
# shuffle the data and assign each row to one of 5 different folds
set.seed(33)
num_folds <- 5
num_data <- nrow(All_HF_Readmission)

ndx <- sample(1:num_data, num_data, replace=F)
All_HF_Readmission <- All_HF_Readmission[ndx, ] %>%
  mutate(fold = (row_number() %% num_folds) + 1)

# implement 5-fold cross-validation to compute the average train / test error (Misclassificaiton Error)
for (f in 1:num_folds) {
  # fit on the training data
  HF_train <- filter(All_HF_Readmission, fold != f)
  
  # evaluate on the validation data
  HF_validate <- filter(All_HF_Readmission, fold == f)
}

K <- 1:7
train_err <- c()
validate_err <- c()
avg_train_err <- c()
avg_validate_err <- c()

for (k in K) {
  # do 5-fold cross-validation within each value of k
  for (f in 1:num_folds) {
    # fit on the training data
    HF_train <- filter(All_HF_Readmission, fold != f)
    fit <- rpart(ExcessReadmission ~ ED_1b + ED_2b + IMM_2 + OP_20 + 
                   CareTransition + Cleanliness + MedCommunication +
                   DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
                   StaffResponsiveness + Carrier + Equipment + HomeHealthAgency + Hospice +
                   Inpatient + Outpatient + SkilledNursing,
                 data = HF_train,
                 method = "class",
                 control = list(cp = 0, maxdepth = k))
    
    # evaluate on the validation data
    HF_validate <- filter(All_HF_Readmission, fold == f)
    predTrain <- predict(fit, newdata = HF_train, type = "class")
    predValidate <- predict(fit, newdata = HF_validate, type = "class")
    mcTrain <- table(HF_train$ExcessReadmission, predTrain)
    mcValidate <- table(HF_validate$ExcessReadmission, predValidate)
    train_err[f] <- 1 - (mcTrain[1,1]+mcTrain[2,2])/sum(mcTrain)
    validate_err[f] <- 1 - (mcValidate[1,1]+mcValidate[2,2])/sum(mcValidate)
  }
  
  # compute the average validation error across folds
  # and the standard error on this estimate
  avg_train_err[k] <- mean(train_err)
  avg_validate_err[k] <- mean(validate_err)
}

# make a plot showing the how the train and test error vary with the tree depth
plot_data <- data.frame(K, avg_train_err, avg_validate_err) %>%
  gather("split", "error", -K)

ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Tree Depth') +
  ylab('Classification Error Rate')

# Use the optimal tree depth to build model
fit <- rpart(ExcessReadmission ~ ED_1b + ED_2b + IMM_2 + OP_20 +
               CareTransition + Cleanliness + MedCommunication +
               DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
               StaffResponsiveness + Equipment + HomeHealthAgency + Hospice +
               SkilledNursing,
             data = All_HF_Readmission,
             method = "class",
             control = list(cp = 0, maxdepth = 2))
prp(fit, type = 4, extra = 2, fallen.leaves = TRUE, varlen = 0)
summary(fit)

#########################################################################
## Decision Tree Modeling COPD##
# shuffle the data and assign each row to one of 5 different folds
set.seed(33)
num_folds <- 5
num_data <- nrow(All_COPD_Readmission)

ndx <- sample(1:num_data, num_data, replace=F)
All_COPD_Readmission <- All_COPD_Readmission[ndx, ] %>%
  mutate(fold = (row_number() %% num_folds) + 1)

# implement 5-fold cross-validation to compute the average train / test error (Misclassificaiton Error)
for (f in 1:num_folds) {
  # fit on the training data
  HF_train <- filter(All_COPD_Readmission, fold != f)
  
  # evaluate on the validation data
  HF_validate <- filter(All_COPD_Readmission, fold == f)
}

K <- 1:7
train_err <- c()
validate_err <- c()
avg_train_err <- c()
avg_validate_err <- c()

for (k in K) {
  # do 5-fold cross-validation within each value of k
  for (f in 1:num_folds) {
    # fit on the training data
    HF_train <- filter(All_COPD_Readmission, fold != f)
    fit <- rpart(ExcessReadmission ~ ED_1b + ED_2b + IMM_2 + OP_20 + 
                   CareTransition + Cleanliness + MedCommunication +
                   DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
                   StaffResponsiveness + Carrier + Equipment + HomeHealthAgency + Hospice +
                   Inpatient + Outpatient + SkilledNursing,
                 data = HF_train,
                 method = "class",
                 control = list(cp = 0, maxdepth = k))
    
    # evaluate on the validation data
    HF_validate <- filter(All_COPD_Readmission, fold == f)
    predTrain <- predict(fit, newdata = HF_train, type = "class")
    predValidate <- predict(fit, newdata = HF_validate, type = "class")
    mcTrain <- table(HF_train$ExcessReadmission, predTrain)
    mcValidate <- table(HF_validate$ExcessReadmission, predValidate)
    train_err[f] <- 1 - (mcTrain[1,1]+mcTrain[2,2])/sum(mcTrain)
    validate_err[f] <- 1 - (mcValidate[1,1]+mcValidate[2,2])/sum(mcValidate)
  }
  
  # compute the average validation error across folds
  # and the standard error on this estimate
  avg_train_err[k] <- mean(train_err)
  avg_validate_err[k] <- mean(validate_err)
}

# make a plot showing the how the train and test error vary with the tree depth
plot_data <- data.frame(K, avg_train_err, avg_validate_err) %>%
  gather("split", "error", -K)

ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Tree Depth') +
  ylab('Classification Error Rate')

fit <- rpart(ExcessReadmission ~ ED_1b + ED_2b + IMM_2 + OP_20 +
               CareTransition + Cleanliness + MedCommunication +
               DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
               StaffResponsiveness + Equipment + HomeHealthAgency + Hospice +
               SkilledNursing,
             data = All_COPD_Readmission,
             method = "class",
             control = list(cp = 0, maxdepth = 3))
prp(fit, type = 4, extra = 2, fallen.leaves = TRUE, varlen = 0)

#########################################################################
## Decision Tree Modeling Pneumonia##
set.seed(33)
num_folds <- 5
num_data <- nrow(All_PN_Readmission)

ndx <- sample(1:num_data, num_data, replace=F)
All_PN_Readmission <- All_PN_Readmission[ndx, ] %>%
  mutate(fold = (row_number() %% num_folds) + 1)

# implement 5-fold cross-validation to compute the average train / test error (Misclassificaiton Error)
for (f in 1:num_folds) {
  # fit on the training data
  HF_train <- filter(All_PN_Readmission, fold != f)
  
  # evaluate on the validation data
  HF_validate <- filter(All_PN_Readmission, fold == f)
}

K <- 1:7
train_err <- c()
validate_err <- c()
avg_train_err <- c()
avg_validate_err <- c()

for (k in K) {
  # do 5-fold cross-validation within each value of k
  for (f in 1:num_folds) {
    # fit on the training data
    HF_train <- filter(All_PN_Readmission, fold != f)
    fit <- rpart(ExcessReadmission ~ ED_1b + ED_2b + IMM_2 + OP_20 + 
                   CareTransition + Cleanliness + MedCommunication +
                   DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
                   StaffResponsiveness + Carrier + Equipment + HomeHealthAgency + Hospice +
                   Inpatient + Outpatient + SkilledNursing,
                 data = HF_train,
                 method = "class",
                 control = list(cp = 0, maxdepth = k))
    
    # evaluate on the validation data
    HF_validate <- filter(All_PN_Readmission, fold == f)
    predTrain <- predict(fit, newdata = HF_train, type = "class")
    predValidate <- predict(fit, newdata = HF_validate, type = "class")
    mcTrain <- table(HF_train$ExcessReadmission, predTrain)
    mcValidate <- table(HF_validate$ExcessReadmission, predValidate)
    train_err[f] <- 1 - (mcTrain[1,1]+mcTrain[2,2])/sum(mcTrain)
    validate_err[f] <- 1 - (mcValidate[1,1]+mcValidate[2,2])/sum(mcValidate)
  }
  
  # compute the average validation error across folds
  # and the standard error on this estimate
  avg_train_err[k] <- mean(train_err)
  avg_validate_err[k] <- mean(validate_err)
}

# make a plot showing the how the train and test error vary with the tree depth
plot_data <- data.frame(K, avg_train_err, avg_validate_err) %>%
  gather("split", "error", -K)

ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Tree Depth') +
  ylab('Classification Error Rate')

fit <- rpart(ExcessReadmission ~ ED_1b + ED_2b + 
               CareTransition + Cleanliness + MedCommunication +
               DischargeInfo + DoctorCommunication + NurseCommunication + PainManagement  + 
               StaffResponsiveness + Equipment + HomeHealthAgency + Hospice +
               SkilledNursing,
             data = All_PN_Readmission,
             method = "class",
             control = list(cp = 0, maxdepth = 4))
prp(fit, type = 4, extra = 2, fallen.leaves = TRUE, varlen = 0)

#######################################################################################################
# Exploratory data analysis for fun
# Average Hospital Ratings by State
Hospital_Rating <- Hospital_General_Info %>%
  arrange(desc(`Hospital overall rating`)) %>% 
  group_by(State, `Hospital overall rating`) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate (rate = total * as.numeric(`Hospital overall rating`)) %>%
  group_by(State) %>%
  summarise(meanR = sum(rate)/sum(total))

us <- map_data("state")
value <- c(3.050633,2.945455,2.836364, 2.841379, 3.450980, 2.714286, 3.6667, 1.4285, 2.658824, 2.936364, 3.536416, 3.132075, 3.481481, 3.394366, 3.344828, 2.975904, 3.125, 3.4375, NA, 3.192982, 3.191304, 3.531646, 2.7778, 3.011628, 3.106383, 3.285714, 2.423077, 3.48, 2.546875, 2.71, 2.311688, 3.106383, 3.1875, 3.379571, 3.144578, 3.153846, 3.006757, 3.272727, 3.207547, 3.857143, 2.873684, 3.233083, 3.448276, 3.25, 2, 3.015152, 2.756757, 3.6111,3)
choro_dat <- data_frame(some_other_name=unique(us$region),
                        Hospital_Ratings=value)

gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(long, lat, map_id=region),
                    color="#2b2b2b", fill=NA, size=0.15)
gg <- gg + geom_map(data=choro_dat, map=us,
                    aes(fill=Hospital_Ratings,
                        map_id=some_other_name),
                    color="white", size=0.15)
+ scale_fill_viridis(name="Value")
+ coord_map("polyconic")
+ theme_map()
+ theme(plot.margin=margin(20,20,20,20))
+ theme(legend.position=c(0.85, 0.2))
gg

