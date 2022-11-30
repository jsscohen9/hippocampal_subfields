# Data wrangling

library(tidyverse)

# Read in data
## All clinical LBD cases + age/sex matched controls
clinical <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery_Output_2022.07.12_09.59_.xlsx")

# Remove INDDID w/ decimal 
clinical <- clinical[-grepl("118913", clinical$INDDID),]

## how many subjects are cases vs controls
### For those with no ClinicalPhenotype1, do they have a duplicate record w/ value for ClinicalPhenotype1?
#finds unique IDs w/o ClinicalPhenotype1

clinical %>% 
  filter(is.na(ClinicalPhenotype1)) %>%
  select(INDDID) %>%
  unique() -> x

# finds unique IDs w/ ClinicalPhenotype1
clinical %>%
  filter(!is.na(ClinicalPhenotype1)) %>%
  select(INDDID) %>%
  unique() -> y 

# shows any elements that exist in both
intersect(x$INDDID, y$INDDID)  

remove(x, y)
# select rows with motor dx but no "clinical phenotype", create equivalent label (DLB, DLB-MCI, Normal, PD, PD-MCI, PDD)
df <- clinical %>% filter(is.na(ClinicalPhenotype1))

## look to see if any conflict between dx for the two

df1 <- df %>% 
  group_by(INDDID) %>%     # first group by INDDID
  mutate(MotorDx_sum = str_c(unique(MotorDx1), collapse="/")) %>% #if any conflict in dx by INDDID, MotorDx_sum will display combined dx
  ungroup()

table(df1$MotorDx_sum)

# confirm that all CognitiveDx are consistent by INDDID
df1 <- df %>% 
  group_by(INDDID) %>% 
  mutate(CognitiveDx_sum = str_c(unique(CognitiveDx), collapse = "/")) %>% 
  ungroup()

table(df1$CognitiveDx_sum) #(there are not any combined Dementia/MCI)

remove(df1)
# confirm that no conflicts between clinical phenotypes
## Select those "clinical phenotype", and ensure no conflicts between duplicates
clinical %>% 
  filter(!is.na(ClinicalPhenotype1)) %>% 
  group_by(INDDID) %>%     # first group by INDDID
  mutate(ClinicalPhenotype_sum = str_c(unique(ClinicalPhenotype1), collapse="/")) %>% #if any conflict in dx by INDDID, ClinicalPhenotype_sum will display combined dx
  ungroup()


# assign MotorDx to ClinicalPhenotype_Sum
df_motor <- df %>% group_by(INDDID) %>% 
  mutate(ClinicalPhenotype_sum = MotorDx1) %>% 
  ungroup()
  
table(df_motor$ClinicalPhenotype_sum)

# assign ClinicalPhenotype1 to ClinicalPhenotype_sum
clinical %>%
  filter(!is.na(ClinicalPhenotype1)) %>% 
  mutate(ClinicalPhenotype_sum = ClinicalPhenotype1) -> df_clinical

## combine df_motor and df_clinical 
bind_rows(df_clinical, df_motor) -> clinical2

remove(clinical)


#fill in missing ClinicalPhenotype_sum with value
clinical3 <- clinical2 %>%
  select(-c(FlywheelProjectLabel, FlywheelSessionURL, ClinicalPhenotype1, MotorDx1, MotorDx2, CognitiveDx, AgeatMRI)) %>% 
  select(INDDID, ClinicalPhenotype_sum, everything()) %>%  
  arrange((INDDID)) %>% 
    group_by(INDDID) %>% 
    mutate(ClinicalPhenotype_sum = str_c(unique(ClinicalPhenotype_sum), collapse="")) %>% #if any conflict in dx by INDDID, ClinicalPhenotype_sum will display combined dx
    ungroup()
    
remove(clinical2)
# how many cases vs controls
clinical4 <- clinical3 %>%
  mutate(Date = as.Date(FlywheelSessionTimestampUTC)) %>% 
  group_by(INDDID) %>% 
  filter(Date == max(Date)) %>%
    slice(1) %>% #takes first occurrence if there is a tie
    ungroup() %>% 
  mutate(INDDID = as.character(INDDID))

  table(clinical4$ClinicalPhenotype_sum == "Normal") #normal are controls

  remove(clinical3)
  
# condense dx groups as new variable in clinical4 
clinical4$ClinicalDx_sum <- recode(clinical4$ClinicalPhenotype_sum, 
                                   `Parkinson Disease` = "PD", 
                                   `DLB-MCI` = "DLB", `PD-MCI` = "PD") 



## ASHS results 
# pull out INDDID and date of session to then query INDD for biomarkers (CSF, serum and PET)
ASHS <- read_csv("~/R_LBD Hippocampal subfields/Data/pd_ashs_volumes.csv") 

# distinct INDDID w/ ASHS output
ashs_inddid <- ASHS %>%
  distinct(INDDID)

write_csv(ashs_inddid, "~/R_LBD Hippocampal subfields/Data/ashs_inddid.csv")

# determine total successful ASHS sessions
ashs_dates <- ASHS %>% 
  select(INDDID,FlywheelSessionTimestampUTC, hemisphere, region, volume, Pipeline, nifti) %>% 
  group_by(INDDID) %>% 
  mutate(date = as.Date(FlywheelSessionTimestampUTC)) %>% 
  distinct(date) %>% 
  ungroup() 


# Table MRI sessions and dx 
left_join(ashs_inddid, clinical4, by = "INDDID") -> df 

table(df$ClinicalDx_sum)
  
# Combine ASHS output w/ clinical, PET, and CSF/plasma biomarkers to see 
# how many cases have info to determine presence of copathology. 
# ASHS file already includes autopsy data 

biomarkers <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.08.10 10.23)_pet_biomarkers.xlsx") 

biomarkers <- biomarkers %>% 
  mutate(INDDID = as.character(INDDID)) #convert INDDID to character for join

clinical4 <- clinical4 %>% 
  mutate(INDDID = as.character(INDDID)) #convert INDDID to character for join

#df with all data
df_all <-
  left_join(biomarkers, ASHS, by = "INDDID") %>% 
  left_join(clinical4, by = "INDDID")


#remove any controls with positive PET
controls_remove <- df_all %>% filter(ClinicalPhenotype_sum == "Normal") %>% 
  select(INDDID, Clinical_Read) %>% 
  unique() %>% 
  filter(Clinical_Read == "Positive") %>% 
  select(INDDID)

df_all <- 
  df_all %>% filter(!INDDID %in% controls_remove$INDDID)

#remove any controls with positive CSF or plasma

# Table 1 -----------------------------------------------------------------

# create new variables to say whether has autopsy, biomarker

df_all$AD_marker[!is.na(df_all$`Plasmap-Tau181 (pg/mL)`)] <- "plasma"
df_all$AD_marker[!is.na(df_all$Abeta42)] <- "CSF"
df_all$AD_marker[df_all$PETTracer == "Florbetaben (amyloid)" | 
                   df_all$PETTracer == "Florbetapir (amyloid)"] <- "PET"
df_all$AD_marker[!is.na(df_all$AutopsyDate)] <- "autopsy"
 
#relabel AD_marker by INDDID
df_all %>% 
  select(INDDID, AD_marker) %>% 
  filter(is.na(AD_marker) == F) %>% 
  distinct() %>% 
  group_by(INDDID) %>% 
  arrange(factor(AD_marker, levels = c("autopsy", "PET", "CSF", "plasma")), .by_group = T) %>% #arrange by heirarchy I will use
  slice_head(n=1) %>% #take the top from each subject
  left_join(df_all, by = "INDDID") %>% 
  select(-AD_marker.y) %>% #remove original from df_all
  mutate(AD_marker = AD_marker.x) 




df_all %>% 
  group_by(INDDID) %>% 
  mutate(AD_marker = case_when(
  is.na(AD_marker) & any(AD_marker == "autopsy") ~ "autopsy",
  is.na(AD_marker) & any(AD_marker == "PET") ~ "PET",
  is.na(AD_marker) & any(AD_marker == "CSF") ~ "CSF",
  is.na(AD_marker) & any(AD_marker == "plasma") ~ "plasma"
  ))%>% select(INDDID, AD_marker) %>% distinct()

df_all %>% 
  group_by(INDDID) %>%
  mutate(AD_marker = ifelse(is.na(AD_marker == T), "NA", 
                            ifelse(any(AD_marker == "autopsy"), "autopsy",
                            ifelse(any(AD_marker == "PET"), "PET",
                                   ifelse(any(AD_marker == "CSF"), "CSF",
                                          ifelse(any(AD_marker == "plasma"), "plasma",
                                                     "NA")))))) %>% select(INDDID, AD_marker)
  mutate(autopsy_yn = any(autopsy_yn == TRUE),
        amyloid_pet_yn = any(amyloid_pet_yn == TRUE), 
        csf_yn = any(csf_yn == TRUE),
        plasma_yn = any(plasma_yn == TRUE),
        T1_yn = any(T1_yn == TRUE),
        T2_yn = any(T2_yn == TRUE)) %>% 
  ungroup()



#need to add data on plasma date of collection
#  **********



intvl %>% 
  select(INDDID, MRI_date = FlywheelSessionTimestampUTC.x, SessionLabel, AutopsyDate, PETDate, PETTracer, SampleDate, autopsy_yn,
         amyloid_pet_yn, csf_yn, plasma_yn) %>% 
  group_by(INDDID) %>% 
  mutate_at(c("AutopsyDate", "PETDate", "CSFDate"), as.Date) %>% 
  mutate(intvl_autopsy = difftime(AutopsyDate, MRI_date, units = "days"))  %>% 
  mutate(intvl_PET = ifelse(PETTracer == "Florbetaben (amyloid)" | PETTracer == "Florbetapir (amyloid)",
                             difftime(PETDate, MRI_date, units = "days"), NA)) %>%
  filter(PETTracer == "Florbetaben (amyloid)" | PETTracer == "Florbetapir (amyloid)" | is.na(PETTracer) == T) %>%  
  group_by(INDDID, SessionLabel) %>% 
  distinct() %>% 
  filter(
    ifelse(
      autopsy_yn ==T, 
      MRI_date == min(MRI_date), 
      ifelse(
        amyloid_pet_yn == T, 
        abs(MRI_date - PETDate) == min(abs(MRI_date - PETDate)),
        abs(MRI_date - SampleDate) == min(abs(MRI_date - SampleDate)
                                          )
      )
    )
  ) %>% 
  arrange(INDDID) %>% 
  slice_head() %>% View()

# create new columns for difference
# then find min within each group
# use that to identify which biomarker will be used 

mutate_at(c("AutopsyDate", "PETDate", "CSFDate"), as.Date)
mutate(CSFtoPET = as.numeric(CSFDate - PETDate)/365)





#make ADNC score

  filter(ifelse(is.na(AutopsyDate) == F, max(abs(intvl_selected)), min(abs(intvl_selected))))  %>% 
  arrange(intvl_selected, .by_group = T) %>% 
  %>% View()




intvl %>% 
  group_by(INDDID) %>%
  slice_min(order_by = ifelse(autopsy_yn == T,
                                  (abs(intvl_autopsy), 
                                  ifelse(amyloid_pet_yn == T,
                                          abs(intvl_PET),
                                          ifelse(csf_yn == T,
                                                  abs(intvl_CSF),
                                                  NA))))) %>%
  ungroup() %>% 
  select(intvl_autopsy, intvl_PET, intvl_CSF, everything())



choose_intvl <-
  (if_else(autopsy_yn == T,
           intvl_autopsy, 
           if_else(amyloid_pet_yn == T,
                   intvl_PET,
                   if_else(csf_yn == T,
                           intvl_CSF,
                           NA))))

PET_intvl %>% 
  mutate(MRI_date = as.Date(FlywheelSessionTimestampUTC.x)) %>% 
  mutate(AutopsyDate = as.Date(AutopsyDate)) %>% 
  mutate(time_to_autopsy = AutopsyDate - MRI_date) %>%
  mutate(time_to_autopsy_yrs = as.numeric(time_to_autopsy)/365) %>% 
  group_by(INDDID) %>% 
  distinct() %>%
  slice_min(order_by = time_to_autopsy) %>%
  ungroup()


#Extract IDs for subjects with autopsy
autopsy_IDs <-  filter(df_all, autopsy_yn == TRUE) %>% distinct(INDDID) 
  
write_csv(autopsy_IDs, "~/R_LBD Hippocampal subfields/Data/autopsy_IDs.csv")

#table 1 for autopsy cases only

#df_all %>% 
#  mutate(AD_highint, ifelse(ABeta == 0 | CERAD == c(0,1)
                            

library(table1)

label(df_tbl1$ClinicalDx_sum) <-
  "Diagnosis"
label(df_tbl1$YOB) <-
  "Birth Year"
label(df_tbl1$autopsy_yn) <-
  "Has Autopsy?"
label(df_tbl1$amyloid_pet_yn) <-
  "Has Amyloid PET?"
label(df_tbl1$csf_yn) <-
  "Has CSF?"
label(df_tbl1$plasma_yn) <-
  "Has Plasma"
label(time_to_autopsy_yrs <- "Time to Autopsy (yrs)")

#reorder levels for table display
df_tbl1$ClinicalDx_sum <- factor(df_tbl1$ClinicalDx_sum, 
                                 levels = c("DLB", "PDD", "PD", "Normal"))
  
df_tbl1 <- df_tbl1 %>% mutate(time_to_autopsy_yrs = as.numeric(time_to_autopsy)/365)

table1(~YOB + Sex + Race + Education + autopsy_yn + amyloid_pet_yn + csf_yn + plasma_yn + T1_yn + T2_yn| ClinicalDx_sum, data = df_tbl1)

autopsy <- df_tbl1 %>% filter(autopsy_yn == T)

table1(~YOB + Sex + Race + time_to_autopsy_yrs + Education + T1_yn + T2_yn| ClinicalDx_sum, data = autopsy)

df_tbl1 %>% filter(T1_yn == F) %>% select(INDDID, FlywheelSessionInternalID) %>% print(n=44) %>% write_csv(., file = 't1_missing.csv')


#time between biomarker and mri
#amyloid PET

  df_all %>% filter( amyloid_pet_yn == TRUE) %>% 
  select(INDDID, ClinicalDx_sum, YOB, Sex, Race, Education, autopsy_yn,
         amyloid_pet_yn, csf_yn, plasma_yn, T1_yn, T2_yn, FlywheelSessionInternalID,
         FlywheelSessionTimestampUTC.x, PETDate,
         Braak03, ABeta, CERAD) %>% 
  mutate(MRI_date = as.Date(FlywheelSessionTimestampUTC.x)) %>% 
  mutate(PETDate = as.Date(PETDate)) %>% 
  mutate(time_to_PET = PETDate - MRI_date) %>%
  mutate(time_to_PET_yrs = as.numeric(time_to_PET)/365) %>%
  group_by(INDDID) %>% 
  distinct() %>%
  slice_min(order_by = time_to_PET) %>%
  ungroup()

#Analyze T2 data
df_t2 <- df_all %>% filter(Pipeline == 'T2') %>%
  select(INDDID, hemisphere, region, volume, MMSETotal) %>% 
  distinct() %>% 
  filter(hemisphere == "right", region == "CA1") %>%  
  group_by(INDDID) %>% 
  slice(1) %>% 
  left_join(clinical4, by = "INDDID")

table1(~volume | ClinicalDx_sum, df_t2)

ggplot(df_t2, aes(volume))

#Analyze T1 data
df_t1 <- df_all %>% filter(Pipeline == 'T1') %>%
  select(INDDID, hemisphere, region, volume) %>% 
  distinct() %>% 
  filter(hemisphere == "right", region == "Anterior_hippocampus") %>%  
  group_by(INDDID) %>% 
  slice(1) %>% 
  left_join(clinical4, by = "INDDID")

table1(~volume | ClinicalDx_sum, df_t1)


## Autopsy data on cases and controls

autopsy <- read_csv("~/R_LBD Hippocampal subfields/Data/pd_ashs_volumes.csv")

#join clinical4 with autopsy columns 
autopsy2 <- autopsy %>% 
  filter(!is.na(NPDx1==F)) %>% 
  select(INDDID, AutopsyDate, NPDx1, NPDx1Likelihood, NPDx2, NPDx2Likelihood)

# Combine data tables 
autopsy2 %>% 
  distinct(INDDID, .keep_all = T) %>% 
  left_join(clinical4, by = "INDDID") %>% 
  group_by(ClinicalPhenotype_sum) %>% 
  count(NPDx1,NPDx2) #Gives counts of autopsy by dx

#look for biomarkers only in subjects without autopsy
autopsy_none <- autopsy %>% 
  filter(is.na(NPDx1==F)) %>% 
  select(INDDID) %>% 
  distinct()

write_csv(autopsy_none, "~/R_LBD Hippocampal subfields/Data/autopsy_none.csv")

# Biomarker data
## Import CSF and PET data 
#biomarkers <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.08.07 07.28).xlsx")

#biomarkers <- biomarkers %>% 
# rename(date_mri = date) #relabel 'date' since this was date from ASHS

#biomarkers <- biomarkers %>% 
# mutate(INDDID = as.character(INDDID))

biomarkers_pet1 <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.08.09 09.23)_pet.xlsx")

# filter for those with PET amyloid
biomarkers_pet2 <- biomarkers_pet1 %>% 
  filter(PETTracer == "Florbetaben (amyloid)" | 
           PETTracer == "Florbetapir (amyloid)") %>% 
  distinct(INDDID) %>% 
  mutate(INDDID = as.character(INDDID)) %>% 
  left_join(clinical4, by = "INDDID")

#Of subjects without autopsy or amyloid PET, which have CSF biomarkers
biomarker_query <- filter(autopsy_none, !(autopsy_none$INDDID %in% biomarkers_pet2$INDDID))

write_csv(biomarker_query, "~/R_LBD Hippocampal subfields/Data/biomarker_query.csv")

#
biomarkers_csf_plasma1 <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.08.09 09.58)_csf_plasma.xlsx")

biomarkers_csf <- biomarkers_csf_plasma1 %>% 
  filter(!is.na(SampleDate)) %>%
  mutate(INDDID = as.character(INDDID)) %>% 
  left_join(clinical4) %>% 
  filter(!is.na(Abeta42 | Abeta40 | Abeta42Abeta40Ratio | `CSFt-Tau (pg/mL)` |   
                  `CSFp-Tau181 (pg/mL)` |
                  `CSFp-Tau181Abeta42Ratio`)) %>% 
  arrange(INDDID) %>% 
  distinct(INDDID, .keep_all = T) %>% 
  select(INDDID, ClinicalPhenotype_sum, everything())

table(biomarkers_csf$ClinicalPhenotype_sum)

# for plasmsa
biomarkers_plasma <- biomarkers_csf_plasma1 %>% 
  filter(!is.na(SampleDate)) %>%
  mutate(INDDID = as.character(INDDID)) %>% 
  left_join(clinical4) %>% 
  filter(is.na(Abeta42 | Abeta40 | Abeta42Abeta40Ratio | `CSFt-Tau (pg/mL)` |   
                 `CSFp-Tau181 (pg/mL)` |
                 `CSFp-Tau181Abeta42Ratio`)) %>% 
  filter(!is.na(`Plasmap-Tau181 (pg/mL)`)) %>% 
  arrange(INDDID) %>% 
  select(INDDID, ClinicalPhenotype_sum, everything()) %>% 
  group_by(INDDID) %>% 
  slice(1) %>% 
  ungroup()

table(biomarkers_plasma$ClinicalPhenotype_sum)

# join with those with no autopsy
left_join(autopsy_none, biomarkers_pet, by = 'INDDID') %>% 
  filter(!is.na(Clinical_Read)) %>% 
  arrange(Clinical_Read) %>% 
  distinct(INDDID, .keep_all = T) %>% 
  left_join(clinical4) %>% 
  select(INDDID, ClinicalPhenotype_sum, Clinical_Read)

# filter for those with CSF biomarkers
biomarkers_csf <- biomarkers %>% 
  filter(!is.na(Abeta42 | Abeta40 | Abeta42Abeta40Ratio | `CSFt-Tau (pg/mL)` |   
                  `CSFp-Tau181 (pg/mL)` |
                  `CSFp-Tau181Abeta42Ratio`))

# those with no csf biomarkers
csf_none <-  biomarkers %>% 
  filter(is.na(Abeta42 | Abeta40 | Abeta42Abeta40Ratio | `CSFt-Tau (pg/mL)` |   
                 `CSFp-Tau181 (pg/mL)` |
                 `CSFp-Tau181Abeta42Ratio`))

# with CSF but without autopsy
biomarkers_csf %>% 
  distinct(INDDID, .keep_all = T) %>% 
  inner_join(autopsy_none) %>% 
  left_join(clinical4) %>% 
  select(ClinicalPhenotype_sum, everything()) %>% 
  View()

# filter for those with blood biomarkers
biomarkers_plasma <- biomarkers %>% 
  filter(!is.na(`CSFp-Tau181Abeta42Ratio`))

# with plasma but no csf or autopsy
biomarkers_plasma %>% 
  distinct(INDDID, .keep_all = T) %>% 
  inner_join(csf_none) %>% 
  left_join(clinical4) %>% 
  select(ClinicalPhenotype_sum, everything())

# Quality check - make this a separate script?


# Create new variables for AD + or -. Use heirarchical algorithm to classify.


# Compare CA1 to CA2/3 volume in LBD +/-AD and LBD vs controls - 
  ## make this a seperate script for data analysis
