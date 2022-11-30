library(tidyverse)

# Read in data
## All clinical LBD cases + age/sex matched controls
clinical <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery_Output_2022.07.12_09.59_.xlsx")

#Remove INDDID w/ decimal 
#clinical <- clinical[-grepl("118913", clinical$INDDID),]

#INDDID list for querying for biomarkers
clinical_inddid <- clinical %>% select(INDDID) %>% distinct()

#write_csv(clinical_inddid, "~/R_LBD Hippocampal subfields/Data/clinical_inddid.csv")

# Demographic data
demographics <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.10.13 16.26).xlsx", sheet = 1) %>% 
  mutate(INDDID = as.character(INDDID)) %>% 
  mutate(Education = na_if(Education, 999))

# Consents (to get center enrolled from)
consents <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.11.13 18.09).xlsx", sheet = 1) %>%
  mutate(INDDID = as.character(INDDID))

names <- consents %>% group_by(ConsentName) %>% summarise(count = n()) %>% arrange(desc(count)) %>% select(ConsentName) %>% as.vector() %>% .[5:81,]

consents2 <- consents 
consents2$ConsentName <- recode(consents$ConsentName, PD = "Udall",
                                Control = "Udall",
                                FTD = "FTD")

consents2$ConsentName[consents2$ConsentName %in% c("Udall", "FTD")==F] <- "Other"

demographics <- demographics %>% left_join(consents2)

## Biomarkers
biomarkers <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.10.02 09.12).xlsx", sheet = 1) %>%
  mutate(INDDID = as.character(INDDID),
         PET_read = Clinical_Read,
         tau_abeta42_ratio = LuminexTTauAbetaRatio)

vlt <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.11.08 09.42).xlsx", sheet = 1) %>% 
  mutate(INDDID = as.character(INDDID))

epworth <- readxl::read_excel("~/R_LBD Hippocampal subfields/Data/INQuery Output (2022.10.04 17.59).xlsx", sheet = 1) %>% 
  mutate(INDDID = as.character(INDDID))

## ASHS results  
ashs <- read_csv("~/R_LBD Hippocampal subfields/Data/ashs_all_output.csv") %>% 
  mutate(session_date = str_sub(MRISession, 0, 8),
         INDDID = as.character(INDDID)) %>% 
  mutate(session_date = as.Date(session_date, format = '%Y%m%d')) %>% 
  filter(Version != "0.1.1") #remove output from ASHS 0.1.1

#INDD data dicitonary
