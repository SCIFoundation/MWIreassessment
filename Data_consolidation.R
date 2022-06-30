#=================================================#
# Lancaster MSc Project

# Create clean datasets for student 
# Write out separate files for historic & 2017-2019 data


# Contact person: p.poon@schisto.org.uk
#=================================================#

#==============================================================================#
# Set up, load functions
#==============================================================================#

setwd("Z:/Publications & Projects/2022/Lancaster MSc Project")

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")


#==============================================================================#
# Import Mapping data (taken from BC code)
#==============================================================================#
mwi_B1 <- read.csv("Z:/Publications & Projects/2019/2019_ESPEN_Data/MWI/Obsolete/Mapping/Mapping_2012_final_MappingB1data.csv",
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))

#Correcting Age errors
mwi_B1$age2 <- ifelse(is.na(mwi_B1$age1), floor(mean(mwi_B1$age1, na.rm = T)),mwi_B1$age1)

## Mapping Baseline Part 2
mwi_B2 <- read.csv("Z:/Countries/Malawi/Mapping/2013_ICOSA/Data/Exported data/clean_2013.csv",
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))

## Mapping Baseline Part 3
mwi_B3 <- read.csv("Z:/Countries/Malawi/Mapping/2015_ICOSA_Likoma/3_Clean_data/Likoma_mapping_clean_data2016-04-07.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))

## Mapping Reassessment Part 1
mwi_R1 <- read.csv("Z:/Countries/Malawi/Mapping/2017_ICOSA_reassessment/3_Clean_data/Malawi_2017_clean_data2019-01-17_new_GPS_coordinates_UPDATED.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))

## Mapping Reassessment Part 2 
mwi_R2 <- read.csv("Z:/Countries/Malawi/Mapping/2018_ICOSA_Reassessment/3_Clean_data/Malawi_2018_clean_data_2018-11-12.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))

## Mapping Reassessment Part 3 
mwi_R3 <- read.csv("Z:/Countries/Malawi/Mapping/2019_Unrestricted_Reassessment/3_Clean_data/MWI_RA_2019_clean_Data_2020-01-17.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))


#==============================================================================#
# Create categorical variable for Smans (taken from BC code)
#==============================================================================#

catLabel <- c("uninfected", "light intensity","moderate intensity","heavy intensity")
## cannot do B1 due to egg count data not provided in the clean data, only prevalence
mwi_B2$Sm_cat <- cut(mwi_B2$Sm_epg, c(-99,1,100,400,999999), labels = catLabel)
mwi_B2$Sh_heavy <- ifelse(mwi_B2$Sh_ep10ml < 50, 0, 1)
mwi_B3$Sm_cat <- cut(mwi_B3$Smans.meanepg, c(-99,1,100,400,999999), labels = catLabel) 
mwi_R1$Sm_cat <- cut(mwi_R1$Smans.meanepg, c(-99,1,100,400,999999), labels = catLabel)       
mwi_R2$Sm_cat <- cut(mwi_R2$Smans.meanepg, c(-99,1,100,400,999999), labels = catLabel)      
mwi_R3$Sm_cat <- cut(mwi_R3$Smans_meanepg, c(-99,1,100,400,999999), labels = catLabel)   

#==============================================================================#
# Epidemiology vars (adapted from BC code)
#==============================================================================#

# Baseline 1
mwiB1_data <- mwi_B1 %>%
  #Removing the NA schools
  filter(!is.na(SchoolName), SchoolName!="MKHOSI F.P") %>%
  mutate(anySCH= case_when(haem ==1 | man ==1 ~ 1,
                           haem==0 & man==0 ~ 0,
                           haem==0 & is.na(man) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(DistrictName, SchoolName) %>%
  summarise(Year = 2012, Test = "KK/UF", 
            #GPS: longitude, latitude
            Long = max(GPS_East),
            Lat = max(GPS_South),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(age2, na.rm = T), max(age2, na.rm = T)),
            #number of people & positive people
            Num_sampled_Sh = sum(!is.na(haem)), Num_pos_Sh = sum(haem == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(man)), Num_pos_Sman = sum(man == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
  # above taken from previous code
  
  # get mean prevalence by school
            Shprev = mean(haem,na.rm=T),
            Smanprev = mean(man,na.rm=T),
            anySCHprev = mean(anySCH,na.rm=T)) %>% 
  select(Year,District=DistrictName, School=SchoolName,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev)

mwi_B1 %>%filter(!is.na(SchoolName), SchoolName!="MKHOSI F.P") %>% group_by(DistrictName) %>%summarise(n=n_distinct(SchoolName))%>% summarise(nsum=sum(n)) #ok

# Baseline 2
mwiB2_data <- mwi_B2 %>%
  mutate(anySCH= case_when(Sh ==1 | Sm ==1 ~ 1,
                           Sh==0 & Sm==0 ~ 0,
                           Sh==0 & is.na(Sm) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(districtname, school) %>%
  summarise(Year = 2013, Test = "KK/UF",
            #GPS: longitude, latitude
            Long = max(arrivaldecimaldegreeeast),
            Lat = -max(arrivaldecimaldegreesouth),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(ageyears, na.rm = T), max(ageyears, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Sh)), Num_pos_Sh = sum(Sh == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Sm)), Num_pos_Sman = sum(Sm == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Shprev = mean(Sh,na.rm=T),
            Smanprev = mean(Sm,na.rm=T),
            anySCHprev = mean(anySCH,na.rm=T)) %>% 
  select(Year,District=districtname, School=school,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev)

length(unique(mwi_B2$school))  #ok

# Baseline 3
mwiB3_data <- mwi_B3 %>%
  mutate(anySCH= case_when(Shaem ==1 | Smans ==1 ~ 1,
                           Shaem==0 & Smans==0 ~ 0,
                           Shaem==0 & is.na(Smans) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(DistrictName, SchoolName) %>%
  summarise(Year = 2015, Test = "KK/Hemastix",
            #GPS: longitude, latitude
            Long = max(GPSEast),
            Lat =  -max(GPSNorth),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(Age, na.rm = T), max(Age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem)), Num_pos_Sh = sum(Shaem == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans)), Num_pos_Sman = sum(Smans == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Shprev = mean(Shaem,na.rm=T),
            Smanprev = mean(Smans,na.rm=T),
            anySCHprev = mean(anySCH,na.rm=T)) %>% 
    select(Year,District=DistrictName, School=SchoolName,Long,Lat,Test,Age,
           Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev)


length(unique(mwi_B3$SchoolName)) #ok

# Reassessment 1
mwiR1_data <- mwi_R1 %>%
  mutate(anySCH= case_when(Shaem_bin ==1 | Smans_bin ==1 ~ 1,
                           Shaem_bin==0 & Smans_bin==0 ~ 0,
                           Shaem_bin==0 & is.na(Smans_bin) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(DistrictName.x, NewSchoolName.x) %>%
  summarise(Year = 2017, Test = "KK/UF",
            #GPS: longitude, latitude
            Long = max(GPS_lon_NEW),
            Lat = max(GPS_lat_NEW),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(Age, na.rm = T), max(Age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem_bin)), Num_pos_Sh = sum(Shaem_bin == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans_bin)), Num_pos_Sman = sum(Smans_bin == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Shprev = mean(Shaem_bin,na.rm=T),
            Smanprev = mean(Smans_bin,na.rm=T),
            anySCHprev = mean(anySCH,na.rm=T)) %>% 
  select(Year,District=DistrictName.x, School=NewSchoolName.x,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev)


mwi_R1 %>% group_by(DistrictName.x) %>%summarise(n=n_distinct(NewSchoolName.x))%>% summarise(nsum=sum(n)) #ok


# Reassessment 2
mwiR2_data <- mwi_R2 %>%
  mutate(anySCH= case_when(Shaem_bin ==1 | Smans_bin ==1 ~ 1,
                           Shaem_bin==0 & Smans_bin==0 ~ 0,
                           Shaem_bin==0 & is.na(Smans_bin) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(region_sch, schoolName_sch) %>%
  summarise(Year = 2018, Test = "KK/UF",
            Long = max(location.Longitude), 
            Lat = max(location.Latitude),
            Age = sprintf("%02d%02d", min(age, na.rm = T), max(age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem_bin)), Num_pos_Sh = sum(Shaem_bin == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans_bin)), Num_pos_Sman = sum(Smans_bin == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Shprev = mean(Shaem_bin,na.rm=T),
            Smanprev = mean(Smans_bin,na.rm=T),
            anySCHprev = mean(anySCH,na.rm=T)) %>% 
  select(Year,District=region_sch, School=schoolName_sch,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev)

mwi_R2 %>% group_by(region_sch) %>%summarise(n=n_distinct(schoolName_sch))%>% summarise(nsum=sum(n)) #ok


# Reassessment 3
mwiR3_data <- mwi_R3 %>%
  mutate(anySCH= case_when(Shaem_bin ==1 | Smans_bin ==1 ~ 1,
                           Shaem_bin==0 & Smans_bin==0 ~ 0,
                           Shaem_bin==0 & is.na(Smans_bin) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(district_name, school_name) %>%
  summarise(Year = 2019, Test = "KK/UF",
            Long = max(lon), 
            Lat = max(lat),
            Age = sprintf("%02d%02d", min(age, na.rm = T), max(age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem_bin)), Num_pos_Sh = sum(Shaem_bin == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans_bin)), Num_pos_Sman = sum(Smans_bin == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Shprev = mean(Shaem_bin,na.rm=T),
            Smanprev = mean(Smans_bin,na.rm=T),
            anySCHprev = mean(anySCH,na.rm=T)) %>% 
  select(Year,District=district_name, School=school_name,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev)


mwi_R3 %>% group_by(district_name) %>%summarise(n=n_distinct(school_name))%>% summarise(nsum=sum(n)) #ok


#==============================================================================#
# Combine relevant datasets
#==============================================================================#
# Historic data (2012, 2013, 2015)
mwi_historic <- rbind(mwiB1_data, mwiB2_data, mwiB3_data)

# Reassessment data (2017, 2018, 2019)
mwi_rs <- rbind(mwiR1_data, mwiR2_data, mwiR3_data)


#==============================================================================#
# Write out files
#==============================================================================#

# Historic data
write.csv(mwi_historic, file = "MWI_Historic_Mapping_data.csv",row.names = FALSE)

# Reassessment data
write.csv(mwi_rs, file = "MWI_Reassessment_Mapping_data.csv",row.names = FALSE)
