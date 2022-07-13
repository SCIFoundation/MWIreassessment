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
pacman::p_load("tidyverse","lubridate")


#==============================================================================#
# Import Mapping data (taken from BC code)
#==============================================================================#
mwi_B1 <- read.csv("Z:/Publications & Projects/2019/2019_ESPEN_Data/MWI/Obsolete/Mapping/Mapping_2012_final_MappingB1data.csv",
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))

#Correcting Age errors
mwi_B1$age2 <- ifelse(is.na(mwi_B1$age1), floor(mean(mwi_B1$age1, na.rm = T)),mwi_B1$age1)

## Mapping Baseline Part 2
mwi_B2 <- read.csv("Z:/Countries/Malawi/Mapping/2013_ICOSA/Data/Exported data/clean_2013.csv",
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA")) %>%
  mutate(dateofsurvey=dmy(dateofsurvey),
         dateofsurvey = format(dateofsurvey, "%b-%Y"))

## Mapping Baseline Part 3
mwi_B3 <- read.csv("Z:/Countries/Malawi/Mapping/2015_ICOSA_Likoma/3_Clean_data/Likoma_mapping_clean_data2016-04-07.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA")) %>%
  mutate(VisitDate=dmy(VisitDate),
         VisitDate = format(VisitDate, "%b-%Y"))

## Mapping Reassessment Part 1
mwi_R1 <- read.csv("Z:/Countries/Malawi/Mapping/2017_ICOSA_reassessment/3_Clean_data/Malawi_2017_clean_data2019-01-17_new_GPS_coordinates_UPDATED.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))%>%
  mutate(Date.y=dmy(Date.y),
         Date.y = format(Date.y, "%b-%Y"))

## Mapping Reassessment Part 2 
mwi_R2 <- read.csv("Z:/Countries/Malawi/Mapping/2018_ICOSA_Reassessment/3_Clean_data/Malawi_2018_clean_data_2018-11-12.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))%>%
  mutate(visit_date=dmy(visit_date),
         visit_date = format(visit_date, "%b-%Y"))

## Mapping Reassessment Part 3 
mwi_R3 <- read.csv("Z:/Countries/Malawi/Mapping/2019_Unrestricted_Reassessment/3_Clean_data/MWI_RA_2019_clean_Data_2020-01-17.csv", 
                   stringsAsFactors = F, na.strings = c("", " ", "  ", "NA"))%>%
  mutate(date=dmy(date),
         date = format(date, "%b-%Y"))


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
# Create categorical variable for STH (taken from BC code)
#==============================================================================#
# Asc
## cannot do for Baseline 1
mwi_B2$Asc_cat <- cut(mwi_B2$Asc_epg, c(-99,1,5000,50000,999999), labels = catLabel)
mwi_B3$Asc_cat <- cut(mwi_B3$Asc.meanepg, c(-99,1,5000,50000,999999), labels = catLabel)
mwi_R1$Asc_cat <- cut(mwi_R1$Asc.meanepg, c(-99,1,5000,50000,999999), labels = catLabel)       
mwi_R2$Asc_cat <- cut(mwi_R2$Asc.meanepg, c(-99,1,5000,50000,999999), labels = catLabel) 
mwi_R3$Asc_cat <- cut(mwi_R3$Asc_meanepg, c(-99,1,5000,50000,999999), labels = catLabel) 

# Hkw
## cannot do for Baseline 1
mwi_B2$Hkw_cat <- cut(mwi_B2$Ank_epg, c(-99,1,2000,10000,999999), labels = catLabel)
mwi_B3$Hkw_cat <- cut(mwi_B3$Hkw.meanepg, c(-99,1,2000,10000,999999), labels = catLabel)
mwi_R1$Hkw_cat <- cut(mwi_R1$HW.meanepg, c(-99,1,2000,10000,999999), labels = catLabel)       
mwi_R2$Hkw_cat <- cut(mwi_R2$HW.meanepg, c(-99,1,2000,10000,999999), labels = catLabel) 
mwi_R3$Hkw_cat <- cut(mwi_R3$Hkw_meanepg, c(-99,1,2000,10000,999999), labels = catLabel) 


# Tri
## cannot do for Baseline 1
mwi_B2$Trich_cat <- cut(mwi_B2$Trich_epg, c(-99,1,1000,10000,999999), labels = catLabel)
mwi_B3$Trich_cat <- cut(mwi_B3$Tri.meanepg, c(-99,1,1000,10000,999999), labels = catLabel)
mwi_R1$Trich_cat <- cut(mwi_R1$Tric.meanepg, c(-99,1,1000,10000,999999), labels = catLabel)       
mwi_R2$Trich_cat <- cut(mwi_R2$Tric.meanepg, c(-99,1,1000,10000,999999), labels = catLabel) 
mwi_R3$Trich_cat <- cut(mwi_R3$Tri_meanepg, c(-99,1,1000,10000,999999), labels = catLabel) 

# STH for ones without the column in clean data
mwi_B2$STH <- apply(mwi_B2[,c("Asc", "Trich", "Ank")], 1, sum)

#==============================================================================#
# Epidemiology vars (adapted from BC code)
#==============================================================================#

# Baseline 1
# No visit date for B1
mwiB1_data <- mwi_B1 %>%
  #Removing the NA schools
  filter(!is.na(SchoolName), SchoolName!="MKHOSI F.P") %>%
  mutate(anySCH= case_when(haem ==1 | man ==1 ~ 1,
                           haem==0 & man==0 ~ 0,
                           haem==0 & is.na(man) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(DistrictName, SchoolName) %>%
  summarise(Year = 2012, Test = "KK/UF", Visit_date=NA,
            #GPS: longitude, latitude
            Long = max(GPS_East),
            Lat = max(GPS_South),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(age2, na.rm = T), max(age2, na.rm = T)),
            #number of people & positive people
            Num_sampled_Sh = sum(!is.na(haem)), Num_pos_Sh = sum(haem == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(man)), Num_pos_Sman = sum(man == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Num_sampled_Asc = sum(!is.na(asc)), Num_pos_Asc = sum(asc == 1, na.rm = T),
            Num_sampled_Hkw = sum(!is.na(hook)), Num_pos_Hkw = sum(hook == 1, na.rm = T),
            Num_sampled_Tri = sum(!is.na(tri)), Num_pos_Tri = sum(tri == 1, na.rm = T),
            Num_sampled_anySTH = sum(!is.na(sth)), Num_pos_anySTH = sum(sth == 1, na.rm = T),
  # above taken from previous code
  
  # get mean prevalence by school
            Shprev = mean(haem,na.omit=T),
            Smanprev = mean(man,na.omit=T),
            anySCHprev = mean(anySCH,na.omit=T),
            Ascprev = mean(asc,na.omit=T),
            Hkwprev = mean(hook,na.omit=T),
            Triprev = mean(tri,na.omit=T),
            anySTHprev = mean(sth,na.omit=T)) %>% 
  select(Year,Visit_date,District=DistrictName, School=SchoolName,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev,
         Num_sampled_Asc,Num_pos_Asc,Ascprev,Num_sampled_Hkw,Num_pos_Hkw,Hkwprev,Num_sampled_Tri,Num_pos_Tri,Triprev,
         Num_sampled_anySTH,Num_pos_anySTH,anySTHprev)

mwi_B1 %>%filter(!is.na(SchoolName), SchoolName!="MKHOSI F.P") %>% group_by(DistrictName) %>%summarise(n=n_distinct(SchoolName))%>% summarise(nsum=sum(n)) #ok

# Baseline 2
mwiB2_data <- mwi_B2 %>%
  mutate(anySCH= case_when(Sh ==1 | Sm ==1 ~ 1,
                           Sh==0 & Sm==0 ~ 0,
                           Sh==0 & is.na(Sm) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(districtname, school) %>%
  summarise(Year = 2013, Test = "KK/UF", Visit_date=unique(na.omit(dateofsurvey)),
            #GPS: longitude, latitude
            Long = max(arrivaldecimaldegreeeast),
            Lat = -max(arrivaldecimaldegreesouth),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(ageyears, na.rm = T), max(ageyears, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Sh)), Num_pos_Sh = sum(Sh == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Sm)), Num_pos_Sman = sum(Sm == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Num_sampled_Asc = sum(!is.na(Asc)), Num_pos_Asc = sum(Asc == 1, na.rm = T),
            Num_sampled_Hkw = sum(!is.na(Ank)), Num_pos_Hkw = sum(Ank == 1, na.rm = T),
            Num_sampled_Tri = sum(!is.na(Trich)), Num_pos_Tri = sum(Trich == 1, na.rm = T),
            Num_sampled_anySTH = sum(!is.na(STH)), Num_pos_anySTH = sum(STH == 1, na.rm = T),
            Shprev = mean(Sh,na.omit=T),
            Smanprev = mean(Sm,na.omit=T),
            anySCHprev = mean(anySCH,na.omit=T),
            Ascprev = mean(Asc,na.omit=T),
            Hkwprev = mean(Ank,na.omit=T),
            Triprev = mean(Trich,na.omit=T),
            anySTHprev = mean(STH,na.omit=T)) %>% 
  select(Year,Visit_date,District=districtname, School=school,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev,
         Num_sampled_Asc,Num_pos_Asc,Ascprev,Num_sampled_Hkw,Num_pos_Hkw,Hkwprev,Num_sampled_Tri,Num_pos_Tri,Triprev,
         Num_sampled_anySTH,Num_pos_anySTH,anySTHprev)

length(unique(mwi_B2$school))  #ok

# Baseline 3
mwiB3_data <- mwi_B3 %>%
  mutate(anySCH= case_when(Shaem ==1 | Smans ==1 ~ 1,
                           Shaem==0 & Smans==0 ~ 0,
                           Shaem==0 & is.na(Smans) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(DistrictName, SchoolName) %>%
  summarise(Year = 2015, Test = "KK/Hemastix", Visit_date=unique(na.omit(VisitDate)),
            #GPS: longitude, latitude
            Long = max(GPSEast),
            Lat =  -max(GPSNorth),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(Age, na.rm = T), max(Age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem)), Num_pos_Sh = sum(Shaem == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans)), Num_pos_Sman = sum(Smans == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Num_sampled_Asc = sum(!is.na(Asc)), Num_pos_Asc = sum(Asc == 1, na.rm = T),
            Num_sampled_Hkw = sum(!is.na(Hkw)), Num_pos_Hkw = sum(Hkw == 1, na.rm = T),
            Num_sampled_Tri = sum(!is.na(Tri)), Num_pos_Tri = sum(Tri == 1, na.rm = T),
            Num_sampled_anySTH = sum(!is.na(STH)), Num_pos_anySTH = sum(STH == 1, na.rm = T),
            Shprev = mean(Shaem,na.omit=T),
            Smanprev = mean(Smans,na.omit=T),
            anySCHprev = mean(anySCH,na.omit=T),
            Ascprev = mean(Asc,na.omit=T),
            Hkwprev = mean(Hkw,na.omit=T),
            Triprev = mean(Tri,na.omit=T),
            anySTHprev = mean(STH,na.omit=T)) %>% 
    select(Year,Visit_date,District=DistrictName, School=SchoolName,Long,Lat,Test,Age,
           Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev,
           Num_sampled_Asc,Num_pos_Asc,Ascprev,Num_sampled_Hkw,Num_pos_Hkw,Hkwprev,Num_sampled_Tri,Num_pos_Tri,Triprev,
           Num_sampled_anySTH,Num_pos_anySTH,anySTHprev)


length(unique(mwi_B3$SchoolName)) #ok

# Reassessment 1
mwiR1_data <- mwi_R1 %>%
  mutate(anySCH= case_when(Shaem_bin ==1 | Smans_bin ==1 ~ 1,
                           Shaem_bin==0 & Smans_bin==0 ~ 0,
                           Shaem_bin==0 & is.na(Smans_bin) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(DistrictName.x, NewSchoolName.x) %>%
  summarise(Year = 2017, Test = "KK/UF",Visit_date=head(Date.y,1),
            #GPS: longitude, latitude
            Long = max(GPS_lon_NEW),
            Lat = max(GPS_lat_NEW),
            #Min-Max of Age
            Age = sprintf("%02d%02d", min(Age, na.rm = T), max(Age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem_bin)), Num_pos_Sh = sum(Shaem_bin == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans_bin)), Num_pos_Sman = sum(Smans_bin == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Num_sampled_Asc = sum(!is.na(Asc_bin)), Num_pos_Asc = sum(Asc_bin == 1, na.rm = T),
            Num_sampled_Hkw = sum(!is.na(Hw_bin)), Num_pos_Hkw = sum(Hw_bin == 1, na.rm = T),
            Num_sampled_Tri = sum(!is.na(Tric_bin)), Num_pos_Tri = sum(Tric_bin == 1, na.rm = T),
            Num_sampled_anySTH = sum(!is.na(STH)), Num_pos_anySTH = sum(STH == 1, na.rm = T),
            Shprev = mean(Shaem_bin,na.omit=T),
            Smanprev = mean(Smans_bin,na.omit=T),
            anySCHprev = mean(anySCH,na.omit=T),
            Ascprev = mean(Asc_bin,na.omit=T),
            Hkwprev = mean(Hw_bin,na.omit=T),
            Triprev = mean(Tric_bin,na.omit=T),
            anySTHprev = mean(STH,na.omit=T)) %>% 
  select(Year,Visit_date,District=DistrictName.x, School=NewSchoolName.x,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev,
         Num_sampled_Asc,Num_pos_Asc,Ascprev,Num_sampled_Hkw,Num_pos_Hkw,Hkwprev,Num_sampled_Tri,Num_pos_Tri,Triprev,
         Num_sampled_anySTH,Num_pos_anySTH,anySTHprev)


mwi_R1 %>% group_by(DistrictName.x) %>%summarise(n=n_distinct(NewSchoolName.x))%>% summarise(nsum=sum(n)) #ok


# Reassessment 2
mwiR2_data <- mwi_R2 %>%
  mutate(anySCH= case_when(Shaem_bin ==1 | Smans_bin ==1 ~ 1,
                           Shaem_bin==0 & Smans_bin==0 ~ 0,
                           Shaem_bin==0 & is.na(Smans_bin) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(region_sch, schoolName_sch) %>%
  summarise(Year = 2018, Test = "KK/UF",Visit_date=unique(na.omit(visit_date)),
            Long = max(location.Longitude), 
            Lat = max(location.Latitude),
            Age = sprintf("%02d%02d", min(age, na.rm = T), max(age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem_bin)), Num_pos_Sh = sum(Shaem_bin == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans_bin)), Num_pos_Sman = sum(Smans_bin == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Num_sampled_Asc = sum(!is.na(Asc_bin)), Num_pos_Asc = sum(Asc_bin == 1, na.rm = T),
            Num_sampled_Hkw = sum(!is.na(Hw_bin)), Num_pos_Hkw = sum(Hw_bin == 1, na.rm = T),
            Num_sampled_Tri = sum(!is.na(Tric_bin)), Num_pos_Tri = sum(Tric_bin == 1, na.rm = T),
            Num_sampled_anySTH = sum(!is.na(STH)), Num_pos_anySTH = sum(STH == 1, na.rm = T),
            Shprev = mean(Shaem_bin,na.omit=T),
            Smanprev = mean(Smans_bin,na.omit=T),
            anySCHprev = mean(anySCH,na.omit=T),
            Ascprev = mean(Asc_bin,na.omit=T),
            Hkwprev = mean(Hw_bin,na.omit=T),
            Triprev = mean(Tric_bin,na.omit=T),
            anySTHprev = mean(STH,na.omit=T)) %>% 
  select(Year,Visit_date,District=region_sch, School=schoolName_sch,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev,
         Num_sampled_Asc,Num_pos_Asc,Ascprev,Num_sampled_Hkw,Num_pos_Hkw,Hkwprev,Num_sampled_Tri,Num_pos_Tri,Triprev,
         Num_sampled_anySTH,Num_pos_anySTH,anySTHprev)

mwi_R2 %>% group_by(region_sch) %>%summarise(n=n_distinct(schoolName_sch))%>% summarise(nsum=sum(n)) #ok


# Reassessment 3
mwiR3_data <- mwi_R3 %>%
  mutate(anySCH= case_when(Shaem_bin ==1 | Smans_bin ==1 ~ 1,
                           Shaem_bin==0 & Smans_bin==0 ~ 0,
                           Shaem_bin==0 & is.na(Smans_bin) ~ 0,
                           TRUE ~ NA_real_)) %>%
  group_by(district_name, school_name) %>%
  summarise(Year = 2019, Test = "KK/UF",Visit_date=unique(na.omit(date)),
            Long = max(lon), 
            Lat = max(lat),
            Age = sprintf("%02d%02d", min(age, na.rm = T), max(age, na.rm = T)),
            Num_sampled_Sh = sum(!is.na(Shaem_bin)), Num_pos_Sh = sum(Shaem_bin == 1, na.rm = T), 
            Num_sampled_Sman = sum(!is.na(Smans_bin)), Num_pos_Sman = sum(Smans_bin == 1, na.rm = T),
            Num_sampled_anySCH = sum(!is.na(anySCH)), Num_pos_anySCH = sum(anySCH == 1, na.rm = T),
            Num_sampled_Asc = sum(!is.na(Asc_bin)), Num_pos_Asc = sum(Asc_bin == 1, na.rm = T),
            Num_sampled_Hkw = sum(!is.na(Hkw_bin)), Num_pos_Hkw = sum(Hkw_bin == 1, na.rm = T),
            Num_sampled_Tri = sum(!is.na(Tri_bin)), Num_pos_Tri = sum(Tri_bin == 1, na.rm = T),
            Num_sampled_anySTH = sum(!is.na(STH)), Num_pos_anySTH = sum(STH == 1, na.rm = T),
            Shprev = mean(Shaem_bin,na.omit=T),
            Smanprev = mean(Smans_bin,na.omit=T),
            anySCHprev = mean(anySCH,na.omit=T),
            Ascprev = mean(Asc_bin,na.omit=T),
            Hkwprev = mean(Hkw_bin,na.omit=T),
            Triprev = mean(Tri_bin,na.omit=T),
            anySTHprev = mean(STH,na.omit=T)) %>% 
  select(Year,Visit_date,District=district_name, School=school_name,Long,Lat,Test,Age,
         Num_sampled_Sh,Num_pos_Sh,Shprev,Num_sampled_Sman,Num_pos_Sman, Smanprev,Num_sampled_anySCH,Num_pos_anySCH,anySCHprev,
         Num_sampled_Asc,Num_pos_Asc,Ascprev,Num_sampled_Hkw,Num_pos_Hkw,Hkwprev,Num_sampled_Tri,Num_pos_Tri,Triprev,
         Num_sampled_anySTH,Num_pos_anySTH,anySTHprev)


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
