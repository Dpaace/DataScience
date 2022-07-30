library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

getwd()
setwd('D:/')
setwd('DATA')

#Cleaning Data For House Price

hp2019 = read_csv("Houseprice/pp-2019.csv", show_col_types = FALSE)
hp2020 = read_csv('Houseprice/pp-2020.csv', show_col_types = FALSE)
hp2021 = read_csv('Houseprice/pp-2021.csv')


colnames(hp2019) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2" )
colnames(hp2020) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2")
colnames(hp2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")
HousePrices = hp2021 %>%
  add_row(hp2020)%>%
  add_row(hp2019)

write.csv(HousePrices, "Houseprice/UncleanedHousePrices.csv")

# Filtering Greater Manchester and Merseyside data
FilteredHousePrices = filter(HousePrices, County == 'GREATER MANCHESTER' | County == 'MERSEYSIDE')


FilteredHousePrices = FilteredHousePrices %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  select(PostCode,shortPostcode,Year,PAON,Price) %>% 
  na.omit()


# exporting filteredhouseprices data set to  csv
write.csv(FilteredHousePrices, "CleanedData/CleanedHousePrices.csv")


#Cleaning Data for Town

uncleanedhouseprices = read_csv('Houseprice/UncleanedHousePrices.csv')

Population = read_csv("Population/Population2011_1656567141570.csv", show_col_types = FALSE)

# Filtering Greater Manchester and Merseyside data
FilteredTown = filter(uncleanedhouseprices, County == 'GREATER MANCHESTER' | County == 'MERSEYSIDE')

Population = Population %>%  
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>%
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>%
  mutate(Population2021= (1.00561255390388033 * Population2020)) %>%
  select(shortPostcode,Population2019,Population2020,Population2021)


FilteredTown = FilteredTown %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  left_join(Population,by="shortPostcode") %>% 
  select(PostCode, shortPostcode, Year, Town, District, County, Population2019,Population2020,Population2021) %>% 
  group_by(shortPostcode) %>%
  filter(row_number()==1) %>%
  arrange(County) %>% 
  na.omit()

write.csv(FilteredTown, "CleanedData/Towns.csv")

#--------------------------------------------------------------------------------------------------------------------------------------



# BROADBAND DATA CLEANING

Towns = read_csv("CleanedData/Towns.csv", show_col_types = FALSE)

Broadband = read_csv("Broadband/BroadbandSpeeds.csv", show_col_types = FALSE)

Broadband = replace(Broadband,is.na(Broadband), 0)


cleanBroadbandSpeeds = Broadband %>%
  mutate(shortPostcode = str_trim(substring(postcode_space, 1,4))) %>% 
  group_by(shortPostcode) %>% 
  summarise_at(vars("Average download speed (Mbit/s)","Maximum download speed (Mbit/s)","Average upload speed (Mbit/s)",
                    "Maximum upload speed (Mbit/s)"),list(name = mean)) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>% 
  arrange(County) %>% 
  select(-County,Town,District,County,Population2019,Population2020,) %>% 
  rename("AverageDownload"="Average download speed (Mbit/s)_name","MaxDownload"="Maximum download speed (Mbit/s)_name",
         "AverageUpload"="Average upload speed (Mbit/s)_name","MaxUpload"="Maximum upload speed (Mbit/s)_name") %>% 
  na.omit()

names(cleanBroadbandSpeeds)


cleanBroadband = cleanBroadbandSpeeds %>%
  select(shortPostcode,AverageDownload, MaxDownload,AverageUpload,MaxUpload,PostCode,Year,
         Town,District,Population2019,Population2020,Population2021,County)

write.csv(cleanBroadband, "CleanedData/cleanBroadband.csv")


BroadbandClean = read_csv("CleanedData/CleanBroadband.csv", show_col_types = FALSE)


#Cleaning Data for School

liverpoolSchool16 = read_csv('SchoolData/liverpoolSchool16.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2016)
liverpoolSchool17 = read_csv('SchoolData/liverpoolSchool17.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2017)
liverpoolSchool18 = read_csv('SchoolData/liverpoolSchool18.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2018)
liverpoolSchool19 = read_csv('SchoolData/liverpoolSchool19.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2019)

manchesterSchool16 = read_csv('SchoolData/manchesterSchool16.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2016)
manchesterSchool17 = read_csv('SchoolData/manchesterSchool17.csv', show_col_types = FALSE) %>% 
  mutate(Year = 2017)
manchesterSchool18 = read_csv('SchoolData/manchesterSchool18.csv', show_col_types = FALSE)%>% 
  mutate(Year = 2018)
manchesterSchool19 = read_csv('SchoolData/manchesterSchool19.csv', show_col_types = FALSE)%>% 
  mutate(Year = 2019)


liverpoolSchool16 = select(liverpoolSchool16, Year, PCODE, SCHNAME, ATT8SCR)
liverpoolSchool17 = select(liverpoolSchool17, Year, PCODE, SCHNAME, ATT8SCR)
liverpoolSchool18 = select(liverpoolSchool18, Year, PCODE, SCHNAME, ATT8SCR)
liverpoolSchool19 = select(liverpoolSchool19, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool16 = select(manchesterSchool16, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool17 = select(manchesterSchool17, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool18 = select(manchesterSchool18, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool19 = select(manchesterSchool19, Year, PCODE, SCHNAME, ATT8SCR)


schoolData = manchesterSchool19 %>% 
  add_row(manchesterSchool18) %>% 
  add_row(manchesterSchool17) %>% 
  add_row(manchesterSchool16) %>% 
  add_row(liverpoolSchool19) %>% 
  add_row(liverpoolSchool18) %>% 
  add_row(liverpoolSchool17) %>% 
  add_row(liverpoolSchool16) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()

colnames(schoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")

write.csv(schoolData, "CleanedData/School.csv")



# School data cleaning separately for Manchester and Liverpool

manchesterSchoolData = manchesterSchool19 %>% 
  add_row(manchesterSchool18) %>%
  add_row(manchesterSchool17) %>%
  add_row(manchesterSchool16) %>%
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()

colnames(manchesterSchoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")

write.csv(manchesterSchoolData, "CleanedData/manchesterSchoolData.csv") 


liverpoolSchoolData = liverpoolSchool19 %>% 
  add_row(liverpoolSchool18) %>% 
  add_row(liverpoolSchool17) %>% 
  add_row(liverpoolSchool16) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()

colnames(liverpoolSchoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")

write.csv(liverpoolSchoolData, "CleanedData/liverpoolSchoolData.csv") 

#--------------------------------------------------------------------------------------------------------------------------------------


# CRIME DATA CLEANING


# Merging the crime data
#greater manchester data
cd201906g = read_csv('CrimeData/2019-06/2019-06-greater-manchester-street.csv', show_col_types = FALSE)

#merseyside data
cd201906 = read_csv('CrimeData/2019-06/2019-06-merseyside-street.csv', show_col_types = FALSE)
cd201907 = read_csv('CrimeData/2019-07/2019-07-merseyside-street.csv', show_col_types = FALSE)
cd201908 = read_csv('CrimeData/2019-08/2019-08-merseyside-street.csv', show_col_types = FALSE)
cd201909 = read_csv('CrimeData/2019-09/2019-09-merseyside-street.csv', show_col_types = FALSE)
cd201910 = read_csv('CrimeData/2019-10/2019-10-merseyside-street.csv', show_col_types = FALSE)
cd201911 = read_csv('CrimeData/2019-11/2019-11-merseyside-street.csv', show_col_types = FALSE)
cd202001 = read_csv('CrimeData/2020-01/2020-01-merseyside-street.csv', show_col_types = FALSE)
cd202002 = read_csv('CrimeData/2020-02/2020-02-merseyside-street.csv', show_col_types = FALSE)
cd202003 = read_csv('CrimeData/2020-03/2020-03-merseyside-street.csv', show_col_types = FALSE)
cd202004 = read_csv('CrimeData/2020-04/2020-04-merseyside-street.csv', show_col_types = FALSE)
cd202005 = read_csv('CrimeData/2020-05/2020-05-merseyside-street.csv', show_col_types = FALSE)
cd202006 = read_csv('CrimeData/2020-06/2020-06-merseyside-street.csv', show_col_types = FALSE)
cd202007 = read_csv('CrimeData/2020-07/2020-07-merseyside-street.csv', show_col_types = FALSE)
cd202008 = read_csv('CrimeData/2020-08/2020-08-merseyside-street.csv', show_col_types = FALSE)
cd202009 = read_csv('CrimeData/2020-09/2020-09-merseyside-street.csv', show_col_types = FALSE)
cd202010 = read_csv('CrimeData/2020-10/2020-10-merseyside-street.csv', show_col_types = FALSE)
cd202011 = read_csv('CrimeData/2020-11/2020-11-merseyside-street.csv', show_col_types = FALSE)
cd202012 = read_csv('CrimeData/2020-12/2020-12-merseyside-street.csv', show_col_types = FALSE)
cd202101 = read_csv('CrimeData/2021-01/2021-01-merseyside-street.csv', show_col_types = FALSE)
cd202102 = read_csv('CrimeData/2021-02/2021-02-merseyside-street.csv', show_col_types = FALSE)
cd202103 = read_csv('CrimeData/2021-03/2021-03-merseyside-street.csv', show_col_types = FALSE)
cd202104 = read_csv('CrimeData/2021-04/2021-04-merseyside-street.csv', show_col_types = FALSE)
cd202105 = read_csv('CrimeData/2021-05/2021-05-merseyside-street.csv', show_col_types = FALSE)
cd202106 = read_csv('CrimeData/2021-06/2021-06-merseyside-street.csv', show_col_types = FALSE)
cd202107 = read_csv('CrimeData/2021-07/2021-07-merseyside-street.csv', show_col_types = FALSE)
cd202108 = read_csv('CrimeData/2021-08/2021-08-merseyside-street.csv', show_col_types = FALSE)
cd202109 = read_csv('CrimeData/2021-09/2021-09-merseyside-street.csv', show_col_types = FALSE)
cd202110 = read_csv('CrimeData/2021-10/2021-10-merseyside-street.csv', show_col_types = FALSE)
cd202111 = read_csv('CrimeData/2021-11/2021-11-merseyside-street.csv', show_col_types = FALSE)
cd202112 = read_csv('CrimeData/2021-12/2021-12-merseyside-street.csv', show_col_types = FALSE)

crimedata = add_row(cd201906) %>%   
  add_row(cd201907) %>%  add_row(cd201908) %>%   add_row(cd201909) %>%  add_row(cd201910) %>%
  add_row(cd201911) %>%   add_row(cd202001) %>%   add_row(cd202002) %>%   add_row(cd202003) %>%   add_row(cd202004) %>%
  add_row(cd202005) %>%   add_row(cd202006) %>%   add_row(cd202007) %>%   add_row(cd202008) %>%   add_row(cd202009) %>%
  add_row(cd202010) %>%   add_row(cd202011) %>%   add_row(cd202012) %>%   add_row(cd202101) %>%   add_row(cd202102) %>% 
  add_row(cd202103) %>%   add_row(cd202104) %>%   add_row(cd202105) %>%   add_row(cd202106) %>%   add_row(cd202107) %>%
  add_row(cd202108) %>%   add_row(cd202109) %>%   add_row(cd202110) %>%  add_row(cd202111) %>%   add_row(cd202112) %>% 
  add_row(cd201906g)

write.csv(crimedata, "CrimeData/MergedCrimeData.csv") 


# Cleaning

crimedata = read_csv('CrimeData/MergedCrimeData.csv') %>% 
  select(Month, `LSOA code`, `Crime type`)

colnames(crimedata) = c("Year", "lsoa11cd", "CrimeType")


LsoaToPostcode = read_csv('Postcode to LSOA.csv')

crimedataCleaned = crimedata %>%  
  left_join(LsoaToPostcode,by="lsoa11cd") %>% 
  mutate(shortPostcode = str_trim(stri_sub(pcds,-3))) %>% 
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  group_by(shortPostcode,CrimeType,Year)%>% 
  select(shortPostcode, Year, CrimeType, ) %>% 
  na.omit() %>% 
  tally()

crimedataCleaned = cbind(ID = 1:nrow(crimedataCleaned), crimedataCleaned)  
colnames(crimedataCleaned)= c("ID","shortPostcode","CrimeType","Year" , "CrimeCount")

write.csv(crimedataCleaned, "CleanedData/Crime.csv") 


#Changing the column names for crime data
crime = read_csv('CleanedData/Crime.csv')
colnames(crime)= c("ID","shortPostcode","Year","CrimeType","CrimeCount")
write.csv(crime, "CleanedData/CleanCrime.csv") 
crime1 = read_csv('CleanedData/CleanCrime.csv')


