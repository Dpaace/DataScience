library(tidyverse)


setwd('D:/')
setwd('DATA')



#---------------house rank------
Towns = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, County)

House_price = read_csv("CleanedData/CleanedHousePrices.csv")

#house rank
Houseprice= House_price %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

housePrices=Houseprice  %>% 
  group_by(Town) %>% 
  summarise(Price=mean(Price)) %>% 
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(Town, HouseScore)

housePrices


#download rank

speed_downloads = read_csv("CleanedData/cleanBroadband.csv")

Towns = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, County)


Speed_Download = speed_downloads %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

colnames(Speed_Download)=c("1","ID","shortPostcode","AverageDownload","MaxDownload","Averageupload","Maxupload")

download_speed=speed_downloads%>% 
  group_by(Town) %>% 
  summarise(downloadSpeed=AverageDownload) %>% 
  arrange(downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/48.4)) %>% 
  select(Town,DownloadScore)

download_speed



#crime score rank
crime_score=read_csv("CleanedData/CleanCrime.csv")

names(crime_score)

Towns = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, County)

crime_rank = crime_score %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(Town) %>% 
  summarise(score=mean(CrimeCount)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1200)) %>% 
  select(Town,score)

crime_rank


#school score
school_score=read_csv("CleanedData/School.csv")

Towns = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, County)


school_rank = school_score %>%
  rename(shortPostcode=shortPostCode) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

school_rank=school_rank%>% 
  group_by(District) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1800)) %>% 
  select(District,score)

school_rank
