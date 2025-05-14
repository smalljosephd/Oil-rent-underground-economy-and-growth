###UK Brent
oil_price1 <- read_xlsx("C:/Users/user/Desktop/Data Bank/ASB2023_all/T72.xlsx",
                       sheet = "Table 7.2",
                       range="a4:IS25",
                       col_types = ,
                       col_names = c('Country', format(seq(as.Date("1983-01-01"), as.Date("2003-12-01"), by = "1 month"), "%b-%Y")),
                                     na='na')%>%
  filter(Country=='United Kingdom - BrentDated')%>%
  pivot_longer(cols = `Jan-1983`:`Dec-2003`, 
               names_to = "Year",
               values_to = "UK_Brent", 
               values_drop_na = FALSE)

oil_price2 <- read_xlsx("C:/Users/user/Desktop/Data Bank/ASB2023_all/T72.xlsx",
                        sheet = "Table 7.2",
                        range="A28:HU49",
                        col_types = ,
                        col_names = c('Country', format(seq(as.Date("2004-01-01"), as.Date("2022-12-01"), by = "1 month"), "%b-%Y")),
                        na='na')%>%
  filter(Country=='United Kingdom - BrentDated')%>%
  pivot_longer(cols = `Jan-2004`:`Dec-2022`,
               names_to = "Year",
               values_to = "UK_Brent",
               values_drop_na = FALSE)

  

oil_price <- oil_price1%>%
  full_join(oil_price2)%>%
  mutate(Qtr = case_when(str_detect(Year, 'Jan|Feb|Mar')~1,
                       str_detect(Year, 'Apr|May|Jun')~2,
                       str_detect(Year, 'Jul|Aug|Sep')~3,
                       str_detect(Year, 'Oct|Nov|Dec')~4,
                       TRUE~1))%>%
  separate(Year, into = c("Month", "Year"), sep = "-", extra = "merge")%>%
  group_by(Year, Qtr)%>%
  mutate(avg_price = mean(UK_Brent))%>%
  slice(1)%>%
  ungroup()%>%
  dplyr::select(-c(UK_Brent, Month))
#Save
writexl::write_xlsx(oil_price, 'C:/Users/user/Desktop/Work/Research Papers/OIL, UE, and ECO/Data/Brent price.xlsx')


#Nigerian Bonny light
oil_price1_ <- read_xlsx("C:/Users/user/Desktop/Data Bank/ASB2023_all/T71.xlsx",
                        sheet = "Table 7.1",
                        range="a13:IS13",
                        col_types = ,
                        col_names = c('Country', format(seq(as.Date("1983-01-01"), as.Date("2003-12-01"), by = "1 month"), "%b-%Y")),
                        na='na')%>%
  #filter(Country=='United Kingdom - BrentDated')%>%
  pivot_longer(cols = `Jan-1983`:`Dec-2003`, 
               names_to = "Year",
               values_to = "Bonny_light", 
               values_drop_na = FALSE)

oil_price2_ <- read_xlsx("C:/Users/user/Desktop/Data Bank/ASB2023_all/T71.xlsx",
                        sheet = "Table 7.1",
                        range="A29:HU29",
                        col_types = ,
                        col_names = c('Country', format(seq(as.Date("2004-01-01"), as.Date("2022-12-01"), by = "1 month"), "%b-%Y")),
                        na='na')%>%
  #filter(Country=='United Kingdom - BrentDated')%>%
  pivot_longer(cols = `Jan-2004`:`Dec-2022`,
               names_to = "Year",
               values_to = "Bonny_light",
               values_drop_na = FALSE)



oil_price_ <- oil_price1_%>%
  full_join(oil_price2_)%>%
  mutate(Qtr = case_when(str_detect(Year, 'Jan|Feb|Mar')~1,
                         str_detect(Year, 'Apr|May|Jun')~2,
                         str_detect(Year, 'Jul|Aug|Sep')~3,
                         str_detect(Year, 'Oct|Nov|Dec')~4,
                         TRUE~1))%>%
  separate(Year, into = c("Month", "Year"), sep = "-", extra = "merge")%>%
  group_by(Year, Qtr)%>%
  mutate(avg_price = mean(Bonny_light))%>%
  slice(1)%>%
  ungroup()%>%
  dplyr::select(-c(Bonny_light, Month))
#Save
writexl::write_xlsx(oil_price_, 'C:/Users/user/Desktop/Work/Research Papers/OIL, UE, and ECO/Data/Bonny light price.xlsx')


##Rainfal

Rainfall <- read_rds("C:/Users/user/Dropbox/Project Macro Impact of Flooding/2 Empirical and data/1_data/2_intermediate/global rainfall_1901-2022.RDa")%>%
  filter(Country=='Nigeria', Year >= '1980')%>%
  mutate(#Month = as.character(Month),
         Qtr = case_when(str_detect(Month, '01|02|03')~1,
                         str_detect(Month, '04|05|06')~2,
                         str_detect(Month, '07|08|09')~3,
                         #str_detect(Year, '10|11|12')~4,
                         TRUE~4))%>%
  group_by(Year, Qtr)%>%
  mutate(avg_rain = mean(avg_prec))%>%
  slice(1)%>%
  ungroup()%>%
  dplyr::select(-c(avg_prec, Month, Date, Country))
  #Save
  writexl::write_xlsx(Rainfall, 'C:/Users/user/Desktop/Work/Research Papers/OIL, UE, and ECO/Data/Average rainfall.xlsx')  

###EM-DAT flooding
###
Flood <- read_xlsx("C:/Users/user/Desktop/Work/Research Papers/OIL, UE, and ECO/Data/public_emdat_custom_request_2024-02-01_849e4273-9e25-4071-be4d-39e67d8c4c8a.xlsx")%>%
  dplyr::select(`Start Year`:`End Month`, -c(`Start Day`))%>%
  filter(`End Year` != 2023)%>%
  mutate(Qtr = case_when(between(`Start Month`, 1, 3) ~ 1, between(`Start Month`, 4, 6) ~ 2, between(`Start Month`, 7, 9) ~ 3, between(`Start Month`, 10, 12) ~ 4, TRUE ~ NA), 
         Qtr2 = case_when(between(`End Month`, 1, 3) ~ 1, between(`End Month`, 4, 6) ~ 2, between(`End Month`, 7, 9) ~ 3, between(`End Month`, 10, 12) ~ 4, TRUE ~ NA),
         Flood_incidence = if_else(Qtr==Qtr2, 1, 2))

         
Flood_ <- read_rds("C:/Users/user/Dropbox/Project Macro Impact of Flooding/2 Empirical and data/1_data/2_intermediate/global rainfall_1901-2022.RDa")%>%
  filter(Country=='Nigeria', Year >= '1980')%>%
  mutate(#Month = as.character(Month),
    Qtr = case_when(str_detect(Month, '01|02|03')~'Q1',
                    str_detect(Month, '04|05|06')~'Q2',
                    str_detect(Month, '07|08|09')~'Q3',
                    #str_detect(Year, '10|11|12')~4,
                    TRUE~'Q4'))%>%
  group_by(Year, Qtr)%>%
  slice(1)%>%
  ungroup()%>%
  dplyr::select(Year, Qtr)%>%
  mutate(flood_incidence = case_when(str_detect(Year, '2005') & Qtr =='Q1' ~ 1,
                                     str_detect(Year, '2000|2001|2002|2004|2009|2011|2013|2014|2020') & Qtr =='Q2' ~ 1,
                                     str_detect(Year, '1985|1988|1994|1998|1999|2000|2001|2003|2004|2005|2006|2007|2009|2010|2011|2012|2013|2015|2016|2017|2018|2019|2020|2021|2022') & Qtr =='Q3' ~ 1,
                                     str_detect(Year, '1998|1999|2003|2006|2007|2011|2012|2018|2020|2022') & Qtr =='Q4' ~ 1,
                                     TRUE ~ 0))

writexl::write_xlsx(Flood_, 'C:/Users/user/Desktop/Work/Research Papers/OIL, UE, and ECO/Data/Flood incidence.xlsx')          
