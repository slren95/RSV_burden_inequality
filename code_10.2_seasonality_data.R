#
# ------------------------------------------------------------------------------
# Script: code_10.2_seasonality_data.R
# Purpose:
#   Collect and standardise RSV seasonality inputs for selected countries (with
#   licensed / NIP-included products such as nirsevimab or RSVpreF), typically by
#   extracting weekly/monthly RSV activity from country reports and public data.
#
# Inputs:
#   - Various country-specific files under `data/` (PDFs/CSVs/XLSX; see script).
#   - functions.R
#
# Outputs:
#   - Cleaned seasonality time series objects created within the script.
#   - Figures and summary tables produced downstream (see plotting/export calls).
#
# Usage:
#   source("code_10.2_seasonality_data.R")
#
# Notes:
#   This is a curated, country-by-country workflow; file paths and page numbers
#   are intentionally explicit and may require maintenance as sources change.
# ------------------------------------------------------------------------------

rm(list = ls())
library(rio)
library(tidyverse)
library(tabulapdf)
library(janitor)
library(skimr)
library(summarytools)
library(ggsci)
library(patchwork)
library(MCMCpack)
library(flextable)
library(officer)
library(grid)

source('functions.R')

# Countries has licensed(Include NIP) Nirsevimab or RSVpreF----
##  Chile ----
# df_chile<-import('data/table-chile-2023.json') %>%
#   mutate(week=str_sub(text,8,9))


# Extract tables from the PDF report
file_path <- "data/chile_2023/SE01-10-01-2023-1.pdf"
page_number <- 4  # target page number

# Extract the table data
extract_tables(file_path, pages = page_number)[[1]]


# tb_list1<-dir('data/chile_2023',full.names = T)[1:33] %>%
#   map(~{
#     week=sub('.*/SE(.{2})-.*','\\1',.x)
#     extract_tables(.x, pages = 4)[[1]] %>%
#       mutate(week=week,.before=1) %>%
#       mutate(across(everything(),~as.character(.x)))
#   })
# 
# tb_list2<-dir('data/chile_2023',full.names = T)[35:52] %>%
#   map(~{
#     week=sub('.*/SE(.{2})-.*','\\1',.x)
#     extract_tables(.x, pages = 4)[[1]] %>%
#       mutate(week=week,.before=1) %>%
#       mutate(across(everything(),~as.character(.x)))
#   })
# 
# extract_tables("data/chile_2023/SE34-29-08-2023-1-1.pdf", pages = 3)
# 
# tb_list3<-import('data/chile-2023-week34.xlsx',skip=1) %>%
#   mutate(week=34,.before=1) %>%
#   mutate(across(everything(),~as.character(.x))) %>%
#   setNames(names(tb_list2[[1]]))

# save(tb_list1,tb_list2,tb_list3,file = 'data/tb_list_1-3.RData')
load('data/tb_list_1-3.RData')


tb_chile<-bind_rows(tb_list1,tb_list2,tb_list3) %>%
  clean_names() %>%
  mutate(across(c(1:2,4:11),~as.numeric(.x)))

tb_chile_lite<-tb_chile %>%
  filter(ano=='2023') %>%
  arrange(week) %>%
  transmute(week,rsv_per_day=vrs/7)

df_chile<-data.frame(day=seq(ymd("2023-01-01"),ymd('2023-12-31'),by='day')) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(tb_chile_lite)

df_ses_chile<-df_chile %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm = T)),.by=month)

plot(df_ses_chile,type='o')


##  USA ----
# https://www.cdc.gov/resp-net/dashboard/index.html
df_usa<-import('data/Rates_of_Laboratory-Confirmed_RSV__COVID-19__and_Flu_Hospitalizations_from_the_RESP-NET_Surveillance_Systems_20241206.csv') %>% 
  clean_names() %>%
  mutate_if(is.POSIXct,as.Date)

df_usa_lite<-df_usa %>%
  filter(surveillance_network=='RSV-NET',season=='2023-24',
         #mmwr_year==2023,
         age_group=='0-<1 yr',site=='Overall',
         sex=='Overall',race_ethnicity=='Overall') %>%
  transmute(season,mmwr_week,weekly_rate,week_ending_date) %>%
  arrange(week_ending_date)


df_ses_usa<-data.frame(day=seq(ymd("2023-10-01"),by='day',length.out=365)) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_usa_lite[,c('mmwr_week','weekly_rate')],by=c('week'='mmwr_week')) %>%
  summarise(rsv=round(sum(weekly_rate,na.rm = T)),.by=month)

plot(df_ses_usa$rsv,type='o')

##  Saudi Arabia ----
# Five Years’ Experience with Respiratory Syncytial  Virus Among Hospitalized Patients:  A Retrospective Study from Jazan, Saudi Arabia

df_ses_arabia<-data.frame(month=1:12,rsv=c(43,23,19,10,10,7,4,3,3,4,12,57))
plot(df_ses_arabia,type='o')

##  India ----
df_india<-import('data/Seasonality-India.xlsx')

df_india %>% dplyr::select(Jan:Dec) %>% colSums()
  
df_ses_india<-data.frame(month=c(1:12),rsv=df_india %>% dplyr::select(Jan:Dec) %>% colSums())
plot(df_ses_india$rsv,type='o')


##  China ----
# Incident changes in the prevalence of respiratory virus among children during COVID-19 pandemic in Hangzhou, China
df_ses_china<-data.frame(month=1:12,rsv=c(1925,962,424,62,15,10,14,9,19,21,172,2159))
plot(df_ses_china,type='o')

##  Brazil ----
# https://www.sciencedirect.com/science/article/pii/S2772707624001681#fig0002
df_brazil<-import('data/Brazil-webplot.csv') %>%
  mutate(across(where(is.numeric),round)) %>%
  rename(week=V1,rsv=V2) %>%
  arrange(week)

df_ses_brazil<-data.frame(day=seq(ymd("2022-01-01"),ymd('2022-12-31'),by='day')) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_brazil) %>%
  summarise(rsv=round(sum(rsv/7,na.rm = T)),.by=month)

plot(df_ses_brazil$rsv,type='o')

##  Australian ----
# https://www.immunisationcoalition.org.au/news-data/respiratory-syncytial-virus-rsv-statistics/
df_ses_australian<-data.frame(month=1:12,rsv=c(2134,3645,9319,14280,19919,22751,19250,14712,8744,4920,3898,4372))
plot(df_ses_australian$rsv,type='o')


## Thailand ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC10248108/#ack1
df_thailand<-import('data/Thailand-webplot.csv') %>%
  round() %>%
  setNames(c('month','pos')) %>%
  mutate(month=rep(1:12,4))

df_ses_thailand<-summarise(df_thailand,rsv=sum(pos),.by=month)

plot(df_ses_thailand$rsv,type='o')

## Singapore ----
# https://www.nature.com/articles/s41598-020-76888-4/figures/1
df_ses_singapore<-import('data/Singapore-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv'))

plot(df_ses_singapore$rsv,type='o')

## Uruguay ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC11521816/#sec13

df_ses_uruguay<-import('data/Uruguay-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv'))

plot(df_ses_uruguay$rsv,type='o')

## Qatar ----
df_ses_qatar<-import('data/Qatar-webplot.csv') %>%
  round() %>%
  group_by(V1) %>%
  mutate(V3=V2-lag(V2)) %>%
  filter(!is.na(V3)) %>%
  dplyr::select(V1,V3) %>%
  setNames(c('month','rsv'))

plot(df_ses_qatar$rsv,type='o')

## United Arab Emirates----

df_ses_arab<-import('data/United Arab Emirates_webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv'))

plot(df_ses_arab$rsv,type='o')

## Kuwait
# https://pmc.ncbi.nlm.nih.gov/articles/PMC7087269/

df_ses_kuwait<-import('data/Kuwait-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv'))

## Japan ----

df_ses_japan<-import('data/Japan-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv'))

plot(df_ses_japan$rsv,type='o')

## Korea----
df_ses_korea<-import('data/Korea-webplot.csv') %>%
  round() %>%
  mutate(V1=c(3:12,1:2)) %>%
  arrange(V1) %>%
  setNames(c('month','rsv'))

## Canada ----
#https://health-infobase.canada.ca/respiratory-virus-surveillance/archive/respiratory-virus-detections/2024-09-05/#exploration

df_canada<-import('data/canada-2025-07-07.csv') %>%
  clean_names() %>%
  mutate(week=as.numeric(surveillance_week),
         rsv_per_day=parse_number(number_of_detections)/7)


df_ses_canada<-data.frame(day=seq(ymd("2023-8-27"),ymd('2024-8-27'),by='day')) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_canada) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm = T)),.by=month)

plot(df_ses_canada$rsv,type='o')

##Switerland----
# https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762%2824%2900217-5/fulltext?utm_source=chatgpt.com#fig1

df_switerland<-import('data/European/Switerland-webplot.csv') %>%
  round() %>%
  setNames(c('index','rsv'))

df_ses_switerland<-data.frame(day=seq(ymd("2023-07-01"),ymd('2024-06-30'),by='month')) %>%
  mutate(month=month(day),index=1:12) %>%
  left_join(df_switerland) %>%
  transmute(month,rsv)


##England----
# https://www.gov.uk/government/statistics/surveillance-of-influenza-and-other-seasonal-respiratory-viruses-in-the-uk-winter-2023-to-2024/surveillance-of-influenza-and-other-seasonal-respiratory-viruses-in-the-uk-winter-2023-to-2024?utm_source=chatgpt.com#note1
df_england<-import('data/European/England-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(27:52,1:26),rsv_per_day=rsv/7)

df_ses_england<-data.frame(day=seq(ymd("2023-07-02"),by='day',length.out=365)) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_england) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

##Italy----
# https://pubmed.ncbi.nlm.nih.gov/40089054/
df_italy<-import('data/European/Italy-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(27:52,1:26),rsv_per_day=rsv/7)

df_ses_italy<-data.frame(day=seq(ymd("2023-07-02"),by='day',length.out=365)) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_italy) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)


##Croatia----
# https://pubmed.ncbi.nlm.nih.gov/36560751/
df_ses_croatia<-import('data/European/Croatia-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(8:12,1:7))

## 15 Country ----
#https://pubmed.ncbi.nlm.nih.gov/29409569/

## Portugal----
#https://pubmed.ncbi.nlm.nih.gov/36377322/

df_ses_portugal<-data.frame(
  month=c(6:12,1:5),
  rsv=c(501,538,728,992,1298,1615,1472,840,462,394,409,448)
)

## Slovenia----
#https://pubmed.ncbi.nlm.nih.gov/32656961/
df_ses_slovenia<-import('data/European/Slovenia-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(10:12,1:9))

## Netherlands----
#https://www.rivm.nl/en/rsv/annual-reporting-2023-2024
df_netherlands<-read_csv2('data/European/Netherland.csv') %>%
  dplyr::select(1,7) %>% 
  mutate(day=seq(ymd("2014-10-01"),by='week',length.out=52),
         week=week(day),rsv_per_day=as.numeric(`Average 2014/2015 – 2018/2019`)/7)

df_ses_netherlands<-data.frame(
  day=seq(ymd("2014-10-01"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_netherlands %>% dplyr::select(week,rsv_per_day)) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)
  

## Luxembourg----
#https://lns.lu/wp-content/uploads/2025/02/revilux_2025-02-05.pdf
df_luxembourg<-import('data/European/Luxembourg-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(40:52,1:39),rsv_per_day=rsv/7)

df_ses_luxembourg<-data.frame(
  day=seq(ymd("2023-10-01"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_luxembourg) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Ploand----
#https://pubmed.ncbi.nlm.nih.gov/38793586/

df_ses_poland<-import('data/European/Poland-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(7:12,1:6))


## France----
#https://link.springer.com/article/10.1007/s40121-022-00737-2
df_ses_france<-import('data/European/France-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(10:12,1:9))


## Finland ----
#https://academic.oup.com/jid/article/230/5/e985/7697800
df_ses_finland<-import('data/European/Finland-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(6:12,1:5))

## Austria----
#https://www.sari-dashboard.at/
df_austria<-import('data/European/Austria-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(32:52,1:31),rsv_per_day=rsv/7)

df_ses_austria<-data.frame(
  day=seq(ymd("2024-08-05"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_austria %>% dplyr::select(week,rsv_per_day)) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Norway----
#https://www.sciencedirect.com/science/article/pii/S1047279725001322
df_norway<-import('data/European/Norway-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(26:52,1:25),rsv_per_day=rsv/7)

df_ses_norway<-data.frame(
  day=seq(ymd("2023-06-24"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_norway %>% dplyr::select(week,rsv_per_day)) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Iceland ----
#  https://island.is/en/respiratory-tract-infections 

df_iceland<-import('data/European/Iceland-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(27:52,1:26),rsv_per_day=rsv/7)

df_ses_iceland<-data.frame(day=seq(ymd("2024-07-01"),by='day',length.out=365)) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_iceland) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Sweden ----
# https://www.folkhalsomyndigheten.se/folkhalsorapportering-statistik/statistik-a-o/sjukdomsstatistik/rs-virus/

df_sweden<-import('data/European/Sweden-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(21:52,1:20),rsv_per_day=rsv/7)

df_ses_sweden<-data.frame(
  day=seq(ymd("2023-5-21"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_sweden) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Denmark ----
# https://en.ssi.dk/surveillance-and-preparedness/surveillance-in-denmark/annual-reports-on-disease-incidence/i/integrated-surveillance-of-respiratory-infections-in-2024-25--consolidated-report
df_denmark<-import('data/European/Denmark-webplot.csv') %>%
  round() %>%
  mutate(V2=ifelse(row_number()<25,0,V2)) %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(21:52,1:20),rsv_per_day=rsv/7)

df_ses_denmark<-data.frame(
  day=seq(ymd("2025-5-21"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_denmark) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Lithuania----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5818254/
df_ses_lithuania<-data.frame(
  month=c(10:12,1:9),
  rsv=c(0,1,4,13,42,38,10,rep(0,5))
)

## Czechia ----
## https://archiv.szu.cz/uploads/documents/CeM/NRLs/chripka/HERA_prezentace/RSV_surveillance_in_covid_times_.pdf
df_czechia<-import('data/European/Czechia-webplot.csv') %>%
  round() %>%
  setNames(c('index','rsv')) %>%
  transmute(week=c(37:52,1:36)[index],rsv_per_day=rsv/7)

df_czechia2<-bind_rows(df_czechia,data.frame(week=setdiff(1:52,df_czechia$week),rsv_per_day=0))

df_ses_czechia<-data.frame(
  day=seq(ymd("2018-9-10"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_czechia2) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Bulgaria ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC11860199/
df_bulgaria<-import('data/European/Bulgaria-webplot.csv') %>%
  round() %>%
  setNames(c('index','n')) %>%
  mutate(week=c(40:52,1:20)[index]) %>%
  group_by(week) %>%
  mutate(rsv=n-lag(n)) %>%
  ungroup() %>%
  filter(!is.na(rsv))

df_bulgaria2<-data.frame(week=c(40:52,1:39)) %>%
  left_join(df_bulgaria %>% dplyr::select(week,rsv)) %>%
  replace_na(list(rsv=0)) %>%
  transmute(week,rsv_per_day=rsv/7)

df_ses_bulgaria<-data.frame(
  day=seq(ymd("2023-10-1"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_bulgaria2) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Ireland ----
# https://respiratoryvirus.hpsc.ie/pages/9718c53078b642fd861c061038d18b35

df_ireland<-import('data/European/Ireland-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(33:52,1:32),rsv_per_day=rsv/7)

df_ses_ireland<-data.frame(
  day=seq(ymd("2024-8-12"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_ireland) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Germany ----
# https://infektionsradar.gesund.bund.de/en/rsv/incidence
df_germany<-data.frame(rsv_rate=c(.03,.04,.06,.04,.03,.03,.05,.07,.03,.04,.07,.07,.1,.11,.18,.27,.44,.52,.82,1.18,.74,1.21,1.79,2.23,3.63,4.71,5.21,5.93,6.51,6.85,
                                  6.54,6.41,5.44,5.10,4.20,3.41,2.25,1.69,1.02,0.89,0.55,0.33,0.19,0.17,0.07,0.05,0.03,0.02,0.01,0.01,0.01,0.01)/100000,
                       week=c(32:52,1:31)) %>%
  transmute(week,rsv_per_day=rsv_rate*80000000/7)

df_ses_germany<-data.frame(
  day=seq(ymd("2024-8-05"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_germany) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Surveillance European ----
# https://erviss.org/
df_eur_raw<-import('data/European/European-RSV-Seasonality-jingzou.xlsx') %>%
  rename(country=1) %>%
  `[`(,-54)

df_type<-import('model_data/df_type.rds')

df_eur<-df_eur_raw %>%
  filter(country %in% c('Belgium','Cyprus','Estonia','Malta','Romania','Spain')) %>%
  mutate(across(-country,~as.numeric(.x) %>% replace_na(1))) %>%
  pivot_longer(cols = -country,names_pattern = '2024-W(.*)',names_to = 'week') %>%
  mutate(rsv_per_day=value/7,week=as.numeric(week))

df_type %>% filter(Country %in% c('Belgium','Cyprus','Estonia','Malta','Romania','Spain')) %>%
  transmute(ISO3=ISOCountry,Country)

df_ses_eur_surv<-c('Belgium','Cyprus','Estonia','Malta','Romania','Spain') %>%
  map_dfr(~{
    data.frame(day=seq(ymd("2024-07-22"),by='day',length.out=365)) %>%
      mutate(week=week(day),month=month(day)) %>%
      left_join(df_eur[df_eur$country==.x,c('country','week','rsv_per_day')]) %>%
      summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month) %>%
      mutate(country=.x)
  }) %>%
  left_join(df_type %>% filter(Country %in% c('Belgium','Cyprus','Estonia','Malta','Romania','Spain')) %>%
              transmute(ISO3=ISOCountry,country=Country))

## Argentina ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC8906024/
df_ses_argentina<-import('data/Argentina-webplot.csv') %>%
  round() %>%
  set_names(c('month','rsv'))

##  Mexico ----
# https://www.paho.org/es/situacion-virus-sincitial-respiratorio-rsv-region-americas
df_mexico<-import('data/WHO/Mexico-webplot.csv') %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(1:52),rsv_per_day=rsv/7,
            rsv_per_day=ifelse(rsv_per_day<0,0,rsv_per_day))

df_ses_mexico<-data.frame(
  day=seq(ymd("2024-1-1"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_mexico) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Panama,Dominican Republic,Costa Rica,El Salvador ----
# https://www.paho.org/es/situacion-virus-sincitial-respiratorio-rsv-region-americas
df_central_amercian<-import('data/WHO/Central-Amercian.xlsx')

df_ses_central_amercian<-data.frame(
  day=seq(ymd("2024-1-8"),by='day',length.out=365)
) %>%
  mutate(month=month(day)) %>%
  left_join(df_central_amercian) %>%
  mutate(rsv_per_day=RSV_positive_samples/7) %>%
  fill(rsv_per_day, .direction = "down") %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## Peru ----
# https://www.paho.org/es/situacion-virus-sincitial-respiratorio-rsv-region-americas
df_peru<-import('data/WHO/Peru-webplot.csv') %>%
  round() %>%
  setNames(c('week','rsv')) %>%
  transmute(week=c(1:52),rsv_per_day=rsv/7)

df_ses_peru<-data.frame(
  day=seq(ymd("2024-1-1"),by='day',length.out=365)
) %>%
  mutate(week=week(day),month=month(day)) %>%
  left_join(df_peru) %>%
  summarise(rsv=round(sum(rsv_per_day,na.rm=T)),.by=month)

## South Africa ----
# https://www.nicd.ac.za/diseases-a-z-index/respiratory-syncytial-virus-rsv/
# https://www.nicd.ac.za/wp-content/uploads/2025/03/2025-RSV-alert.pdf

df_ses_south_africa<-import('data/WHO/South-Africa-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(1:12))

## Philippines ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5176282/
df_ses_philippines<-import('data/WHO/Philippines-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(1:12))

## Oman ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC7166482/ 

df_ses_oman<-import('data/WHO/Oman-webplot.csv') %>%
  round() %>%
  setNames(c('rsv','month')) %>%
  mutate(month=c(1:12))

## Pakistan ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5805860/
df_ses_pakistan<-import('data/WHO/Pakistan-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(1:12))

##Turkey ----
# https://turkjpediatr.org/article/view/1624/1606
df_ses_turkey<-import('data/WHO/Turkey-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(8:12,1:7))

## Israel ----
# https://ijhpr.biomedcentral.com/articles/10.1186/s13584-025-00693-5
df_ses_israel<-import('data/WHO/Israel-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(7:12,1:6))

## Ghana ----
# https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-025-11201-0
df_ses_ghana<-import('data/WHO/Ghana-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(8:12,1:7))


## Malaysia ----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC9914795/

df_ses_malaysia<-import('data/WHO/Malaysia-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(1:12))

## Egypt ----
# https://onlinelibrary.wiley.com/doi/10.1111/irv.12409
df_ses_egypt<-import('data/WHO/Egypt-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(1:12))

## Indonesia ----
# https://pubmed.ncbi.nlm.nih.gov/15654405/
df_ses_indonesia<-import('data/WHO/Indonesia-webplot.csv') %>%
  round() %>%
  setNames(c('month','rsv')) %>%
  mutate(month=c(1:12))

# Merge Data-----
df_seasonality<-bind_rows(list(
  USA=df_ses_usa,
  CHN=df_ses_china,
  IND=df_ses_india,
  SAU=df_ses_arabia,
  CHL=df_ses_chile,
  AUS=df_ses_australian,
  BRA=df_ses_brazil,
  THA=df_ses_thailand,
  SGP=df_ses_singapore,
  URY=df_ses_uruguay,
  QAT=df_ses_qatar,
  ARE=df_ses_arab,
  KWT=df_ses_kuwait,
  JPN=df_ses_japan,
  KOR=df_ses_korea,
  CAN=df_ses_canada,
  CHE=df_ses_switerland,
  GBR=df_ses_england,
  ITA=df_ses_italy,
  HRV=df_ses_croatia,
  PRT=df_ses_portugal,
  SVN=df_ses_slovenia,
  NLD=df_ses_netherlands,
  LUX=df_ses_luxembourg,
  POL=df_ses_poland,
  FRA=df_ses_france,
  FIN=df_ses_finland,
  AUT=df_ses_austria,
  NOR=df_ses_norway,
  ISL=df_ses_iceland,
  SWE=df_ses_sweden,
  DNK=df_ses_denmark,
  LTU=df_ses_lithuania,
  CZE=df_ses_czechia,
  BGR=df_ses_bulgaria,
  IRL=df_ses_ireland,
  DEU=df_ses_germany,
  ARG=df_ses_argentina,
  ## WHO 
  MEX=df_ses_mexico,
  PAN=df_ses_central_amercian,
  DOM=df_ses_central_amercian,
  SLV=df_ses_central_amercian,
  CRI=df_ses_central_amercian,
  PER=df_ses_peru,
  ZAF=df_ses_south_africa,
  PHL=df_ses_philippines,
  OMN=df_ses_oman,
  PAK=df_ses_pakistan,
  TUR=df_ses_turkey,
  ISR=df_ses_israel,
  GHA=df_ses_ghana,
  MYS=df_ses_malaysia,
  EGY=df_ses_egypt,
  IDN=df_ses_indonesia
),.id='ISO3') %>%
  bind_rows(df_ses_eur_surv %>% dplyr::select(-country)) %>%
  mutate(index = ave(rsv, ISO3, FUN = seq_along),
         monthabb=month.abb[month]) %>%
  group_by(ISO3) %>%
  arrange(ISO3,desc(rsv)) %>%
  mutate(AAP=rsv/sum(rsv),
         CumAAP=cumsum(rsv)/sum(rsv)) %>%
  mutate(top_six=row_number()<7) %>%
  arrange(ISO3,index) %>%
  ungroup()

# Consecutive six months with highest cases ----
begin_month<-split(df_seasonality,df_seasonality$ISO3) %>%
  purrr::map_dbl(~{
    month<-.x$month
    rsv<-rep(.x$rsv,2)
    #print(month)
    #print(rsv)
    res<-purrr::map(1:12,~{
      sum(rsv[seq(.x,.x+5)])
    })
    month[which.max(res)]
  })

begin_month

df_seasonality<-df_seasonality %>%
  mutate(epi_onset=begin_month[ISO3],
    epi_period=map(epi_onset,~c(1:12,1:12)[seq(.x,.x+5)]),
    epi_month=map2_lgl(month,epi_period,~.x %in% .y),
    epi_period_str=map_chr(epi_period,~paste0(month.abb[.x[1]],'-',month.abb[.x[6]])))

df_seasonality %>% count(ISO3)

# Add Region and Income ----
df_pop<-left_join(
  import('rda/df_pop_center_nir.rds') %>% 
    transmute(ISOCountry,WHORegion,region_ns_zone,Country.Name,WHOName),
  import('model_data/df_pop_2019_2026.rds')) %>%
  mutate(region_income=paste(WHORegion,Income2019,sep='_'),.after=WHORegion)

df_type2<-df_type %>%
  left_join(df_pop %>% transmute(ISOCountry,WHORegion,Income2019,region_income,region_ns_zone))

df_type2 %>% pull(Country)

df_seasonality2<-df_seasonality %>%
  left_join(df_type2 %>% dplyr::select(ISOCountry,Country,WHORegion,Income2019,region_income,region_ns_zone),by=c('ISO3'='ISOCountry')) %>%
  mutate(across(c(WHORegion,region_income,region_ns_zone),toupper)) %>%
  mutate(stripe_text=sprintf('%s (%s)',Country,region_ns_zone))


# appendix_list <- c(
#   "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
#   "Czechia", "Denmark", "Estonia", "Finland", "France",
#   "Germany", "Greece", "Hungary", "Ireland", "Italy",
#   "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
#   "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
#   "Spain", "Sweden", "Switzerland", "Liechtenstein", "United Kingdom",
#   "Norway", "Iceland", "United States", "Canada", "China",
#   "Japan", "Republic of Korea", "Singapore", "India", "Thailand",
#   "United Arab Emirates", "Saudi Arabia", "Qatar", "Kuwait", "Brazil",
#   "Paraguay", "Australia", "Uruguay", "Chile", "Argentina"
# )

# Export appendix table ----

data.frame(Country=df_type2 %>% 
             mutate(WHORegion=ifelse(Country=='Liechtenstein','Eur',WHORegion),
                    ns=gsub('.*_(.)_.x','\\1',region_ns_zone)) %>%
             arrange(WHORegion,ns,Country) %>%
             pull(Country)) %>%
  left_join(df_type2 %>% dplyr::select(Country,nir,abr,WHORegion,region_income,region_ns_zone)) %>%
  left_join(df_seasonality2 %>% distinct(Country,epi_period_str)) %>%
  mutate(nir=recode(nir,'Licensed'='licensed','NIP-included'='NIP',.missing='unlicensed'),
         abr=recode(abr,'Licensed'='licensed','NIP-included'='NIP',.missing='unlicensed')) %>%
  mutate(across(c(WHORegion,region_income,region_ns_zone),toupper)) %>%
  relocate(Country,nir,abr,region_ns_zone,WHORegion,epi_period_str,region_income) %>%
  createAppendixTable('docs/Appendix_countries_list.xlsx')

# setdiff(df_seasonality2$Country,appendix_list)
# setdiff(appendix_list,df_seasonality2$Country)


df_seasonality %>%
  filter(ISO3=='USA') %>%
  mutate(monthabb=factor(monthabb,levels=monthabb)) %>%
  ggplot(aes(monthabb,rsv,color=ISO3))+
  geom_point()+
  geom_line(aes(group = ISO3))+
  labs(title='USA')+
  theme(legend.position = 'NULL')

# Top six AAP
plots <- unique(df_seasonality2$Country) %>%
  purrr::map(~{
    df_seasonality2 %>%
      filter(Country == .x) %>%
      mutate(monthabb = factor(monthabb, levels = monthabb)) %>%
      ggplot(aes(monthabb, rsv, color = top_six)) +
      geom_point(aes(shape=factor(top_six))) +
      geom_line(aes(group = ISO3)) +
      labs(title = .x) +
      theme(legend.position = 'none')
  })


plots[[1]]

df_seasonality2 %>%
  distinct(Country,.keep_all = T) %>%
  count(region_ns_zone)

# Epidemic onset month (consecutive six months with highest cases)----

# X axis: Jan to Dec
ggplot(df_seasonality2,aes(month,rsv,color=factor(epi_month)))+
  geom_line(aes(group = ISO3))+
  geom_point(shape = 21,fill='white')+
  facet_wrap(vars(stripe_text),scales = 'free',nrow = 4)+
  theme_bw()+
  scale_x_continuous(breaks = seq(1,11,2),labels = month.abb[seq(1,11,2)])+
  theme(legend.position='top')+
  labs(x='Month',y='RSV positive cases',color='Epidemic Season')+
  scale_color_lancet()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(size=8))+
  scale_y_continuous(expand = expansion(mult=c(0.1,.1)))

ggsave(filename = 'plot/Seasonality2.png',width = 30, height = 20, dpi = 300)

ggplot(df_seasonality2,aes(month,rsv,fill=factor(epi_month)))+
  geom_col()+
  facet_wrap(vars(stripe_text),scales = 'free',nrow = 2)+
  theme_bw()+
  scale_x_continuous(breaks = seq(1,11,2))+
  theme(legend.position='top')+
  labs(x='Month',y='RSV positive cases',color='Epidemic Season')+
  scale_color_lancet()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(size=8))+
  scale_y_continuous(expand = expansion(mult=c(0.1,.1)))

# Re sort by begin month ----

df_seasonality3<-df_seasonality2 %>%
  mutate(index_by_epi_onset=if_else(month-epi_onset<0,13+(month-epi_onset),
                                    month-epi_onset+1)) %>%
  mutate(max=max(rsv),
         monthabb2=ifelse(index_by_epi_onset%%2 ==1,monthabb,''))
  
# ➡️ Plot Seasonality ----
ggplot(df_seasonality3,aes(index_by_epi_onset,rsv,color=ordered(epi_month,levels=c('TRUE','FALSE'))))+
  geom_line(aes(group = ISO3))+
  geom_point(shape = 21,fill='white')+
  facet_wrap(vars(stripe_text),scales = 'free',nrow = 2)+
  theme_bw()+
  scale_x_continuous(breaks = seq(1,12,1),labels=rep('',12))+
  theme(legend.position='top')+
  labs(x='Month',y='RSV positive cases',color='')+
  scale_color_manual(values = rev(pal_lancet()(2)),labels=c('Seasonal','Catch up'))+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(size=8))+
  scale_y_continuous(expand = expansion(mult=c(0.1,.1)))

df_plot_li<-split(df_seasonality3,df_seasonality3$ISO3)

plot_li<-map(df_plot_li,~{
  ggplot(.x,aes(index_by_epi_onset,rsv,color=ordered(epi_month,levels=c('TRUE','FALSE'))))+
    geom_line(aes(group = ISO3))+
    geom_point(shape = 21,fill='white')+
    facet_wrap(vars(stripe_text),scales = 'free',nrow = 2)+
    theme_bw()+
    scale_x_continuous(breaks = seq(1,12,1),labels=.x %>%
                         arrange(index_by_epi_onset) %>% pull(monthabb2))+
    theme(legend.position='top')+
    labs(x='Month',y='RSV positive cases',color='')+
    scale_color_manual(values = rev(pal_lancet()(2)),labels=c('Seasonal','Catch up'))+
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.title = element_text(size=8),
          strip.text = element_text(size=10),
          text = element_text(size=12))+
    scale_y_continuous(expand = expansion(mult=c(0.1,.1)))
})

plot_li[[1]]

region_list<-sort(unique(df_seasonality3$region_ns_zone)) %>% setNames(.,.) %>%
  map(~df_seasonality3 %>% filter(region_ns_zone==.x) %>% pull(ISO3) %>% unique())

plot_li[c('AUT','BEL')]

wrap_plots(plot_li[region_list[[12]][1:15]])+
  plot_layout(ncol=5,nrow=3)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'top')

ggsave('Figures/Seasonality-EUR-1.tif',width = 12, height = 6, units = "in", dpi = 300)

wrap_plots(plot_li[c(region_list[[12]][16:28],region_list[[11]])])+
  plot_layout(ncol=5,nrow=3)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'top')

ggsave('Figures/Seasonality-EUR-2.tif',width = 12, height = 6, units = "in", dpi = 300)

wrap_plots(plot_li[unlist(region_list[3:8])])+
  plot_layout(ncol=4,nrow=3)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'top')

ggsave('Figures/Seasonality-AMR.tif',width = 12, height = 6, units = "in", dpi = 300)

wrap_plots(plot_li[unlist(region_list[c(15:17,13:14,9:10,1:2)])])+
  plot_layout(ncol=5,nrow=4)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'top')

ggsave('Figures/Seasonality-WPR-SEAR-EMR-AFR.tif',width = 12, height = 6, units = "in", dpi = 300)

# Simulate Sampling based on Bayes Method ----
gen_random_smaple<-function(alpha=rep(1,12),observed,N=1000){
  post_alpha=alpha+observed
  MCMCpack::rdirichlet(N,alpha=post_alpha)
}

gen_random_smaple(observed=rep(0:1,each=6),N=10)

set.seed(20241211)
df_sample_pro_by_sea<-split(df_seasonality3,df_seasonality3$ISO3) %>%
  purrr::map(~{
    print(slice_min(.x,index_by_epi_onset,n=12) %>% pull(index_by_epi_onset))
    slice_min(.x,index_by_epi_onset,n=12) %>% pull(rsv)
  }) %>%
  enframe(name = "ISO3", value = "observed") %>%
  mutate(posterior_sample=purrr::map(observed,~gen_random_smaple(observed=.x+1))) %>%
  left_join(df_seasonality3 %>% dplyr::select(ISO3,Country,region_ns_zone) %>% distinct())

df_sample_pro_by_sea$posterior_sample[[1]] %>% class()
df_sample_pro_by_sea[1,'posterior_sample'] %>% class()

png('Figures/Mean_for_postrior_sample(pro by season)_1.png',width = 1000,height = 800)
op<-par(mfrow=c(6,5))
for(i in 1:30) {
  plot(df_sample_pro_by_sea[i,'posterior_sample'][[1]][[1]] %>% 
         apply(2,mean),type='h',lwd=10,col=i,ylab='Pro',main=df_sample_pro_by_sea[i,'Country'])
}
par(op)
dev.off()

png('Figures/Mean_for_postrior_sample(pro by season)_2.png',width = 1000,height = 800)
op<-par(mfrow=c(6,5))
for(i in 31:60) {
  plot(df_sample_pro_by_sea[i,'posterior_sample'][[1]][[1]] %>% 
         apply(2,mean),type='h',lwd=10,col=i,ylab='Pro',main=df_sample_pro_by_sea[i,'Country'])
}
par(op)
dev.off()

# For Countries lacking seasonality data ----
df_type2 %>% filter(!ISOCountry %in% df_seasonality3$ISO3)

df_replaced<-data.frame(ISO3=c('GRC','HUN','SVK','LIE','PRY','LVA','BHR','BLR'),
                 replaced_by=c('BGR','AUT','AUT','CHE','BRA','DNK','QAT','POL')) %>%
  left_join(df_sample_pro_by_sea %>% dplyr::select(ISO3,observed),by=c('replaced_by'='ISO3'))

df_sample_pro_by_sea_all<-bind_rows(
  df_sample_pro_by_sea,
  df_replaced
)
# ➡️ Export df_sample_pro_by_sea ----
rio::export(df_sample_pro_by_sea_all,'model_data/df_sample_pro_by_sea_all.rds')


# Plot Seasonality Use Proportion not Count ----
df_plot_li[[1]]
df_plot_li_pro<-df_plot_li %>%
  map(~.x %>%
        mutate(rsv_pro=rsv*100/sum(rsv)))

df_plot_li_pro[[1]]

plot_li<-map(df_plot_li_pro,~{
  ggplot(.x,aes(index_by_epi_onset,rsv_pro,color=ordered(epi_month,levels=c('TRUE','FALSE'))))+
    geom_line(aes(group = ISO3))+
    geom_point(shape = 21,fill='white')+
    facet_wrap(vars(stripe_text),scales = 'free',nrow = 2)+
    theme_bw()+
    scale_x_continuous(breaks = seq(1,12,1),labels=.x %>%
                         arrange(index_by_epi_onset) %>% pull(monthabb2))+
    theme(legend.position='NULL')+
    labs(x='Month',y='Annual proportion of RSV cases (%)',color='')+
    scale_color_manual(values = rev(pal_lancet()(2)),labels=c('Seasonal','Catch up'))+
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.title = element_text(size=8),
          strip.text = element_text(size=9),
          text = element_text(size=12))+
    scale_y_continuous(expand = expansion(mult=c(0.1,.1))
                       #labels = scales::percent_format()
                       )
})

plot_li[[1]]

region_list<-sort(unique(df_seasonality3$region_ns_zone)) %>% setNames(.,.) %>%
  map(~df_seasonality3 %>% filter(region_ns_zone==.x) %>% pull(ISO3) %>% unique())


wrap_plots(plot_li[region_list[[12]][1:15]])+
  plot_layout(ncol=5,nrow=3)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'NULL')

ggsave('Figures/Seasonality-Pro-EUR-1.tif',width = 12, height = 6, units = "in", dpi = 300)

wrap_plots(plot_li[c(region_list[[12]][16:28],region_list[[11]])])+
  plot_layout(ncol=5,nrow=3)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'NULL')

ggsave('Figures/Seasonality-Pro-EUR-2.tif',width = 12, height = 6, units = "in", dpi = 300)

wrap_plots(plot_li[unlist(region_list[3:8])])+
  plot_layout(ncol=4,nrow=3)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'NULL')

ggsave('Figures/Seasonality-Pro-AMR.tif',width = 12, height = 6, units = "in", dpi = 300)

wrap_plots(plot_li[unlist(region_list[c(15:17,13:14,9:10,1:2)])])+
  plot_layout(ncol=5,nrow=4)+plot_layout(guides = "collect",axes = 'collect_y',axis_titles = 'collect') &
  theme(legend.position = 'NULL')

ggsave('Figures/Seasonality-Pro-WPR-SEAR-EMR-AFR.tif',width = 12, height = 6, units = "in", dpi = 300)

df_type2 %>% filter(nir=='NIP-included'|abr=='NIP-included') %>%
  createAppendixTable('docs/Countries-NIP-copy.xlsx')

save.image('rda/code_10.2_seasonality_data.RData')
