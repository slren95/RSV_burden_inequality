#
# ------------------------------------------------------------------------------
# Script: code_12_sensitive_analysis_use_GBD_data.R
# Purpose:
#   Use GBD 2023 RSV mortality data as an alternative input to perform a
#   sensitivity analysis and compare results against the main mortality inputs.
#
# Inputs:
#   - data/GBD/IHME-GBD_2023_DATA-*.csv (GBD 2023 results extract)
#   - data/GBD/IHME_GBD_2023_HIERARCHIES_*.XLSX (location hierarchy metadata)
#   - Additional model inputs referenced downstream (see `import()` calls)
#
# Outputs:
#   - Sensitivity datasets and/or figures created by the script (see export calls).
#
# Usage:
#   source("code_12_sensitive_analysis_use_GBD_data.R")
#
# Notes:
#   Country name harmonisation (ISO3 mapping) is handled manually in `iso3_map()`.
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(rio)
library(janitor)
library(ggsci)
library(patchwork)
library(scales)


## GBD Etiology Mortality Data RSV----

df_gbd<-import('data/GBD/IHME-GBD_2023_DATA-af5e7aac-1.csv')

df_gbd2<-df_gbd %>%
  filter(metric_name %in% c('Rate','Number'))

df_gbd2 %>% count(location_id,location_name,sort = T)


df_hie<-import('data/GBD/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX') %>%
  clean_names()

df_hie_country<- df_hie %>% filter(level==3)


df_gbd3<-df_gbd2 %>%
  left_join(df_hie_country) %>%
  relocate(level,.after=location_name)


df_country<-df_gbd3 %>% 
  filter(!is.na(level) | location_name %in% c('Türkiye',"Côte d'Ivoire"))

df_country$location_name

iso3_map <- function(){c(
  "Saint Lucia"="LCA",
  "Northern Mariana Islands"="MNP",
  "Slovenia"="SVN",
  "Kenya"="KEN",
  "Palau"="PLW",
  "Kiribati"="KIR",
  "Bermuda"="BMU",
  "Burkina Faso"="BFA",
  "Belgium"="BEL",
  "Saint Vincent and the Grenadines"="VCT",
  "Morocco"="MAR",
  "Maldives"="MDV",
  "Azerbaijan"="AZE",
  "Barbados"="BRB",
  "Georgia"="GEO",
  "Brunei Darussalam"="BRN",
  "Algeria"="DZA",
  "Puerto Rico"="PRI",
  "Suriname"="SUR",
  "Chile"="CHL",
  "Trinidad and Tobago"="TTO",
  "Philippines"="PHL",
  "Cabo Verde"="CPV",
  "Malawi"="MWI",
  "Denmark"="DNK",
  "Afghanistan"="AFG",
  "Belarus"="BLR",
  "Bangladesh"="BGD",
  "Micronesia (Federated States of)"="FSM",
  "Saint Kitts and Nevis"="KNA",
  "Cuba"="CUB",
  "Kazakhstan"="KAZ",
  "Lesotho"="LSO",
  "Oman"="OMN",
  "Bhutan"="BTN",
  "Egypt"="EGY",
  "Austria"="AUT",
  "Greenland"="GRL",
  "Kyrgyzstan"="KGZ",
  "Bolivia (Plurinational State of)"="BOL",
  "India"="IND",
  "Mozambique"="MOZ",
  "Latvia"="LVA",
  "San Marino"="SMR",
  "Republic of Korea"="KOR",
  "Thailand"="THA",
  "Mongolia"="MNG",
  "Tajikistan"="TJK",
  "Tokelau"="TKL",
  "France"="FRA",
  "Dominican Republic"="DOM",
  "Samoa"="WSM",
  "Saudi Arabia"="SAU",
  "Iraq"="IRQ",
  "Nepal"="NPL",
  "Tuvalu"="TUV",
  "Republic of Moldova"="MDA",
  "Indonesia"="IDN",
  "Peru"="PER",
  "Cameroon"="CMR",
  "Seychelles"="SYC",
  "Cyprus"="CYP",
  "Viet Nam"="VNM",
  "United States of America"="USA",
  "Uzbekistan"="UZB",
  "Pakistan"="PAK",
  "Turkmenistan"="TKM",
  "Greece"="GRC",
  "Sierra Leone"="SLE",
  "Ecuador"="ECU",
  "Kuwait"="KWT",
  "Tonga"="TON",
  "United States Virgin Islands"="VIR",
  "Ukraine"="UKR",
  "Angola"="AGO",
  "Burundi"="BDI",
  "Colombia"="COL",
  "Albania"="ALB",
  "Malaysia"="MYS",
  "Congo"="COG",
  "Fiji"="FJI",
  "United Republic of Tanzania"="TZA",
  "Romania"="ROU",
  "Ireland"="IRL",
  "Ghana"="GHA",
  "Chad"="TCD",
  "Libya"="LBY",
  "Equatorial Guinea"="GNQ",
  "Slovakia"="SVK",
  "Finland"="FIN",
  "Central African Republic"="CAF",
  "Zambia"="ZMB",
  "Myanmar"="MMR",
  "El Salvador"="SLV",
  "Poland"="POL",
  "Bulgaria"="BGR",
  "Djibouti"="DJI",
  "Bosnia and Herzegovina"="BIH",
  "Italy"="ITA",
  "South Sudan"="SSD",
  "Bahamas"="BHS",
  "Marshall Islands"="MHL",
  "Japan"="JPN",
  "Guyana"="GUY",
  "Serbia"="SRB",
  "Paraguay"="PRY",
  "Guinea-Bissau"="GNB",
  "American Samoa"="ASM",
  "Botswana"="BWA",
  "Estonia"="EST",
  "Palestine"="PSE",
  "Sri Lanka"="LKA",
  "Costa Rica"="CRI",
  "Czechia"="CZE",
  "Sweden"="SWE",
  "Germany"="DEU",
  "Croatia"="HRV",
  "Malta"="MLT",
  "Democratic Republic of the Congo"="COD",
  "Papua New Guinea"="PNG",
  "Gambia"="GMB",
  "Honduras"="HND",
  "Mali"="MLI",
  "Jamaica"="JAM",
  "Ethiopia"="ETH",
  "Qatar"="QAT",
  "Guatemala"="GTM",
  "Lithuania"="LTU",
  "Cook Islands"="COK",
  "Singapore"="SGP",
  "Nicaragua"="NIC",
  "Democratic People's Republic of Korea"="PRK",
  "Gabon"="GAB",
  "North Macedonia"="MKD",
  "Hungary"="HUN",
  "Senegal"="SEN",
  "Guinea"="GIN",
  "Iceland"="ISL",
  "Norway"="NOR",
  "Belize"="BLZ",
  "Bahrain"="BHR",
  "Guam"="GUM",
  "Timor-Leste"="TLS",
  "Solomon Islands"="SLB",
  "China"="CHN",
  "Montenegro"="MNE",
  "Mexico"="MEX",
  "Russian Federation"="RUS",
  "Niger"="NER",
  "Comoros"="COM",
  "Madagascar"="MDG",
  "United Kingdom"="GBR",
  "New Zealand"="NZL",
  "Venezuela (Bolivarian Republic of)"="VEN",
  "Togo"="TGO",
  "Spain"="ESP",
  "Sudan"="SDN",
  "Israel"="ISR",
  "Eritrea"="ERI",
  "Taiwan"="TWN",
  "Panama"="PAN",
  "Dominica"="DMA",
  "Liberia"="LBR",
  "Andorra"="AND",
  "Sao Tome and Principe"="STP",
  "Armenia"="ARM",
  "Iran (Islamic Republic of)"="IRN",
  "Tunisia"="TUN",
  "Argentina"="ARG",
  "Switzerland"="CHE",
  "South Africa"="ZAF",
  "Australia"="AUS",
  "Mauritius"="MUS",
  "Monaco"="MCO",
  "Brazil"="BRA",
  "Namibia"="NAM",
  "Jordan"="JOR",
  "Nauru"="NRU",
  "Mauritania"="MRT",
  "Niue"="NIU",
  "Cambodia"="KHM",
  "United Arab Emirates"="ARE",
  "Rwanda"="RWA",
  "Zimbabwe"="ZWE",
  "Luxembourg"="LUX",
  "Uruguay"="URY",
  "Vanuatu"="VUT",
  "Nigeria"="NGA",
  "Syrian Arab Republic"="SYR",
  "Eswatini"="SWZ",
  "Benin"="BEN",
  "Somalia"="SOM",
  "Grenada"="GRD",
  "Lebanon"="LBN",
  "Lao People's Democratic Republic"="LAO",
  "Uganda"="UGA",
  "Canada"="CAN",
  "Haiti"="HTI",
  "Netherlands"="NLD",
  "Portugal"="PRT",
  "Antigua and Barbuda"="ATG",
  "Yemen"="YEM",
  "Türkiye"="TUR",
  "Côte d'Ivoire"="CIV"
)}


df_country<-df_country %>%
  mutate(ISOCountry=iso3_map()[location_name]) %>%
  relocate(ISOCountry,.after = location_name)

df_country_lite <-df_country %>%
  dplyr::select(ISOCountry,location_id,location_name,level,year,age_name,year,metric_name,val,upper,lower) %>%
  mutate(metric_name=recode(metric_name,'Rate'='R.GBD','Number'='N.GDB')) %>% 
  rename(est=val,lci=lower,uci=upper) %>%
  pivot_wider(names_from = metric_name,values_from = c(est,lci,uci), names_glue = '{metric_name}_{.value}') %>%
  relocate(1:6,starts_with('N'),starts_with('R'))

## Previous Mortality ----
mr_mc_all<-rio::import('model_data/mr_mc_all.rds',trust=T)

df_mort_sum<-mr_mc_all %>%
  summarise(
    R_lci = quantile(mor_all, 0.025)*100,
    R_est = quantile(mor_all, 0.5)*100,
    R_uci = quantile(mor_all, 0.975)*100,
    .by = c(ISOCountry, Country.Name, Income2019,WHORegion)
  )

df_compare<-inner_join(df_mort_sum,df_country_lite)

names(df_compare)

df_compare %>%
  dplyr::select(ISOCountry, Income2019, starts_with("R.GBD_"), starts_with("R_")) %>%
  pivot_longer(
    cols = -c(ISOCountry, Income2019),
    names_to = c("metric", "stat"),
    names_sep = "_"
  ) %>%
  group_by(ISOCountry, Income2019, metric, stat) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = ISOCountry, y = est, color = metric)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(aes(ymin = lci, ymax = uci),
                 position = position_dodge(width = 0.5),
                 linewidth = 1.2) +
  facet_wrap(~Income2019, scales = "free") +
  scale_color_lancet()+
  theme_bw() +
  labs(y='RSV-attributable mortality(<1years,per 100,000 person-years)')+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

ggsave('plot/Compare_mortality_with_GBD.png',width = 3000,height = 1800,dpi=200,unit='px')

df_compare %>%
  dplyr::select(ISOCountry, WHORegion, starts_with("R.GBD_"), starts_with("R_")) %>%
  pivot_longer(
    cols = -c(ISOCountry, WHORegion),
    names_to = c("metric", "stat"),
    names_sep = "_"
  ) %>%
  group_by(ISOCountry, WHORegion, metric, stat) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = ISOCountry, y = est, color = metric)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(aes(ymin = lci, ymax = uci),
                 position = position_dodge(width = 0.5),
                 linewidth = 1.2) +
  facet_wrap(~WHORegion, scales = "free") +
  scale_color_lancet()+
  theme_bw() +
  labs(y='RSV-attributable mortality(<1years,per 100,000 person-years)')+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )


recover_log_params <- function(est, lci, uci) {
  mu1 <- log(est)
  mu2 <- (log(lci) + log(uci)) / 2
  se <- (log(uci) - log(lci)) / (2 * 1.96)
  
  return(c('est1'=mu1,'est2'=mu2,'se'=se))
}


df_compare2<- df_compare %>% dplyr::select(ISOCountry,Country.Name,Income2019,WHORegion,starts_with('R.GBD_')) %>%
  rowwise() %>%
  mutate(est1=recover_log_params(R.GBD_est,R.GBD_lci,R.GBD_uci)[1],
         est2=recover_log_params(R.GBD_est,R.GBD_lci,R.GBD_uci)[2],
         se=recover_log_params(R.GBD_est,R.GBD_lci,R.GBD_uci)[3])

# Generate sensitive analysis data GBD Mortality Data ----
inner_join(df_mort_sum,df_country_lite)

df_pop<-left_join(
  import('rda/pop_mor.raw.rds') %>% # code_08_
    dplyr::select(ISOCountry,Income2019),
  import('rda/df_pop_center_nir.rds') %>% 
    transmute(ISOCountry,WHORegion,region_ns_zone,Country.Name,WHOName,type,pop_infant_2026=x0)) %>%
  mutate(region_income=paste(WHORegion,Income2019,sep='_'),.after=WHORegion)

setdiff(df_pop$ISOCountry,df_compare2$ISOCountry)
# [1] "ABW" "ESH" "GLP" "GUF" "MTQ" "MYT" "NCL" "PYF" "REU" # 9
setdiff(df_compare2$ISOCountry,df_pop$ISOCountry)

df_pop %>%
  filter(ISOCountry %in% setdiff(df_pop$ISOCountry,df_compare2$ISOCountry)) %>%
  View()

set.seed(1942)
mr_mc_197<-df_compare2 %>%
  dplyr::select(ISOCountry,Country.Name,Income2019,WHORegion,est1,se) %>%
  mutate(data=pmap(list(est1,se),~{
    tibble(index=1:1000,R.GBD=exp(rnorm(1000,..1,..2)))
  })) %>%
  unnest(data)

mr_mc_197_sum_by_income<-mr_mc_197 %>%
  dplyr::summarise(across(R.GBD,median),.by=c(Income2019,index))

mr_mc_9<-df_pop %>%
  filter(ISOCountry %in% setdiff(df_pop$ISOCountry,df_compare2$ISOCountry)) %>%
  dplyr::select(ISOCountry,Country.Name,WHORegion,Income2019,WHOName) %>%
  left_join(mr_mc_197_sum_by_income,by='Income2019') %>%
  mutate(replace_by='Median_by_income_index')

mr_mc_all2<-bind_rows(mr_mc_197,
                      mr_mc_9) %>%
  mutate(mor_all=R.GBD/100) # GBD per 10000 person-years,change to per 1000 person-years

hist(mr_mc_all2$mor_all,breaks = 50)
summary(mr_mc_all2$mor_all)

(mr_mc_all2 %>%
  ggplot(aes(mor_all,fill=WHORegion)) +
  geom_density())/(mr_mc_all %>%
                     ggplot(aes(mor_all,fill=WHORegion)) +
                     geom_density())

rio::export(mr_mc_all2,'model_data/mr_mc_all2.rds')

# Import Population data ----
df_pop<-left_join(
  import('rda/df_pop_center_nir.rds') %>% 
    transmute(ISOCountry,WHORegion,region_ns_zone,Country.Name,WHOName),
  import('model_data/df_pop_2019_2026.rds')) %>%
  mutate(region_income=paste(WHORegion,Income2019,sep='_'),.after=WHORegion)

df_type<-import('model_data/df_type.rds')
df_type2<-df_type %>% left_join(dplyr::select(df_pop,ISOCountry,Income2019,WHORegion))

df_mor_mc<-import('model_data/mr_mc_all.rds')

df_mor_mc2<-df_mor_mc %>%
  dplyr::select(ISOCountry,index,mor_all) %>%
  nest(.by = ISOCountry) %>%
  mutate(mort=map(data,~pull(.x,mor_all)))

# GBD mortality Sensitive analysis
df_mor_mc.GBD<-import('model_data/mr_mc_all2.rds')

df_mor_mc2.GBD<-df_mor_mc.GBD %>%
  dplyr::select(ISOCountry,index,mor_all) %>%
  nest(.by = ISOCountry) %>%
  mutate(mort=map(data,~pull(.x,mor_all)))


df_pars_data.compare<-df_pop %>%
  left_join(df_type2 %>% transmute(ISOCountry,nir,abr,nir_abr,Income2019)) %>%
  left_join(df_compare %>% dplyr::select(ISOCountry,starts_with('R'))) %>%
  mutate(N_est=pop_infant_2026*R_est,N.GBD_est=pop_infant_2026*R.GBD_est)


df_pars_data.compare %>% 
  reframe(across(c(N_est, N.GBD_est), ~sum(.x, na.rm = T)), .by = c(WHORegion, nir_abr)) %>% 
  pivot_longer(cols = c(N_est, N.GBD_est), names_to = "source", values_to = "count") %>%
  ggplot(aes(x = source, y = count, fill = nir_abr)) +
  geom_col(position = "fill", width = 0.7) + 
  facet_wrap(~ WHORegion, scales = "free_x") + 
  scale_y_continuous(labels = percent_format()) +
  labs(y='Proportion of RSV-atttributable deaths under no-implemantation scenario')+
  theme(legend.position = "right")

(df_pars_data.compare %>%
  ggplot(aes(x = reorder(ISOCountry, R_est), y = R_est)) +
  geom_col()+
  labs(title='main analysis',y='RSV-attributable mortality rate(EUR)'))/(df_pars_data.compare %>%
  ggplot(aes(x = reorder(ISOCountry, R.GBD_est), y = R.GBD_est)) +
  geom_col()+
  labs(title='GBD',y='RSV-attributable mortality rate(EUR)'))
