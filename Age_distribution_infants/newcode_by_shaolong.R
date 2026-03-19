#From full_code_line 1-153----
#rm(list=ls())
# Please confirm that the following  packages have been installed.
library(readxl)
library(MCMCpack)
library(progress)
library(coda)
library(ggplot2)
library(tidyverse)
library(rgdal)
library(ggpubr)
library(extraDistr)
library(DirichletReg)
library(rio)
library(ggsci)
library(patchwork)
#### Considering the data licences, only the full code is shown here without the corresponding data. 
#### We have given simulation data in the 'working example for illustration' file, which allows to run the core code of Bayesian model.

#0. data import----

# Exclude studies reported pooled results from multiple countries across different income levels
# and studies that report data after 2019
mixedID <- c("L61501", "L68601") 
after2019 <-c("L58201","L64201")
metaData <- read_excel("SCEQ_v4.2.xlsx", sheet = "Cataloguing")%>% 
  filter(!is.na(Study_ID)& !(Study_ID %in% mixedID )& !(Study_ID %in% after2019 ))

#data for main analysis
df <- read_excel("SCEQ_v4.2.xlsx", sheet = "Extraction") 
df <- df %>% filter(!is.na(Study_ID) & !(Study_ID %in% mixedID )) %>% left_join(metaData,by=c("Study_ID","m_analysis"))
df$RSV_count <- round(df$RSV_count)
df.IP <- df %>% filter(Setting=="Hospital"& m_analysis==1) 

#Country and latitude/longitude information for each study
LocationInfo <- read_excel("LocationInfo.xlsx", sheet = "LocationInfo")
LocationInfo <- LocationInfo %>% inner_join(metaData[,c("Study_ID", "Income")]) #Country income levels for the median year of the study period

LocationInfo$lat_group <- NA
for (i in 1:nrow(LocationInfo)) {
  if (!is.na(LocationInfo[i, "lat"])) {
    if (LocationInfo[i, "lat"] > 35) {
      LocationInfo[i, "lat_group"] <- "Temperate_region"
    } else if (LocationInfo[i, "lat"] < 35 & LocationInfo[i, "lat"] > 23.5) {
      LocationInfo[i, "lat_group"] <- "Subtropical_region"
    } else if (LocationInfo[i, "lat"] < 23.5 & LocationInfo[i, "lat"] > -23.5) {
      LocationInfo[i, "lat_group"] <- "Tropical_region"
    } else if (LocationInfo[i, "lat"] < -23.5& LocationInfo[i, "lat"] > -35) {
      LocationInfo[i, "lat_group"] <- "Subtropical_region"
    }else if (LocationInfo[i, "lat"] < -35) {
      LocationInfo[i, "lat_group"] <- "Temperate_region"
    }
  }
}
df.IP <- df.IP %>% left_join(LocationInfo[,c("Study_ID","lat_group")],multiple = "first")
df.IP$Income_lat <- paste(df.IP$Income,df.IP$lat_group,sep = "_")
####0.1 Estimated the value of alpha----
# install.packages("DirichletReg")
AgeDistr_12 <- df.IP %>% filter(N_subAge==12)
AgeDistr_12 <- do.call(rbind,by(AgeDistr_12, AgeDistr_12$Study_ID, function(x) {x$proportion = x$RSV_count/sum(x$RSV_count); return(x)}))
Dirichlet_data <- AgeDistr_12[,c(1,4,15,19,30,33,34,35)] %>% pivot_wider(names_from = Age_end,names_prefix = "p",values_from = proportion)
Dirichlet_dataH <- Dirichlet_data %>% filter(Income=="H")
Dirichlet_matrixH <- DR_data(Dirichlet_dataH[,7:18])

Dirichlet_fitH <- DirichReg(Dirichlet_matrixH ~ 1, model = "common")
summary(Dirichlet_fitH)

alpha_estimated <- as.data.frame(coef(Dirichlet_fitH))
alpha_estimated <- exp(alpha_estimated) 
alpha0_estimated <- sum(alpha_estimated)
print(alpha0_estimated)

#1.1 Estimating RSV hospitalisation by chronological month of age----
n.iteration <- 6000
n.burnin <- 1000
n.thin <- 10
source("full_function.R")
#### Age distribution stratified by income level (income level in the median year of the study)
set.seed(1114)
paramSummary_Income <- do.call(rbind, by(df.IP, df.IP$Income, function(data) {
  genRes(inputdata = data, Group = data$Income[1], n.iteration = n.iteration, n.burnin = n.burnin, n.thin = n.thin, k_init = 10000)
}))

paramSummary_Income$Group <- factor(paramSummary_Income$Group,levels =c("H","UM","LM","L"),labels=c("HICs","UMICs","LMICs/LICs","LICs"))

genAgePlots(inputdata = paramSummary_Income,type ="Income",analysis = "main") #plots for age distribution

#plot for age distribution of cumulative proportion
ggsave(ggplot(data=paramSummary_Income, aes(x=parameter, y=param_cumsum.est, ymin=param_cumsum.lci, ymax=param_cumsum.uci, fill=Group, linetype=Group)) + 
         geom_line()+
         geom_ribbon(alpha=0.2)+
         labs(x="Age",y="Cumulative Proportion",fill="Income",linetype="Income")+
         scale_x_continuous(breaks = seq(1,12,1),labels = c("0-<1m","1-<2m","2-<3m","3-<4m","4-<5m","5-<6m",
                                                            "6-<7m","7-<8m","8-<9m","9-<10m","10-<11m","11-<12m"))+
         theme(legend.position = "bottom",
               legend.justification = "center",
               text = element_text(size = 20),
               panel.background = element_blank(),
               axis.line.x = element_line(linetype=1,color="black",linewidth=0.25),
               axis.line.y = element_line(linetype=1,color="black",linewidth=0.25)),
       filename = paste("results/","AgeDistribution_Income_cum",".pdf",sep = ""), width = 14, height = 7)

#### Age distribution stratified by latitude
set.seed(1115)
paramSummary_Lat <- do.call(rbind,by(df.IP,df.IP$lat_group,function(data) {
  genRes(inputdata = data, Group = data$lat_group[1], n.iteration = n.iteration, n.burnin = n.burnin, n.thin = n.thin, k_init = 10000)
}))

paramSummary_Lat$Group <- factor(paramSummary_Lat$Group,levels =
                                   c("Temperate_region","Subtropical_region","Tropical_region"),
                                 labels=c("Temperate region","Subtropical region","Tropical region"))

genAgePlots(inputdata = paramSummary_Lat,type ="regions",analysis = "main") #plots for age distribution

#### Simultaneously by country income and latitudinal group
set.seed(1116)
paramSummary_Income.Lat <- do.call(rbind,by(df.IP,df.IP$Income_lat,function(data) {
  genRes(inputdata = data, Group = data$Income_lat[1], n.iteration = n.iteration, n.burnin = n.burnin, n.thin = n.thin, k_init = 10000)
}))

paramSummary_Income.Lat$Income <- sapply(strsplit(as.character(paramSummary_Income.Lat$Group), "_"), function(x) x[1])
paramSummary_Income.Lat$Income <- factor(paramSummary_Income.Lat$Income,levels =c("H","UM","LM"),labels=c("HICs","UMICs","LMICs/LICs"))

paramSummary_Income.Lat$Latitude <- sapply(strsplit(as.character(paramSummary_Income.Lat$Group), "_"), function(x) paste(x[2], x[3], sep = " "))
paramSummary_Income.Lat$Latitude <- factor(paramSummary_Income.Lat$Latitude,levels =
                                             c("Temperate region","Subtropical region","Tropical region"),
                                           labels=c("Temperate region","Subtropical region","Tropical region"))

paramSummary_Income.Lat$Group <- factor(paramSummary_Income.Lat$Group,levels =
                                          c("H_Temperate_region","H_Tropical_region","H_Subtropical_region",
                                            "UM_Temperate_region","UM_Tropical_region","UM_Subtropical_region",
                                            "LM_Temperate_region","LM_Tropical_region","LM_Subtropical_region"),labels=
                                          c("HIC (Temperate region)","HIC (Tropical region)","HIC (Subtropical region)",
                                            "UMIC (Temperate region)","UMIC (Tropical region)","UMIC (Subtropical region)",
                                            "LMIC/LIC (Temperate region)","LMIC/LIC (Tropical region)","LMIC/LIC (Subtropical region)"))

ggsave(ggplot(data=paramSummary_Income.Lat, aes(x=parameter, y=param.est, ymin=param.lci, ymax=param.uci, fill=Latitude, linetype=Latitude)) + 
         geom_line()+
         geom_ribbon(alpha=0.2)+
         labs(x="Age",y="Proportion",fill="regions",linetype="regions")+
         scale_x_continuous(breaks = seq(1,12,1),labels = c("0-<1m","1-<2m","2-<3m","3-<4m","4-<5m","5-<6m",
                                                            "6-<7m","7-<8m","8-<9m","9-<10m","10-<11m","11-<12m"))+
         theme(legend.position = "bottom",
               legend.justification = "center",
               text = element_text(size = 20),
               panel.background = element_blank(),
               axis.line.x = element_line(linetype=1,color="black",linewidth=0.25),
               axis.line.y = element_line(linetype=1,color="black",linewidth=0.25))+
         facet_wrap(~paramSummary_Income.Lat$Income,nrow = 3),
       filename = paste("results/","AgeDistribution","_","facet",".pdf",sep = ""), width = 12, height = 14)



# #加载thin_chain
# thin_chain_H <- load_and_rename("thin_chain_H.RData", "thin_chain_H")
# thin_chain_UM <-load_and_rename("thin_chain_UM.RData", "thin_chain_UM")
# thin_chain_LM <-load_and_rename("thin_chain_LM.RData", "thin_chain_LM")

# NEW CODE for infant burden by WHO region----
library(ggsci)
library(rio)
library(showtext)
library(patchwork)

df.who<-read_csv('country_WHO_region.csv')[-1]

setdiff(unique(df.IP$Country),df.who$WHOName)


df.IP %>% filter(Country %in% setdiff(unique(df.IP$Country),df.who$WHOName)) %>% view()


df.IP2<-df.IP %>%
  mutate(Country2=case_when(Country=='Vietnam'~'Viet Nam',
                            Country=='United States'~'USA',
                            Country=='Korea, Rep.'~'Republic of Korea',
                            #Country=='United States and Finland'~'USA',
                            #Country=='Netherlands, Spain, and the United Kingdom'~'Netherlands',
                            #Country=='Spain, Finland, England, Scotland, and the Netherlands'~'Spain',
                            T~Country
  ))

setdiff(unique(df.IP2$Country2),df.who$WHOName)

df.IP_who<-left_join(df.IP2,df.who,by=c('Country2'='WHOName')) %>%
  mutate(region_income=paste(WHORegion,Income,sep='_')) %>%
  filter(!is.na(ISOCountry)) # 去除未匹配上的

df.IP_who %>% count(WHORegion,Income,region_income)


#按WHOregion 和 income----

set.seed(1117)
paramSummary_region_income <- do.call(rbind, by(df.IP_who, df.IP_who$region_income, function(data) {
  genRes(inputdata = data, Group = data$region_income[1], n.iteration = n.iteration, n.burnin = n.burnin, n.thin = n.thin, k_init = 10000)
}))

export(paramSummary_region_income,'rda/paramSummary_region_income.rds')

#按WHOregion----
set.seed(1118)
paramSummary_region <- do.call(rbind, by(df.IP_who, df.IP_who$WHORegion, function(data) {
  genRes(inputdata = data, Group = data$WHORegion[1], n.iteration = n.iteration, n.burnin = n.burnin, n.thin = n.thin,k_init = 10000)
}))

export(paramSummary_region,'rda/paramSummary_region.rds')

#按income----
set.seed(1119)
paramSummary_income <- do.call(rbind, by(df.IP_who, df.IP_who$Income, function(data) {
  genRes(inputdata = data, Group = data$Income[1], n.iteration = n.iteration, n.burnin = n.burnin, n.thin = n.thin,k_init = 10000)
}))

export(paramSummary_income,'rda/paramSummary_income.rds')

# 百分比折线图 按照region income分组
genPlot<-function(df,legend="Income level"){
  ggplot(data=df, aes(x=parameter, y=param.est, ymin=param.lci, ymax=param.uci, fill=Group, linetype=Group)) + 
    geom_line()+
    geom_ribbon(alpha=0.2)+
    labs(x="Age",y="Proportion of RSV-ALRI hospital admissions",fill=legend,linetype=legend)+
    scale_x_continuous(breaks = seq(1,12,1),labels = c("0-<1m","1-<2m","2-<3m","3-<4m","4-<5m","5-<6m",
                                                       "6-<7m","7-<8m","8-<9m","9-<10m","10-<11m","11-<12m"))+
    scale_fill_lancet()+
    theme(legend.position = "top",
          legend.justification = "center",
          text = element_text(size = 16),
          axis.title = element_text(size=20),
          panel.background = element_blank(),
          axis.line.x = element_line(linetype=1,color="black",linewidth=0.25),
          axis.line.y = element_line(linetype=1,color="black",linewidth=0.25))
}

paramSummary_region_income<-import('rda/paramSummary_region_income.rds')
paramSummary_region<-import('rda/paramSummary_region.rds')
paramSummary_income<-import('rda/paramSummary_income.rds')


ggsave(plot=genPlot(paramSummary_region),filename = 'plot/by_region.png',width = 10,height = 5,dpi=200)

plot_by_region_income<-paramSummary_region_income %>%
  mutate(Group=toupper(Group) %>% str_replace('_','-') %>% paste0('IC')) %>%
  genPlot(legend='Region-Income level')

ggsave(plot=plot_by_region_income,filename = 'plot/by_region_income.png',width = 10,height = 6,dpi=200)


plot_by_income<-paramSummary_income %>%
  mutate(Group=factor(Group,levels=c('H','UM','LM'),labels=c('HIC','UMIC','LMIC'))) %>%
  genPlot()

ggsave(plot=plot_by_income,filename = 'plot/by_income.png',width = 10,height = 6,dpi=200)

# Figure S1 ----
plot_by_region_income/plot_by_income+
  plot_layout(axes = 'collect_y')+
  plot_annotation(tag_levels = list(c("A", "B"))) &
  theme(plot.tag = element_text(size=20),
        plot.tag.position = c(0.05, .96))

ggsave(filename = 'plot/Figure S1.png',width = 10,height = 12,dpi=200)

# 收敛判定图示 ----
dir(pattern = 'thin_chain')

table(thin_chain$index_new==thin_chain$index_seq)

plot_chain<-function(this_chain,title=''){
  this_chain %>%
    ggplot(aes(index,value,color=chain_index))+
    geom_point(alpha=.5)+
    facet_wrap(vars(parameter),nrow=3)+
    theme_bw()+
    labs(title=title)
}
plot_chain(thin_chain)


# by WHO region 
dir(pattern = 'thin_chain.*(Afr|Amr|Emr|Eur|Sear|Wpr)\\.RData') %>%
  walk(~{
    load(.x)
    title<-str_extract(.x,"(?<=thin_chain_)[A-Za-z]+(?=\\.RData)") %>% str_to_upper()
    cat(title,'\n')
    plot_chain(thin_chain,title)
    ggsave(path = 'plot/by_WHOregion',filename=paste0(title,'.pdf'),
           width = 10,height = 6,dpi=200)
  })

# by income
dir(pattern = 'thin_chain_(H|L|UM|LM)\\.RData') %>%
  walk(~{
    load(.x)
    title<-str_extract(.x,"(?<=thin_chain_)[A-Za-z]+(?=\\.RData)") %>% paste0('ICs')
    cat(title,'\n')
    plot_chain(thin_chain,title)
    ggsave(path = 'plot/by_income',filename=paste0(title,'.pdf'),
           width = 10,height = 6,dpi=200)
  })

# by WHO region and income
dir(pattern = 'thin_chain_(Afr|Amr|Emr|Eur|Sear|Wpr)_(H|L|UM|LM)\\.RData') %>%
  walk(~{
    load(.x)
    title<-str_extract(.x,"(?<=thin_chain_).*(?=\\.RData)") %>% str_to_upper() %>% str_replace('_','-') %>% paste0('ICs')
    cat(title,'\n')
    plot_chain(thin_chain,title)
    ggsave(path = 'plot/by_region_income',filename=paste0(title,'.pdf'),
           width = 10,height = 6,dpi=200)
  })

# 


# 获取马尔科夫链的数据----

get_chain_data<-function(file='thin_chain_UM.RData'){
  load(file = file)
  thin_chain
}
get_chain_data()

unique(df.IP_who$region_income)
unique(df.IP_who$Income)

df_chain_pro_by_age<-data.frame(region_income=unique(c(df.IP_who$region_income,df.IP_who$Income))) %>%
  mutate(income=gsub('.*_(.*)','\\1',region_income),
         converge=!region_income %in% c('Amr_LM','Emr_H','Emr_LM','Sear_LM','Sear_UM','Wpr_LM'),
         file=sprintf('thin_chain_%s.RData',ifelse(converge,region_income,income))) %>%
  mutate(data=map(file,get_chain_data))

rio::export(df_chain_pro_by_age,'rda/df_chain_pro_by_age.rds')
rio::export(df_chain_pro_by_age,'D:/NJMU/For Shaolong/model_data/df_chain_pro_by_age.rds')

save.image('rda/newcode_by_shaolong.RData')

thin_chain %>% 
  count(index_seq) %>% view()

# Patchwork Plot
plot_list1<-dir(pattern = 'thin_chain_(Afr|Amr|Emr|Eur|Sear|Wpr)_(H|L|UM|LM)\\.RData') %>%
  map(~{
    load(.x)
    title<-str_extract(.x,"(?<=thin_chain_).*(?=\\.RData)") %>% str_to_upper() %>% str_replace('_','-') %>% paste0('ICs')
    cat(title,'\n')
    plot_chain(thin_chain,title)
  })

wrap_plots(plot_list1[c(1,2,3)], nrow = 3)+
  plot_layout(axes = 'collect_y')+
  theme(plot.tag = element_text(size=20),
        plot.tag.position = c(0.05, .96))

ggsave(filename = 'plot/Trace Plot 1.png',width = 10,height = 15,dpi=200)


wrap_plots(plot_list1[c(5,8,9)], nrow = 3)+
  plot_layout(axes = 'collect_y')+
  theme(plot.tag = element_text(size=20),
        plot.tag.position = c(0.05, .96))

ggsave(filename = 'plot/Trace Plot 2.png',width = 10,height = 15,dpi=200)


wrap_plots(plot_list1[c(10,13,15)], nrow = 3)+
  plot_layout(axes = 'collect_y')+
  theme(plot.tag = element_text(size=20),
        plot.tag.position = c(0.05, .96))

ggsave(filename = 'plot/Trace Plot 3.png',width = 10,height = 15,dpi=200)

plot_list2<-dir(pattern = 'thin_chain_(H|L|UM|LM)\\.RData') %>%
  map(~{
    load(.x)
    title<-str_extract(.x,"(?<=thin_chain_)[A-Za-z]+(?=\\.RData)") %>% paste0('ICs')
    cat(title,'\n')
    plot_chain(thin_chain,title)
  })

wrap_plots(plot_list2[c(1,3,2)], nrow = 3)+
  plot_layout(axes = 'collect_y')+
  theme(plot.tag = element_text(size=20),
        plot.tag.position = c(0.05, .96))

ggsave(filename = 'plot/Trace Plot 4.png',width = 10,height = 15,dpi=200)
