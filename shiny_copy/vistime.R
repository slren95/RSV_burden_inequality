# Vistime ----
library(shiny)
library(timevis)
df_event<-df_nirsevimab %>%
  filter(!is.na(Licensed_Date)) %>%
  mutate(Country_NIP=if_else(
    type=='NIP-included',paste0(Country,'(NIP)'),Country
  )) %>%
  select(ISOCountry,Country,Country_NIP,Licensed_Date,Recommend_Date,type,EU,NIP) %>%
  arrange(type,Licensed_Date) %>%
  mutate(Country_NIP=factor(Country_NIP,levels=unique(Country_NIP))) %>%
  pivot_longer(contains('_Date'),names_to = 'event',names_pattern = '(.*)_Date',
               values_to='start',values_drop_na = T) %>%
  mutate(end=start)

df_duration<-df_event %>%
  group_by(Country) %>%
  arrange(event) %>%
  mutate(end=case_when(!is.na(lead(start))~as.character(ymd(lead(start))+1),
                       is.na(lead(start))~format(Sys.Date())
  )) %>%
  mutate(event=paste0('post-',event))

cols<-c('#c6dbef','#6baed6','#3182bd','#08519c')

df_vis<-bind_rows(df_event,df_duration) %>%
  mutate(color=case_when(
    event=='post-Licensed'~'skyblue',
    event=='post-Recommend'~'salmon'
  ))

vistime(df_vis,col.group = 'Country_NIP',linewidth=10)

data <- data.frame(
  id      = 1:4,
  content = c(HTML('<a href="https://www.baidu.com">123</a>') , "Item two"  ,"Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-10", "2016-01-20", "2016-02-14 15:00:00"),
  end     = c(NA          ,           NA, "2016-02-04", NA)
)

timevis(data)
