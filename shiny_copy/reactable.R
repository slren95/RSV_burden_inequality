library(shiny)
library(reactable)
library(bslib)
library(sparkline)

df_nirsevimab<-import('../rda/df_nirsevimab.csv')

ui <- page_fillable(
  includeCSS("custom_styles.css"), 
  reactableOutput('table')
)

library(htmltools)

server <- function(input, output, session) {
  output$table<-renderReactable(
    reactable(df_nirsevimab,pagination = FALSE,filterable = FALSE,searchable = TRUE,
              striped = TRUE, highlight = TRUE, compact = TRUE,
              defaultColDef = colDef(
                show=F
              ),
              columns=list(
                ISO2 = colDef(
                  show=T,name='Country',
                  defaultSortOrder = "asc",
                  minWidth = 120,
                  headerStyle = list(fontWeight = 700),
                  cell = function(value, index) {
                    div(
                      class = "team",
                      img(class = "team-flag", alt = paste(value, "flag"), src = sprintf("https://flagpedia.net/data/flags/h80/%s.png", tolower(value))),
                      div(
                        span(class = "team-name", df_nirsevimab[index,'Country']),
                        span(class = "team-record", df_nirsevimab[index,'ISOCountry'])
                      )
                    )
                  }
                ),
                Licensed_Date=colDef(name='Licensed Date',show=T),
                Source1=colDef(name = 'Source',show=T,cell=function(value){
                  tags$a(href=value,'link',target='_blank')
                }),
                Recommend_Date=colDef(name='Recommend Date',show=T),
                Source2=colDef(show=T,name='Source',cell=function(value,index){
                  links <- list()
                  if (df_nirsevimab[index, 'Source2']!='') {
                    links <- append(links, list(tags$span(" "), tags$a(href = df_nirsevimab[index, 'Source2'], 'link', target = '_blank')))
                  }
                  
                  # 如果 Source3 不为 NA，添加额外的链接，并在每个链接之间添加空格
                  if (df_nirsevimab[index, 'Source3']!='') {
                    links <- append(links, list(tags$span(" "), tags$a(href = df_nirsevimab[index, 'Source3'], 'link', target = '_blank')))
                  }
                  
                  # 如果 Source5 不为 NA，添加额外的链接，并在每个链接之间添加空格
                  if (df_nirsevimab[index, 'Source5']!='') {
                    links <- append(links, list(tags$span(" "), tags$a(href = df_nirsevimab[index, 'Source5'], 'link', target = '_blank')))
                  }
                  
                  # 返回所有链接，包含空格
                  return(tagList(links))
                }),
                EU = colDef(show=T,width = 50,
                  cell=function(value,index){
                    if(value=='y'){
                      img(class='team-flag',src='https://flagcdn.com/h80/eu.png')
                    } else {
                      div()
                    }
                  }
                ),
                type = colDef(show=T,cell = function(value) {
                  class <- paste0("tag status-", tolower(value))
                  div(class = class, value)
                })
              ),
              columnGroups = list(
                colGroup(name = "Licensed", columns = c("Licensed_Date","Source1")),
                colGroup(name = "Recommended", columns = c("Recommend_Date","Source2"))
              ))
  )
}

shinyApp(ui, server)

