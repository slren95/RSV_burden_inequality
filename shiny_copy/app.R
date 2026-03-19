library(shiny)
library(bslib)
library(leaflet)
library(rio)
library(reactable)
library(tidyverse)
library(sf)

# file.copy('rda/df_nirsevimab.csv','shiny/df_nirsevimab.csv',overwrite = T)
# file.copy('rda/df_abrysvo.csv','shiny/df_abrysvo.csv',overwrite = T)
# file.copy('rda/df_nir_abr_sf.rds','shiny/df_nir_abr_sf.rds',overwrite = T)


# From code_09_nirsevimab_tracker.R
df_nirsevimab <- import('df_nirsevimab.csv') %>% drop_na(Licensed_Date) %>%
  relocate(WHORegion,Income2019,.after=ISO2)

df_abrysvo <- import('df_abrysvo.csv')%>%
  relocate(WHORegion,Income2019,.after=ISO2)

# From code_10_seasonality.R
df_nir_abr_sf<-readRDS('df_nir_abr_sf.rds') %>% drop_na(Licensed_Date) %>%
  st_as_sf()

# 1️⃣ 定义 WHORegion 的固定颜色

region_colors <- c(
  "AFR" = "#0392cf", "AMR" = "#ffad60", "EMR" = "#E6B800",
  "EUR" = "#7bc043", "SEAR" =  "#d81159", "WPR" = "#6a4c93"
)

# 2️⃣ 定义 Income Level 的颜色梯度
income_palette <- colorRampPalette(c("#d1e5f0", "#2166ac"))  # 低 -> 高
income_levels <- c("L", "LM", "UM", "H")
income_colors <- setNames(income_palette(length(income_levels)), income_levels)

ui <- page_sidebar(fillable = T,
                   title = div(
                     style = "display: flex; align-items: center; justify-content: space-between; width: 100%",
                     # 左侧标题
                     div(
                       style = "flex: 0 0 auto;",
                       h4("Global RSV Prevention Product(for infant) Promotion Tracker", 
                          style = "margin: 0; color: #2c3e50;")
                     ),
                     # 中间留空（自动填充）
                     div(style = "flex: 1 1 auto;"),
                     # 右侧三个logo
                     div(
                       style = "display: flex; align-items: center; gap: 10px; flex: 0 0 auto;",
                       tags$a(
                         href="https://leoly2017.github.io/group/",target = "_blank", 
                         img(src = "logo_IDEM.png", height = "80px", 
                             style = "cursor: pointer;" ,title = "Infectious Diseases Epidemiology & Modelling Group")
                       ),
                       tags$a(
                         href="https://english.njmu.edu.cn/",target = "_blank", 
                         img(src = "logo_NJMU.png", height = "75px", 
                             style = "cursor: pointer;" ,title = "Nanjing Medical University")
                       )
                     )
                   ),
  includeCSS("custom_styles.css"),
  tags$script(HTML("
    $(window).resize(function() {
      var mapHeight = $(window).height() - 150;  
      $('#map').css('height', mapHeight + 'px');
      $('#table').css('height', mapHeight + 'px');
    });
    $(window).trigger('resize'); 
  ")),
  sidebar = sidebar(width = '15%',
    radioButtons("product", "Products:",
                 choices = c("Nirsevimab"='Nirsevimab',"RSVpreF maternal vaccine"='Abrysvo'),
                 selected = "Nirsevimab"),
    radioButtons("status_filter", "Promotion Status:",
                 choices = c("All" = "NIP-included|Licensed",
                             "NIP-included" = "NIP-included",
                             "Licensed" = "Licensed"),
                 selected = "NIP-included|Licensed"),
    downloadButton(
      "download",
      "Download",
      icon = icon("download"),
      class = "btn-primary",
      style = ""
    ),
    helpText('This dashboard provides an overview of the global promotion status of RSV prevention products for infants. The data were last updated on April 1, 2025. If you spot any missing or incorrect information, please feel free to contact us at slren@njmu.edu.cn — your feedback is highly appreciated!')
  ),
  navset_tab(
    nav_panel(
      "Map",
      div(
        style = "position: relative;",
        leafletOutput("map", height = "600px"),
        absolutePanel(
          top = 10, right = 10, width = 300, draggable = TRUE, 
          style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0px 0px 5px rgba(0,0,0,0.2); z-index: 1000;",
          uiOutput("map_info")  # 统计信息
        )
        )
    ),
    nav_panel(
      "Data",
      reactableOutput("table",height='600px')
    ),
    tabPanel(
      "Introduction",
      includeMarkdown("intro.md")
    )
  )
)

server <- function(input, output, session) {
  df_filtered<-reactive({
    if(input$product=='Nirsevimab') df<-df_nirsevimab %>% filter(str_detect(type,input$status_filter)) %>% arrange(desc(type))
      else df<-df_abrysvo %>% filter(str_detect(type,input$status_filter)) %>% arrange(desc(type))
      df %>% relocate(EU,.after = ISO2)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0('data-',input$product,'-',Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(df_filtered(), file, row.names = TRUE)
    }
  )


  # Helper function to create link HTML
  create_link_html <- function(url, text = "link") {
    ifelse(is.na(url) | url=='',"",
           sprintf('<a href="%s" target="_blank">%s</a>', url, text))
  }
  # Map output
  output$map <- renderLeaflet({
    #map_data <- filtered_data()
    
    pal <- colorFactor(
      palette = c("skyblue", "salmon"),
      domain = c("Licensed", "NIP-included")
    )
    
    df<-df_nir_abr_sf %>%
      filter(Product==input$product,str_detect(type,input$status_filter))
    
    leaflet(df,
            options = leafletOptions(worldCopyJump = FALSE,minZoom = 1)) %>%
      setView(lng = 10, lat = 50, zoom = 2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # setMaxBounds(
      #   lng1 = -30, lat1 = 35,   # 左下角
      #   lng2 = 60, lat2 = 70     # 右上角
      # ) %>% 
      addPolygons(
        fillColor=~pal(type),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = ~paste0(
          '<div style="font-size:20px; line-height:1.5;">',
          sprintf('<img class="team-flag" style="margin-right:10px" alt="%s flag" src="https://flagpedia.net/data/flags/h80/%s.png">', ISO2, tolower(ISO2)),
          sprintf("<strong>%s</strong>", Country)," (",ifelse(input$product=='Nirsevimab','Nirsevimab','RSVpreF'),")<br/>",
          sprintf('<span>Promotion Status:</span> <span class="status-%s"><stroug>%s</strong></span>',tolower(type),type), "<br/>",
          ifelse(!is.na(Licensed_Date), 
                 paste0("License Date: ", Licensed_Date, "<br/>"), ""),
          ifelse(!is.na(Source1), 
                 paste0("Source: ", create_link_html(Source1), "<br/>"), ""),
          ifelse(!is.na(Recommend_Date), 
                 paste0("Recommended Date: ", Recommend_Date, "<br/>"), ""),
          ifelse(!is.na(Recommend_Date) &!is.na(Source2), 
                 paste0("Source: ", create_link_html(Source2)," ",create_link_html(Source3)," ",create_link_html(Source5),"<br/>"), ""),
          '</div>'
        )
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = c( "Licensed","NIP-included"),
                labels = c("NIP-included22","Licensed22"),
                title = "Promotion Status")
  })
  output$map_info <- renderUI({
    if(input$product=='Nirsevimab') df<-df_nirsevimab else df<-df_abrysvo
    if(input$product=='Nirsevimab') prod<-'Nirsevimab' else prod<-'RSVpreF maternal vaccine'
    counts<-df %>% count(type) %>% deframe()
    
    div(div(style = "color: #333; padding-bottom: 1px; text-align:center;
                    border-bottom: 1px solid #ddd;margin: 0px;",
            h5("Promotion Status",style="font-weight:bold;margin: 0px;"),
            p(em(sprintf("of %s,data as of Apr 1, 2025",prod)), 
              style = "color: #777; font-size: 11px; text-align: center; margin: 0px 2px;")),
        
        p("Licensed: ", style="margin:0",
          span(paste0(counts['Licensed'],' counties'), style = "color: skyblue; font-weight: bold; font-size: 14px;")),
        
        p("NIP-included: ", style="margin:0;",
          span(paste0(counts['NIP-included'],' counties'), style = "color: salmon; font-weight: bold; font-size: 14px;"))
    )
  })
  output$table<-renderReactable({
    reactable(df_filtered(),pagination = FALSE,filterable = FALSE,searchable = FALSE,
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
                        span(class = "team-name", df_filtered()[index,'Country']),
                        span(class = "team-record", df_filtered()[index,'ISOCountry'])
                      )
                    )
                  }
                ),
                WHORegion=colDef(name='WHO Region',show=T,width=100,
                                 style = function(value) {
                                   color <- region_colors[[value]]
                                   list(background = color, color = "white", fontWeight = "bold", textAlign = "center")
                                 }),
                Income2019=colDef(name='Income Level(2019)',show=T,width=100,
                                  style = function(value) {
                                    color <- income_colors[[value]]
                                    list(background = color, color = "white", fontWeight = "bold", textAlign = "center")
                                  }),
                Licensed_Date=colDef(name='Date',show=T,width=100),
                Source1=colDef(name = 'Source',width=80,show=T,cell=function(value){
                  tags$a(href=value,'link',target='_blank')
                }),
                Recommend_Date=colDef(name='Date',show=T,width=100),
                Source2=colDef(show=T,name='Source',width=100,cell=function(value,index){
                  links <- list()
                  if (df_filtered()[index, 'Source2']!='') {
                    links <- append(links, list(tags$span(" "), tags$a(href = df_filtered()[index, 'Source2'], 'link', target = '_blank')))
                  }
                  
                  # 如果 Source3 不为 NA，添加额外的链接，并在每个链接之间添加空格
                  if (df_filtered()[index, 'Source3']!='') {
                    links <- append(links, list(tags$span(" "), tags$a(href = df_filtered()[index, 'Source3'], 'link', target = '_blank')))
                  }
                  
                  # 如果 Source5 不为 NA，添加额外的链接，并在每个链接之间添加空格
                  if (df_filtered()[index, 'Source5']!='') {
                    links <- append(links, list(tags$span(" "), tags$a(href = df_filtered()[index, 'Source5'], 'link', target = '_blank')))
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
                type = colDef(show=T,name='Promotion status',cell = function(value) {
                  class <- paste0("tag status-", tolower(value))
                  div(class = class, value)
                })
              ),
              columnGroups = list(
                colGroup(name = "Licensed", columns = c("Licensed_Date","Source1")),
                colGroup(name = "Recommended", columns = c("Recommend_Date","Source2"))
              ))
  }
  )
}

shinyApp(ui, server)