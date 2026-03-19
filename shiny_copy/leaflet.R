# 加载必要的包
library(leaflet)
library(maps)
library(mapdata)
library(dplyr)
library(sf)

# 提取 worldHires 中的世界地图数据
world_map <- map("worldHires", plot = FALSE, fill = TRUE)

# 将地图数据转换为 sf 格式，并指定 WGS84 坐标系
world_sf <- st_as_sf(world_map, crs = 4326)  # WGS84坐标系

# 添加示例数据（例如人均 GDP）
set.seed(123)
world_sf$gdp_per_capita <- sample(500:100000, nrow(world_sf), replace = TRUE)

# 创建颜色调色板
palette <- colorBin("YlOrRd", domain = world_sf$gdp_per_capita, bins = 7)

# 使用 leaflet 绘制分级地图，并设置显示范围
leaflet(world_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette(gdp_per_capita),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste0(ID, ": $", round(gdp_per_capita, 2)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  # 设置显示范围：setView 或 fitBounds
  setView(lng = 105, lat = 35, zoom = 3) %>%  # 通过 setView 设置地图中心和缩放级别
  # 或者使用 fitBounds 指定边界经纬度范围
  fitBounds(lng1 = -180, lat1 = -60, lng2 = 180, lat2 = 80) %>%
  addLegend(pal = palette, values = ~gdp_per_capita, opacity = 0.7,
            title = "GDP per Capita", position = "bottomright")

