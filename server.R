shiny <- function(input, output) {
    Points <- reactive({
      linePoints[routeName == input$selectLine, ]
    })
    Stops <- reactive({
      lineStops[routeName == input$selectLine, ]
    })
    StopsPlot <- reactive({
      lineStopsPlot[routeName == input$selectLine, ]
    })
    LinesPlot <- reactive({
      emptyVolumnTime[name == input$selectLine, ]
    })
    RouteIndex <- reactive({
      routeIndex[name == input$selectLine, ]
    })
    Travel <- reactive({
      travelDiagram[route_id == RouteIndex()$route_id, ]
    })
    output$stationCount <- renderInfoBox({
      infoBox("站点数量(个)", RouteIndex()$stopsCount, 
              paste("排名前", RouteIndex()$stopsCountRank, "%"), icon = icon("tag"))
    })
    output$passengerVolumn <- renderInfoBox({
      infoBox("客流量(人次)", RouteIndex()$passengerVolumn, 
              paste("排名前", RouteIndex()$passengerVolumnRank, "%"), icon = icon("male"))
    })
    output$routeLength <- renderInfoBox({
      infoBox("线路长度(公里)", RouteIndex()$routeLength, 
              paste("排名前", RouteIndex()$routeLengthRank, "%"), icon = icon("magic"))
    })
    output$routeIndirect <- renderInfoBox({
      infoBox("非直线系数", RouteIndex()$routeIndirect, 
              paste("排名前", RouteIndex()$routeIndirectRank, "%"), icon = icon("line-chart"))
    })
    output$headway <- renderInfoBox({
      infoBox("发车间隔(分钟)", RouteIndex()$headway, 
              paste("排名前", RouteIndex()$headwayRank, "%"), icon = icon("xing"))
    })
    output$vehicle <- renderInfoBox({
      infoBox("配车数(辆)", RouteIndex()$vehicle, 
              paste("排名前", RouteIndex()$vehicleRank, "%"), icon = icon("bus"))
    })
    output$volumnByLength <- renderInfoBox({
      infoBox("车公里载客人数(人/辆/公里)", RouteIndex()$volumnByLength, 
              paste("排名前", RouteIndex()$volumnByLengthRank, "%"), icon = icon("users"))
    })
    output$avgHaulDistance <- renderInfoBox({
      infoBox("平均运距(公里)", RouteIndex()$avgHaulDistance, 
              paste("排名前", RouteIndex()$avgHaulDistanceRank, "%"), icon = icon("space-shuttle"))
    })
    output$turnoverTime <- renderInfoBox({
      infoBox("行程时间(分钟)", RouteIndex()$turnoverTime, 
              paste("排名前", RouteIndex()$turnoverTimeRank, "%"), icon = icon("bell"))
    })
    output$peakHourRatio <- renderInfoBox({
      infoBox("高峰小时系数", RouteIndex()$peakHourRatio, 
              paste("排名前", RouteIndex()$peakHourRatioRank, "%"), icon = icon("compress"))
    })
    output$operationTime <- renderInfoBox({
      infoBox("运营时间", paste(RouteIndex()$startTime, "-", RouteIndex()$endTime), 
              paste("排名前", RouteIndex()$operationRank, "%"), icon = icon("paper-plane"))
    })
    output$operationCompany <- renderInfoBox({
      infoBox("运营公司", RouteIndex()$company, icon = icon("bars"))
    })
    output$line_map <- renderLeaflet({
      leaflet() %>% addTiles() %>% addPolylines(data = Points(), lng = ~lngwgs, lat = ~latwgs) %>% 
            addCircleMarkers(data = Stops(), lng = ~lngwgs, lat = ~latwgs, color = "navy", 
                             radius = ~volumn, popup = ~stopName)
    })
    output$total_volumn <- renderPlotly({
        gg <- ggplot(StopsPlot(), aes(x = stopName, y = volumn, fill = type)) + 
            geom_bar(stat = "identity", position = "dodge") + ylab("上下车人数") + xlab("") + 
            theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = .5, size = 10))
        ggplotly(gg)
    })
    output$time_volumn <- renderPlotly({
        gg <- ggplot(LinesPlot(), aes(x = time, y = volumn, group = 1)) + geom_line() +
            geom_point() + ylab("客运量") + xlab("")
        ggplotly(gg)
    })
    output$travel_diagram <- renderPlotly({
      gg <- ggplot(Travel(), aes(x = time, y = stop_id, group = bus_id, color = bus_id)) + 
        geom_path() + ylab("站点") + xlab("时间(min)") + xlim(360, 1320) + 
          theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = .5, size = 10))
      ggplotly(gg)
    })
    output$station_map <- renderLeaflet({
      leaflet() %>% addTiles() %>% addMarkers(data = lineStops, popup = ~stopName, lng = ~lngwgs, lat = ~latwgs, clusterOptions = markerClusterOptions())
    })
    output$station_volumn <- renderLeaflet({
        leaflet() %>% addTiles() %>% addCircleMarkers(data = lineStopsCombine, popup = ~stopName, lng = ~lngwgs, lat = ~latwgs, radius = ~volumn)
    })
    output$transit_O <- renderLeaflet({
      leaflet() %>% addTiles() %>% addCircleMarkers(data = lineStopsSuperCombine, lng = ~lngwgs, lat = ~latwgs, radius = ~boardVolumn / 3000)
    })
    output$transit_D <- renderLeaflet({
      leaflet() %>% addTiles() %>% addCircleMarkers(data = lineStopsSuperCombine, lng = ~lngwgs, lat = ~latwgs, radius = ~alightVolumn / 3000)
    })
    output$transit_OD <- renderLeaflet({
        leaflet() %>% addTiles() %>% addPolylines(data = OD[volumn > input$range[1] & volumn < input$range[2], ], lng = ~lng, lat = ~lat, group = ~id, color = "red", weight = ~volumn / 3000, popup = "volumn") %>% 
            addCircleMarkers(data = lineStopsSuperCombine, lng = ~lngwgs, lat = ~latwgs, radius = ~(alightVolumn + boardVolumn) / 6000)
    })
    output$get_location <- renderLeaflet({
        leaflet() %>% addTiles() %>% setView(114.2, 22.65, zoom = 10)
    })
    observeEvent(input$get_location_click, {
        click <- input$get_location_click
        if(is.null(click))
            return()
        point <- data.table(lng = click$lng, lat = click$lat)
        point[, c("x", "y") := list(lng, lat)]
        coordinates(point) <- c("x", "y")
        proj4string(point) <- CRS("+init=epsg:4326")
        point <- spTransform(point, CRS("+init=epsg:2414"))
        point <- data.table(coordinates(point), point@data)
        
        InCircleStops <- lineStops[dir == 1 & sqrt((lineStops$x - point$x) ^ 2 + (lineStops$y - point$y) ^ 2) < input$radius, route_id]
        InCircleStops <- lineStops[route_id %in% InCircleStops & dir == 1, ]

        output$dis_transit <- renderLeaflet({
            leaflet() %>% addTiles() %>% setView(lng = 114.2, lat = 22.65, zoom = 10) %>% 
                addMarkers(data = point, lng = ~lng, lat = ~lat) %>% 
                addCircles(data = point, lng = ~lng, lat = ~lat, radius = input$radius) %>% 
                addCircleMarkers(data = InCircleStops, lng = ~lngwgs, lat = ~latwgs, radius = 1)
        })
    })
}




