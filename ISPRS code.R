library(data.table)
library(dplyr)
library(rgdal)
library(rgeos)
library(FNN)
library(fastcluster)

# 读数据
route <- fread("/home/public/data/BUS_ROUTE_DIC.csv")

gps <- fread("/home/public/data/GPS_DATA.csv")
gps <- gps[lng > 113.766 & lng < 115.617 & lat > 22.450 & lat < 23.867, ] # 深圳经纬度 东经113°46'～114°37'，北纬22°27'～22°52'
sum(duplicated(gps[, .(bus_id, day, time)]))
gps <- unique(gps, by=c("bus_id", "day", "time"), fromFirst=TRUE)
# 57900167 records

afc <- fread("/home/public/data/AFC_DATA.csv")
sum(duplicated(afc[, .(card_id, day, time)]))
afc <- unique(afc, by=c("card_id", "day", "time"), fromFirst=TRUE)
# 20036579 records

setkey(gps, bus_id, day, time)
setkey(afc, bus_id, day, time)

gps[, timeUp := time]
setnames(gps, c("lng", "lat"), c("lngUp", "latUp"))
afc <- gps[afc, roll=-Inf]

setnames(gps, c("lngUp", "latUp", "timeUp"), c("lngDown", "latDown", "timeDown"))
afc <- gps[afc, roll=Inf]

afc[, c("timeUp", "timeDown") := list(timeUp - time, time - timeDown)]

setnames(gps, c("lngDown", "latDown"), c("lng", "lat"))
gps[, timeDown := NULL]

# 准备Task1数据
getGeo <- function(timeDown, lngDown, latDown, timeUp, lngUp, latUp) {
    if (!is.na(timeDown) & !is.na(timeUp)) {
        if (timeDown == 0) {
            lng <- lngDown
            lat <- latDown
            trust <- 0
            return(c(lng, lat, trust))
        }
        else {
            lng <- (timeUp * lngDown + timeDown * lngUp) / (timeUp + timeDown)
            lat <- (timeUp * latDown + timeDown * latUp) / (timeUp + timeDown)
            trust <- (timeUp + timeDown) / 2
            return(c(lng, lat, trust)) 
        }
    }
    if (!is.na(timeDown) & is.na(timeUp)) {
        lng <- lngDown
        lat <- latDown
        trust <- timeDown ^ 2
        return(c(lng, lat, trust))
    }
    if (is.na(timeDown) & !is.na(timeUp)) {
        lng <- lngUp
        lat <- latUp
        trust <- timeUp ^ 2
        return(c(lng, lat, trust))
    }
    lng <- NA
    lat <- NA
    trust <- NA
    return(c(lng, lat, trust))
}
afcFull <- mapply(getGeo, afc$timeDown, afc$lngDown, afc$latDown, afc$timeUp, afc$lngUp, afc$latUp)
afc[, c("lng", "lat", "trust") := list(afcFull[1, ], afcFull[2, ], afcFull[3, ])]

afc[, timeLag := lag(time, 1L), by=.(bus_id, day)]
afc[, c("stopDiff", "dirDiff") := list(ifelse(time - timeLag >= 90, 1, 0), ifelse(time - timeLag >= 480, 1, 0))]

afc <- left_join(afc, route, by="bus_id")

afc[is.na(stopDiff), stopDiff := 0]
afc[, stopIndex := cumsum(stopDiff), by=.(bus_id, day)]

rm(afcFull)

# 投影
afcSpatial <- afc[!is.na(trust), ]
afcSpatial <- afcSpatial[, c("lngDown", "latDown", "timeDown", "lngUp", "latUp", "timeUp") := 
    list(NULL, NULL, NULL, NULL, NULL, NULL)]
afcSpatial <- data.frame(afcSpatial, x=afcSpatial$lng, y=afcSpatial$lat)
coordinates(afcSpatial) <- c("x", "y")
proj4string(afcSpatial) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 + datum=WGS84")
afcSpatial <- spTransform(afcSpatial, CRS("+init=epsg:2414 +datum=WGS84"))
afcSpatial <- data.table(coordinates(afcSpatial), afcSpatial@data)
# 19030638 records

# 首末站点聚类
getClustSE <- function(routeStop) {
    routeStopClust <- hclust(dist(data.frame(x=routeStop[, x], y=routeStop[, y])), 
                             method="complete")
    routeStopClust <- cutree(routeStopClust, h=300)
    return(routeStopClust)
}

afcSE <- afcSpatial[dirDiff == 1, ]
afcSENew <- data.table()
for (i in 1:500) {
    afcSEOne <- afcSE[route_id == i, ]
    afcSEClust <- data.table(Clust=getClustSE(afcSEOne))
    afcSEOne <- cbind(afcSEOne, afcSEClust)
    afcSEClust[, Count := .N, by=Clust]
    afcSEClust <- afcSEClust[, .(Count = mean(Count)), by=Clust]
    afcSEClust[, Rank := rank(-Count, ties.method="min")]
    afcSENew <- rbind(afcSENew, merge(afcSEOne, afcSEClust, by="Clust"))
}
afcSENew <- afcSENew[Rank < 3, .(lng = mean(lng), lat = mean(lat), x=mean(x), y=mean(y)), by=.(route_id, Rank)]

getSE <- function(Route_id, X, Y) {
    Near <- get.knnx(data.frame(X, Y), afcSENew[route_id == Route_id, list(x, y)], k=1)
    Dis <- min(Near[[2]])
    return(ifelse(Dis < 300, 1, 0))
}
dirIndex <- mapply(getSE, afcSpatial$route_id, afcSpatial$x, afcSpatial$y)
afcSE <- cbind(afcSpatial[, list(bus_id, day, time)], dirIindex)
afcSE <- afcSE[dirIndex == 1, ]

setkey(afcSpatial, bus_id, day, time)
setkey(afcSE, bus_id, day, time)
afcSpatial <- afcSE[afcSpatial, roll=TRUE]
setnames(afcSE, "dirIndex", "dirIndexDel")
afcSpatial <- afcSE[afcSpatial, roll=-Inf]
afcSpatial[is.na(dirIndex), dirIndex := ifelse(dirIndexDel == 1, 0, 1)]
afcSpatial <- afcSpatial[is.na(dirIndex), dirIndex := sample(0:1, 1)]
afcSpatial[, dirIndexDel := NULL]

rm(afcSE, afcSENew, afcSEOne, afcSEClust, Near, Dis, i)

# 所有站点聚类
getClust <- function(routeStop) {
    routeStopClust <- hclust(dist(data.frame(x=routeStop[, x], y=routeStop[, y])), 
                                    method="ward.D2")
    routeStopClust <- cutree(routeStopClust, h=200)
    return(routeStopClust)
}

stopSpatial <- afcSpatial[, .(time = mean(time), span = max(time) - min(time), 
                              num = .N, lat = mean(lat), lng = mean(lng), x = mean(x), 
                              y = mean(y), Trust = mean(trust)), 
                          by=.(route_id, bus_id, day, dirIndex, stopIndex)]

stopNew <- data.table()
for (i in 1:500) {
    for (j in 1:2) {
        stopSpatialOne <- stopSpatial[route_id == i & dirIndex == j, ]
        stopSpatialClust <- getClust(stopSpatialOne)
        stopSpatialClust <- cbind(stopSpatialOne, Clust=stopSpatialClust)
        stopSpatialClust[, Count := sum(num), by=Clust]
        stopNew <- rbind(stopNew, stopSpatialClust)
    }
}
afcSpatial <- left_join(afcSpatial, stopNew[, list(route_id, bus_id, day, dirIndex, stopIndex, Trust, Clust, Count)], 
                       by=c("route_id", "bus_id", "day", "dirIndex", "stopIndex"))
stopNew <- stopNew %>% group_by(route_id, dirIndex, Clust) %>% 
    summarize(x=mean(x), y=mean(y), lng=mean(lng), lat=mean(lat), Trust=mean(Trust), Count=mean(Count))
stopNew <- stopNew[Count > 20, ]

rm(stopSpatialOne, stopSpatialClust, i, j)

# 输出Task1结果
stopOut <- stopNew
stopOut[, seq := 1:.N, by=.(route_id, dirIndex)]
stopOut <- data.frame(stop_id=1:nrow(stopOut), route_id=stopOut$route_id, 
                         direction=stopOut$dirIndex, seq=stopOut$seq,
                         lng=stopOut$lng, lat=stopOut$lat)
write.table(stopOut, "RESULT_STOP_LIST.csv", sep=",", row.names=FALSE, col.names=FALSE)



# 准备Task2数据
setkey(afcSpatial, card_id, day, time)
afcSpatial[, swipe := .N, by=.(card_id, day)]

afcSpatial[swipe >= 2, c("route_id_next", "Clust_next", "lng_next", "lat_next", "x_next", "y_next") := 
           list(c(lead(route_id, 1L)[-.N], .SD[1, route_id]), c(lead(Clust, 1L)[-.N], .SD[1, Clust]), 
                c(lead(lng, 1L)[-.N], .SD[1, lng]), c(lead(lat, 1L)[-.N], .SD[1, lat]), 
                c(lead(x, 1L)[-.N], .SD[1, x]), c(lead(y, 1L)[-.N], .SD[1, y])), 
                by=.(card_id, day)]

# afcGood
afcGood <- afcSpatial[!is.na(route_id_next), ]

getAlight <- function(Route_id, DirIndex, Route_id_next, X_next, Y_next) {
    Route <- stopNew[route_id == Route_id, dirIndex == DirIndex, list(x, y, lng, lat)]
    Next <- data.frame(X_next, Y_next)
    Near <- get.knnx(Next, Route[, list(x, y)], k=1)
    Dis <- min(Near[[2]])
    Index <- which.min(Near[[2]])
    return(c(Route[[Index, "lng"]], Route[[Index, "lat"]], Dis))
}

afcGoodFull <- mapply(getAlight, afcGood$route_id, afcGood$dirIndex, 
                      afcGood$route_id_next, afcGood$x_next, afcGood$y_next)
afcGood[, c("lng_end", "lat_end", "dis") := list(afcGoodFull[1, ], afcGoodFull[2, ], afcGoodFull[3, ])]
rm(afcGoodFull)

# afcBad
afcBad <- afcSpatial[is.na(route_id_next), ]
setkey(afcGood, card_id, route_id, time)
setkey(afcBad, card_id, route_id, time)
sum(duplicated(afcGood[, .(card_id, route_id, time)]))
afcGoodJoin <- unique(afcGood, by=c("card_id", "route_id", "time"), fromFirst=TRUE)
afcBad <- afcGoodJoin[, list(card_id, route_id, time, lng_end, lat_end, dis)][afcBad, roll="nearest"]
rm(afcJoin)

# afcBadRemain
afcBadRemain <- afcBad[is.na(lng_end), ]
alightDis <- afcGood[, .(route_id, dirIndex, Clust, route_id_next, dirIndex_next, Clust_next, lng_end, lat_end)]
alightDis <- alightDis[, sample_n(.SD, 10, replace=TRUE), by=.(route_id, dirIndex, Clust)]
setkey(alightDis, route_id, dirIndex, Clust)

assignDis <- function(Route_id, DirIndex, clust) {
    afcGoodOne <- alightDis[.(Route_id, DirIndex, clust), ]
    if (nrow(afcGoodOne) > 0) {
        afcGoodOne <- sample_n(afcGoodOne, size=1)
        return(c(afcGoodOne$lng_end, afcGoodOne$lat_end))
    }
    else {
        return(c(NA, NA))
    }
}
alightDisFull <- mapply(assignDis, afcBadRemain$route_id, afcBadRemain$dirIndex, afcBadRemain$Clust)
afcBadRemain[, c("lng_end", "lat_end", "dis") := list(alightDisFull[1, ], alightDisFull[2, ], NA)]
setkey(afcBad, guid)
setkey(afcBadRemain, guid)
afcBad[, c("lng_end", "lat_end") := afcBadRemain[.(.SD), .(lng_end, lat_end)]]
rm(afcBadRemain, alightDis, alightDisFull)

# 输出Task2结果
setcolorder(afcBad, names(afcGood))
afcOut <- rbind(afcGood, afcBad)
afcOut <- data.frame(guid=afcOut$guid, lng=afcOut$lng_end, lat=afcOut$lat_end)
write.table(afcOut, "RESULT_ALIGHT_LIST.csv", sep=",", row.names=FALSE, col.names=FALSE)
save.image(".RData")