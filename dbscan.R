library(dbscan)
library(data.table)
library(ggplot2)
library(rgdal)
library(rgeos)

# 读数据
route <- fread("D:/data/ISPRS复赛数据/BUS_ROUTE_DIC.csv")
routeName <- fread("D:/data/ISPRS复赛数据/selected_route_list.csv")
gps <- fread("D:/data/ISPRS复赛数据/GPS_DATA.csv")
afc <- fread("D:/data/ISPRS复赛数据/AFC_DATA.csv")
afc <- merge(afc, route, by="bus_id")

setkey(route, route_id, bus_id)
setkey(gps, bus_id, day, time)
setkey(afc, bus_id, day, time)

gps[, timeUp := time]
setnames(gps, c("lng", "lat"), c("lngUp", "latUp"))
afc <- gps[afc, roll = -Inf]

setnames(gps, c("lngUp", "latUp", "timeUp"), c("lngDown", "latDown", "timeDown"))
afc <- gps[afc, roll = Inf]

afc[, c("timeUp", "timeDown") := .(timeUp - time, time - timeDown)]

setnames(gps, c("lngDown", "latDown"), c("lng", "lat"))
gps[, timeDown := NULL]

rm(gps)

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
afc[, c("lng", "lat", "trust") := .(afcFull[1, ], afcFull[2, ], afcFull[3, ])]
afc[, c("lngDown", "latDown", "timeDown", "lngUp", "latUp", "timeUp") := NULL]
rm(afcFull)

setkey(afc, bus_id, day, time)
afc[, timeInterval := time -  shift(time, 1L, type = "lag"), by = .(bus_id, day)]
afc[, timeGroup := ifelse(timeInterval >= 60, 1, 0)]
afc[is.na(timeGroup), timeGroup := 0]
afc[, timeGroup := cumsum(timeGroup), by = .(bus_id, day)]

afcTimeGroup <- afc[!is.na(trust), ]
setkey(afcTimeGroup, bus_id, day, timeGroup, trust)
afcTimeGroup <- unique(afcTimeGroup, by = c("bus_id", "day", "timeGroup"), fromFirst = TRUE)
# quantile(afcTimeGroup$trust, 0.5)
# afcTimeGroup <- afcTimeGroup[trust <= 24, ]

afcTimeGroup[, spaceGroup := dbscan(.SD[, .(lng, lat)], eps = 0.00045, minPts = 50)$cluster, by = route_id]

setkey(afcTimeGroup, route_id, spaceGroup, trust)
afcSpaceGroup <- unique(afcTimeGroup[spaceGroup > 0, ], by = c("route_id", "spaceGroup"), fromFirst = TRUE)

routeCount <- afcSpaceGroup[, .N, by = route_id]
routeName <- merge(routeName, routeCount, by = "route_id")

ggplot(afcSpaceGroup[route_id == 2, ], aes(x = lng, y = lat)) +geom_point()

# Test
res <- dbscan(afcTimeGroup[route_id == 2, .(lng, lat)], eps = 0.00405, minPts = 80) # 90m
res
kNNdistplot(afcTimeGroup[route_id == 2, .(lng, lat)], k = 80)
abline(h = 0.000405, lty = 2)

test <- data.table(afcTimeGroup[route_id == 2, .(lng, lat, trust)], group = res$cluster)
setkey(test, group, trust)
test <- unique(test[group > 0, ], by = "group", fromFirst = TRUE)
ggplot(data = test, aes(x = lng, y = lat, label = group)) + geom_point() + geom_label()

# best parameter
stopCount <- data.table()
for (i in c(0.00045*0.7, 0.00045*0.8, 0.00045*0.9, 0.00045*1, 0.00045*1.1, 0.00045*1.2, 0.00045*1.3)) {
    for (j in c(40, 50, 60, 70, 80, 90, 100)) {
        for (k in c(2, 4, 5, 7, 8, 9, 10, 12, 13, 14)) {
            res <- dbscan(afcTimeGroup[route_id == k, .(lng, lat)], eps = i, minPts = j)
            stopCount <- rbind(stopCount, data.table(k, i, j, max(res$cluster)))
        }
    }
}

stopNum <- data.table(k = c(2, 4, 5, 7, 8, 9, 10, 12, 13, 14), V5 = c(59, 77, 52, 64, 81, 88, 57, 72, 23, 48))
stopCount <- merge(stopCount, stopNum, by = "k")

stopTest <- stopCount[, ks.test(V4, V5), by = .(i, j)]

dcast(stopTest[, .(i, j, statistic)], i ~ j)

# route 2 322路 54-59
# route 4 332路 76-77
# route 5 333路 50-52
# route 7 336路 63-64
# route 8 337路 77-81
# route 9 338路 87-88
# route 10 339路 56-57
# route 12 362路 71-72
# route 13 376路 16-23
# route 14 383路 47-48