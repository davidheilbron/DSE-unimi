# GOOGLE MAPS API

# CRAN install / stable version 
install.packages("gmapsdistance")
library(gmapsdistance)
set.api.key("...")

library(readxl)
it <- read_excel("it.xlsx", range = "A1:C101", col_names = T) #read file
it <- data.frame(it)
rownames(it) <- it[,1]
it <- it[,-1]
it$lat <- as.character(it$lat)
it$lng <- as.character(it$lng)
it$coord <- paste(it$lat, it$lng, sep = '+')
str(it)

#Get Distance and Time between coordinates (cities)
results <- gmapsdistance(origin = it$coord[81:100], #must change the window to cover all the locations
                         destination = it$coord[81:100], #could improve it using a loop
                         combinations = "all",
                         mode = "driving",
                         key = "...",
                         #departure = round(as.numeric(Sys.time())),
                         dep_date = "2023-02-09",
                         dep_time = "12:00:00",
                         traffic_model = "optimistic",
                         shape = 'long')

#Append results to matrix M
str(results)
M <-rbind(M, data.frame(results[[1]], results[[2]]))
head(M)
dim(M)

# Compute distances
colnames(M) <- c("Origin", "Destination", "Distance", "Time")

install.packages("geosphere")
install.packages("stirngr")
library(geosphere)
library(stringr)
library(dplyr)
library(tidyr)

H <- data.frame(lapply(M[,1:2], as.character), stringsAsFactors=FALSE)

#Origin coordinates
S <- t(data.frame(strsplit(H$Origin, split = '+', fixed=T)))
colnames(S) <- c("O_lat", "O_lng")
rownames(S) <- NULL
head(S)

#Destination coordinates
X <- t(data.frame(strsplit(H$Destination, split = '+', fixed=T)))
colnames(X) <- c("D_lat", "D_lng")
rownames(X) <- NULL
head(X)

H <- cbind(S,X)
rm(S,X)

H <- as.data.frame(apply(H, 2, as.numeric))
str(H)

# As the crow-flies distance
H <-H %>% mutate(data.frame(distHaversine(H[,1:2], H[,3:4], r=6378137)))
H <-H %>% mutate(M$Distance, M$Time)
colnames(H)[5:7] <- c("Haversine", "Distance", "Time")
M <-H
head(M) #check results
rm(H)

# Crow-flies ratio
M$cr_ratio <- M$Distance/M$Haversine
M[is.na(M)] <- 0

#Translate coordinated into cities
cities <- data.frame(cbind(it$coord, rownames(it)))
colnames(cities) <- c("coord", "city")

M$O <-paste(M$O_lat, M$O_lng, sep='+')
M <- right_join(M, cities, by=c('O'='coord'))
colnames(M)[10] <- c("Or_city")

M$D <-paste(M$D_lat, M$D_lng, sep='+')
M <- right_join(M, cities, by=c('D'='coord'))
colnames(M)[12] <- c("De_city")

rm(cities, results)

#Now our data is ready to make calculations and graphs
data <- data.frame(M$Or_city, M$De_city, M[,5:8])
colnames(data)[1:2] <- c("origin", "destiny")
head(data)
tail(data)

# Mean Speed Score
MS <- data.frame(aggregate(data$Distance, by=list(origin = data$origin), FUN = sum))
MS$t <- data.frame(aggregate(data$Time, by=list(origin = data$origin), FUN = sum))
MS <- data.frame(MS$x, MS$t)
rownames(MS) <- MS[,2]
MS <- MS[,-2]
colnames(MS) <- c("distance", "time")
MS$mean_speed <- MS$distance/MS$time
head(MS[order(-MS$mean_speed),]) #fastest cities
MS$mean_speed <- MS$mean_speed*3.6 #translate score into km/h

# Geometric Mean Speed Score
data$ms <- data$Distance/data$Time
data["ms"][data["ms"] == 0] <- NA
data <- na.omit(data)

gmean <- function(x) exp(mean(log(x)))
gmean(data$ms)
GMS <- data.frame(aggregate(data$ms, by = list(origin = data$origin), FUN = gmean))
rownames(GMS) <- GMS[,1]
colnames(GMS)[2] <- c("geometric_ms")
GMS$geometric_ms <- GMS$geometric_ms*3.6

# Adjusted Mean Speed
data$sq_cr_r <- sqrt(data$cr_ratio)
data$time_cr_r <- data$Time/data$sq_cr_r
aMS <- data.frame(aggregate(data$Distance, by=list(origin = data$origin), FUN = sum))
aMS$ad_t <- data.frame(aggregate(data$time_cr_r, by=list(origin = data$origin), FUN = sum))

aMS <- data.frame(aMS$x, aMS$ad_t)
rownames(aMS) <- aMS[,2]
aMS <- aMS[,-2]
colnames(aMS) <- c("distance", "ad_time")
aMS$adj_mean_speed <- (aMS$distance/aMS$ad_time)*3.6
head(aMS[order(-aMS$adj_mean_speed),])
rm(gmean)


# Create a data frame with all the results and the coordinates from 'it'
results <- data.frame(MS$mean_speed, GMS$geometric_ms, aMS$adj_mean_speed)
colnames(results)[4:6] <- c("MS", "GMS", "aMS")
rownames(results) <- rownames(MS)
results <- merge(it, results, by=0, all.x=T)
write.xlsx(results, file = 'results.xlsx', sheetName = "Sheet1", 
           colnames = TRUE, rownames = T, append = FALSE) #just in case something happens with M

# Map the data
library(tidyverse)
library(sf)
library(mapview)
library(viridis)

results %>% 
  glimpse()

res_sf <- st_as_sf(results, coords = c("lng", "lat"),  crs = 4326)
pal <- magma(n = length(unique(res_sf$MS)), direction = 1)

MS_map <- mapview(res_sf[,2], 
                  map.types = "Stamen.TonerLite", 
                  zcol = 'MS',
                  legend = T,
                  layer.name = 'Mean Speed')  

GMS_map <- mapview(res_sf[,3], 
        map.types = "Stamen.TonerLite", 
        zcol = 'GMS',
        legend = T,
        layer.name = 'Geometric Mean Speed'
        ) 

aMS_map <- mapview(res_sf[,4], 
                  map.types = "Stamen.TonerLite", 
                  zcol = 'aMS',
                  legend = T,
                  layer.name = 'Adjusted Mean Speed') 

# Graph maps
MS_map
GMS_map
aMS_map
