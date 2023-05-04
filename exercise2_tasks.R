git config --global user.email "jakobmi1@students.zhaw.ch"

###Task1: import data
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tmap)
library(gitcreds)
library(dplyr)


wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)


###Task2: getting an overview

wildschwein_BE$TierID |> unique()
#How many individuals were tracked? -> three, "002A" "016A" "018A"
  

#visualize with gggplot
ggplot(wildschwein_BE, aes(DatetimeUTC, TierName))+
  geom_point()

#Vorlesung: group by individuum, time difference between subsequent rows
wildschwein <- wildschwein_BE |> 
  group_by(TierName) |> 
  mutate(diff_s = difftime(lead(DatetimeUTC), DatetimeUTC))

ggplot(wildschwein, aes(diff_s/60))+
  geom_histogram(binwidth = 1)+
  lims(x=c(0,5000/60))+
  scale_y_log10()


wildschwein |> 
  filter(DatetimeUTC < "2014-09-01") |>
  ggplot(wildschwein, aes(DatetimeUTC, diff_s, colour=TierName))+
  geom_point()+
  geom_line()

##What is the temporal sampling interval between the locations?
wildschwein$timelag <- as.numeric(difftime(lead(wildschwein$DatetimeUTC), wildschwein$DatetimeUTC))

wildschwein |> # Take wildschwein...
group_by(TierName) |> # ...group it by Tiername
  summarise( # Summarise the data...
    mean_timelag = mean(timelag, na.rm = T) # ...by calculating the mean timelag
  )


#Task3: Deriving movement parameters I: Speed

#Formula
sqrt((E1-E2)^2 + (N1-N2)^2)

# From Sabi, does it needs those start values??
E1<-2570409
E2<-2570402
N1<-1204752
N2<-1204863


#Distance
wildschwein <- wildschwein |> 
  group_by(TierName) |> 
  mutate(steplength_m = sqrt((E1-lead(E,1))^2 + (N1-lead(N,1))^2)) # Distanz

#Speed
wildschwein <- wildschwein |> 
  mutate(speed_ms = steplength_m/timelag) 



#Task4: Cross-scale movement analysis
#import data
caro <- read_delim("caro60.csv", ",")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

#reduce granularity of sampling intervall
nrow(caro) #200

caro3<- caro |> 
  slice(seq(from=1, to=200, by=3))
nrow(caro3) #67

caro6<- caro |> 
  slice(seq(from=1, to=200, by=6))
nrow(caro6) #34

caro9<- caro |> 
  slice(seq(from=1, to=200, by=9))
nrow(caro9) #23

#calculate timelag, steplength and speed
caro <- caro |> 
  mutate(
    timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
    steplength = sqrt((E1-lead(E))^2 + (N1-lead(N))^2),
    speed = steplength/timelag
  )

caro3 <- caro3 |> 
  mutate(
    timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
    steplength = sqrt((E1-lead(E))^2 + (N1-lead(N))^2),
    speed = steplength/timelag
  )

caro6 <- caro6 |> 
  mutate(
    timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
    steplength = sqrt((E1-lead(E))^2 + (N1-lead(N))^2),
    speed = steplength/timelag
  )

caro9 <- caro9 |> 
  mutate(
    timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
    steplength = sqrt((E1-lead(E))^2 + (N1-lead(N))^2),
    speed = steplength/timelag
  )

#compare the speeds visually in a line plot and also visualize the trajectories in a map
ggplot()+
  geom_sf(data=caro)+
  geom_line(data = caro, aes(DatetimeUTC, speed), color="blue")+
  geom_path()
#how can I visualize the trajectories??? it doesnt work

ggplot()+
  geom_line(data = caro3, aes(DatetimeUTC, speed), color="green")+
  geom_line(data = caro6, aes(DatetimeUTC, speed), color="yellow")+
  geom_line(data = caro9, aes(DatetimeUTC, speed), color="red")
#less datapoints (bigger intervall) results in lower speed  



#Task 5 (optional): no time to solve it (Solve and Check later solutions!)

#Task 6: Add your movement data to your repository
#Done

#Task 7: Explore your movement data
#Import data
posmo <- read_delim("posmo_2023-04-01T00_00_00+02_00-2023-05-04T23_59_59+02_00.csv", ",")

posmo <- st_as_sf(posmo, coords = c("lon_x", "lat_y"), crs = 2056, remove = FALSE)

#plot map

library("ggplot2")

ggplot(posmo, aes(lon_x, lat_y, colour = transport_mode)) +
  geom_point() +
  theme(legend.position = "none")
#wrong coordinate system in map. Why?

#interactive map doesnt works, "Error: posmo consists of spatial points, so it cannot accept tm_fill/tm_borders/tm_polygons."
library("tmap")
tmap_mode("view")

tm_shape(posmo) +
  tm_polygons(col = "transport_mode", alpha = 0.4, border.col = "red") +
  tm_legend(bg.color = "white")

