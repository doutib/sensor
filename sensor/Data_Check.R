
# Load data ---------------------------------------------------------------

locations = read.table('data/mote-location-data.txt',header = TRUE)
head(locations)

values = read.csv('data/sonoma-data-net.csv')
head(values)


# Location: distribution check --------------------------------------------

head(locations)

## # ID
range(locations$ID)

## # Height
hist(locations$Height,
     main = "Height of sensors along the trees",
     xlab = "Height (m)")

## # Direction
unique(locations$Direc)

## # Distance
hist(locations$Dist[locations$Dist<=1], ##OUTLIERS##
     main = "Distance from trunk",
     xlab = "Distance (m)",
     xlim=c(0,1))

sum(locations$Dist>1)

## # Tree
unique(locations$Tree)


# Location: missing values ------------------------------------------------

all(is.na(locations))


# Values: distribution check ----------------------------------------------

head(values)

## # Epoch
hist(values$epoch,
     main = "Id of each measurement time",
     xlab = "Id",
     xlim = c(2000,12000))

## # Time
unix_time = as.numeric(as.POSIXct(values$result_time))
time_seconds = unix_time - min(unix_time)
time_days = time_seconds/(3600*24)
hist(time_days,
     main = "Distribution of measurement times",
     xlab = "Time (day)",
     xlim = c(0,30))

## # Nodes ID
hist(values$nodeid,
     breaks = length(values$nodeid),
     main = "Number of measurements of nodes",
     xlab = "Node ID")
# Number of nodes 
length(unique(values$nodeid))
# Number of parent nodes
length(unique(values$parent))
# Id of parents/children nodes
sort(unique(values$parent)) ##OUTLIERS## outlier: 65535
sort(unique(values$nodeid))

## # Voltage
par(mfrow=c(1,2))
hist(values$voltage,
     main = "Nodes voltage",
     xlab = "voltage")
hist(values$voltage[values$voltage<300],
     main = "Nodes voltage\nwithout outliers",
     xlab = "voltage")
par(mfrow=c(1,1))
summary(values$voltage)
sum(values$voltage<250,na.rm = TRUE)/length(values$voltage)
# Proportion of outliers
sum(values$voltage>300,na.rm = TRUE)/length(values$voltage)  ##OUTLIERS##

## # Depth
summary(values$depth)
par(mfrow=c(1,2))
boxplot(values$depth,
        main = "Node depth in network",  ##OUTLIERS## 255
        ylab = "Depth")
boxplot(values$depth[values$depth<250],
        main = "Node depth in network\nwithout 255 outlier",
        ylab = "Depth")
par(mfrow=c(1,1))

## # Humidity
hist(values$humidity,              ##OUTLIERS##
     main = "Humidity measures",
     xlab = "Humidity (RH%)")
range(values$humidity,na.rm = TRUE)
# Number of outliers
sum(values$humidity>100,na.rm = TRUE)/length(values$humidity)
sum(values$humidity<0,na.rm = TRUE)/length(values$humidity)

## # Adjusted humidity
hist(values$humid_adj, ##OUTLIERS##
     main = "Adjusted humidity measures",
     xlab = "Humidity (RH%)")
hist(values$humid_adj[values$humid_adj>=0 & values$humid_adj<=100], ##OUTLIERS##
     main = "Adjusted humidity measures",
     xlab = "Humidity (RH%)")
# Number of outliers
sum(values$humid_adj>120,na.rm = TRUE)/length(values$humid_adj)
(sum(values$humid_adj<0,na.rm = TRUE)+sum(values$humid_adj<120 & values$humid_adj>100,na.rm = TRUE))/length(values$humid_adj)
range(values$humid_adj,na.rm = TRUE)

## # Temperture
hist(values$humid_temp, ##OUTLIERS##
     main = "Temperature measures",
     xlab = "Temperature (°C)")
hist(values$humid_temp[values$humid_temp<=40], ##OUTLIERS##
     main = "Temperature measures",
     xlab = "Temperature (°C)")
range(values$humid_temp,na.rm = TRUE)
# Number of outliers
sum(values$humid_temp>50,na.rm = TRUE)
sum(values$humid_temp>40,na.rm = TRUE)/length(values$humid_temp)


## # Incident PAR
hist(values$hamatop,
     main = "Incident PAR values",
     xlab = "PAR")

## # Reflected PAR
hist(values$hamatop,
     main = "Reflected PAR values",
     xlab = "PAR")


# Values: missing values --------------------------------------------------

## # Check which columns contain N/As
which(apply(is.na(values),2,any))

## # Proportion of N/As
prop_na = function(column){
  # Compute the proportion of missing values
  sum(is.na(column))/length(column)
}
apply(values[,7:11],2,prop_na)

## # Check that the N/As appear at the same rows
bool = TRUE
for (i in 7:11){
  for (j in 7:11){
    bool  = bool & all(which(is.na(values[,i]))==which(is.na(values[,j])))
  }
}
bool

## # Get rows with N/As
nas = which(is.na(values[,11]))




