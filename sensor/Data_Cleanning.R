# Load data ---------------------------------------------------------------

locations = read.table('data/mote-location-data.txt',header = TRUE)
head(locations)

values = read.csv('data/sonoma-data-net.csv')
head(values)


# Remove N/As -------------------------------------------------------------

values = na.omit(values)


# Outliers locations ------------------------------------------------------

## # Distance
outliers_distance = which(locations$Dist>1)

## # Remove outliers
locations = locations[-outliers_distance,]

# Outliers values ---------------------------------------------------------

## # Parent
outliers_parent = which(values$parent==65535)

## # Voltage
outliers_voltage = which(values$voltage>300)

## # Depth
outliers_depth = which(values$depth>=250)

## # Adjusted humidity
outliers_humid_adj = which(values$humid_adj>120)

## # Temperature
outliers_humid_temp = which(values$humid_temp>40)


## # Remove outliers
outliers_values = unique(c(outliers_parent,
                           outliers_voltage,
                           outliers_depth,
                           outliers_humid_temp,
                           outliers_humid_adj))
values = values[-outliers_values,]

## # Reassign outliers
values$humidity[values$humidity<0] = 0
values$humidity[values$humidity>100] = 100
values$humid_adj[values$humid_adj<0] = 0
values$humid_adj[values$humid_adj>100] = 100
