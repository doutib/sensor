library(stringr)
library(ggplot2)
library(gridExtra)

# Load cleaned data -------------------------------------------------------
source("Data_Cleanning.R")
head(values)
head(locations)

# Data processing ---------------------------------------------------------

# Store hours, in local time
values$hours = (as.integer(str_extract(values$result_time,"[^0-9-]+[0-9]+"))-9) %% 24
values$Portion_day = cut(values$hours, breaks = c(0,6,12,18,24),right=F)
values$Portion_day_bis = cut(values$hours, breaks = c(0,4,8,12,16,20,24),right=F)


# Merge data sets
merged_data = merge(values,locations,by.y = "ID", by.x = "nodeid")
head(merged_data)


# Humidity/Temperature ----------------------------------------------------

plot(values$humidity,values$humid_adj,
     main = "Effect of the adjustment of the humidity",
     ylab = "Adjusted humidity ( %RH )",
     xlab = "Relative humidity ( %RH )",
     cex = .1)

ggplot(values,aes(x = humidity,y = humid_temp)) + 
  scale_colour_gradient2(low = "navyblue", 
                         mid = "orange", 
                         high = "navyblue", 
                         midpoint = 12) + 
  geom_point(aes(colour = hours),alpha = 4/10,size=.5) +
  ggtitle ("Humidity vs Temperature") +
  xlab("Humidity ( %RH )") +  ylab ("Temperature ( ºC )")





# Temperature/Height ------------------------------------------------------


bottom = merged_data$Height < 40
top =    merged_data$Height > 65
Position = rep(NA,length(merged_data$Height))
Position[bottom] = "Bottom"
Position[top] = "Top"
Position = as.factor(Position)
df_temp_height = data.frame(Position=Position,
                            Temperature=merged_data$humid_temp,
                            hamatop=merged_data$hamatop,
                            Portion_day = merged_data$Portion_day,
                            Portion_day_bis = merged_data$Portion_day_bis,
                            hours=merged_data$hours)
df_temp_height = na.omit(df_temp_height)

boxes = function(x) {
  r = quantile(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))
  names(r) = c("ymin", "lower", "middle", "upper", "ymax")
  r
}

dodge <- position_dodge(width = 0.4)
ggplot(data = df_temp_height, aes(x = as.factor(Portion_day_bis), y = Temperature, fill = Position)) +
  geom_violin(position = dodge)+
  geom_boxplot(width=.2, outlier.colour=NA, position = dodge) +
  scale_fill_manual(values=c("seagreen4","palegreen3"))+
  ggtitle ("Difference of temperature between the top and the bottom of the tree in a typical day") +
  xlab("Time Intervals ( h )") +  ylab ("Temperature ( °C )") 
  

# Combined plots ----------------------------------------------------------

interquartile <- function(x){
  out <- quantile(x, probs = c(0.25, 0.5, 0.75))
  names(out) <- c("ymin", "y", "ymax")
  out
}

p1 = ggplot(df_temp_height, aes(x = as.factor(Portion_day_bis), y = Temperature, fill=Position)) +
  geom_boxplot(width=.6, position = position_dodge(.6),show.legend=F) +
  xlab("Time Intervals ( h )") +  ylab ("Temperature ( °C )")+
  ggtitle ("Distribution of the temperature per 4 hours tranches")

p2 = ggplot(df_temp_height, aes(x=hours, y=Temperature, col = Position, group = Position))+
  stat_summary(fun.data = "interquartile", geom = "errorbar", width=0.5)+
  stat_summary(fun.y = 'median', geom='line', lwd=1.5)+
  xlab("Time ( h )") +
  ggtitle ("Distribution of the temperature per hour")+
  theme(axis.title.y = element_blank()) 

limits <- c(5, 35)
breaks <- seq(limits[1], limits[2], by=5)

# assign common axis to both plots
p1.common.y <- p1 + coord_cartesian(ylim=limits) 
p2.common.y <- p2 + coord_cartesian(ylim=limits) 

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))


# Orientation -------------------------------------------------------------

or_data = merged_data
head(or_data)
or_data$Direction = or_data$Direc
or_data = or_data[or_data$Direc != "NW",]tt
head(or_data)

ggplot(or_data,aes(x = Direction,y = log(hamabot+1),fill=Direction) )+ 
  geom_violin(scale = "width")+
  ggtitle ("Photo-synthetically Active Radiation per Hour Intervals") +
  xlab("Orientation") +  ylab ("Incident PAR")+
  facet_wrap(~Portion_day_bis, ncol = 3)+
  scale_fill_brewer(palette="Blues")
