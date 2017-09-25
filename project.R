```{r}
#install.packages("data.table")
library(data.table)
#install.packages("ggplot2")
library(ggplot2)

install.packages("xts")
library(xts)
install.packages("chron")
library(chron)
install.packages("scales")
library(scales)
install.packages("devtools")
library(devtools)
install_github("Rforecastio", "hrbrmstr")
install.packages(Rforecastio)
library(plyr)
library(ggplot2)
library(reshape2)
install.packages(ggmap)
library(ggmap)
library(qplot)
library(zoo)

plotmap <- function(data) {
  data_2016_ORIG <- subset(completeDataTable, DATE > "2015-12-31")
  data_2016 <- subset(data_2016_ORIG, data_2016_ORIG$BOROUGH != "");
  data_2016 <- subset(data_2016, !is.na(data_2016$LATITUDE));
  
  # getting the map
  map <- get_map(location = c(lon = mean(data_2016$LONGITUDE),
                              lat = mean(data_2016$LATITUDE)),
                 zoom = "auto", 
                 scale = "auto",
                 maptype = "roadmap");
  # plotting the map with some points on it
  map_df <- data.frame(lon = data_2016$LONGITUDE, lat = data_2016$LATITUDE);
  ggmap(map) +
    geom_point(data = map_df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE);
  
  data_2016_manhattan <- subset(data_2016, data_2016$BOROUGH == "MANHATTAN");
  # getting the map
  map_manhattan <- get_map(location = c(lon = mean(data_2016_manhattan$LONGITUDE),
                                        lat = mean(data_2016_manhattan$LATITUDE)),
                           zoom = 13, 
                           scale = "auto",
                           maptype = "roadmap");
  # plotting the map with some points on it
  ggmap(map_manhattan) +
    geom_point(data = data_2016_manhattan, aes(x = data_2016_manhattan$LONGITUDE, y = data_2016_manhattan$LATITUDE, fill = "red", alpha = 0.8), size = 5, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE);
  
  
}


getFilteredDataAsDataTable <- function(data, year, borough) {
  filteredData <- data;
  
  # Filter based on input params
  if (!is.null(year)) {
    filteredData <- subset(filteredData, format.Date(DATE, "%Y") == year);
  }
  
  if (!is.null(borough) && borough != "") {
    filteredData <- subset(filteredData, BOROUGH == borough);
  }
  
  # Create a data table
  filteredData <- as.data.table(filteredData)
  
  # Check data formats, dates are strings 
  str(filteredData)
  
  filteredData$DATE <- as.Date(filteredData$DATE, "%m/%d/%Y")
  filteredData$DATE.TIME <- as.POSIXct(paste(filteredData$DATE, filteredData$TIME), 
                                       format="%Y-%m-%d %H:%M", tz="EST")
  filteredData$CUSTOM.WEIGHT <- 1
  
  return(filteredData);
}

hourWiseCollision <- function(inputData) {
  final_data <- inputData
  
  # use data table to aggregate on hours 
  # First lets add a field plot time
  final_data[, PlotTime := format(DATE.TIME, "%H:%M:%S")]
  
  # key by this plot time
  setkeyv(final_data, "PlotTime")
  
  # aggregate the data for each minute
  plotdata <- final_data[, .(Count.monthly = sum(as.integer(CUSTOM.WEIGHT))), by = PlotTime]
  
  # aggregate the data for each hour
  plotdata$PlotTime <- as.POSIXct(plotdata$PlotTime, format="%H:%M:%S", tz="EST")
  plotdata.xts <- xts(plotdata$Count.monthly, plotdata$PlotTime)
  ep <- endpoints(plotdata.xts, 'hours', 1)
  plot.xts <- period.apply(plotdata.xts,ep,sum)
  plot.ggplot.df <- data.frame(time=index(plot.xts), count=coredata(plot.xts))
  plot.ggplot <- as.data.table(plot.ggplot.df)
  plot.ggplot[, dtTime := format(time, "%H:%M")]
  
  xlab <- paste("Hour of day (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")
  
  return(ggplot(plot.ggplot, aes(dtTime,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           scale_x_discrete(labels=c("00:59" = "00:00 - 01:00", 
                                     "01:59" = "01:00 - 02:00", 
                                     "02:59" = "02:00 - 03:00",
                                     "03:58" = "03:00 - 04:00",
                                     "03:59" = "03:00 - 04:00",
                                     "04:59" = "04:00 - 05:00",
                                     "05:58" = "05:00 - 06:00",
                                     "05:59" = "05:00 - 06:00", 
                                     "06:59" = "06:00 - 07:00", 
                                     "07:59" = "07:00 - 08:00", 
                                     "08:59" = "08:00 - 09:00", 
                                     "09:59" = "09:00 - 10:00", 
                                     "10:59" = "10:00 - 11:00", 
                                     "11:59" = "11:00 - 12:00", 
                                     "12:59" = "12:00 - 13:00", 
                                     "13:59" = "13:00 - 14:00", 
                                     "14:59" = "14:00 - 15:00",
                                     "15:59" = "15:00 - 16:00", 
                                     "16:59" = "16:00 - 17:00", 
                                     "17:59" = "17:00 - 18:00", 
                                     "18:59" = "18:00 - 19:00",
                                     "19:59" = "19:00 - 20:00", 
                                     "20:59" = "20:00 - 21:00", 
                                     "21:59" = "21:00 - 22:00", 
                                     "22:59" = "22:00 - 23:00",
                                     "23:59" = "23:00 - 24:00")) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

monthWiseCollision <- function(inputData) {
  final_data <- inputData
  # use data table to aggregate on hours 
  # First lets add a field plot time
  final_data[, PlotTime := format(DATE.TIME, "%m")]
  
  # key by this plot time
  setkeyv(final_data, "PlotTime")
  
  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = PlotTime]
  
  xlab <- paste("Month of day (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")
  
  return(ggplot(plotdata, aes(PlotTime,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           scale_x_discrete(labels=c("01" = "January", 
                                     "02" = "February", 
                                     "03" = "March",
                                     "04" = "April",
                                     "05" = "May",
                                     "06" = "June",
                                     "07" = "July",
                                     "08" = "August",
                                     "09" = "September",
                                     "10" = "October",
                                     "11" = "November",
                                     "12" = "December")) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

boroughWiseCollision <- function(inputData) {
  final_data <- inputData
  # use data table to aggregate on borough 
  setkeyv(final_data, "BOROUGH")
  
  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = BOROUGH]
  
  # Remove missing borough data
  plotdata <- plotdata[which(plotdata$BOROUGH != ""),]
  
  xlab <- paste("Borough (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")
  
  return(ggplot(plotdata, aes(BOROUGH,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

injuredKilledCount <- function(inputData) {
  final_data <- inputData
  pedestriansInjured <- sum(final_data$NUMBER.OF.PEDESTRIANS.INJURED, na.rm = TRUE)
  pedestriansKilled <- sum(final_data$NUMBER.OF.PEDESTRIANS.KILLED, na.rm = TRUE)
  
  cyclistInjured <- sum(final_data$NUMBER.OF.CYCLIST.INJURED, na.rm = TRUE)
  cyclistKilled <- sum(final_data$NUMBER.OF.CYCLIST.KILLED, na.rm = TRUE)
  
  motoristInjured <- sum(final_data$NUMBER.OF.MOTORIST.INJURED, na.rm = TRUE)
  motoristKilled <- sum(final_data$NUMBER.OF.MOTORIST.KILLED, na.rm = TRUE)
  
  injuredKilledData <- data.frame(reason = character(), count = numeric(), stringsAsFactors = FALSE)
  injuredKilledData[1, ] <-  c("Pedestrians Injured", pedestriansInjured)
  injuredKilledData[2, ] <-  c("Cyclist Injured", cyclistInjured)
  injuredKilledData[3, ] <-  c("Motorist Injured", motoristInjured)
  
  injuredKilledData[4, ] <-  c("Pedestrians Killed", pedestriansKilled)
  injuredKilledData[5, ] <-  c("Cyclist Killed", cyclistKilled)
  injuredKilledData[6, ] <-  c("Motorist Killed", motoristKilled)
  
  xlab <- paste("Category (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")
  return(ggplot(injuredKilledData, aes(reason,count,fill=count,width=1)) +
           xlab(xlab) + ylab("Count") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
  
}

file = "H:\Documents\Pace - Data warehousing,mining\plots"
data <- read.csv(file)
data$DATE <- as.Date(data$DATE, "%m/%d/%Y")

plot.dir <- "H:\Documents\Pace - Data warehousing,mining\plots";
allBorough <- unique(data[which(data$BOROUGH != ""), "BOROUGH"]);

completeDataTable <- getFilteredDataAsDataTable(data, NULL, NULL);
plot.dir.agg <- "H:\Documents\Pace - Data warehousing,mining\plots";


# Plot the month wise collision graph for the complete dataset
file.name <- "plot_monthWiseCollision_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(monthWiseCollision(completeDataTable))
dev.off()

# Plot the borough wise collision graph for complete dataset
file.name <- "plot_boroughWiseCollision_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(boroughWiseCollision(completeDataTable))
dev.off()

# Plot the no. of injured & killed graph for the complete dataset
file.name <- "plot_injuredKilledCount_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(injuredKilledCount(completeDataTable))
dev.off()
```
