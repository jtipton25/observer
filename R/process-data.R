# ## Write code info here
# #  
# 
library(raster)
library(plyr)
library(ggplot2)
library(mgcv)
library(automap)
library(scales)
# 
# # We just use this raster to crop the USHCN records.  I could do it easily with
# # an extent.
# extent.map <- raster(ncols=240, nrows=132, xmn=-100, xmx=-80, ymn=40, ymx=51)
# 
# # 
# #  Get the USHCN data:
# #  This ought to be a fixed width table:
# ushcn.metadata <- read.fwf('./data/ushcn-stations.txt', 
#                            widths = c(6, 9, 10, 7, 3, 31, 7, 7, 7, 3),
#                            stringsAsFactors = FALSE,
#                            comment.char = '')
# 
# ushcn.climate <- read.fwf('./data/9641C_201112_F52.avg',
#                           widths = c(11, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
#                           stringsAsFactors = FALSE)
# 
# #  Let's first get just the july temperatures:
# ushcn.climate <- ushcn.climate[,c(1, 8)]
# 
# #  Currently, the climate data is clipped, so we're only really interested in 
# #  USHCN stations within the clipped domain.  The first six characters of the
# #  first column let us know where the point was:
# 
# #  Ignore the warnings about NAs
# in.domain <- findInterval(ushcn.metadata[,2], bbox(extent.map)[2,]) == 1 &
#              findInterval(ushcn.metadata[,3], bbox(extent.map)[1,]) == 1
# 
# ushcn.stn <- substr(ushcn.climate[,1], 1,6) %in% ushcn.metadata[in.domain,1]
# ushcn.lookup <- substr(ushcn.climate[ushcn.stn,1], 1,6)
# 
# ushcn.table <- data.frame(year = as.numeric(substr(ushcn.climate[ushcn.stn,1], 8, 11)),
#                           lat  = ushcn.metadata[match(ushcn.lookup, ushcn.metadata[,1]),2],
#                           long = ushcn.metadata[match(ushcn.lookup, ushcn.metadata[,1]),3],
#                           temp = as.numeric(ushcn.climate[ushcn.stn,2]) / 10,
#                           data = 'USHCN',
#                           stn  = as.numeric(substr(ushcn.climate[ushcn.stn,1], 1, 7)),
#                           class = NA)
# 
# ################################################################################
# #  Forts data:
# #  The forts data is going to be tricky.  Some of it is daily min or max, some
# #  is hourly, some is other stuff.  To resolve this problem we build a model that
# #  accounts for the diurnal cycle.  This is processed in a `bam` below:
# 
# fort.files <- list.files('./data/Fort Data/', full.names = TRUE)
# 
# fort.clim <- fort.files[regexpr('.data.', fort.files, fixed = TRUE) > 0]
# fort.meta <- fort.files[regexpr('.metadata.', fort.files, fixed = TRUE) > 0]
# 
# trim.data <- function(x){
#   data <- read.csv(x, header = TRUE, stringsAsFactors = FALSE)
#   
#   temp.cols <- (1:ncol(data))[regexpr('T', colnames(data)) == 1]
#   
#   out <- data.frame(year  = rep(data$YEAR, length(temp.cols)),
#                     day   = rep(data$DAY, length(temp.cols)),
#                     month = rep(data$MO, length(temp.cols)),
#                     lat   = rep(data$LAT, length(temp.cols)),
#                     long  = rep(data$LON, length(temp.cols)),
#                     temp  = as.numeric(as.character(unlist(data[,temp.cols]))),
#                     data  = 'Forts',
#                     stn   = rep(data$STNO, length(temp.cols)),
#                     name  = substr(x, regexpr('//', x) + 2, nchar(x) - 9),
#                     class = rep(colnames(data)[temp.cols], each = nrow(data)))
#   
#   out <- out[!is.na(out$temp),]
#   out
# }
# 
# ## we ignore the warnings about NAs
# fort.table <- ldply(fort.clim, trim.data, .progress = 'text')
# fort.table$class <- as.character(fort.table$class)
# fort.table$class <- substr(fort.table$class, 1, 7)
# 
# fort.table$hour <- as.numeric(substr(fort.table$class, 6, 7))
# fort.table$measure <- substr(fort.table$class, 2, 4)
# 
# #  Clear all forts with bad lat/long (n=558)
# fort.table <- fort.table[rowSums(fort.table == -999, na.rm = TRUE) == 0,]
# 
# #  For some reason some of the records in the Champaign Urbana station are in degrees C:
# #  I actually don't think it is.  The commented line was orignially used, 
# # but it's unclear which values are in oC and which are in F.  The record is a mess.
# # We'll drop it.
# fort.table <- fort.table[!fort.table$stn == 111399, ]
# 
# #  The records indicate special time codes:
# #  Hour 91 - Sunrise (in July between 5:20 - 5:40, but shifts to 4:30 by 1820 because of the institution of DST)
# #  Hour 92 - Sunset  (7:50pm in 1880)
# #  Hour 93 - Morning (think about assignment, use 9am)
# #  Hour 94 - Afternoon (as above, use 3pm)
# #  Hour 95 - Evening (as above, use 7pm)
# #  Hour 99 - Unknown
# 
# fort.table$hour[fort.table$hour == 91] <- 4.5
# fort.table$hour[fort.table$hour == 92] <- 19.6
# fort.table$hour[fort.table$hour == 93] <- 8
# fort.table$hour[fort.table$hour == 94] <- 15
# fort.table$hour[fort.table$hour == 95] <- 18
# 
# # This is based on assessment of hourly records from the region:
# fort.table$hour[regexpr('MAX', fort.table$class) > 0] <- 15
# fort.table$hour[regexpr('MIN', fort.table$class) > 0] <- 6
# 
# # Assign a compound, decimal julian period:
# fort.table$ymdh <- fort.table$year + (fort.table$month + (fort.table$day + fort.table$hour/24)/31)/12
# 
# stn_plot <- function(x, thing='temp', path=''){
#   stn_rec <- subset(fort.table, stn == x)
#   
#   ggsave(plot = ggplot(stn_rec, aes(x = ymdh, y = eval(thing))) + geom_line() + ggtitle(x), 
#           filename = paste0('./StnFigs/',path, x, '_',thing, '.png'))
# }
# 
# #  Now get rid of all the 99s, and all MIN/MAX values, they're weird..
# fort.table <- fort.table[fort.table$hour < 25, ]
# 
# #  this still leaves us with 94471 unique entries.
# fort.table$stn <- factor(fort.table$stn)
# 
# interval <- read.csv('./data/fort_interval.csv')[,-1]
# colnames(interval)[9] <- 'name'
# 
# # There are 748 Stations, with 20 stations that overlap between the two records.
# interval <- subset(interval, !(lat == 0 | long == 0))
# 
# fort.table <- rbind(fort.table, interval)
# 
# fort.table$stn <- as.character(fort.table$stn)
# 
# #######################
# #  Now build a model of hourly temperatures using modern data:
# #  
# 

###############################################################################
############################  Load updated data  ##############################
###  csv generated from a script similar to above but from Simon's computer  ##
##  Need to get the raw data exactly??  #######################################
##  Is the csv sufficient for reproducability?  ##############################

july.table <- read.csv("../data/fort_july_full.csv")
july.table <- july.table[which(july.table$year < 1895), ]

# july.table <- subset(fort.table, month == 7)
# july.table$stn <- factor(july.table$stn)

# This is just so big!  450205 individual records:
# This runs, it takes a while, and the "summary" seems to take forever as well.
#  There is an apparent effect of latitude in the daily model (we can see it in the
#  julyplot figure), so we want to account for that.
model <- bam(temp ~ s(hour, bs = 'cc', k = 5) + s(stn, bs = 're') + lat + year, 
                method = "REML",
                data = july.table)

## Add in a station by year random effect
model_yr <- bam(temp ~ s(hour, bs = 'cc', k = 5) + s(stn, year, bs = 're') + lat, 
             method = "REML",
             data = july.table)

# Note, that with the model as:
# model <- bam(temp ~ s(hour, bs='cc', k=4) + s(stn, bs='re'), 
#              data = july.table)
#  We get a range of daily temperatures for samples that are taken in the same day.
#  Basically, it doesn't seem to detrend things.  This may be, in part, because the timing
#  of measurement varies by latitude.  More a function of a change in when samples were taken
#  during the "filling in" of temperature measurements.
#
#  With this data cleaning we can explain 69.6% of the total variance.  It gives
#  us a mean diurnal cycle across each station.  It would be interesting to see if
#  there is any sort of spatial pattern, but for now, I'm going to sort of 
#  ignore those questions.

# july.table$cleaned <- 
#   predict(model, newdata = data.frame(stn  = july.table$stn,
#                                       hour = 8.5,
#                                       year = july.table$year,
#                                       lat  = july.table$lat), 
#           type = 'response')
# 
# july.table$year_re <- 
#   predict(model_yr, newdata = data.frame(stn  = july.table$stn,
#                                          hour = 8.5,
#                                          year = july.table$year,
#                                          lat  = july.table$lat), 
#           type = 'response') + resid(model_yr, type = 'response')


hour_pred=seq(0, 24, length = 24*60+1)[-(24*60+1)]
pred_out <- matrix(0, dim(july.table)[1], 24*60)
## parallelize this part
for (i in 1:(24*60)) {
  if(i %% 50 == 0) {
  message("iteration ", i, " of ", 24*60)
  }
  pred_out[, i] <-  predict(model_yr, newdata = data.frame(stn  = july.table$stn,
                                                            hour = hour_pred[i],
                                                            year = july.table$year,
                                                            lat  = july.table$lat), 
                             type = 'response') + resid(model_yr, type = 'response')
}

min_temp <- rep(0, dim(july.table)[1])
max_temp <- rep(0, dim(july.table)[1])
min_temp_idx <- rep(0, dim(july.table)[1])
max_temp_idx <- rep(0, dim(july.table)[1])

for (i in 1:dim(july.table)[1]) {
  if(i %% 1000 == 0) {
    message("iteration ", i, " of ", dim(july.table)[1])
  }
  min_temp[i] <- min(pred_out[i, ])
  max_temp[i] <- max(pred_out[i, ])
  min_temp_idx[i] <- which(pred_out[i, ] == min(pred_out[i, ]))
  max_temp_idx[i] <- which(pred_out[i, ] == max(pred_out[i, ]))
}
temp_avg <- (min_temp + max_temp)/2
temp_mn <- apply(pred_out, 1, mean)

## compare temp_avg = (T_min+T_max)/2 to true average T = temp_mn in degrees F
mean(temp_avg) - mean(temp_mn)

head(hour_pred[min_temp_idx])
## minimum temperature occurs at 3.65
head(hour_pred[max_temp_idx])
## maximum temp occurs at 14.26667
hist(temp_avg)
hist(july.table$year_re)
mean(temp_avg)
mean(july.table$year_re)
delta_t <- mean(temp_avg) - (mean(july.table$year_re)*9/5+32)
## overall meanshift of 3.841034

july.table$predicted <- temp_avg

# ## Functions to explore model predictions
# 
# stn_plot <- function(x){
#   
#   # A ggplot for each station, faceted by year.
#   
#   stn_rec <- subset(july.table, stn == x)
#   
#   ggsave(plot = ggplot(stn_rec, aes(x = day, y = cleaned)) + 
#            geom_line() + 
#            geom_line(aes(x = day, y = temp), color = 'blue', alpha = 0.5) +
#            geom_line(aes(x = day + 0.25, y = predicted), color = 'red', alpha = 0.5) +
#            ggtitle(x) + 
#            facet_wrap(~year) + 
#            theme_bw(), 
#          filename = paste0('./StnFigs/july/', x, '.png'),
#          dpi = 96, width = 6, height = 6)
# }
# 
# year_plot <- function(x){
#   
#   # A plot for each year, faceted by station:
#   
#   stn_rec <- subset(july.table, year == x)
#   
#   ggsave(plot = ggplot(stn_rec, aes(x = day, y = cleaned)) + 
#            geom_line() + 
#            geom_line(aes(x = day, y = temp), color = 'blue', alpha = 0.5) +
#            geom_line(aes(x = day + 0.25, y = predicted), color = 'red', alpha = 0.5) +
#            ggtitle(x) + 
#            facet_wrap(~stn) + 
#            theme_bw(), 
#          filename = paste0('./StnFigs/july/years/', x, '.png'))
# }



#  Convert to Celcius:
july.table$predicted <- (july.table$predicted - 32) / 9 * 5


################################################################################
################################################################################
################################################################################

## Uncomment to save data
# write.csv(july.table, '~/spatialModeling/data/fort_july_full.csv')

################################################################################
##############################  Load PRISM Data  ###############################
################################################################################

library(raster)
library(rgdal)
library(fields)

## Load PRISM data for raster skeleton
setwd("../data/Temp")
## The Prism is the grid data. It is a 132 x 240 grid with each gridcell consisting of approximately 8km x 8km (8.3 x 8.3)
#  Get the PRISM data:
#  For now we'll just use July temperatures:
all.temp <- list.files(full.names=TRUE)
july.temp <- all.temp[regexpr(".07.tif", all.temp, fixed=TRUE)>0]
prism <- stack(july.temp)
for (i in 1:dim(prism)[3]) {
  tmp_file_name <- prism[[i]]@file@name
  ## subset string of file name to relative path
  prism[[i]]@file@name <- paste("~", substr(tmp_file_name, 12, 59), sep="")
}
setwd("./../../../")

################################################################################
####################  Extract Coordinates from PRISM Data  #####################
################################################################################

ncells <- ncell(prism[[1]])
na.rows <- which(is.na(values(prism[[1]])) == TRUE)
prism.points <- xyFromCell(prism[[1]], 1:ncells)[ - na.rows, ]

## calcluate pairwise distance from all points on the grid
D <- as.matrix(dist(prism.points))

year.1 <- min(july.table$year)
july.table$year <- july.table$year - (year.1 - 1)
## Only analyze years before PRISM
july.table <- july.table[which(july.table$year <= 75), ]
## create indicator variable to rearrange the data structure
july.table$sort <- factor(with(july.table, interaction(stn, year), drop=TRUE))

select_first <- function(x) {
  ## helper function to rearrange data
  return(x[1])
}

################################################################################
############################  Rearrange the Data  ##############################
################################################################################

fort.year <- data.frame(year=c(by(july.table$year, july.table$sort, select_first)),
                        long=-abs(c(by(july.table$long, july.table$sort, select_first))),
                        lat=c(by(july.table$lat, july.table$sort, select_first)),
                        temp=c(by(july.table$predicted, july.table$sort, mean)))


################################################################################
######  Find records that occur in the same gridcell and take an average  ######
################################################################################

year <- list(length = 75)
H.list.dup <- list(length = 75)
test.dist <- list(length = 75)

for(i in 1:75){
  year[[i]] <- fort.year[which(fort.year$year == i), -1]
  test.dist[[i]] <- pointDistance(prism.points, year[[i]][, 1:2], longlat = TRUE)
  H.list.dup[[i]] <- vector(length = dim(year[[i]][, 1:2])[1])
  for(j in 1:dim(year[[i]][, 1:2])[1]){
    if(is.null(dim(test.dist[[i]])) == TRUE){
      H.list.dup[[i]][j] <- which(test.dist[[i]] == min(test.dist[[i]]))
    } else {
      H.list.dup[[i]][j] <- which(test.dist[[i]][, j] == min(test.dist[[i]][, j]))
    }
  }
  if (i %% 10 == 0){
    message("Processing record for duplicate locations: ", i, " of 75")
  }
}

################################################################################
#######  Set up dummy raster and average over PRISM gridcells that have  #######
#######  two or more fort observations at that location  #######################
################################################################################

## create empty raster
fort.raster <- list()
for(i in 1:75){
  fort.raster[[i]] <- prism[[1]]
  values(fort.raster[[i]]) <- rep(NA, ncells)
}

make.sum <- function(i, j){
  ## helper function to average over duplicates
  sum(year[[i]]$temp[which(H.list.dup[[i]] == j)]) / length(which(H.list.dup[[i]] == j))
}

avg <- list()
for(i in 1:75){
  avg[[i]] <- c()
  for(j in unique(H.list.dup[[i]])){
    avg[[i]] <- sapply(unique(H.list.dup[[i]]), make.sum, i = i)
  }
}  

for(i in 1:75){
  values(fort.raster[[i]])[ - na.rows][unique(H.list.dup[[i]])] <- avg[[i]]
}

##
## change PRISM data to celsius
##

X.temp <- values(prism)[ - na.rows, ]

X.celsius <- X.temp / 100 # From 100 * Celsius
X <- X.celsius


## Construct data objects for model
H.list <- vector('list', length = 75)
for(i in 1:75){
  H.list[[i]] <- unique(H.list.dup[[i]])
}

Y.list <- vector('list', length = 75)
for(i in 1: 75){
  Y.list[[i]] <- values(fort.raster[[i]])[ - na.rows][H.list[[i]]]
}

locs <- prism.points

##
## Plot a sample of years to verify code
##

# subset <- c(12, 28, 55, 75)
# layout(matrix(1:4, nrow = 2))
# for(i in subset){
#   palette(topo.colors(64))
#   image((fort.raster[[i]] - 32) * 5 / 9,
#         main = paste("Fort data for year ", i + 1819), col = topo.colors(64),
#         zlim = c(12, 32), xlab = 'latitude', ylab = 'longitude')
#   points(prism.points[H.list[[i]], ], pch = 16, 
#          col = (values(fort.raster[[i]])[ - na.rows][H.list[[i]]]), cex = 1.5)
#   image.plot((fort.raster[[i]] - 32) * 5 / 9, col = topo.colors(64),
#              legend.only = TRUE, zlim = c(12, 32), add = TRUE)
#   map(database = 'state', add = TRUE, col = 'black')
# }


################################################################################
##############################  Save data file  ################################
################################################################################

save(Y.list, H.list, fort.raster, X.celsius, prism, ncells, na.rows, 
     prism.points, locs, file='./data/observer.RData')
# save.image('~/fortTemp/data/fortTempData.RData')