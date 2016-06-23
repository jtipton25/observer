## File to load observer data
##
## Note: sites that are co-located are averaged as the model
## is not capable of dealing with multiple observations per site
##
## created 06-06-2016
## last modified 06-16-2016
##

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

##
## extract coordinates from PRISM
##

ncells <- ncell(prism[[1]])
na.rows <- which(is.na(values(prism[[1]])) == TRUE)
prism.points <- xyFromCell(prism[[1]], 1:ncells)[ - na.rows, ]

D <- as.matrix(dist(prism.points))

##
## read in updated fort data
##
data <- read.csv(file="~/observer/data/fort_july_full-06-06-16.csv")
year.1 <- min(data$year)
data$year <- data$year - (year.1 - 1)
## Only analyze years before PRISM
data <- data[which(data$year <= 75), ]
data$sort <- factor(with(data, interaction(stn, year), drop=TRUE))

select_first <- function(x) {
  return(x[1])
}


monthly_year <- c(by(data$year, data$sort, select_first))
monthly_long <- -abs(c(by(data$long, data$sort, select_first)))
monthly_lat <- c(by(data$lat, data$sort, select_first))
monthly_temp <- c(by(data$year_re, data$sort, mean))

fort.year <- data.frame(year=monthly_year, long=monthly_long, lat=monthly_lat,
                        temp=monthly_temp)

year <- list(length = 75)
H.list.dup <- list(length = 75)
test.dist <- list(length = 75)


## Find duplicates
for(i in 1:75){
  year[[i]] <- fort.year[which(fort.year$year == i), -1]
  test.dist[[i]] <- pointDistance(prism.points, year[[i]][, 1:2], longlat = TRUE)
  H.list.dup[[i]] <- vector(length = dim(year[[i]][, 1:2])[1])
  for(j in 1:dim(year[[i]][, 1:2])[1]){
    if(is.null(dim(test.dist[[i]])) == TRUE){
      H.list.dup[[i]][j] <- which(test.dist[[i]] == min(test.dist[[i]]))
    } else {
      H.list.dup[[i]][j] <- which(test.dist[[i]][, j] == min(test.dist[[i]][, j]))
      # H.list.dup[[i]][j] <-  - (j - 1) * dim(test.dist[[i]])[1] + which(test.dist[[i]] == min(test.dist[[i]][, j]))
    }
  }
  cat(" ", i)
}

##
## Set up dummy raster and average over PRISM gridcells that have two or more fort observations at that location
##

fort.raster <- list()
for(i in 1:75){
  fort.raster[[i]] <- prism[[1]]
  values(fort.raster[[i]]) <- rep(NA, ncells)
}

make.sum <- function(i, j){
  #   sum(year[[i]]$temp[which(loc.id[[i]] == j)]) / length(which(loc.id[[i]] == j))
  sum(year[[i]]$temp[which(H.list.dup[[i]] == j)]) / length(which(H.list.dup[[i]] == j))
}

avg <- list()
for(i in 1:75){
  avg[[i]] <- c()
  #   for(j in unique(loc.id[[i]])){
  for(j in unique(H.list.dup[[i]])){
    #     avg[[i]] <- sapply(unique(loc.id[[i]]), make.sum, i = i)
    avg[[i]] <- sapply(unique(H.list.dup[[i]]), make.sum, i = i)
  }
}  

for(i in 1:75){
  #     values(fort.raster[[i]])[ - na.rows][unique(loc.id[[i]])] <- avg[[i]]
  values(fort.raster[[i]])[ - na.rows][unique(H.list.dup[[i]])] <- avg[[i]]
}

##
## Image data to check on data quality  
##


X.temp <- values(prism)[ - na.rows, ]

X.celsius <- X.temp / 100 # From 100 * Celsius
X <- X.celsius


H.list <- vector('list', length = 75)
for(i in 1:75){
  H.list[[i]] <- unique(H.list.dup[[i]])
}

Y.list <- vector('list', length = 75)
for(i in 1: 75){
  Y.list[[i]] <- values(fort.raster[[i]])[ - na.rows][H.list[[i]]]
}

locs <- prism.points

# save.image(file = '~/fortTemp/data/fortTempData.RData')

##
## Plot a representative sample of years
##

subset <- c(12, 28, 55, 71)
# jpeg(file = '~/fortTemp/plots/fort_subset_data.jpeg', width = 6, height = 6, quality = 100, res  = 600, units = 'in')
layout(matrix(1:4, nrow = 2))
for(i in subset){
  palette(topo.colors(64))
  image((fort.raster[[i]] - 32) * 5 / 9, main = paste("Fort data for year ", i + 1819), col = topo.colors(64), zlim = c(12, 32), xlab = 'latitude', ylab = 'longitude')
  #   points(prism.points[loc.id[[i]], ], pch = 16, col = (values(fort.raster[[i]])[ - na.rows][loc.id[[i]]] - 32) * 5 / 9, cex = 2)
  points(prism.points[H.list[[i]], ], pch = 16, col = (values(fort.raster[[i]])[ - na.rows][H.list[[i]]]), cex = 1.5)
  image.plot((fort.raster[[i]] - 32) * 5 / 9, col = topo.colors(64), legend.only = TRUE, zlim = c(12, 32), add = TRUE)
  map(database = 'state', add = TRUE, col = 'black')
}
# dev.off()    


#library(animation)
#saveGIF(for(i in 1:75){
#  image((fort.raster[[i]] - 32) * 5 / 9, main = paste("Fort data for year ", i + 1819), col = topo.colors(64), zlim = c(12, 32))
#  points(prism.points[loc.id[[i]], ])
#  image.plot((fort.raster[[i]] - 32) * 5 / 9, col = topo.colors(64), legend.only = TRUE, zlim = c(12, 32), add = TRUE)
#}, movie.name = "fort.data.gif")


#
save(Y.list, H.list, fort.raster, file='~/../data/fortTempDataUpdated.RData')
# save.image('~/fortTemp/data/fortTempData.RData')