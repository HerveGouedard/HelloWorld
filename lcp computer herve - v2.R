# install.packages('gdistance')
# install.packages('raster')
library(gdistance)
library(raster)

# Hervé needed to add this: 
# install.packages('rgdal')
library(rgdal)

# https://cran.r-project.org/web/packages/gdistance/gdistance.pdf

setwd('C:/Users/Hervé/Documents/ARPE/wind_polluting_tunnels/Data')
#setwd('C:/Dropbox/__Research/Air Pollutions - Winds')

## Create cost surface
## Importing the elevation raster as a cost raster
cost <- raster('Shapefiles/elevation_raster/srtm_37_04.tif')
print(cost)

## Crop size (it's too big || 6001, 6001, 36012001  (nrow, ncol, ncell) 
# Hervé doesn't understand this 2 lines: 
# -> dimensions  : 840, 480, 403200  (nrow, ncol, ncell))
#cost <- crop(cost, extent(min(coords[, 1]), max(coords[ ,1]), min(coords[, 2]), max(coords[, 2])))

# crop function allows to select the fraction of the raster we want (in lon-lat):
# We went from 36M cells to 3.6M cells
cost <- crop(cost, extent(1, 3.5, 41.2, 42.2))
print(cost)


## Histogram of the cost sizes (altitudes in this case)
hist(cost, c='orange', main='Cost histogram', xlab='Height')
## Change costs == 0 to be != 0. It might also work without this, just in case.
# Converting negative values to 0.0001
cost[cost <= 0] = 0.0001
# Converting na values to 0.0001 
cost[is.na(cost)] = 0.0001
# Same shape, except that we created a lot of 0 values (from NA values). 
hist(cost, c='orange', main='Cost histogram', xlab='Height')


# Load airquality stations
# with the following command, we read the csv and create a "dataframe",
# a handy object with indexes (or number of observation) in line, 
# and variables in columns: 
data <- read.csv('winds_6vars_1day.csv')
#data <- read.csv('Files - Wind Data/winds_6vars_1day.csv')
# Then we create a new object (a new data frame), with only 2 variables:
# variable x : corresponding to longitude of our initial dataframe
# variable y : corresponding to latitude of our initial dataframe
# coords for now is the list of lon-lat couples of all of our meteo stations. 
coords <- data.frame(x=data$lon, y=data$lat)


## COMMAND WHICH: selecting indexes respecting conditions
# Here we create "aux", the list of indexes of observations in coords that
# respect : 2.1 < x < 2.4 & 41.35 < y < 42.1.
# >> only 130 indexes
# coords [i, j] means: considering the j-th column of the i index. 
# here, we first put a condition on the 'x' column for every index,
# before putting a condition on the 'y' column for every index: 
aux <- which(coords[,'x'] > 2.1 & coords[,'x'] < 2.4 & coords[,'y'] > 41.35 & coords[,'y'] < 42.1)
# We "crop" our meto station, by only considering these 130 indexes. 
coords <- coords[aux,]


## First draft plot: elevation
plot(cost, useRaster=TRUE)
# We add Barcelona and Vic: 
points(2.1685, 41.3818, col='black')
points(2.25, 41.9333, col='black')

# We add a green circle for each of our 130 meteo stations:  
for (i in 1:length(aux)){
    points(coords[i, 1], coords[i, 2], col='green')
}

## Line not finished (I guess):
## add text(pts[,1]+2, pts[,2]+2, 1:nrow(pts))

## Create two points
# cbind: "Take a sequence of vector, matrix or data-frame arguments and 
# combine by columns or rows, respectively."
# We create mini vectors of lon and lat for Barcelona and Vic, that
# will generate a mini data frame of points. 
# UNSOLVED: why columns are "pts" and "y", and not "x" and "y"? 
pts <- cbind(x=c(2.1685, 2.25), y=c(41.3818, 41.9333))
# Plot red dots:
plot(SpatialPoints(pts), add=TRUE, pch=20, col="red")


## Possible directions = 8 (N, NE, E, SE, S, SW, W, NW)
# We create a "transition" raster which computes at each cell, 
# the mean of (1/cost) for the 9 cells considered 
# (initial cell + 8 adjacent cells, hence the name "transition"). 
trCost1 <- transition(1/cost, mean, directions=8)
# A geographic correction is necessary for "all objects of the class Transition 
# (...) made with directions > 4."
trCost1 <- geoCorrection(trCost, type="c")


### If we want to improve precision: 
## Possible directions = 16 (N, NE, E, SE, S, SW, W, NW) (Also including horse-chess like moves)
#trCost2 <- transition(1/cost, mean, direction=16)
#trCost2 <- geoCorrection(trCost2, type='c')


# Least Cost Path between Barcelona and Vic: 
shortestPath(trCost1, pts[1,], pts[2,])
plot(shortestPath(trCost1, pts[1,], pts[2,], output="SpatialLines"), add=TRUE)


# uNSOLVED: Hervé doesn't understand this:
# I thought we were creating 3 new variables in our coords data frame, 
# with 0 values, but in fact, no: 
# Maybe Feliu had computed these variables, and this is a way to reset these
# variables? 
coords$lcp_mean <- NULL
coords$cost_to_bcn <- NULL
coords$cost_to_vic <- NULL

# Compute the LCP between point_i and Vic: 
# NB: running this command takes time! 
# for i from 1 to 130: 
for (i in 1:dim(coords)[1]){
    tryCatch(
        {
            # Hervé: 
            # Try: when looping, sometimes we ask the software to do impossible
            # things (ex: dividing by 0).
            # The "Try" (or TryCatch apparently in R) allows to handle 
            # errors and exceptions by telling R what to do in each case. 
          
          
            # Feliu: 
            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully
            
            message("This is the 'try' part")
            # We compute the least cost path between point_i and Vic:
            # pts contains in an iterative way the lon-lat couples 
            # of point_i and Vic: 
            pts <- cbind(x=c(coords[i, 1], 2.25), y=c(coords[i, 2], 41.9333))
            plot(shortestPath(trCost1, pts[1,], pts[2,], output="SpatialLines"), add=TRUE)      
            
            # UNSOLVED: Hervé doesn't understand this: 
            # Gives a 0 value for columns 3, 4 and 5, but what are these
            # columns? 
            # the 3 created ones previously (lcp_mean, cost_to_bcn, 
            # cost_to_vic)?
            # maybe code is not finished
            coords[i, 3] <- NULL #shortestPath(trCost1, pts[1, ], pts[2, ])
            coords[i, 4] <- NULL #costDistance(trCost1, pts[1, ], pts[2, ])
            coords[i, 5] <- NULL #
            
            
            # UNSOLVED: Hervé doesn't understand this:
            ## !! Most important feature:
            # passage(x, origin, goal, theta, ...)
            # coords[i, 5] <- passage(trCost1, pts[1, ], pts[2, ])
            # Description
            # Calculate the overlap and non-overlap of paths departing from 
            # a common origin. Two algorithms are available: 
            # random walk and randomised shortest paths.
            # Usage
            # pathInc(x, origin, from, to, theta, weight, ...)
            #
            #
            #
            # Hervé : OK!
            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped insided a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
            message(paste("Desired point does not seem to exist:"))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            return(NA)
        },
        warning=function(cond) {
            message(paste("The point caused a warning:"))
            message("Here's the original warning message:")
            message(cond)
            # Choose a return value in case of warning
            return(NULL)
        },
        finally={
            # NOTE:
            # Here goes everything that should be executed at the end,
            # regardless of success or error.
            # If you want more than one expression to be executed, then you 
            # need to wrap them in curly brackets ({...}); otherwise you could
            # just have written 'finally=<expression>' 
            message(paste("Processed LCP, distance")) # Convert LCP into a number (mean?)
            message("DONE")
        }
    )    
}


# Defining a function that computes the angle between two points: 
compute_angle_between_points <- function(pts){
    # pts needs to be the following way: (pts only has two rows: Barcelona and Vic)
    # 
    #             x       y
    # [1,] 2.399567 41.9032
    # [2,] 2.250000 41.9333
    
    angle <- atan2(pts[1, 2], pts[1, 1]) - atan2(pts[2, 1], pts[2, 2])
    angle <- angle * 360 / (2*pi)
    if (angle < 0){
        angle <- angle + 360;
    }
    
    return(angle)
}
compute_angle_between_points(pts)

# Exploring to do a new function where we take Barcelona as an anchor:
# first, create additional obs in an existing dataframe:
pts <- rbind(41.85, 3.13) # Palamos
pts <- rbind(42.1, 1.84) # Berga
# QUESTION: Do we now have 4 obs in pts???

# Do a mini case in which I look at angle from BCN to both Palamos and Berga
compute_angle_between_points2 <- function(pepe1,pepe2){
  angle <- atan2(pts[1, 2], pts[1, 1]) - atan2(pepe1[], pepe2[])
  angle <- angle * 360 / (2*pi)
  # if (angle < 0){
  #  angle <- angle + 360;
  # }
  return(angle)
}
data$angle <- compute_angle_between_points2(data$lon,data$lat)

# Creating a variable that says "yes" if at this point, windbearing minus
# the angle between barcelona and this point is less than 30°.
# UNSOLVED: warning message: "the condition has length > 1"? 
data$points_to_bcn <- if(
    data$windbearing - compute_angle_between_points(
                                                cbind(pts[1, ], 
                                                      data$lat, data$lon)
                                                ) < 30){'yes'}


# Hervé not sure: 
# looks like new ways of computing a least cost path between Barcelona and Vic?
# Create a Transition object from the raster
# (previous one): trCost1 : the mean of 1/cost, with mean considering 9 cells
# tr : 1/mean(cost), with mean considering 5 cells 
tr <- transition(cost,function(x) 1/mean(x),4)

# Create the set of coordinates
# Hervé: better to store our geographical points this way than 
# we did before ("pts", the mini data frame)? 
sP1 <- SpatialPoints(cbind(c(2.1685, 2.225),c(41.3818, 41.9333)))
# Calculate the resistance distance between the points
# UNSOLVED: Hervé doesn't know about this:  
commuteDistance(tr,sP1)
costDistance(tr, sP1)



# Example of the plotwind command: 
install.packages("RadioSonde")
library(RadioSonde)
data("ExampleSonde")
plotwind(ExampleSonde)

## Create two points
pts <- cbind(x=c(2.1685, 2.225), y=c(41.3818, 41.9333))


## Display results
# NB: trCost2: equals trCost1, but with 16 directions instead of 8. 
plot(cost)
plot(SpatialPoints(pts), add=TRUE, pch=5, col="red")
# UNSOLVED: what does do this line: 
text(pts[,1]+2, pts[,2]+2, 1:nrow(pts))

# shortest path between BCn and Vic with a more precise cost raster: 
plot(shortestPath(trCost2, pts[1,], pts[2,], output="SpatialLines"), add=TRUE)
# shortest path between a specific point and Vic with a more precise cost raster: 
# UNSOLVED: does coords[1050] exist?!
plot(shortestPath(trCost2, coords[1050,], pts[2, ], output='SpatialLines'), add=TRUE)

#Plot points of grid
for (i in 1:1702){plot(shortestPath(trCost1, coords[aux,][i, ], as.matrix(pts[2,]), output='SpatialLines'), add=TRUE, pch=1, col='green')}
############################
heightDiff <- function(x){x[2] - x[1]}
hd <- transition(cost, heightDiff, 8, symm=FALSE)
slope <- geoCorrection(hd, scl=FALSE)
adj <- adjacent(r, cells=1:ncell(r), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- exp(-3.5 * abs(slope[adj] + 0.05))
x <- geoCorrection(speed, scl=FALSE)
A <- c(2.1685, 41.3818)
B <- c(2.25, 41.9333)
AtoB <- shortestPath(cost, A, B, output="SpatialLines")
BtoA <- shortestPath(cost, B, A, output="SpatialLines")
plot(r)
lines(AtoB, col="red", lwd=2)
lines(BtoA, col="blue")
text(A[1]-10,A[2]-10,"A")
text(B[1]+10,B[2]+10,"B")


