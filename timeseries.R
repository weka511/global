#We are turning you loose on huge datasets of climate records and model results.
#We want you examine this data, think about its implications, and draw your own
#conclusions. We expect that most of you will use data in the Time Series
#Browser at http://climatemodels.uchicago.edu/timeseries/. This system gives you
#access to the global meteorological monthly mean temperature data from over
#7000 stations around the world-plus lengths of over 450 glaciers and access to
#surface temperature results from the new AR5 climate model runs. Alternatively
#you could explore climate model results other than temperature using the AR5
#Mapper at http://climatemodels.uchicago.edu/maps/.
#
#Project PossibilitiesThere are a lot of ways you can approach datasets of this
#size, but here are some possible projects you could attempt:
#
#Select a set of meteorological stations that represent all climate zones and
#average them to estimate the global mean temperature evolution of the past
#decades, and compare it with the global mean estimate from the IPCC. Evaluate
#how different climate models driven by the historical and natural-only climate
#forcings do in their hindcasts. Choose a favorite glacier and examine the
#temperature changes in the area around it. Compare the temperature change the
#area has seen so far with forecasts for the future under the optimistic and
#pessimistic (RCP2.6 and RCP8.5) scenarios. Choose a subset of meteorological
#stations, grouped by vegetation type, region, altitude, latitude, or locality,
#and estimate the temperature change, relative to the global mean change. Do the
#different climate models agree on their hindcasts for this type of stations? 
#Author Michael Crichton showed several temperature / time plots which indicated
#cooling in his fiction book State of Fear. You could analyze how likely it is
#he got those results by chance, or whether (or how intensely) he was "cherry
#picking." You might pursue the question of whether one type of stations differs
#systematically from another, or in the proportions of warming and cooling
#trends, or in the way that the temperature trends depend on the time range over
#which you do the regression. Compare the predicted impacts of climate change on
#patterns of rainfall, soil moisture, water vapor, atmospheric temperature,
#cloudiness, runoff, or vegetation (leaf area index) between different climate
#models from around the world. Explore the behavior of one of the other online
#models in more detail than we do in one of the directed labs. Come up with
#details on the band saturation effects of different gases, on the effects of
#clouds on visible and IR light, on the geological history of the carbon cycle. 
#Or think of something else.
#
#Assignment Preparationless
#
#Assignment Preparation
#
#As you select stations from the database, the ID's of the stations you select
#are saved in the hash tag of the URL at the top of the browser (a series of
#random-looking characters after the "#" at the end of the web address in the
#browser). After you've compiled an interesting list of meteorological stations
#that you want to work with, you will submit the list by submitting the entire
#URL, including the hash tag (copy the whole thing from your web browser), as
#part of your response. This will allow your peers to see what data you were
#working with.
#
#Your assignment is to write a 300-500 word description of your project idea and
#the results you got. You can also upload up to two image files derived from
#screen shots as "Figures" to refer to in the text. Be quantitative in the
#reporting of your results - use numbers, not just qualitative "warmer, colder"
#descriptions.
#
#Think About the Data Quality
#
#As part of your analysis, evaluate the quality of the station data. Is the time
#series long enough to get a meaningful trend? In order to combine them, they
#have to be normalized to the temperature between 1900-1950. Sometimes if there
#are only a few years in that range the normalization becomes unreliable, or if
#there are no data for that time range the station can't be normalized. You will
#probably also be mostly interested in stations that have data up to
#near-present day (rather than ending in 1960 for example).
#
#Your project description should address the following questions:
#
#What are your search criteria for your stations? Why did you choose these
#criteria? Do the stations as a group have any wider significance? (They don't
#have to, you are allowed to investigate the climate signal in a particular
#region that may not interest everybody else, but if you do have a topic that
#has a wider significance, like "there is permafrost here", by all means put it
#in your report.
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

setwd('D:\\root\\sandbox\\coursera\\global-warming-model\\')

rm(list=ls())

#temperatures<-read.csv(file='pcCqF1cL')

direct.surface <- function(d=3){
  x<-rnorm(d,1/sqrt(d))
  Sigma<-sum(x*x)
  return (x/sqrt(Sigma))
}

xyz.coordinate <- function(latitude,longitude,R = 1) {
  return (R * c(
    cos(NISTdegTOradian(latitude)) * cos(NISTdegTOradian(longitude)),
    cos(NISTdegTOradian(latitude)) * sin(NISTdegTOradian(longitude)),
    sin(NISTdegTOradian(latitude))
  ))
}
x.coordinate <- function(latitude,longitude,R = 1) {
  return (cos(NISTdegTOradian(latitude)) * cos(NISTdegTOradian(longitude)))
}
y.coordinate <- function(latitude,longitude,R = 1) {
  return (cos(NISTdegTOradian(latitude)) * sin(NISTdegTOradian(longitude)))
}
z.coordinate <- function(latitude,longitude,R = 1) {
  return (sin(NISTdegTOradian(latitude)))
}

# ID                 1-11        Integer
# LATITUDE          13-20        Real
# LONGITUDE         22-30        Real
# STNELEV           32-37        Real
# NAME              39-68        Character
# GRELEV            70-73        Integer
# POPCLS            74-74        Character
# POPSIZ            75-79        Integer
# TOPO              80-81        Character
# STVEG             82-83        Character
# STLOC             84-85        Character
# OCNDIS            86-87        Integer
# AIRSTN            88-88        Character
# TOWNDIS           89-90        Integer
# GRVEG             91-106       Character
# POPCSS            107-107      Character
read.index <- function(name = 'ghcnm.tavg.v3.3.0.20161026.qca.inv', n = -1) {
  return(read.fwf(
    name,
    c(11,1,8,1,8,1,6,1,30,1,4,1,5,2,2,2,2,1,2,17,1),
    col.names = c(
      'ID','PAD0',
      'LATITUDE','PAD1',
      'LONGITUDE','PAD2',
      'STNELEV','PAD3',
      'NAME','PAD4',
      'GRELEV',
      'POPCLS',
      'POPSIZ',
      'TOPO',
      'STVEG',
      'STLOC',
      'OCNDIS',
      'AIRSTN',
      'TOWNDIS',
      'GRVEG',
      'POPCSS'),
    colClasses=c("character","NULL",
                "numeric","NULL",
                "numeric","NULL",
                "numeric","NULL",
                "character","NULL",
                "integer",
                "character",
                "character",#
                rep("character",3),
                "character",#
                "character",
                "character",#
                rep("character",2)),
   n=n,
  fill = TRUE
  ))
}

build.cols<-function(my.table) {
  xs<-as.data.frame(mapply(x.coordinate,my.table$LATITUDE,my.table$LONGITUDE))
  ys<-as.data.frame(mapply(y.coordinate,my.table$LATITUDE,my.table$LONGITUDE))
  zs<-as.data.frame(mapply(z.coordinate,my.table$LATITUDE,my.table$LONGITUDE))
  return (cbind(ii,xs,ys,zs))
  
}
