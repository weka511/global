# Copyright (C) 2016 Greenweaves Software Pty Ltd
#
# simon@greenweaves.nz
#
# This is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

if (!require(NISTunits)) {
  install.packages("NISTunits", dependencies = TRUE)
  library(NISTunits)
}

if (!require(png)) {
  install.packages("png", dependencies = TRUE)
  library(png)
}

setwd("~/../global")

rm(list=ls())

# direct.surface
# This function generates points on the surface of a sphere with a uniform distribution
#
# See Statistical Mechanics: Algorithms and Computations, Werber Krauth
# http://blancopeck.net/Statistics.pdf
# https://www.amazon.com/Statistical-Mechanics-Algorithms-Computations-Physics/dp/0198515367

direct.surface <- function(d=3){
  x<-rnorm(d,1/sqrt(d))
  Sigma<-sum(x*x)
  return (x/sqrt(Sigma))
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
read.index <-
  function(name = 'ghcnm.tavg.v3.3.0.20161026.qca.inv', n = -1) {
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
        'POPCSS'
      ),
      colClasses = c(
        "character","NULL",
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
        rep("character",2)
      ),
      n = n,
      fill = TRUE
    ))
  }

add.cartesian.coordinates<-function(my.table) {
  x.coordinate <- function(latitude,longitude,R = 1) {
    return (cos(NISTdegTOradian(latitude)) * cos(NISTdegTOradian(longitude)))
  }
  y.coordinate <- function(latitude,longitude,R = 1) {
    return (cos(NISTdegTOradian(latitude)) * sin(NISTdegTOradian(longitude)))
  }
  z.coordinate <- function(latitude,longitude,R = 1) {
    return (sin(NISTdegTOradian(latitude)))
  }
  xs<-as.data.frame(mapply(x.coordinate,my.table$LATITUDE,my.table$LONGITUDE))
  ys<-as.data.frame(mapply(y.coordinate,my.table$LATITUDE,my.table$LONGITUDE))
  zs<-as.data.frame(mapply(z.coordinate,my.table$LATITUDE,my.table$LONGITUDE))
  result<-cbind(my.table,xs,ys,zs)
  colnames(result)[length(result)-2]<-'X'
  colnames(result)[length(result)-1]<-'Y'
  colnames(result)[length(result)]<-'Z'
  rownames(result)<-result$ID
  return (result)
}



find.closest.station<-function(point,station.data) {
  get.distance.squared <- function(id,point,station.data) {
    station <- station.data[id,]
    return (
      (point[1] - station$X) * (point[1] - station$X) +
        (point[2] - station$Y) * (point[2] - station$Y) +
        (point[3] - station$Z) * (point[3] - station$Z)
    )
  }
  distances<-lapply(rownames(station.data),get.distance.squared,point,station.data)
  index<-which.min(distances)
  return (rownames(station.data)[index])
}



random.station.ids<-function(n,station.data) {
  random.station<-function(dummy,station.data){
    find.closest.station(direct.surface(),station.data)
  }
  return(unlist(lapply(rep(0,n),random.station,station.data)))
}

read.temperatures<-function(name = 'ghcnm.tavg.v3.3.0.20161026.qca.dat', n = 120){
 temperature.data<-read.fwf(name,
                   c(11,4,4,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1,
                     5,1,1,1),
                   col.names=c('ID','YEAR','ELEMENT',
                              'VALUE1', 'DMFLAG1', 'QCFLAG1', 'DSFLAG1',
                              'VALUE2', 'DMFLAG2', 'QCFLAG2', 'DSFLAG2',
                              'VALUE3', 'DMFLAG3', 'QCFLAG3', 'DSFLAG3',
                              'VALUE4', 'DMFLAG4', 'QCFLAG4', 'DSFLAG4',
                              'VALUE5', 'DMFLAG5', 'QCFLAG5', 'DSFLAG5',
                              'VALUE6', 'DMFLAG6', 'QCFLAG6', 'DSFLAG6',
                              'VALUE7', 'DMFLAG7', 'QCFLAG7', 'DSFLAG7',
                              'VALUE8', 'DMFLAG8', 'QCFLAG8', 'DSFLAG8',
                              'VALUE9', 'DMFLAG9', 'QCFLAG9', 'DSFLAG9',
                              'VALUE10','DMFLAG10','QCFLAG10','DSFLAG10',
                              'VALUE11','DMFLAG11','QCFLAG11','DSFLAG11',
                              'VALUE12','DMFLAG12','QCFLAG12','DSFLAG12'
                              ),
                   colClasses=c('character','integer','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character',
                                'numeric','character','character','character'  ),
                   n = n,
                   fill=TRUE)
  value.names<-colnames(temperature.data)[4*seq(1,12)]
  for (j in value.names)
    for (i in 1:nrow(temperature.data))
      if (temperature.data[i,j] == -9999)
        temperature.data[i,j]=NA

  temperature.data[,value.names]<-temperature.data[,value.names]/100
  return (temperature.data)
}



get.random.stations.with.data <- function(n,index,temperatures,from=1950,to=9999){
  get.stations.with.data<-function(temperatures,from=1950,to=9999){
    records<-temperatures[from <= temperatures$YEAR & temperatures$YEAR<=to,]
    ids=records$ID
    df<-data.frame(unique(ids))
    colnames(df)<-c('ID')
    return (df)
  }
  stations.with.data<-get.stations.with.data(temperatures,from=from,to=to)
  index.with.cartesian.coordinates<-add.cartesian.coordinates(index)
  index.with.data<-merge(index.with.cartesian.coordinates,stations.with.data)
  rownames(index.with.data)<-index.with.data$ID
  return(random.station.ids(n,index.with.data))
}

get.data.for.station<-function(id,temperature.data,from=1950,to=9999) {
  return (temperature.data[temperature.data$ID==id & from<=temperature.data$YEAR & temperature.data$YEAR<=to,])
}


attach.average.temperature<-function(temperature.data){
  get.average.temperature.year<-function(year,temperature.data){
    value.names<-colnames(temperature.data)[4*seq(1,12)]
    values<-as.numeric(temperature.data[temperature.data$YEAR==year,value.names])
    return (mean(values,na.rm = TRUE))
  }
  averages<-data.frame(sapply(temperature.data$YEAR,get.average.temperature.year,temperature.data))
  result<-cbind(temperature.data,averages)
  colnames(result)[length(result)]<-'MEAN'
  return (result)
}
