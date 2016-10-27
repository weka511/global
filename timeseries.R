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


setwd('D:\\root\\sandbox\\coursera\\global-warming-model\\')

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

get.distance.squared <- function(id,point,station.data) {
  station <- station.data[id,]
  return (
    (point[1] - station$X) * (point[1] - station$X) +
      (point[2] - station$Y) * (point[2] - station$Y) +
      (point[3] - station$Z) * (point[3] - station$Z)
  )
}

find.closest.station<-function(point,station.data) {
  distances<-lapply(rownames(station.data),get.distance.squared,point,station.data)
  index<-which.min(distances)
  return (rownames(station.data)[index])
}

random.station<-function(dummy,station.data){
  find.closest.station(direct.surface(),station.data)
}

random.station.ids<-function(n,station.data) {
  return(lapply(rep(0,n),random.station,station.data))
}
