get.file.name<-function(prefix='ghcnm',
                        reading='tmax',
                        date='latest',
                        adj='qcu',
                        tar='tar',
                        gz='gz') {
  return (paste(prefix,reading,date,adj,tar,gz,sep='.')) 
}

download.temperature.data<-function(
  data.path='ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3',
  prefix='ghcnm',
  reading='tmax',
  date='latest',
  adj='qcu') {
  file.name<-get.file.name(prefix,reading,date,adj)
  fn<-file.path(data.path,file.name)
  download.file(fn,dest=file.name)
  return(untar(file.name,list=TRUE))
}