library(data.table)

get.file.name<-function(prefix,reading,date,adj,tar='tar',gz='gz') {
  return (paste(prefix,reading,date,adj,tar,gz,sep='.')) 
}

my.untar<-function (file,list=TRUE,out.path='.\\data', program='C:\\7z1604-extra\\7za',cmd='e'){
#   file='C:\\Users\\Weka\\global\\data\\ghcnm.tmax.latest.qcu.tar.gz'
#   program<-'C:\\7z1604-extra\\7za'
  
  
  # out='.\\data'
  system(sprintf('%s %s %s -o%s',program,cmd,file,out.path))
  tar.name<-sub('.gz$','',file)
  system(sprintf('%s %s %s -o%s',program,cmd,tar.name,out.path))
}

download.temperature.data<-function(
  data.path='ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3',
          prefix='ghcnm',
          reading='tmax',
          date='latest',
          adj='qcu',
          subdir='./data',
          untar.function=my.untar) {
  file.name<-get.file.name(prefix,reading,date,adj)
  source.file.name<-file.path(data.path,file.name)
  dest.file<-file.path(subdir,file.name)
  download.file(source.file.name,dest=dest.file)
  print (file.name)
  ut<-untar.function(dest.file,list=TRUE)
  print(ut)
  return(ut)
}

fread.data<-function(name) {
  raw<-fread(name,sep='\t',header=FALSE)
  ids<-substr(raw$V1,1,11)
  year<-as.integer(substr(raw$V1,12,15))
  element<-substr(raw$V1,16,19)
  result<-cbind(raw,ids,year,element)
  result$V1<-NULL
  columns<-c('ID','YEAR','ELEMENT')
  for (i in 1:12) {
    pos<-8*i+12
    value<-as.integer(substring(raw$V1,pos,pos+4))
    dmflag<-substring(raw$V1,pos+5,pos+5)
    qcflag<-substring(raw$V1,pos+6,pos+6)
    dsflag<-substring(raw$V1,pos+7,pos+7)
    result<-cbind(result,value,dmflag,qcflag,dsflag)
    columns<-c(columns,
               sprintf('VALUE%d',i),
               sprintf('DMFLAG%d',i),
               sprintf('QCFLAG%d',i),
               sprintf('DSFLAG%d',i))
  }
  colnames(result)<-columns
  return (result)
}