
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(jsonlite)
packages(RCurl)


# This function is used to generate automatically the dataModel for SPSS Modeler
getMetaData <- function (data) {
  if( is.null(dim(data)))
    stop("Invalid data received: not a data.frame")
  if (dim(data)[1]<=0) {
    print("Warning : modelerData has no line, all fieldStorage fields set to strings")
    getStorage <- function(x){return("string")}
  } else {
    getStorage <- function(x) {
      x <- unlist(x)
      res <- NULL
      #if x is a factor, typeof will return an integer so we treat the case on the side
      if(is.factor(x)) {
        res <- "string"
      } else {
        res <- switch(typeof(x),
                      integer="integer",
                      double = "real",
                      "string")
      }
      return (res)
    }
  }
  col = vector("list", dim(data)[2])
  for (i in 1:dim(data)[2]) {
    col[[i]] <- c(fieldName=names(data[i]),
                  fieldLabel="",
                  fieldStorage=getStorage(data[i]),
                  fieldMeasure="",
                  fieldFormat="",
                  fieldRole="")
  }
  mdm<-do.call(cbind,col)
  mdm<-data.frame(mdm)
  return(mdm)
}


urlLong<-paste("https://myadapa.zementis.com:443/adapars/models")
pmml<-getURL(urlLong, userpwd ="%%username%%:%%pwd%%", ssl.verifypeer = FALSE)
json_file <- fromJSON(pmml) 
DF<-as.data.frame(do.call("rbind", json_file))
DF<-t(DF)
DF2<-data.frame()
for(i in 1:nrow(DF)) {
  name=DF[i,1]
  print(name)
  urlModelProperties<-paste("https://myadapa.zementis.com:443/adapars/model/",name,sep="")
  ModelPropiertes<-getURL(urlModelProperties, userpwd ="%%username%%:%%pwd%%", ssl.verifypeer = FALSE)
  json_file <- fromJSON(ModelPropiertes) 
  DF1<-as.data.frame(do.call("rbind", json_file))
  DF2<-rbind(DF2,DF1)
}

modelerData<-DF2
modelerDataModel <- getMetaData(modelerData)
