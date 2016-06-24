options(warn=-1)
#' @export
train <- function(data.path = "train/"){
  all.files = file.path(data.path,list.files(data.path))
  all.pixmap = lapply(all.files,function(file){
    bitmap = pixmap::read.pnm(file=file)
    bitmap = pixmap::addChannels(bitmap, coef = NULL)
    file = gsub(paste(data.path,"/",sep=""),"",file)
    list(bitmap = bitmap@grey, character = substr(file,1,1))
    })
  return(all.pixmap)
}

#' @export
test <-function(filePath, train){
  file = file.path(filePath)
  map = pixmap::read.pnm(file=file)
  map = pixmap::addChannels(map, coef = NULL)
  map = map@grey
  mapNum = length(train)
  dis = numeric()
  char = character()
  for(i in 1:mapNum){
    dis = c(dis,CharRecog::mindist(train[[i]]$bitmap,map))
    char = c(char,train[[i]]$character)
  }
  kNNChar = character()
  for(i in 1:5){
    index = which.min(dis)
    kNNChar = c(kNNChar,char[index])
    char = char[-index]
    dis = dis[-index]
  }
  return(kNNChar)
}