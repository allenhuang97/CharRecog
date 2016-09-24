library(pixmap)

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
test <-function(file, train,k){
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
  kNNIndices = numeric()
  for(i in 1:k){
    index = which.min(dis)
    kNNIndices = c(kNNIndices,index)
    kNNChar = c(kNNChar,char[index])
    char = char[-index]
    dis = dis[-index]
  }
  return(kNNIndices)
}

mode <- function(v) {
  uniqv = unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# data.path = "~/Documents/CharRecog/train/"
# all.files.train = file.path(data.path,list.files(data.path))
# for(i in 1:length(all.files)){
#   x <- read.pnm(all.files[i])
#   png(file=paste("testPNG/",substr(all.files[i],29,31),".png",sep=""))
#   plot(x)
#   dev.off()
# }
# 
# data.path = "~/Documents/CharRecog/train/"
# all.files.train = file.path(data.path,list.files(data.path))
# data.path = "~/Documents/CharRecog/test/"
# all.files.test = file.path(data.path,list.files(data.path))
# model = train("~/Documents/CharRecog/train/")
# for(i in 1:length(all.files.test)){
#   t = test(all.files.test[i],model,5)
#   for(ii in 1:5){
#     x <- read.pnm(all.files.train[t[ii]])
#     png(paste("testMatch/",substr(all.files.test[i],29,31),"/",ii,".png",sep=""))
#     plot(x)
#     dev.off()
#   }
# }