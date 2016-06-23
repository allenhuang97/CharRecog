#' @export
train <- function(data.path = "train/"){
  all.files = file.path(data.path,list.files(data.path))
  all.pixmap = lapply(all.files,function(file){
    list(bitmap = pixmap::read.pnm(file=file)@red, file)
    })
  return(all.pixmap)
}