getFileExt <- function(data){
    temp.ext = sub(".*/", "", temp)
    temp.ext = temp.ext[grepl(".*[.].*", temp.ext)]
    temp.ext = sub(".*[.]", "", temp.ext)
    sort(unique(temp.ext))
}
