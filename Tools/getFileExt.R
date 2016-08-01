getFileExt <- function(fileLoc, wExt = TRUE){
    # Replace everything before last forward slash with nothing. (Delete everything before last '/')
    fileLoc.ext = sub(".*/", "", fileLoc)

    # If curious only about files with extensions... (Default)
    if(wExt){
        # From list of filepaths, select only those filepaths that include a period.
        fileLoc.ext = fileLoc.ext[grepl(".*[.].*", fileLoc.ext)]

        # Replace everything before period with nothing. (Delete everything before period)
        fileLoc.ext = sub(".*[.]", "", fileLoc.ext)
    }

    # Return a sorted list of the unique values of extensions.
    # I suggest this list is toupper() or tolower() on this list.
    sort(unique(fileLoc.ext))
}
