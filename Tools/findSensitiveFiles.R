####################
# Preamble ----
####################
library(tcltk2)
library(foreign)
library(tcltk)
    #.sav : data = read.spss("C:\\PathToFile\\MyDataFile.sav", to.data.frame=TRUE)
    #.dat : data = read.table("C:\\PathToFile\\MyDataFile.dat", header=TRUE)
library(sas7bdat)
    #.sas7bdat ; data = read.sas7bdat("C:\\PathToFile\\MyDataFile.sas7bdat")

#####################################
# List of removable extensions. -----
# ###################################
# Write list of extensions that don't need examination.
ext.rm = "HLP, DLL, tiff, tif, xps, ppt, graphml, swf, jar, ico, gif, exe, SAS, htm, toc, PDF, lnk, js, ttf, png, PH1, PH2, PH3, sublime-workspace, sublime-project, OUT, doc, out, jpg, deb, c, rua, rsp, png, sys, S, Rproj, md, dvi, log, tex, Rmd, Rnw, brew, dll, html, pdf, doxc, pptx, R, docx, mo, css, xpt, rds, rdx, rdb, h, yml, sps, syd, eps, ipp, hpp, ppd, bat, PPD, sas, r, PNG, EXE, ods, DOC, cpp, lnk, PH0, PDF, bmp, $$$, bib, blm, BLM, motor, revealjs, stagec, T03, T14, T88, T94, T95, T96, T97, T98, T99"
# Convert convenient string to vector of extensions
ext.rm = unlist(strsplit(ext.rm, ", "))

########################################
# Remove Files from list with ext. -----
# ######################################

# check if pasting and function work as expected.
#as.vector(sapply(ext.rm, function(x) paste(".",x,sep='') ))
# check if grepl works as expected.
#sum(grepl(paste(".","pdf","$",sep=''), temp))
#tmp2 = temp[-grepl(paste(".","pdf","$",sep=''), temp)]
# Actually remove values with those extensions.
#tmp = sapply(ext.rm, function(x) temp[-grepl(paste(".",x,"$",sep=''), temp)])
# Returns list of strings with removed extensions....
# loop instead
#
# Use loop to remove extention types.
temp = itp.filetree_fn

for( i in 1:length(ext.rm) ){
    x = ext.rm[i]
    temp = temp[!grepl(paste(".*[.]",x,"$",sep=''), temp)]
}

 paste(outFldr, "OUT/allExtensionsWG2.txt")

#####################################
# Examine leftovr extensions. -----
# ###################################
temp.ext = sub(".*/", "", temp)
temp.ext = temp.ext[grepl(".*[.].*", temp.ext)]
temp.ext = sub(".*[.]", "", temp.ext)
sort(unique(temp.ext))



#####################################
# Add column of extension-----
# ###################################
itp.filetree_fn.ext = itp.filetree_fn
itp.filetree_fn.ext = sub(".*/", "", itp.filetree_fn.ext)
itp.filetree_fn.ext[!grepl(".*[.].*", itp.filetree_fn.ext)] = ".none"
itp.filetree_fn.ext = sub(".*[.]", "", itp.filetree_fn.ext)
itp.filetree = data.frame(loc=itp.filetree_fn,ext=itp.filetree_fn.ext, stringsAsFactors = FALSE)

write.csv(itp.filetree[1:10,], file="/home/cvkramer/Desktop/itpFT.csv")
write.csv(itp.filetree_fn, file="/home/cvkramer/Desktop/itpFT_FN.csv")

#####################################
# Notes. -----
# ###################################
# Find conventions, eg Iowa0708... or likewise
# "Match" normally has student levels data

itp.filetree$ext = tolower(itp.filetree$ext)

itp.filetree[["obs"]] = NA
itp.filetree[["obs.e"]] = NA

iX = multiClass(itp.filetree, "ext", c("xls","xlsx"))

xls.files = itp.filetree[iX,"loc"]

pb = tkProgressBar(title = "Reading XLS/XLSX Files", min = 0, max = length(xls.files))
s.tm.xls = Sys.time()
for(i in 1:length(xls.files)){
    if( is.na(itp.filetree[iX,"obs"][i]) ){
        itp.filetree[iX,"obs"][i] =
            tryCatch(
                dim(read_excel(xls.files[i],skip=0))[1],
                error = function(e){
                    itp.filetree[iX,"obs.e"][i] = e
                    return(.1) },
                warning = function(w){
                    itp.filetree[iX,"obs.e"][i] = w
                    return(.2) }
            )
    }
    setTkProgressBar(pb, i)
}
close(pb)
e.tm.xls = Sys.time()

##############################
# sas7bdat ----
##############################
iX = multiClass(itp.filetree, vector="ext", keys="sas7bdat")

sas.files = itp.filetree[iX,"loc"]

pb = tkProgressBar(title = "R progress bar", min = 0, max = length(sas.files))
s.tm.sas = Sys.time()
for(i in 1:length(sas.files)){
    print(i)
    if( is.na(itp.filetree[iX,"obs"][i]) ){
        itp.filetree[iX,"obs"][i] =
            tryCatch(
                dim(read.sas7bdat(sas.files[i]))[1],
                error = function(e){
                    itp.filetree[iX,"obs.e"][i] = e
                    return(.3) },
                warning = function(w){
                    itp.filetree[iX,"obs.e"][i] = w
                    return(.4) }
            )
    }
    setTkProgressBar(pb, i)
}
close(pb)
e.tm.sas = Sys.time()


##############################
# sav ----
##############################

##############################
# dat ----
##############################

