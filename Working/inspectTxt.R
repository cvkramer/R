inspectTxt = function(fLoc, n=5, tlk=FALSE){
    fLoc = "/home/cvkramer/Desktop/SensitiveFileFinder.R"
    if(tcl){
        require(tcltk2)
        winTxt = tktoplevel()
        tktitle(winTxt) = "Inspect Text"
        fIn = readLines(fLoc, n=5)
        out <- readLines(fLoc, n=5)
        tV = tclVar("test")
        winTxt$env$num.subwin
        for(i in 1:length(out)){
            msg = tklabel(winTxt,
                          font = "helvetica",
                          wraplength="4i",
                          justify="left",
                          text=out[i]
                        )
            tkpack(msg)
        }
    }
    readLines(fLoc, n=1)

}

library(tcltk2)
win1 <- tktoplevel()
tkgrid(tk2label(win1, text = "This is a text label"))

win2 <- tktoplevel()
labelText <- tclVar("This is a text label")
win2$env$label <- tk2label(win2, textvariable = labelText)
tkgrid(win2$env$label)

changeText <- function()
    tclvalue(labelText) <- "This text label has changed!"
win2$env$butChange <- tk2button(win2, text = "Change text label", command = changeText)
tkgrid(win2$env$butChange)
