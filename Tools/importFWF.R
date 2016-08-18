importFWF = function(file, col.num, col.wdt, col.nme=c(1:length(col.num)) ){

    # Example Usage: importFWF("./11.TBL", col.num=c(23, 25, 28, 31), col.wdt=c(2,3,2,3), col.nme=c("t1","t2","t3","t4"))
    # Source       : source("~/git/R/Tools/importFWF.R")
    # Also need vars: in.file, col.nme
    #
    tmp.nme = c()
    num.var = length(col.num)
    tmp.wdt = c()

    ####################
    # Check for errors #
    ####################

    # Sort vectors based on col.num
    arrange = order(col.num)
    col.num = col.num[arrange]
    col.wdt = col.wdt[arrange]
    col.nme = col.nme[arrange]

    test = col.num + col.wdt
    # Column number plus width should not exceed the subsequent column numbers
    if( sum(test[-num.var] > col.num[-1]) > 0){
        stop("VARIABLE COLUMNS OVERLAP")
    }

    # Check input variables.
    #stopifnot(as.vector(col.num))
    #stopifnot(as.vector(col.wdt))
    #stopifnot(as.vector(col.nme))

    ##################
    # Function Start #
    ##################
    # The first observations
    #   Can likely be removed with better programming
    if(col.num[1] == 1){
        tmp.wdt[1] = col.wdt[1]

        tmp.nme[1] = col.nme[1]
    }else{
        tmp.wdt[1] = col.num[1] - 1
        tmp.wdt[2] = col.wdt[1]

        tmp.nme[1] = paste("sep",1)
        tmp.nme[2] = col.nme[1]
    }

    # Iterative Work on Subsequent Variables
    for(var in 2:num.var){
        # Find length of temporary width file.
        pos = length(tmp.wdt) + 1

        # If the next variables starting column
        #    is the sum of all previous widths
        #    set next width to next variable
        #    width
        if(col.num[var] == sum(tmp.wdt)){
            tmp.wdt[pos] = col.wdt[var]

            tmp.nme[pos] = col.nme[pos]
            # Otherwise, (if width is needed)
            #   add spacer then variable width
        }else{
            tmp.wdt[pos] = col.num[var] - sum(tmp.wdt) - 1
            tmp.wdt[pos+1] = col.wdt[var]

            tmp.nme[pos] = paste("sep",pos)
            tmp.nme[pos+1] = col.nme[var]
        }
    }

    tmp.dat = read.fwf(file=file, widths = tmp.wdt, col.names = tmp.nme)
    return(tmp.dat[,col.nme])
}
