# R

The files herein contain macros that I have made for various projects over the years.

# Tools/
**getFileExt.R**

Returns a sorted list of extensions attached to files within a folder path
    
**importFWF.R**

This function imports fixed-width files in a similar fashion to SAS.  Given column numbers and variable widths (column names optional), a data.frame object will be returned with only the requested columns.  Minor error checking is included; column overlap is not allowed.
    
**listDel.R**

Simple implementation of lapply to delete corresponding elements in a list.
    
**loadTools.R**

Will load some of the included R files.

**multiClass.R**

Returns an indicator vector given a data.frame, reference column and keys to match in reference column.
