install.packages("data.table")
install.packages("jsonlite")
install.packages("tcltk2")
install.packages("RCurl")
install.packages("httr")
install.packages("RForcecom")
install.packages("plyr")

bat.file <- paste0("CALL ", path.expand(paste0("~/R/R-",paste0(R.version$major,".",R.version$minor),"/bin/Rscript.exe"))," \"",
                   path.expand(paste0("~/xfa/UIprototype.R")),"\"")
bat.file <- c(bat.file,"pause")
filecon <- file(path.expand(paste0("~/xfa/XFA_UI.bat")))
writeLines(bat.file,filecon)
