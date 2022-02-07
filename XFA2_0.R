### XFA Version 2
#' @author : Miguel Ajon
#' @purpose : Connect and integrate QB, SF, and SL into one platform.

#### Global Functions ####
### Get directory list
require(data.table)
require(jsonlite)
require(tcltk)
require(tcltk2)
require(RCurl)
require(httr)

json.dir <- path.expand("~/xfa/config")
setwd(path.expand("~/xfa/"))


creds <- fromJSON(list.files(json.dir, full.names = T, pattern = "apiAccessCreds.json"))
options(scipen = 999)

workspace.size <- function() {
  ws <- sum(sapply(ls(envir=globalenv()), function(x)object.size(get(x))))
  class(ws) <- "object_size"
  ws
}

#### TCL Functions ####
tcl.createArray<- function(table_matrix){
  # table_matrix <- mtcars
  
  table_matrix[, ] <- lapply(table_matrix[, ], as.character)
  table_matrix <- as.matrix(table_matrix)
  tclTable <- tclArray()
  for (i in 1:nrow(table_matrix)){
    for (j in 1:ncol(table_matrix)){
      tclTable[[i-1, j-1]] <- strsplit(table_matrix[i, j], " ", fixed = TRUE)[[1]]
    }
  }
  return(tclTable)
}

exportall_placements <- function(sl_data = SL_PROCESS,  qb_data = QB_PROCESS){
  sl_data$Amount <- as.data.frame(qb_data[match(sl_data$SFNPN.Concat,qb_data$SFNPN.Concat),"Amount"])
  sl_data$Difference <- round(as.numeric(sl_data$`Actualized Cost`) - as.numeric(sl_data$Amount),2)
  placement_view <- sl_data[,c("Month","SFNPN.Month","SF_OPPORTUNITY_ID","SF_NETWORK_PLACEMENT_NAME","Actualized Cost","Amount","Difference")]
  names(placement_view) <- c("SL Month","QB Month","SF_OPPORTUNITY_ID","SF_NETWORK_PLACEMENT_NAME","Actualized Cost","Amount","Difference")
  file.name <- tclvalue(tkgetSaveFile(initialfile = "Placement List.csv",
                                      filetypes = "{ {CSV Comma Delimited File} {.csv} } { {All Files} * }"))
  fwrite(placement_view,file.name)
  
}

call_exportall_placements <- function(){
  exportall_placements(sl = SL_PROCESS,  qb = QB_PROCESS)
}

placement_check <- function(id = tclvalue(placement_id),sl = SL_PROCESS, qb = QB_PROCESS){
  id <- tclvalue(placement_id)
  qb_data <- qb[which(QB_PROCESS$`IO Number`==id),]
  sl_data <- sl[which(SL_PROCESS$SF_OPPORTUNITY_ID==id)]
  
  sl_data$Amount <- as.data.frame(qb_data[match(sl_data$SFNPN.Concat,qb_data$SFNPN.Concat),"Amount"])
  sl_data$Difference <- round(as.numeric(sl_data$`Actualized Cost`) - as.numeric(sl_data$Amount),2)
  
  
  placement_view <- sl_data[,c("Month","SFNPN.Month","SF_NETWORK_PLACEMENT_NAME","Actualized Cost","Amount","Difference")]
  names(placement_view) <- c("SL Month","QB Month","SF_NETWORK_PLACEMENT_NAME","Actualized Cost","Amount","Difference")
  
  # grid.table(head(placement_view))
  # headers_placement_view <- data.frame(rbind(c("SL Month","QB Month","SF_NETWORK_PLACEMENT_NAME","Actualized Cost","Amount","Difference")),stringsAsFactors = F)
  # names(headers_placement_view) <- colnames(placement_view)
  # placement_view <- rbind(headers_placement_view,placement_view)
  
  View(placement_view)
  # utils::edit(placement_view)
  # table_id <- tclArrayVar(as.data.frame(placement_view))
  # edit(table_id, title = "Placement Check", height = 50, width = 500)
  # tk.window_placement <- tktoplevel()
  # tktitle(tk.window_placement) <- "Placement Check"
  # tryCatch({
  #   tcl('wm', 'iconphoto', tk.window_placement, tcl('image', 'create', 'photo', "-file", paste0(getwd(),creds$logo$logo_loc)))
  # }, error = function(x){
  #   NULL
  # })
  # tk.window_placement$env$placement_table <- tk2table(tk.window_placement, variable = tcl.createArray(placement_view), rows = nrow(placement_view), cols = length(placement_view),
  #                                               titlerows = 1, selectmode = "readonly", colwidth = 100, background = "white")
  # tkgrid(tk.window_placement$env$placement_table)
  # tkconfigure(tk.window_placement$env$placement_table, selectmode = "extended", rowseparator = "\"\n\"", colseparator = "\"\t\"")
}

call_placement_check <- function(){
  if(exists("placement_table_id")){
    tryCatch({
      tkdestroy(placement_table_id)
    }, error = function(x){
      NULL
    })
    placement_check()
  } else {
    placement_check()
  }
}

agency_table_popup <- function(year_number,data_table = agency_data){
  cat(year_number,"\n")
  data_table <- data_table[which(data_table$Year==year_number),]
  data_table$`Actualized Cost` <- as.character(round(as.numeric(data_table$`Actualized Cost`),2))
  data_table$`QB Amount` <- as.character(round(as.numeric(data_table$`QB Amount`),2))
  data_table_headers <- data.table(rbind(colnames(data_table)))
  names(data_table_headers) <- colnames(data_table)
  data_table <- rbind(data_table_headers,data_table)
  tk.window_agency <- tktoplevel()
  tktitle(tk.window_agency) <- "Agency Spend"
  tryCatch({
    tcl('wm', 'iconphoto', tk.window_agency, tcl('image', 'create', 'photo', "-file", paste0(getwd(),creds$logo$logo_loc)))
  }, error = function(x){
    NULL
  })
  tk.window_agency$env$agency_table <- tk2table(tk.window_agency, variable = tcl.createArray(data_table), rows = nrow(data_table), cols = length(data_table),
                                                titlerows = 1, selectmode = "readonly", colwidth = 25, background = "white")
  tkgrid(tk.window_agency$env$agency_table, columnspan =2)
  tkconfigure(tk.window_agency$env$agency_table, selectmode = "extended", rowseparator = "\"\n\"", colseparator = "\"\t\"")
  return(tk.window_agency$ID)
}

### TO DO
ioList_table_popup <- function(data_table = id_Data){
  data_table_headers <- data.table(rbind(colnames(data_table)))
  names(data_table_headers) <- colnames(data_table)
  data_table <- rbind(data_table_headers,data_table)
  data_table <- as.data.frame(data_table)
  # View(data_table)
  # Optional Table view
  table_io <- tclArrayVar(data_table)
  edit(table_io, title = "IO List", height = 20, width = 10)
}

call_ioList_table <- function(){
  if(exists("ioList_table_id")){
    tryCatch({
      tkdestroy(ioList_table_id)
    }, error = function(x){
      NULL
    })
    ioList_table_popup()
    ioList_table_id <<- iotable.id
  } else {
    ioList_table_popup()
    ioList_table_id <<- iotable.id
  }
}

iolist_table_export <- function(data_table = id_Data){
  file.name <- tclvalue(tkgetSaveFile(initialfile = "IO List.csv",
                                      filetypes = "{ {CSV Comma Delimited File} {.csv} } { {All Files} * }"))
  fwrite(data_table,file.name)
}

call_ioList_table_export <- function(){
  iolist_table_export()
}


agency_table_export <- function(year_number,data_table = agency_data){
  require(data.table)
  cat(year_number,"\n")
  data_table <- data_table[which(data_table$Year==year_number),]
  data_table$`Actualized Cost` <- as.character(round(as.numeric(data_table$`Actualized Cost`),2))
  data_table$`QB Amount` <- as.character(round(as.numeric(data_table$`QB Amount`),2))
  file.name <- tclvalue(tkgetSaveFile(initialfile = "Agencylist.csv",
                         filetypes = "{ {CSV Comma Delimited File} {.csv} } { {All Files} * }"))
  fwrite(data_table,file.name)
}

call_agency_table <- function(){
  if(exists("agency_table_id")){
    tryCatch({
      tkdestroy(agency_table_id)
    }, error = function(x){
      NULL
    })
    agency_table_id <<- agency_table_popup(year_number = tclvalue(years_value),data_table = agency_data)
  } else {
    agency_table_id <<- agency_table_popup(year_number = tclvalue(years_value),data_table = agency_data)
  }
}

call_agency_table_export <- function(){
  agency_table_export(year_number = tclvalue(years_value),data_table = agency_data)
}

export_data <- function(){
  require(data.table)
  dir_drop <- choose.dir()
  if(!is.na(dir_drop)){
    print("export_data()")
    print("Downloading Files")
    # SL_SF_Import()
    # QB_INPUT <<- fread(xax.getFile(paste0("XaxFA/QB_INPUT/",xax.GetDir("/XaxFA/QB_INPUT/", key = key)), 
    #                                as.file = "", 
    #                                tmp = paste0(getwd(),"/QB_INPUT.csv"), 
    #                                key = key), 
    #                    stringsAsFactors = F,
    #                    colClasses = c("IO Number"="character"))
    print("Writing Files")
    fwrite(QB_INPUT, paste0(dir_drop,"/QB_INPUT.csv"))
    fwrite(SF_INPUT, paste0(dir_drop,"/SF_INPUT.csv"))
    fwrite(SL_INPUT, paste0(dir_drop,"/SL_INPUT.csv"))
    tkmessageBox(title = "XFA - Xaxis Financial Audit",message = paste0("Files available in:\n",dir_drop), type = "ok")
  }

}

evaluate_iocheck <- function(){
  io.ids <- c(tclvalue(ID1),tclvalue(ID2),tclvalue(ID3),tclvalue(ID4),tclvalue(ID5))
  io.ids <- io.ids[which(nchar(io.ids)>=1)]
  io.ids <- paste0(io.ids, collapse = "|")
  data_iocheck <- id_Data[grep(io.ids,id_Data$`IO Number`,useBytes = T),]
  
  tclvalue(ID1_QBAmount) <- ifelse(tclvalue(ID1)!="",data_iocheck[match(tclvalue(ID1),data_iocheck$`IO Number`),]$QB_Amount,"0")
  tclvalue(ID1_SLAmount) <-  ifelse(tclvalue(ID1)!="",data_iocheck[match(tclvalue(ID1),data_iocheck$`IO Number`),]$SL_Amount,"0")
  tclvalue(ID1_Difference) <-  ifelse(tclvalue(ID1)!="",data_iocheck[match(tclvalue(ID1),data_iocheck$`IO Number`),]$Difference,"0")
  
  tclvalue(ID2_QBAmount) <- ifelse(tclvalue(ID2)!="",data_iocheck[match(tclvalue(ID2),data_iocheck$`IO Number`),]$QB_Amount,"0")
  tclvalue(ID2_SLAmount) <-  ifelse(tclvalue(ID2)!="",data_iocheck[match(tclvalue(ID2),data_iocheck$`IO Number`),]$SL_Amount,"0")
  tclvalue(ID2_Difference) <-  ifelse(tclvalue(ID2)!="",data_iocheck[match(tclvalue(ID2),data_iocheck$`IO Number`),]$Difference,"0")
  
  tclvalue(ID3_QBAmount) <- ifelse(tclvalue(ID3)!="",data_iocheck[match(tclvalue(ID3),data_iocheck$`IO Number`),]$QB_Amount,"0")
  tclvalue(ID3_SLAmount) <-  ifelse(tclvalue(ID3)!="",data_iocheck[match(tclvalue(ID3),data_iocheck$`IO Number`),]$SL_Amount,"0")
  tclvalue(ID3_Difference) <-  ifelse(tclvalue(ID3)!="",data_iocheck[match(tclvalue(ID3),data_iocheck$`IO Number`),]$Difference,"0")
  
  tclvalue(ID4_QBAmount) <- ifelse(tclvalue(ID4)!="",data_iocheck[match(tclvalue(ID4),data_iocheck$`IO Number`),]$QB_Amount,"0")
  tclvalue(ID4_SLAmount) <-  ifelse(tclvalue(ID4)!="",data_iocheck[match(tclvalue(ID4),data_iocheck$`IO Number`),]$SL_Amount,"0")
  tclvalue(ID4_Difference) <-  ifelse(tclvalue(ID4)!="",data_iocheck[match(tclvalue(ID4),data_iocheck$`IO Number`),]$Difference,"0")
  
  tclvalue(ID5_QBAmount) <- ifelse(tclvalue(ID5)!="",data_iocheck[match(tclvalue(ID5),data_iocheck$`IO Number`),]$QB_Amount,"0")
  tclvalue(ID5_SLAmount) <-  ifelse(tclvalue(ID5)!="",data_iocheck[match(tclvalue(ID5),data_iocheck$`IO Number`),]$SL_Amount,"0")
  tclvalue(ID5_Difference) <-  ifelse(tclvalue(ID5)!="",data_iocheck[match(tclvalue(ID5),data_iocheck$`IO Number`),]$Difference,"0")
  
}

tclArrayVar <- function(x = NULL) {
  # Check argument
  if (!is.null(x) && !is.vector(x) && length(dim(x))!= 2)
    stop("Array must be one-dimensional or two-dimensional, or NULL.")
  
  library(tcltk2)
  
  # Create the Tcl variable and the R Tcl object
  n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 1
  name <- paste0("::RTcl", n)
  l <- list(env = new.env(), nrow = 0, ncol = 0, ndim = 0)
  assign(name, NULL, envir = l$env)
  reg.finalizer(l$env, function(env) tkcmd("unset", ls(env)))
  class(l) <- "tclArrayVar"
  
  # A NULL array
  if (is.null(x)) {
    .Tcl(paste0("set ", name, "(0,0) \"\""))
    l$nrow <- 0
    l$ncol <- 0
    l$ndim <- 2
    return(l)
  }
  
  # A vector, matrix, or data frame
  if (is.vector(x)) {
    ndim <- 1
    x <- as.data.frame(x)
  } else ndim <- 2
  
  # Populate the Tcl array
  for (i in (1:nrow(x)))
    for (j in (1:ncol(x)))
      .Tcl(paste0("set ", name, "(", i, ",", j,") \"", x[i, j], "\""))
  
  # Process dim names
  if (nrow(x)) {
    if (is.null(rownames(x)))
      rownames(x) <- rep("", nrow(x))
    for (i in 1:nrow(x))
      .Tcl(paste0("set ", name, "(", i, ",", 0, ") \"", 
                  rownames(x)[i], "\""))
  } 
  
  if (ncol(x)) {
    if (is.null(colnames(x)))
      colnames(x) <- rep("", ncol(x))
    for (j in 1:ncol(x))
      .Tcl(paste0("set ", name, "(", 0, ",", j, ") \"", 
                  colnames(x)[j], "\""))
  }
  
  l$nrow <- nrow(x)
  l$ncol <- ncol(x)
  l$ndim <- ndim
  l
}

# edit() generic function is defined in the utils package
edit.tclArrayVar <- function(name, height = 20, width = 10, title) {
  library(tcltk2)
  
  win <- tktoplevel()
  
  tclArrayName <- ls(name$env)
  print(tclArrayName)
  if(missing(title)){
    tkwm.title(win, tclArrayName)
  } else {
    tkwm.title(win, title)
  }

  
  table <- tk2table(win,
                    rows = name$nrow + 1, cols = name$ncol + 1,
                    titlerows = 1, titlecols = 1,
                    maxwidth = 1000, maxheight = 1000,
                    drawmode = "fast",
                    height = height + 1, width = width + 1,
                    xscrollcommand = function(...) tkset(xscr, ...),
                    yscrollcommand = function(...) tkset(yscr,...))
  xscr <-tk2scrollbar(win, orient = "horizontal",
                      command = function(...) tkxview(table, ...))
  yscr <- tk2scrollbar(win, orient = "vertical",
                       command = function(...) tkyview(table, ...))
 
  tkgrid(table, yscr)
  tkgrid.configure(yscr, sticky = "nsw")
  tkgrid(xscr, sticky = "new")
  tkgrid.rowconfigure(win, 0, weight = 1)
  tkgrid.columnconfigure(win, 0, weight = 1)
  tkconfigure(table, variable = tclArrayName,
              background = "white", selectmode = "extended",rowseparator = "\"\n\"", colseparator = "\"\t\"", state = "disabled")
  
  if(title=="IO List"){
    iotable.id <<- win$ID  
  }
  if(title=="Placement Check"){
    placement_table_id <<- win$ID  
  }

}

`[.tclArrayVar` <- function(object, i, j = NULL) {
  library(tcltk2)
  
  if (is.null(j) && object$ndim != 1)
    stop("Object is not a one-dimensional tclArrayVar")
  if (!is.null(j) && object$ndim != 2)
    stop("Object is not a two-dimensional tclArrayVar")
  
  if (object$ndim == 1) j <- 1
  tclArrayName <- ls(object$env)
  tclvalue(paste0(tclArrayName, "(", i, ",", j, ")"))
}

`[<-.tclArrayVar` <- function(object, i, j = NULL, value) {
  library(tcltk2)
  
  if (is.null(j) && object$ndim != 1)
    stop("Object is not a one-dimensional tclArrayVar")
  if (!is.null(j) && object$ndim != 2)
    stop("Object is not a two-dimensional tclArrayVar")
  
  if (object$ndim == 1) j <- 1
  tclArrayName <- ls(object$env)
  .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") ", value))
  if (i > object$nrow) object$nrow <- i
  object
}

#### FTP ####
key <- paste0(creds$FTP$usr,":",creds$FTP$pwd)
ftpurl <- creds$FTP$ftpurl

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  r["CRANextra"] <- "http://www.stats.ox.ac.uk/pub/RWin"
  options(repos = r)
})

if(!any(grepl("RCurl",installed.packages()))){
  install.packages("RCurl",repos = "http://cran.us.r-project.org")
}
if(!any(grepl("data.table",installed.packages()))){
  install.packages("data.table",repos = "http://cran.us.r-project.org")
}

### Get directory listing 
xax.GetDir <- function(dir, key){
  ### Load package RCurl into the function
  require(RCurl)
  
  ### Check if dir .arg is missing
  if(missing(dir)){
    
    ### if dir .arg is missing, default to root folder of FTP
    url <- "ftp://analytics.xaxis.com"
  } else {
    
    ### if dir .arg is given, use paste0 function to add the directory to the url 
    url <- paste0("ftp://analytics.xaxis.com/",dir,"/") 
  }
  
  ### Call FTP using a getURL call, then splitting the string and unlisting the list into a standard vector
  return <- unlist(strsplit(getURL(url = url, 
                                   ftp.use.epsv=TRUE, 
                                   dirlistonly=T, 
                                   userpwd = key), 
                            split = "\\\r\\\n"))
  
  ### Return directory in a vector format
  return(return)
}


### Upload a file to ftp 
xax.Upload <- function(file,todir,key,ftp.override = FALSE, url = NULL){
  
  # file = paste0(output.dir,"/",name.file,".csv")
  # todir = inputs$ftpurl[[m]]
  # url = inputs$ftpurl[[m]]
  # key = paste0(c(inputs$ftpusr[[m]],inputs$ftppwd[[m]]),collapse = ":")
  # ftp.override = T
  # 
  # file = "schedule.csv"
  # todir = "/Audience Data/Schedule"
  # key = key
  # 
  ####!!! if FTP Override == TRUE, it will automatically use url as default
  # file = paste0(output.dir,"/",name.file,".csv");todir = "/lab/";url = inputs$ftpurl[[m]];key = paste0(c(inputs$ftpusr[[m]],inputs$ftppwd[[m]]), collapse = ":"); ftp.override = T
  
  if(!ftp.override){
    url <- "ftp://analytics.xaxis.com/"
  } else {
    url <- url
    if(!grepl("ftp://",url)){
      warning("[url] must have [ftp://] in the beginning")
      url <- paste0("ftp://",url)
    }
    if(is.null(url)){
      warning("url = NULL")
    }
  }
  
  ### tryCatch statement for errors usually would be caused by how people would paste their files (if they paste manually)
  tryCatch({
    
    ### Split the file name with backslash or forward slash, then unlisting it to make it a normal vector
    file.name <- unlist(strsplit(file,split = "/|\\\\"))
    
    ### Get the last item in the vector 
    file.name <- file.name[length(file.name)]
    
    ### Load required package RCurl for FTP commands
    require(RCurl)
    
    ### Create options, putting logical state to TRUE to create missing dirs if stated within the filename
    opts <- list(ftp.create.missing.dirs = TRUE)
    
    ### If upload file is a directory/folder, return an error print
    if(dir.exists(file)){
      return(print("Cannot upload directories"))
    } else {
      
      ### If todir is missing, default to root folder with the file name
      if(ftp.override){
        todir <- paste0(url,"/",file.name)
      } else if(missing(todir)){
        todir <- paste0(url,file.name)
      } else {
        
        ### If todir is added, construct the string to add the todir .arg and the file.name together
        todir <- paste0(url,todir,"/",file.name)
      }
      
      
      ### Push upload request to the FTP and return a logical
      x <- ftpUpload(what = file,
                     userpwd = key, asText = F,
                     to = todir,
                     .opts = opts)
      
      ### FTP says that 0 = TRUE and 1 = FALSE, simply invert the output
      ### If upload is successful, it will return a TRUE
      ### If upload fails, it will return a FALSE
      return(!as.logical(x))
    }
  }, error = function(x){
    
    ### Return invalid file name to user
    print("Invalid file name")
  })
}

### Download file to local disk and read into R --- deprecated
xax.getCSV <- function(getfile, key){
  
  ### Load required packages RCurl and data.table into function
  require(RCurl)
  require(data.table)
  
  ### Use documents/tmpfile.csv to temporarily download the CSV
  file.0 <- path.expand("~/tmpfile.csv")
  
  ### the URL for FTP
  url <- "ftp://analytics.xaxis.com/"
  
  ### Use writelines to write the file into local disk (Could not find a way to parse or delimit the read file from the FTP directly into R)
  writeLines(getURL(paste0(url,getfile), userpwd = key), file.0)
  
  ### Read file from local disk to import back to R environment
  x <- read.csv(file.0, stringsAsFactors = F)
  
  ### Remove tmp file
  file.remove(file.0)
  
  ### Return CSV back into the global env of R
  return(x)
}

### Delete files in the FTP
xax.DelFile <- function(subdir,file,key){
  ### Load required package into function
  require(RCurl)
  
  ### The URL for FTP
  ftpurl <- "ftp://analytics.xaxis.com/"
  
  ### If file missing from argument, send an error
  if(missing(file)){
    return(print("File argument missing. Please specify a file."))
  } else {
    
    ### Construct quote request for curlPerform action
    file.0 <- paste0("DELE ",subdir,"/",file)
  }
  
  ### Perform HTTP request and store HTTP response to x
  x <- curlPerform(url=ftpurl, quote=file.0, userpwd = key)
  
  ### Return response from vector
  ### If return is TRUE, file deletion is successful
  return(!as.logical(x))
}

###does the pc have a connection?
connectivity.test <- function() {
  
  ### Test OS type, if windows then use ipconfig, if not then use ifconfig
  ifelse(.Platform$OS.type=="windows", 
         ipmessage.2 <- system("ipconfig", intern = TRUE),
         ipmessage.2 <- system("ifconfig", intern = TRUE))
  
  ### Create valid IP numbers to search in the ipconfig response
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  
  ### If any grep is true, then return a TRUE
  ### TRUE = computer is connected to the internet
  ### FALSE = computer has no internet
  return(any(grep(validIP, ipmessage.2)))
}

xax.getBin <- function(getfile, key){
  #getFile function is the same
  require(RCurl)
  require(data.table)
  url <- "ftp://analytics.xaxis.com/"
  bin <- getBinaryURL(paste0(url,getfile), userpwd = key)
  writeBin(bin,path.expand("~/tmpbin"))
  x <- readRDS(path.expand("~/tmpbin"))
  logical.return <- file.remove(path.expand("~/tmpbin"))
  return(x)
}

xax.getFile <- function(getfile, as.file , key, tmp = NULL){
  # getfile <- paste0("FTP_Drop/",XFAfile)
  # as.file = ".csv"
  # key = key
  # tmp = "tmp_SL.csv"
  if(is.null(tmp)){
    name.file <- path.expand("~/tmpFile")
  } else {
    name.file <- tmp
  }
  require(RCurl)
  require(data.table)
  url <- "ftp://analytics.xaxis.com/"
  bin <- getBinaryURL(paste0(url,getfile), userpwd = key)
  writeBin(bin,paste0(name.file,as.file))
  x <- paste0(name.file,as.file)
  return(x)
}

xax.isDir <- function(dir, key){
  require(RCurl)
  tryCatch({
    if(missing(dir)){
      url <- "ftp://analytics.xaxis.com"
    } else {
      url <- paste0("ftp://analytics.xaxis.com/",dir,"/") 
    }
    return <- unlist(strsplit(getURL(url = url, 
                                     ftp.use.epsv=TRUE, 
                                     dirlistonly=T, 
                                     userpwd = key), 
                              split = "\\\r\\\n"))
    if(length(return)>=0){
      return(TRUE)
    } 
  }, error = function(x){
    return(FALSE)
  })
}

read.as.raw <- function(f){
  file = file(f, "rb")
  raw.content <- as.raw(0)
  i <- 1
  tryCatch({
    while(1){
      raw.content[i] <- readBin(file, "raw")
      i <- i+1
    }
  }, error = function(x){
    "End of file"
  })
  return(raw.content)
}


#### SQL XFA ####
find.split <- function(x,y){
  # x = QB_INPUT$`Memo/Description`[[row]]
  # y = ":"
  split <- unlist(strsplit(x,split = ""))
  char.Loc <- which(split==y)-3
  split <- split[char.Loc:length(split)]
  return(paste0(split,collapse = ""))
}

find.remove.after <- function(what,remove.after){
  # what = QB_INPUT$SF_NETWORK_PLACEMENT_NAME[[1]]
  # remove.after = "$"
  split <- unlist(strsplit(what,split = ""))
  char.Loc <- which(split==remove.after)
  char.Loc <- char.Loc[length(char.Loc)]
}

SQL_Query_XFA <- function(username = creds$SF_SOQL$usr, password = creds$SF_SOQL$pwd,crypt_key = creds$SF_SOQL$crypt_key) {
  require(RForcecom)
  require(plyr)
  require(XML)
  require(data.table)
  
  print("Retrieving SF Data")

  password <- paste0(password,crypt_key)
  instanceURL <- "https://na49.salesforce.com/"
  apiVersion <- "39.0"
  
  session <- rforcecom.login(username, password, instanceURL, apiVersion)
  
  ##### Body #####
  tryCatch({
    xax.DelFile(xax.GetDir("XaxFA/SF_INPUT", key=key),subdir = "XaxFA/SF_INPUT",key=key)
  }, error = function(x){
    message("Error on SQL_Query_XFA function: Failed flush of SF_INPUT, error code -- ER00003B")
  })
  
  cat("XFA Query_Main SQL start\n")
  SOQLQuery <- ("SELECT End_Date__c,Legacy_Order_No__c,Name,Opportunity_Number__c,Planned_Gross_Budget__c,Start_Date__c,Total_Planned_Billable_Impressions__c FROM Opportunity")
  queryR <- rforcecom.query(session, SOQLQuery)
  queryR[] <- lapply(queryR, as.character)
  queryR[is.na(queryR)] <- ""
  names(queryR) <- c("Opportunity Name","Opportunity Number","End Date","Legacy Order No","Total Planned Gross Budget","Start Date","Billable Impressions")
  # fwrite(queryR,"~/SF_INPUT.csv", row.names = F)
  # uploadFTP <- xax.Upload(file = "~/SF_INPUT.csv",todir = "XaxFA/SF_INPUT", key = key)
  # if(as.logical(uploadFTP)){
  #   cat("Upload complete \n")
  # }
  saveRDS(queryR,paste0(getwd(),"/SF_INPUT.csv"))
  # fwrite(queryR,paste0(getwd(),"/SF_INPUT.csv"))
  xax.Upload(paste0(getwd(),"/SF_INPUT.csv"), todir = "/XaxFA/SF_INPUT/", key = key)
  file.remove(paste0(getwd(),"/SF_INPUT.csv"))
  return(queryR)
  cat("\n\n\nDone\n")
}

#FTP Drop to stream XFA
SLXFADATASTREAM <- function() {
  print("Retrieving SL Data")
  require(data.table)
  XFAfile <- grep("XFA00",xax.GetDir("/FTP_Drop",key=key),value = T)
  XFAfile <- grep(gsub("-","",as.character(as.Date(Sys.Date())-1)),XFAfile, value = T)
  if(length(XFAfile)>=1){
    tryCatch({
      del <- xax.GetDir("XaxFA/SL_INPUT", key = key)
      if(length(del)!=0){
        xax.DelFile("XaxFA/SL_INPUT",file = del,key = key)
      }
    }, error = function(x){
      message("Error on SLXFADATASTREAM function: Failed flush on SL_INPUT, error code -- ER00001A")
    })
    tryCatch({
      XFAfile2 <- fread(xax.getFile(paste0("FTP_Drop/",XFAfile),as.file = ".csv", key = key, tmp = "tmp_SL.csv"), stringsAsFactors = F)
      saveRDS(XFAfile2,paste0(getwd(),"/data/SL_INPUT.csv"))
      # fwrite(XFAfile2,paste0(getwd(),"/data/SL_INPUT.csv"))
      xax.Upload(paste0(getwd(),"/data/SL_INPUT.csv"),todir="XaxFA/SL_INPUT", key = key)
      file.remove(paste0(getwd(),"/data/SL_INPUT.csv"))
      
      XFAfile.delete <- grep("XFA00",xax.GetDir("/FTP_Drop",key=key),value = T)
      if(length(XFAfile.delete)>=1){
        for(del in 1:length(XFAfile.delete)){
          xax.DelFile("FTP_Drop",file = XFAfile.delete[del],key=key)
        }
      }
      cat("\n\nTRUE")
      return(XFAfile2)
    }, error = function(x){
      message("Error on SLXFADATASTREAM function: Failed upload of SL_INPUT to FTP, error code -- ER00002A")
      return(FALSE)
    })
  }
}

SL_SF_Import <- function(){
  require(data.table)
  print("Importing Unprocessed Files.")
  e <- parent.env(environment())
  SL_INPUT <- xax.getFile(paste0("/XaxFa/SL_INPUT/",
                                 xax.GetDir("XaxFA/SL_INPUT", key = key)),
                          as.file = ".csv", key = key, 
                          tmp = paste0(getwd(),"/SL_INPUT.csv"))
  SF_INPUT <- xax.getFile(paste0("XaxFa/SF_INPUT/",
                                 xax.GetDir("XaxFA/SF_INPUT", key = key)),
                          as.file = ".csv", key = key, 
                          tmp = paste0(getwd(),"/SF_INPUT.csv"))
  SL_INPUT <<- readRDS(SL_INPUT)
  SF_INPUT <<- readRDS(SF_INPUT)
}

QB_Update <- function(){
  SL_SF_Import()
  require(tcltk)
  require(tcltk2)
  choice <- ifelse(tclvalue(tkmessageBox(title = "QB Update", message=  "Update QB File data base?", type = "yesno", icon = "question"))=="yes",T,F)
  if(choice){
    while(1){
      ff <- file.choose()
      file.format <- grepl(".csv$",ff)
      if(file.format){
        tmp <- fread(ff, stringsAsFactors = F, colClasses = c("IO Number"="character"))
        tmp$Date <- gsub("/","-",tmp$Date)
        structure.tmp <- colnames(tmp)
        structure.orig <- readRDS(xax.getFile(getfile = paste0("/XaxFA/",grep(x = xax.GetDir("/XaxFA/",key = key), pattern = "structure", value = T)),as.file = ".RDS",tmp = paste0(getwd(),"/structure"), key = key)) 
        tt.true <- all(structure.tmp == structure.orig)
        if(tt.true){
          break
        } else {
          tkmessageBox(title = "Error: Upload -- UP1000", message = "Column/Fields do not match Original structure.\n Contact miguel.ajon@groupm.com for support.", type = "ok", icon = "error")
        }
      } else {
        tkmessageBox(title = "Error: Upload -- UP1000", message = "File must be a csv for data merge to be successful!", type = "ok", icon = "error")
      }
    }
    tryCatch({
      print("Updating database")
      
      QB_INPUT <- readRDS(xax.getFile(paste0("XaxFA/QB_INPUT/",xax.GetDir("/XaxFA/QB_INPUT/", key = key)), 
                                    as.file = "", 
                                    tmp = paste0(getwd(),"/QB_INPUT.csv"), 
                                    key = key))
      
      ### Check duplicates from rbind
      QB_INPUT.match <- paste0(QB_INPUT$Date,QB_INPUT$`Memo/Description`)
      tmp_input.match <- paste0(tmp$Date,tmp$`Memo/Description`)
      loc <- which(is.na(match(tmp_input.match,QB_INPUT.match))==TRUE)
      tmp <- as.data.frame(tmp)
      tmp.instance <- rbind(QB_INPUT,tmp[loc,])
      tmp.instance <- as.data.frame(tmp.instance)
      tmp.instance <- tmp.instance[!duplicated(tmp.instance[c("Date","IO Number","Memo/Description","#","Amount")]),]
      
      QB_INPUT <- as.data.table(tmp.instance)
      saveRDS(QB_INPUT,paste0(getwd(),"/QB_INPUT.csv"))
      xax.Upload(paste0(getwd(),"/QB_INPUT.csv"), todir = "XaxFA/QB_INPUT", key = key)
      file.remove(paste0(getwd(),"/QB_INPUT.csv"))
      QB_INPUT <<- QB_INPUT
      date.range <- unique(format(as.Date(QB_INPUT$Date,"%d-%m-%Y"),"%Y-%m"))
      date.range <- date.range[order(date.range)]
      date.range <- date.range[!is.na(date.range)]
      tkmessageBox(title = "QB Database", 
                   message = paste0("Date Range:\n",paste0(date.range, collapse = "\n")), type = "ok", icon = "info")
      tkmessageBox(title = "QB Database", 
                   message = paste0("Updating tables. ETA: 20 minutes"), type = "ok", icon = "info")
      ## TO DO
      data_cleaner()
      import_process_SLQB()
      tkmessageBox(title = "QB Database", 
                   message = paste0("Updating Complete"), type = "ok", icon = "info")
    }, error = function(x){
      print("First fill of data base")
      saveRDS(tmp,paste0(getwd(),"/QB_INPUT.csv"))
      xax.Upload(paste0(getwd(),"/QB_INPUT.csv"), todir = "XaxFA/QB_INPUT", key = key)
      file.remove(paste0(getwd(),"/QB_INPUT.csv"))
      QB_INPUT <<- tmp
      date.range <- unique(format(as.Date(QB_INPUT$Date,"%d-%m-%Y"),"%Y-%m"))
      date.range <- date.range[order(date.range)]
      date.range <- date.range[!is.na(date.range)]
      tkmessageBox(title = "QB Database", 
                   message = paste0("Date Range:\n",paste0(date.range, collapse = "\n")), type = "ok", icon = "info")
      tkmessageBox(title = "QB Database", 
                   message = paste0("Updating tables. ETA: 20 minutes"), type = "ok", icon = "info")
      SLXFADATASTREAM()
      SQL_Query_XFA()
      data_cleaner()
      import_process_SLQB()
      tkmessageBox(title = "QB Database", 
                   message = paste0("Updating Complete"), type = "ok", icon = "info")
    })
  } else {
    print("Retrieving Database")
    QB_INPUT <<- readRDS(xax.getFile(paste0("XaxFA/QB_INPUT/",xax.GetDir("/XaxFA/QB_INPUT/", key = key)), 
                                  as.file = "", 
                                  tmp = paste0(getwd(),"/QB_INPUT.csv"), 
                                  key = key))
    date.range <- unique(format(as.Date(QB_INPUT$Date,"%d-%m-%Y"),"%Y-%m"))
    date.range <- date.range[order(date.range)]
    date.range <- date.range[!is.na(date.range)]
    tkmessageBox(title = "QB Database", 
                 message = paste0("Date Range:\n",paste0(date.range, collapse = "\n")), type = "ok", icon = "info")
  }
}


### Rewrite data_cleaner???
data_cleaner <- function(qb = QB_INPUT, sf = SF_INPUT, sl = SL_INPUT, invoice.exceptions = creds$invoice$invoice_exceptions, 
                         agency_names = creds$agencies$agency_names){
  require(data.table)
  
  progress.bar <<- "Verifying QB IO Numbers"
  {
    #### verify QB IO Numbers against SL data
    for(t in 1:nrow(qb)){
      n <- 1
      orig.length <- nchar(qb$`IO Number`[t])
      while(1){
        if(any(grepl(qb$`IO Number`[t],sl$SF_OPPORTUNITY_ID))){
          print(TRUE)
          cat(t/nrow(qb)*100,"\n")
          break
        } else {
          print(qb$`IO Number`[t])
          qb$`IO Number`[t] <- paste0(qb$`IO Number`[t],"0")
          n <- n+1
          if(n>7){
            print(FALSE)
            n <- 1
            qb$`IO Number`[t] <- paste0(paste0(unlist(strsplit(qb$`IO Number`[t],split=""))[1:orig.length],collapse = ""),"_FLAG")
            break
          }
        }
      }
    }
  }
  gc()
  
  ### By SF_NETWORKPLACEMENT
  qb$Memo_Cut <- ""
  qb$Date <- as.Date(qb$Date,"%d-%m-%Y")
  cat("Processign QB Data...\n")
  
  progress.bar <<- "Processing QB Data"
  
  for(row in 1:nrow(qb)){
    tryCatch({
      qb$Memo_Cut[[row]] <- find.split(qb$`Memo/Description`[[row]],":")
      setTxtProgressBar(pb, row)
    }, error = function(x){
      NULL
    })
    cat(row/nrow(qb)*100,'%  - QB Data\n')
  }
  gc()
  
  qb$SF_NETWORK_PLACEMENT_NAME <- ""
  qb_Blank <- qb[which(qb$Memo_Cut==""),]
  
  current.year <- as.numeric((format(Sys.time(),"%Y")))
  last.year <- as.numeric((format(Sys.time(),"%Y")))-1
  next.year <- as.numeric((format(Sys.time(),"%Y")))+1
  remove.months <- c(paste0(month.name,current.year),paste0(month.name,next.year),paste0(month.name,last.year),"Activity")
  
  sl$SF_NETWORK_PLACEMENT_NAME.ORIGINAL <- gsub("\\(|\\)|\\ |\n|-|\\+|\\\\","",sl$SF_NETWORK_PLACEMENT_NAME)
  qb$Memo_Cut <- gsub("\\(|\\)|\\ |\n|-|\\+|\\\\","",qb$Memo_Cut)
  
  sl <- sl[-which(sl$SF_NETWORK_PLACEMENT_NAME==""),]
  sl <- sl[-which(sl$SF_NETWORK_PLACEMENT_NAME=="PH"),]
  if(length(which(sl$SF_NETWORK_PLACEMENT_NAME=="Xaxis"))>=1){
    sl <- sl[-which(sl$SF_NETWORK_PLACEMENT_NAME=="Xaxis"),]
  }
  sl <- sl[-which(sl$SF_NETWORK_PLACEMENT_NAME=="Other"),]
  sl <- sl[-which(nchar(sl$SF_NETWORK_PLACEMENT_NAME)<=6),]
  
  sl$SFNPN.Month <- format(as.Date(sl$Month,"%Y-%m-%d")+31,"%Y-%m-01")
  
  ### SL Concat date and network placements. Get sf id here
  sl$SFNPN.Concat <- paste0(sl$SFNPN.Month,"--",sl$SF_NETWORK_PLACEMENT_NAME.ORIGINAL)
  
  cat("Processing Spotlight Data\n")
  progress.bar <<- "Processing Spotlight Data"
  
  
  unique.sl_networkplacements <- unique(sl$SF_NETWORK_PLACEMENT_NAME.ORIGINAL)
  unique.qb_memocut <- unique(qb$Memo_Cut)
  
  for(i in 1:length(unique.sl_networkplacements)){
    qb[grep(unique.sl_networkplacements[i],qb$Memo_Cut, ignore.case = T),"SF_NETWORK_PLACEMENT_NAME"] <-  unique.sl_networkplacements[i]
    cat(i/length(unique.sl_networkplacements)*100,"% - Processing Spotlight Data\n")
  }
  gc()
  
  qb_Blank <- qb[which(qb$SF_NETWORK_PLACEMENT_NAME==""),]
  
  qb$SFNPN.Concat <- paste0(qb$Date,"--",qb$SF_NETWORK_PLACEMENT_NAME)
  
  qb$`Actualized Cost` <- sl[match(qb$SFNPN.Concat,sl$SFNPN.Concat),"Actualized Cost"]#as.numeric(unlist())
  qb$Amount <- as.numeric(gsub(",","",qb$Amount))
  qb$Date <- as.character(qb$Date)
  qb[is.na(qb)] <- 0
  qb$SF_BUDGET <- sl[match(qb$SFNPN.Concat,sl$SFNPN.Concat),"SF_GROSS_BUDGET"]
  qb$difference <- qb$`Actualized Cost` - qb$Amount
  SF_NETWORK_PLACEMENT_NAME.ORIGINAL <- sl[match(qb$SF_NETWORK_PLACEMENT_NAME,sl$SF_NETWORK_PLACEMENT_NAME.ORIGINAL),
                                           "SF_NETWORK_PLACEMENT_NAME.ORIGINAL"]
  qb$SF_NETWORK_PLACEMENT_NAME.ORIGINAL <- SF_NETWORK_PLACEMENT_NAME.ORIGINAL 
  qb$Nonbilling <-  grepl(paste0(invoice.exceptions,collapse = "|"),qb$`Memo/Description`, ignore.case = T)
  qb$isSFPlacement <- ifelse(qb$Memo_Cut!="",TRUE,FALSE)
  
  ### Aggregating
  progress.bar <<- "Grouping SF IDs"
  actualized.cost <- ""
  amount <- ""
  io.numbers <- unique(qb$`IO Number`)

  
  cat("Aggregating...")

  for(i in 1:length(io.numbers)){
    actualized.cost[i] <- tryCatch({
       sum(sl[which(sl$SF_OPPORTUNITY_ID==io.numbers[i]),"Actualized Cost"])
    }, error = function(x){
      return(0)
    })
  }
  gc()
  for(i in 1:length(io.numbers)){
    amount[i]  <- tryCatch({
      as.numeric(sum(qb[which(qb$`IO Number`==io.numbers[i]),"Amount"]))
    }, error = function(x){
      return(0)
    })
  }
  gc()
  amount <- as.numeric(amount)
  actualized.cost <- as.numeric(actualized.cost)

  pivot_bySF_ID <- data.table(cbind(io.numbers,amount,actualized.cost), stringsAsFactors = F)
  names(pivot_bySF_ID) <- c("IO Number","QB_Amount","SL_Amount")
  pivot_bySF_ID$Difference <- round(as.numeric(pivot_bySF_ID$QB_Amount)-as.numeric(pivot_bySF_ID$SL_Amount),2)
  pivot_bySF_ID$SL_Amount <- round(as.numeric(pivot_bySF_ID$SL_Amount),2)
  pivot_bySF_ID[is.na(pivot_bySF_ID)] <- "--"
  
  ### write pivot sf id @PivotbySFID is defunct
  saveRDS(pivot_bySF_ID, paste0(getwd(),"/pivot_bySF_ID.csv"))
  xax.Upload(paste0(getwd(),"/pivot_bySF_ID.csv"),todir = "/XaxFA/PIVOT",key=key)
  file.remove(paste0(getwd(),"/pivot_bySF_ID.csv"))
  
  qb$io.number <- pivot_bySF_ID[match(qb$`IO Number`,pivot_bySF_ID$`IO Number`),"IO Number"]
  qb$SFNPN.Concat <- paste0(qb$SFNPN.Concat,"--",qb$`IO Number`)
  sl$SFNPN.Concat <- paste0(sl$SFNPN.Concat,"--",sl$SF_OPPORTUNITY_ID)
  qb$`Actualized Cost` <- sl[match(qb$SFNPN.Concat,sl$SFNPN.Concat),"Actualized Cost"]
  qb[is.na(qb)] <- 0


  progress.bar <<- "Rounding up the agencies"
  
  qb$Agency <- ""
  sl$Agency <- ""
  for(i in 1:length(agency_names)){
   qb[grep(agency_names[i],qb$`Memo/Description`, ignore.case = T),"Agency"] <- unlist(strsplit(agency_names[i],split = "\\|"))[1]
   sl$Agency[grep(agency_names[i],sl$SF_OPPORTUNITY_NAME, ignore.case = T)] <- unlist(strsplit(agency_names[i],split = "\\|"))[1]
  }
  
  qb$Year <- format(as.Date(qb$Date,"%Y-%m-%d"),"%Y")
  sl$Year <- format(as.Date(sl$Month,"%Y-%m-%d"),"%Y")
  sl$Year <- ifelse(format(as.Date(sl$Month,"%Y-%m-%d"),"%m")==12,as.numeric(sl$Year)+1,sl$Year)
  
  
  #### TO DO, make agency
  sl_agency <- sl[,(sum=sum(as.numeric(`Actualized Cost`))),by=.(Agency,Year)]
  qb_agency <- qb[,(sum=sum(as.numeric(Amount))),by=.(Agency,Year)]
  names(sl_agency) <- c("Agency","Year","Actualized Cost")
  names(qb_agency) <- c("Agency","Year","QB Amount")
  
  ### bind both sl and qb to sl_agency
  sl_agency$`QB Amount` <- qb_agency[match(paste0(sl_agency$Agency,sl_agency$Year),paste0(qb_agency$Agency,qb_agency$Year)),"QB Amount"]
  
  ### write sl agency 
  saveRDS(sl_agency,paste0(getwd(),"/QB_AGENCY.csv"))
  xax.Upload(paste0(getwd(),"/QB_AGENCY.csv"),todir = "/XaxFA/QB_AGENCY/",key=key)
  file.remove(paste0(getwd(),"/QB_AGENCY.csv"))
  
  
  ### write qb process
  saveRDS(qb,paste0(getwd(),"/QB_Process.csv"))
  xax.Upload(paste0(getwd(),"/QB_Process.csv"),todir = "/XaxFA/QB_PROCESS/",key=key)
  file.remove(paste0(getwd(),"/QB_Process.csv"))
  
  ### write SL process
  saveRDS(sl,paste0(getwd(),"/SL_Process.csv"))
  xax.Upload(paste0(getwd(),"/SL_Process.csv"),todir = "/XaxFA/SL_PROCESS/",key=key)
  file.remove(paste0(getwd(),"/SL_Process.csv"))
  
  print("Update finished")
}

import_process_SLQB <- function(){
  print("Importing Processed Files")
  QB_PROCESS <<- readRDS(xax.getFile(paste0("XaxFA/QB_PROCESS/",xax.GetDir("/XaxFA/QB_PROCESS/", key = key)), 
                                 as.file = "", 
                                 tmp = paste0(getwd(),"/QB_PROCESS.csv"), 
                                 key = key))
  SL_PROCESS <<- readRDS(xax.getFile(paste0("XaxFA/SL_PROCESS/",xax.GetDir("/XaxFA/SL_PROCESS/", key = key)), 
                                   as.file = "", 
                                   tmp = paste0(getwd(),"/SL_PROCESS.csv"), 
                                   key = key))
  file.remove(paste0(getwd(),"/SL_PROCESS.csv"))
  file.remove(paste0(getwd(),"/QB_PROCESS.csv"))
}


