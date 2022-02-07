##### Control ! XFT ! #####
XFA_Audit <- function() {
  
  drive.letter <- "S:/Xaxis/Insights & Analytics/Finance/"
  
  # #Create compute instance
  # usrinst <- file.exists(paste0(drive.letter,"/usrinst.txt"))
  # if(!as.logical(usrinst)){
  #   user.inst <- unlist(strsplit(path.expand("~"),split="/"))[3]
  #   write(user.inst, paste0(drive.letter,"/usrinst.txt"))
  #   green <- TRUE
  #   cat(paste0("User: \"",scan(paste0(drive.letter,"/usrinst.txt"), what="character"),"\" is computing the audit. Check back in a few minutes and refresh the data instead of running the code."))
  # } else {
  #   cat(paste0("User: \"",scan(paste0(drive.letter,"/usrinst.txt"), what="character"),"\" is computing the audit. Check back in a few minutes and refresh the data instead of running the code."))
  #   green <- FALSE
  # }
  
  green <- T
  
  switch <- dir.exists("S:/")
  if(as.logical(switch)){
    source("S:/Xaxis/Insights & Analytics/Documentation (Misc)/Coding Projects/XaxFTPRLib/xaxftp.R")
  } else {
    directorymapping <- c("NET USE * /delete /y","NET USE H: \"\\\\TORFPSP01102\\XAX\\users\\%username%\"","NET USE G: \"\\\\TORFPSP01102\\\\MCM\\globalinst\"","NET USE M: \"\\\\TORFPSP01102\\\\MCM\\apps\"","NET USE R: \"\\\\TORFPSP01102\\XAX\\Client\"","NET USE S: \"\\\\TORFPSP01102\\XAX\\Dept\"","net use U: \\\\PSCfpcp00101x.ad.insidemedia.net\\DFS\\Universal /persistent:no")
    file.0 <- path.expand("~/mapr5XaxDrives.bat")
    writeLines(directorymapping,file.0)
    system(path.expand("~/mapr5XaxDrives.bat"))
    file.remove(path.expand("~/mapr5XaxDrives.bat"))
    Sys.sleep(3)
    source("S:/Xaxis/Insights & Analytics/Documentation (Misc)/Coding Projects/XaxFTPRLib/xaxftp.R")
  }
  
  if(as.logical(green)){
    options(warn = -1,scipen = 999)
    # Libraries #
    require(data.table)
    require(RcppRoll)
    require(readxl)
    
    source(paste0(drive.letter,"XAXFinAudit/XFA-Functions.R"))
    #### Header #####
    #sheetpw = Xax_Analytics123
    main.dir <- paste0(drive.letter,"XAXFinAudit/")
    sub.dir <- c("SL_INPUT","QB_INPUT","SF_INPUT","OUTPUT")
    if(!file.exists(paste0(main.dir,"check.txt"))){
      createFolder(maindir = main.dir, subdir = sub.dir)
      write("",paste0(main.dir,"check.txt"))
    }
    
    #if(file.exists(path.expand("~/XFAAudit.bat"))){
    #  source("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/installer.R")
    #}
    
    find.split <- function(x,y){
      x = QB_INPUT$`Memo/Description`[[row]]
      y = ":"
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
    
    ##### Body #####
    # Import data
    
    SL_INPUT <- fread(xax.getFile(paste0("/XaxFa/SL_INPUT/",xax.GetDir("XaxFA/SL_INPUT", key = key)),as.file = ".csv", key = key, tmp = "~/SL_INPUT.csv"), stringsAsFactors = F)
    SF_INPUT <- fread(xax.getFile(paste0("XaxFa/SF_INPUT/",xax.GetDir("XaxFA/SF_INPUT", key = key)),as.file = ".csv", key = key, tmp = "~/SF_INPUT.csv"), stringsAsFactors = F)
    tryCatch({
      choice <- ifelse(interactive(),ifelse(winDialog("yesno","Update a new file?")=="YES",TRUE,FALSE),TRUE)
      if(choice){
        
        ff <- list.files(paste0(drive.letter,"XAXFinAudit/DropBox"), full.names = T)
        if(length(ff)!=0){
          tmp <- fread(ff, stringsAsFactors = F, colClasses = c("IO Number"="character"))
          tmp$Date <- gsub("/","-",tmp$Date)
          structure.tmp <- colnames(tmp)
          structure.orig <- readRDS("~/XFA/structure")
          tt.true <- all(structure.tmp == structure.orig)
          
          if(!tt.true){
            stop()
          }
          
        } else {
          stop()
        }
        
      } 
      
      
      QB_INPUT <- fread("~/QB_INPUT.csv.csv") #fread(xax.getFile(paste0("/XaxFA/",grep("QB_INPUTtmp.csv",xax.GetDir("XaxFA", key = key),value=T)),as.file = ".csv",key = key, tmp = "~/QB_INPUT.csv"), stringsAsFactors = F,colClasses = c("IO Number"="character"))
      
      init <- colnames(QB_INPUT)
      
      if(choice){
        ### Check duplicates from rbind
        QB_INPUT.match <- paste0(QB_INPUT$Date,QB_INPUT$`Memo/Description`)
        tmp_input.match <- paste0(tmp$Date,tmp$`Memo/Description`)
        loc <- which(is.na(match(tmp_input.match,QB_INPUT.match))==TRUE)
        tmp <- as.data.frame(tmp)
        tmp.instance <- rbind(QB_INPUT,tmp[loc,])
        tmp.instance <- as.data.frame(tmp.instance)
        tmp.instance <- tmp.instance[!duplicated(tmp.instance[c("Date","IO Number","Memo/Description","#","Amount")]),]
        
        QB_INPUT <- as.data.table(tmp.instance)
        fwrite(QB_INPUT,"S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/QB_INPUTtmp.csv")
        xax.Upload("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/QB_INPUTtmp.csv",todir="/XaxFA", key = key)
      }
    },  error = function(x){
      winDialog("ok","File must be a CSV file with the correct structure.\n\nContact xaxis.analyticsCanada@xaxis.com for more information.")
      stop("Wrong file.")
    })
    
    fwrite(SL_INPUT,"S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/SL_INPUT.csv")
    
    
    names(SL_INPUT) <- gsub("\\."," ",colnames(SL_INPUT))
    QB_INPUT$`IO Number` <- gsub("\\,","",QB_INPUT$`IO Number`)
    
    #### data cleaner  ####
    
    ### By SF_NETWORKPLACEMENT
    QB_INPUT$Memo_Cut <- ""
    QB_INPUT$Date <- as.Date(QB_INPUT$Date,"%d-%m-%Y")
    cat("Processign QB Data...\n")
    
    pb <- txtProgressBar(min=0, max=nrow(QB_INPUT),style=3)
    for(row in 1:nrow(QB_INPUT)){
      tryCatch({
        QB_INPUT$Memo_Cut[[row]] <- find.split(QB_INPUT$`Memo/Description`[[row]],":")
        setTxtProgressBar(pb, row)
      }, error = function(x){
        NULL
      })
    }
    close(pb)
    
    QB_INPUT$SF_NETWORK_PLACEMENT_NAME <- ""
    QB_Blank <- QB_INPUT[which(QB_INPUT$Memo_Cut==""),]
    
    current.year <- as.numeric((format(Sys.time(),"%Y")))
    last.year <- as.numeric((format(Sys.time(),"%Y")))-1
    next.year <- as.numeric((format(Sys.time(),"%Y")))+1
    remove.months <- c(paste0(month.name,current.year),paste0(month.name,next.year),paste0(month.name,last.year),"Activity")
    non.imp.invoice <- c("overbill","underbill","unbill","credit","adjustment","reissue")
    
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME.ORIGINAL <- SL_INPUT$SF_NETWORK_PLACEMENT_NAME
    QB_INPUT$Memo_Cut <- gsub("\\(","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub("\\(","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    QB_INPUT$Memo_Cut <- gsub("\\)","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub("\\)","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    QB_INPUT$Memo_Cut <- gsub(" ","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub(" ","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    QB_INPUT$Memo_Cut <- gsub("\n","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub("\n","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    QB_INPUT$Memo_Cut <- gsub(paste0(remove.months,collapse = "|"),"",QB_INPUT$Memo_Cut)
    QB_INPUT$Memo_Cut <- gsub("-","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub("-","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    QB_INPUT$Memo_Cut <- gsub("\\+","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub("\\+","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    QB_INPUT$Memo_Cut <- gsub("\\\\","",QB_INPUT$Memo_Cut)
    SL_INPUT$SF_NETWORK_PLACEMENT_NAME <- gsub("\\\\","",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    
    SL_INPUT <- SL_INPUT[-which(SL_INPUT$SF_NETWORK_PLACEMENT_NAME==""),]
    SL_INPUT <- SL_INPUT[-which(SL_INPUT$SF_NETWORK_PLACEMENT_NAME=="PH"),]
    if(length(which(SL_INPUT$SF_NETWORK_PLACEMENT_NAME=="Xaxis"))>=1){
      SL_INPUT <- SL_INPUT[-which(SL_INPUT$SF_NETWORK_PLACEMENT_NAME=="Xaxis"),]
    }
    # SL_INPUT <- SL_INPUT[-which(SL_INPUT$SF_NETWORK_PLACEMENT_NAME=="Xaxis"),]
    SL_INPUT <- SL_INPUT[-which(SL_INPUT$SF_NETWORK_PLACEMENT_NAME=="Other"),]
    SL_INPUT <- SL_INPUT[-which(nchar(SL_INPUT$SF_NETWORK_PLACEMENT_NAME)<=7),]
    
    SL_INPUT$SFNPN.Month <- format(as.Date(SL_INPUT$Month,"%Y-%m-%d")+31,"%Y-%m-01")
    SL_INPUT$SFNPN.Concat <- paste0(SL_INPUT$SFNPN.Month,"--",SL_INPUT$SF_NETWORK_PLACEMENT_NAME)
    
    cat("Processing Spotlight Data")
    
    pb <- txtProgressBar(min=0,max=nrow(SL_INPUT),style=3)
    for(row in 1:nrow(SL_INPUT)){
      if(length(grep(SL_INPUT$SF_NETWORK_PLACEMENT_NAME[[row]],QB_INPUT$Memo_Cut,ignore.case = T))>=1){
        x <- SL_INPUT$SF_NETWORK_PLACEMENT_NAME[[row]]
        loc.P <- grep(x,QB_INPUT$Memo_Cut)
        QB_INPUT[loc.P,"SF_NETWORK_PLACEMENT_NAME"] <- x
        setTxtProgressBar(pb, row)
        # cat(row,SL_INPUT$SF_NETWORK_PLACEMENT_NAME[[row]],"\n")
      }
    }
    close(pb)
    QB_Blank <- QB_INPUT[which(QB_INPUT$SF_NETWORK_PLACEMENT_NAME==""),]
    
    QB_INPUT$SFNPN.Concat <- paste0(QB_INPUT$Date,"--",QB_INPUT$SF_NETWORK_PLACEMENT_NAME)
    
    
    QB_INPUT$`Actualized Cost` <- as.numeric(unlist(SL_INPUT[match(QB_INPUT$SFNPN.Concat,SL_INPUT$SFNPN.Concat),"Actualized Cost"]))
    QB_INPUT$Amount <- as.numeric(gsub(",","",QB_INPUT$Amount))
    QB_INPUT$Date <- as.character(QB_INPUT$Date)
    QB_INPUT[is.na(QB_INPUT)] <- 0
    QB_INPUT$SF_BUDGET <- SL_INPUT[match(QB_INPUT$SFNPN.Concat,SL_INPUT$SFNPN.Concat),"SF_GROSS_BUDGET"]
    difference <- QB_INPUT$`Actualized Cost` - QB_INPUT$Amount
    QB_INPUT$difference <- difference
    SF_NETWORK_PLACEMENT_NAME.ORIGINAL <- SL_INPUT[match(QB_INPUT$SF_NETWORK_PLACEMENT_NAME,SL_INPUT$SF_NETWORK_PLACEMENT_NAME),"SF_NETWORK_PLACEMENT_NAME.ORIGINAL"]
    QB_INPUT$SF_NETWORK_PLACEMENT_NAME.ORIGINAL <- SF_NETWORK_PLACEMENT_NAME.ORIGINAL 
    Nonbilling <- grepl(paste0(non.imp.invoice,collapse = "|"),QB_INPUT$`Memo/Description`, ignore.case = T)
    QB_INPUT$Nonbilling <- Nonbilling
    isSFPlacement <- ifelse(QB_INPUT$Memo_Cut!="",TRUE,FALSE)
    QB_INPUT$isSFPlacement <- isSFPlacement
    
    fwrite(QB_INPUT,"S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/butmp.csv")
    xax.Upload("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/butmp.csv",todir = "/XaxFA",key=key)
    
    ##### groupby By SF ID #####
    actualized.cost <- ""
    amount <- ""
    io.numbers <- unique(QB_INPUT$`IO Number`)
    io.numbers.2 <- unique(QB_INPUT$`IO Number`)
    
    cat("Aggregating...")
    
    pb <- txtProgressBar(min=0,max=length(io.numbers),style=3)
    for(i in 1:length(io.numbers)){
      if(grepl("\\.",io.numbers[i])){
        io.loc <- which(SL_INPUT$SF_OPPORTUNITY_ID==io.numbers[i])
        setTxtProgressBar(pb,i)
        if(length(io.loc)>=1){
          sum.Actualized <- SL_INPUT[io.loc,]
          sum.Actualized <- sum(as.numeric(sum.Actualized$`Actualized Cost`))
          actualized.cost[i] <- sum.Actualized
        } 
        if(nchar(io.numbers.2[i])<=10){
          if(length(io.loc)==0){
            io.numbers[i] <- paste0(io.numbers[i],0,collapse = "")
            io.loc <- which(SL_INPUT$SF_OPPORTUNITY_ID==io.numbers[i])
            if(length(io.loc)>=1){
              sum.Actualized <- SL_INPUT[io.loc,]
              sum.Actualized <- sum(as.numeric(sum.Actualized$`Actualized Cost`))
              actualized.cost[i] <- sum.Actualized
            }
            if(length(io.loc)==0){
              io.numbers[i] <- paste0(io.numbers[i],0,collapse = "")
              io.loc <- which(SL_INPUT$SF_OPPORTUNITY_ID==io.numbers[i])
              if(length(io.loc)>=1){
                sum.Actualized <- SL_INPUT[io.loc,]
                sum.Actualized <- sum(as.numeric(sum.Actualized$`Actualized Cost`))
                actualized.cost[i] <- sum.Actualized
              }
            }
          }
        }
      } 
    }
    close(pb)
    
    pb <- txtProgressBar(min=0,max=length(io.numbers), style=3)
    for(i in 1:length(io.numbers)){
      if(grepl("\\.",io.numbers[i])){
        io.loc <- which(QB_INPUT$`IO Number`==io.numbers.2[i])
        setTxtProgressBar(pb,i)
        # print(io.loc)
        if(length(io.loc)>=1){
          sum.Actualized <- QB_INPUT[io.loc,]
          sum.Actualized <- sum(as.numeric(sum.Actualized$Amount))
          amount[i] <- sum.Actualized
        }
      }
      close(pb)
    }
    
    pivot_bySF_ID <- data.table(cbind(io.numbers,io.numbers.2,amount,actualized.cost), stringsAsFactors = F)
    names(pivot_bySF_ID) <- c("QB IO Number","IO Number","QB_Amount","SL_Amount")
    pivot_bySF_ID$Difference <- round(as.numeric(pivot_bySF_ID$QB_Amount)-as.numeric(pivot_bySF_ID$SL_Amount),2)
    pivot_bySF_ID$SL_Amount <- round(as.numeric(pivot_bySF_ID$SL_Amount),2)
    pivot_bySF_ID[is.na(pivot_bySF_ID)] <- "--"
    fwrite(pivot_bySF_ID, "S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/pivot_bySF_ID.csv")
    xax.Upload("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/pivot_bySF_ID.csv",todir = "/XaxFA",key=key)
    
    ##### 2nd part #####
    #Create concatenation and relations
    QB_INPUT$SF_OPP_ID.ORIGINAL <- pivot_bySF_ID[match(QB_INPUT$`IO Number`,pivot_bySF_ID$`IO Number`),"QB IO Number"]
    QB_INPUT$SFNPN.Concat <- paste0(QB_INPUT$SFNPN.Concat,"--",QB_INPUT$SF_OPP_ID.ORIGINAL)
    SL_INPUT$SFNPN.Concat <- paste0(SL_INPUT$SFNPN.Concat,"--",SL_INPUT$SF_OPPORTUNITY_ID)
    
    QB_INPUT$`Actualized Cost` <- SL_INPUT[match(QB_INPUT$SFNPN.Concat,SL_INPUT$SFNPN.Concat),"Actualized Cost"]
    QB_INPUT$Amount <- gsub(",","",QB_INPUT$Amount)
    QB_INPUT[is.na(QB_INPUT)] <- 0
    QB_INPUT$SF_BUDGET <- SL_INPUT[match(QB_INPUT$SFNPN.Concat,SL_INPUT$SFNPN.Concat),"SF_GROSS_BUDGET"]
    QB_INPUT$Nonbilling <- grepl(paste0(non.imp.invoice,collapse = "|"),QB_INPUT$`Memo/Description`, ignore.case = T)
    QB_INPUT$Difference <- round(as.numeric(QB_INPUT$`Actualized Cost`)-as.numeric(QB_INPUT$Amount),2)
    
    fwrite(QB_INPUT,"S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/QB_Process.csv")
    xax.Upload("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/QB_Process.csv",todir = "/XaxFA",key=key)
    
    
    # #Rename files in SL Input
    # 
    # file.remove("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/SL_INPUT.csv")
    # file.copy(from = list.files("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/SL_INPUT/", full.names = T), to = "S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/SL_INPUT.csv")
    # 
    classify.agency <- ""
    ## Get Agency billings
    agencies <- c("MEC Toronto","Maxus","Mindshare|MindShare","Xaxis Direct","Mediacom|MediaCom")
    
    for(row in 1:nrow(QB_INPUT)){
      if(grepl(agencies[1],QB_INPUT$`Memo/Description`[row])){
        classify.agency[row] <- agencies[1] 
      }
      if(grepl(agencies[2],QB_INPUT$`Memo/Description`[row])){
        classify.agency[row] <- agencies[2] 
      }
      if(grepl(agencies[3],QB_INPUT$`Memo/Description`[row])){
        classify.agency[row] <- agencies[3] 
      }
      if(grepl(agencies[4],QB_INPUT$`Memo/Description`[row])){
        classify.agency[row] <- agencies[4] 
      }
      if(grepl(agencies[5],QB_INPUT$`Memo/Description`[row])){
        classify.agency[row] <- agencies[5] 
      }
      # print(row)
    }
    
    QB_INPUT$Agency <- classify.agency
    QB_INPUT$Year <- format(as.Date(QB_INPUT$Date,"%Y-%m-%d"),"%Y")
    
    SL_INPUT$SF_OPPORTUNITY_NAME <- gsub("Maxus \\(Canada\\)","Maxus Canada",SL_INPUT$SF_OPPORTUNITY_NAME)
    SL_INPUT$SF_OPPORTUNITY_NAME <- gsub("Mediacom \\(Canada\\)","Mediacom Canada",SL_INPUT$SF_OPPORTUNITY_NAME)
    
    by.Agency <- QB_INPUT[,(sum=sum(as.numeric(Amount))),by=.(Agency,Year)]
    by.Agency <- by.Agency[!is.na(by.Agency$Agency),]
    by.Agency$True.Agency <- ""
    for(i in 1:nrow(by.Agency)){
      by.Agency$True.Agency[i] <- unlist(strsplit(by.Agency$Agency[[i]], split = "\\|"))[1]
    }
    
    SL_INPUT$Year <- format(as.Date(SL_INPUT$Month,"%Y-%m-%d"),"%Y")
    SL_INPUT$Year <- ifelse(format(as.Date(SL_INPUT$Month,"%Y-%m-%d"),"%m")==12,as.numeric(SL_INPUT$Year)+1,SL_INPUT$Year)
    SL_INPUT$True.Agency <- ifelse(grepl("Maxus Toronto|Maxus Canada",SL_INPUT$SF_OPPORTUNITY_NAME,ignore.case = T),"Maxus",ifelse(grepl("Mediacom Canada|Mediacom Toronto|Mediacom Montreal",SL_INPUT$SF_OPPORTUNITY_NAME, ignore.case = T),"Mediacom",ifelse(grepl("MEC",SL_INPUT$SF_OPPORTUNITY_NAME),"MEC Toronto",ifelse(grepl("Xaxis",SL_INPUT$SF_OPPORTUNITY_NAME,ignore.case = T),"Xaxis Direct",ifelse(grepl("Mindshare",SL_INPUT$SF_OPPORTUNITY_NAME,ignore.case = T),"Mindshare",NA)))))
    
    SL.by.Agency <- SL_INPUT[,(sum=sum(as.numeric(`Actualized Cost`))),by=.(True.Agency,Year)]
    
    fwrite(SL.by.Agency, "S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/SL_Agency.csv")
    xax.Upload("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/SL_Agency.csv",todir = "/XaxFa", key = key)
    fwrite(by.Agency, "S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/QB_Agency.csv")
    xax.Upload("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/QB_Agency.csv",todir = "/XaxFa", key = key)
    
    file.remove(paste0(drive.letter,"/usrinst.txt"))
    #file.remove(list.files("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/XFA/DropBox/", full.names = T))
    gc()
    
  }
  
}

cat("XFA Active\n")
while(1){
  run <- grep("XFAruncommand",list.files("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/cmd",full.names = T),value=T)
  if(length(run)==0){
    Sys.sleep(5)
  } else {
    log <- scan(list.files("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/cmd",full.names = T),fileEncoding = "UTF-16LE", what = "character")
    file.remove(list.files("S:/Xaxis/Insights & Analytics/Finance/XAXFinAudit/cmd",full.names = T))
    cat(log,"\n\nRunning")
    
    log <- paste0(log,"///",Sys.time())
    write(log,paste0("~/XFA/log/log___",format(Sys.time(),"%Y-%m-%d-%H%M%S")))
    
    print("Processing... XFA")
    XFA_Audit()
  }
  
}






