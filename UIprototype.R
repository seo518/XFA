### initial data imports
source(path.expand("~/xfa/XFA2_0.R"))

### TO DO- put into XFA2_0 later




# Agency data
agency_data <- readRDS(xax.getFile("/XaxFA/QB_AGENCY/QB_AGENCY.csv",as.file = "",tmp = paste0(getwd(),"QB_AGENCY.csv"),key = key))
agency_data <- agency_data[complete.cases(agency_data)]
agency_data$Difference <- round(agency_data$`Actualized Cost` - agency_data$`QB Amount`,2)
# pivotbySF_ID data
id_Data <- readRDS(xax.getFile("/XaxFA/PIVOT/pivot_bySF_ID.csv",as.file = "",tmp = paste0(getwd(),"QB_AGENCY.csv"),key = key))
system.time({
  SL_SF_Import()
  import_process_SLQB()
})

tk.window <- tktoplevel()
tktitle(tk.window) <- "XFA - Xaxis Financial Audit"
tabs_list <- c("Controls","Agency","IO List", "IO Check", "Placement Check")

# Create Tabs
tk.window$env$notebook <- tk2notebook(tk.window, tabs = tabs_list)
tkpack(tk.window$env$notebook, fill = "both", expand = TRUE, pady = c(10,10), padx = c(10,10))

if(!file.exists(paste0(path.expand("~/"),creds$logo$logo_loc))){
  geticon <- GET("https://www.exchangewire.com/wp-content/uploads/2014/02/Xaxis.Logo_.png")
  writeBin(geticon$content,paste0(getwd(),creds$logo$logo_loc))
}

# Create Icon
tryCatch({
  tcl('wm', 'iconphoto', tk.window, tcl('image', 'create', 'photo', "-file", paste0(getwd(),creds$logo$logo_loc)))
}, error = function(x){
  NULL
})

#### Agency Tab ####
tk.window$env$tab1 <- tk2notetab(tk.window$env$notebook, "Agency")
### Year drop down
years_value = tclVar("2017")
tk.window$env$comboBox <- ttkcombobox(tk.window$env$tab1, values = unique(agency_data$Year), textvariable = years_value, state = "readonly")
tkgrid(tk.window$env$comboBox, pady = c(10,10), padx = c(10,10), columnspan = 2)
tk.window$env$but <- tk2button(tk.window$env$tab1, text = "View",
                               command = call_agency_table)
tk.window$env$agency_export_but <- tk2button(tk.window$env$tab1, text = "Export Agencies",
                                             command = call_agency_table_export)
tkgrid(tk.window$env$but, padx = c(10,0),pady = c(10,10), sticky = "w")
tkgrid(tk.window$env$agency_export_but, padx = c(10,0),pady = c(10,10), sticky = "w")


#### Controls Tab ####
tk.window$env$tab2 <- tk2notetab(tk.window$env$notebook, "Controls")
tk.window$env$controls_button <- tk2button(tk.window$env$tab2, text = "QB Update", command = QB_Update)
tk.window$env$exports_button <- tk2button(tk.window$env$tab2, text = "Export Files", command = export_data)
tkgrid(tk.window$env$controls_button, pady = c(10,10), padx = c(10,10), columnspan = 2)
tkgrid(tk.window$env$exports_button, padx = c(10,10), sticky = "w")

#### IO List Tab ####
tk.window$env$tab3 <- tk2notetab(tk.window$env$notebook, "IO List")
tk.window$env$iolist_view <- tk2button(tk.window$env$tab3, text = "View",
                                       command = call_ioList_table)
tk.window$env$iolist_export <- tk2button(tk.window$env$tab3, text = "Export List",
                                         command = call_ioList_table_export)
tkgrid(tk.window$env$iolist_view , padx = c(10,0),pady = c(10,10), sticky = "w")
tkgrid(tk.window$env$iolist_export, padx = c(10,0),pady = c(10,10), sticky = "w")

#### IO Checker ####
### <<< Will only appear in IO Checker if it exists in Quickbooks data base. Otherwise, update with new file.
tk.window$env$tab4 <- tk2notetab(tk.window$env$notebook, "IO Check")
tkgrid(tk2label(tk.window$env$tab4,text="Individual IO Number Check:"), columnspan = 20)
tkgrid(tk2label(tk.window$env$tab4,text="If there is error, data is not in QB database"), columnspan = 20)
tkgrid(tk2label(tk.window$env$tab4,text=""))

ID1 <- tclVar("")
ID2 <- tclVar("")
ID3 <- tclVar("")
ID4 <- tclVar("")
ID5 <- tclVar("")

IDentry1 <- tk2entry(tk.window$env$tab4, textvariable=ID1, width="25")
IDentry2 <- tk2entry(tk.window$env$tab4, textvariable=ID2, width="25")
IDentry3 <- tk2entry(tk.window$env$tab4, textvariable=ID3, width="25")
IDentry4 <- tk2entry(tk.window$env$tab4, textvariable=ID4, width="25")
IDentry5 <- tk2entry(tk.window$env$tab4, textvariable=ID5, width="25")

tkgrid(tk2label(tk.window$env$tab4,text="ID:", justify = "left"), IDentry1, padx = 0, pady = 0)
tkgrid(tk2label(tk.window$env$tab4,text="ID:", justify = "left"), IDentry2, padx = 0, pady = 0)
tkgrid(tk2label(tk.window$env$tab4,text="ID:", justify = "left"), IDentry3, padx = 0, pady = 0)
tkgrid(tk2label(tk.window$env$tab4,text="ID:", justify = "left"), IDentry4, padx = 0, pady = 0)
tkgrid(tk2label(tk.window$env$tab4,text="ID:", justify = "left"), IDentry5, padx = 0, pady = 0)

tkgrid(tk2label(tk.window$env$tab4,text=""))
tk.window$env$Evaluate_Button <- tk2button(tk.window$env$tab4, text = "Evaluate",
                                         command = evaluate_iocheck)
tkgrid(tk.window$env$Evaluate_Button,  columnspan = 20)
tkgrid(tk2label(tk.window$env$tab4,text=""))

tkgrid(tk2label(tk.window$env$tab4,text="ID"),
       tk2label(tk.window$env$tab4,text="QB Amount"),
       tk2label(tk.window$env$tab4,text="SL Amount"),
       tk2label(tk.window$env$tab4,text="Difference"))

ID1_QBAmount <- tclVar("0")
ID1_SLAmount <- tclVar("0")
ID1_Difference <- tclVar("0")
ID2_QBAmount <- tclVar("0")
ID2_SLAmount <- tclVar("0")
ID2_Difference <- tclVar("0")
ID3_QBAmount <- tclVar("0")
ID3_SLAmount <- tclVar("0")
ID3_Difference <- tclVar("0")
ID4_QBAmount <- tclVar("0")
ID4_SLAmount <- tclVar("0")
ID4_Difference <- tclVar("0")
ID5_QBAmount <- tclVar("0")
ID5_SLAmount <- tclVar("0")
ID5_Difference <- tclVar("0")

id_1_data_ID <- tklabel(tk.window$env$tab4, textvariable = ID1)
id_1_data_QBAmount <- tklabel(tk.window$env$tab4, textvariable = ID1_QBAmount)
id_1_data_SLAmount <- tklabel(tk.window$env$tab4, textvariable = ID1_SLAmount)
id_1_data_Difference <- tklabel(tk.window$env$tab4, textvariable = ID1_Difference)
tkgrid(id_1_data_ID,id_1_data_QBAmount,id_1_data_SLAmount,id_1_data_Difference )
id_2_data_ID <- tklabel(tk.window$env$tab4, textvariable = ID2)
id_2_data_QBAmount <- tklabel(tk.window$env$tab4, textvariable = ID2_QBAmount)
id_2_data_SLAmount <- tklabel(tk.window$env$tab4, textvariable = ID2_SLAmount)
id_2_data_Difference <- tklabel(tk.window$env$tab4, textvariable = ID2_Difference)
tkgrid(id_2_data_ID,id_2_data_QBAmount,id_2_data_SLAmount,id_2_data_Difference )
id_3_data_ID <- tklabel(tk.window$env$tab4, textvariable = ID3)
id_3_data_QBAmount <- tklabel(tk.window$env$tab4, textvariable = ID3_QBAmount)
id_3_data_SLAmount <- tklabel(tk.window$env$tab4, textvariable = ID3_SLAmount)
id_3_data_Difference <- tklabel(tk.window$env$tab4, textvariable = ID3_Difference)
tkgrid(id_3_data_ID,id_3_data_QBAmount,id_3_data_SLAmount,id_3_data_Difference )
id_4_data_ID <- tklabel(tk.window$env$tab4, textvariable = ID4)
id_4_data_QBAmount <- tklabel(tk.window$env$tab4, textvariable = ID4_QBAmount)
id_4_data_SLAmount <- tklabel(tk.window$env$tab4, textvariable = ID4_SLAmount)
id_4_data_Difference <- tklabel(tk.window$env$tab4, textvariable = ID4_Difference)
tkgrid(id_4_data_ID,id_4_data_QBAmount,id_4_data_SLAmount,id_4_data_Difference )
id_5_data_ID <- tklabel(tk.window$env$tab4, textvariable = ID5)
id_5_data_QBAmount <- tklabel(tk.window$env$tab4, textvariable = ID5_QBAmount)
id_5_data_SLAmount <- tklabel(tk.window$env$tab4, textvariable = ID5_SLAmount)
id_5_data_Difference <- tklabel(tk.window$env$tab4, textvariable = ID5_Difference)
tkgrid(id_5_data_ID,id_5_data_QBAmount,id_5_data_SLAmount,id_5_data_Difference )

#### Placement Check ####
tk.window$env$tab5 <- tk2notetab(tk.window$env$notebook, "Placement Check")
tkgrid(tk2label(tk.window$env$tab5,text=""))
tkgrid(tk2label(tk.window$env$tab5,text="Placement:"), columnspan = 20)
tkgrid(tk2label(tk.window$env$tab5,text=""))
placement_id <- tclVar("")
placement_check_box <- tk2entry(tk.window$env$tab5, textvariable=placement_id, width="25")
tkgrid(tk2label(tk.window$env$tab5,text="ID:", justify = "left"), placement_check_box , padx = 0, pady = 0)

tk.window$env$placement_check_button <- tk2button(tk.window$env$tab5, text = "Check",
                                           command = call_placement_check)
tk.window$env$placement_exportall_button <- tk2button(tk.window$env$tab5, text = "Export All",
                                                  command = call_exportall_placements)
tkgrid(tk2label(tk.window$env$tab5,text=""))
tkgrid(tk.window$env$placement_check_button, columnspan = 20)
tkgrid(tk.window$env$placement_exportall_button, columnspan = 20)





tkwait.window(tk.window)

# 
# 
# 
# sng <- tclVar("[song name]")
# tk.window$env$label <- tk2label(tk.window, textvariable= sng)
# tkgrid(tk.window$env$label, pady = c(5,5), padx = c(50), columnspan = 2)
# 



