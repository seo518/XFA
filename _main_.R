### XFA Version 2 ##########  _MAIN_ ##############
#' @author : Miguel Ajon
#' @purpose : Connect and integrate QB, SF, and SL into one platform.
#' 
#' 

library(tcltk2)

win1 <- tktoplevel()

# Create two tabs
win1$env$nb <- tk2notebook(win1, tabs = c("Controls", ""))
tkpack(win1$env$nb, fill = "both", expand = TRUE, padx = c(0,500))

# Populate these tabs with various widgets
win1$env$tab_<- tk2notetab(win1$env$nb, "Test")
win1$env$lab <- tk2label(win1$env$nottab, text = "Nothing here.")
tkpack(win1$env$lab)

win1$env$tb2 <- tk2notetab(win1$env$nb, "Button")
win1$env$but <- tk2button(win1$env$tb2, text = "Click me",
                          command = function() tkdestroy(win1))
# You can use a different manager than for the notebook
tkgrid(win1$env$but, padx = 50, pady = 30)

# Select a tab programmatically
tk2notetab.select(win1$env$nb, "Button")
tk2notetab.text(win1$env$nb) # Text of the currently selected tab
## [1] "Button"