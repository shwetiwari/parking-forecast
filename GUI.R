if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if (!require("shinydashboard")) install.packages("shinydashboard", dependencies = TRUE)
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)

library(shiny)
library(leaflet)
library(shinydashboard)
library(lubridate)

#intializes global variables when they don't exist.
if (!exists("inputdata")){
  inputdata <<- read.csv2(file.choose(), header = TRUE, row.names = 1)
  occupiedSpaces <<- inputdata[, seq(2, ncol(inputdata), 4)]
  capacity <<- inputdata[, seq(4, ncol(inputdata), 4)]
  latitudes <<- occupiedSpaces[1,]
  longitudes <<- occupiedSpaces[2,]
  parkingGarages <<- colnames(occupiedSpaces)
  startingDate <<- as.POSIXct(occupiedSpaces[3,1], origin = "1970-01-01")  
  endingDate <<- startingDate+3600*(length(occupiedSpaces[,1])-4)
  occupiedSpaces<<-occupiedSpaces[4:length(occupiedSpaces[,1]),]
  capacity<<-capacity[4:length(capacity[,1]),]
  remove(hardStartHour)
  remove(hardEndHour)
  remove(hardStartdate)
  remove(hardEnddate)
}
if (!exists("isValid")){
  isValid <<- TRUE
}
if (!exists("hardStartHour") | !isValid){
  hardStartHour <<- hour(startingDate)
}
if (!exists("hardEndHour") | !isValid){
  hardEndHour <<- hour(endingDate)
}
if (!exists("hardStartdate") | !isValid){
  hardStartdate <<- as.Date(startingDate)
}
if (!exists("hardEnddate") | !isValid){
  hardEnddate <<- as.Date(endingDate)
}
if (!exists("startHour") | !isValid){
  startHour <<-hardStartHour
}
if (!exists("endHour") | !isValid){
  endHour <<- hardEndHour
}
if (!exists("startdate") | !isValid){
  startdate <<- hardStartdate
}
if (!exists("enddate") | !isValid){
  enddate <<- hardEnddate
}
if (!exists("checkboxesOutput")){
  checkboxesOutput <<- c("Graph", "Statistics", "Bottlenecks")
}
if (!exists("checkboxesActive")){
  checkboxesActive <<- ""
}
if (!exists("showOnlySelected")){
  showOnlySelected <<- FALSE
}
if (!exists("isFullScreen")){
  isFullScreen <<- FALSE
}


#functions that are used in the ui
initializeParkingSelectCheckboxes <- function(checkboxAll, checkboxNone, checkboxGroup, active){
  showModal(modalDialog(
    title = "Select parking locations",
    checkboxInput(checkboxAll, "All", FALSE),
    checkboxInput(checkboxNone, "None", FALSE),
    checkboxGroupInput(checkboxGroup, "",
                       parkingGarages,
                       selected = unlist(active)),
    easyClose = TRUE,
    footer = NULL
  ))
}

selectAll <- function(allClicked, checkboxAll, checkboxGroup, session){
  if (length(allClicked) > 0){
    if (allClicked) {
      updateCheckboxGroupInput(session,checkboxGroup,choices=parkingGarages,selected=unlist(parkingGarages)) #Turns on all parking garage chekboxes when All is selected
      updateCheckboxGroupInput(session,checkboxAll,choices="All",selected=unlist("")) #Turns off All checkbox afterwards
    }
  }
}
selectNone <- function(noneClicked, checkboxNone, checkboxGroup, session){
  if (length(noneClicked) > 0){
    if (noneClicked) {
      updateCheckboxGroupInput(session,checkboxGroup,choices=parkingGarages,selected=unlist("")) #Turns off all parking garage chekboxes when None is selected
      updateCheckboxGroupInput(session,checkboxNone,choices="None",selected=unlist("")) #Turns off None checkbox afterwards
    }
  }
}

checkValidDateTime <- function(startHour, endHour, dates){
  isValid <<- FALSE
  startHourInput = suppressWarnings(as.integer(startHour)) #tries to convert to integer, if not it will convert to NA (this is intended so the warning if NA's are created is supressed)
  endHourInput = suppressWarnings(as.integer(endHour))
  if(is.na(startHourInput)){
    message = "Start hour is not a valid number"
  }else if(is.na(endHourInput)){
    message = "End hour is not a valid number"
  }else if(is.na(dates[1])){
    message = "Startdate is not a valid date"
  }else if(is.na(dates[2])){
    message = "Enddate is not a valid date"
  }else if(startHourInput < 0){
    message = "Start hour can not be smaller than 0"
  }else if(startHourInput > 23){
    message = "Start hour can not be bigger than 23"
  }else if(as.Date(dates[1], origin = "1970-01-01") == hardStartdate & startHourInput < as.integer(hardStartHour)){
    message = "Start hour is before start predictions"
  }else if(as.Date(dates[1], origin = "1970-01-01") == hardEnddate & startHourInput > as.integer(hardEndHour)){
    message = "Start hour is after end predictions"
  }else if(endHourInput < 0){
    message = "End hour can not be smaller than 0"
  }else if(endHourInput > 23){
    message = "End hour can not be bigger than 23"
  }else if(as.Date(dates[2], origin = "1970-01-01") == hardStartdate & endHourInput < as.integer(hardStartHour)){
    message = "End hour is before start predictions"
  }else if(as.Date(dates[2], origin = "1970-01-01") == hardEnddate & endHourInput > as.integer(hardEndHour)){
    message = "End hour is after end predictions"
  }else if(as.Date(dates[2], origin = "1970-01-01") < as.Date(dates[1], origin = "1970-01-01") | (as.Date(dates[2], origin = "1970-01-01") == as.Date(dates[1], origin = "1970-01-01") & endHourInput < startHourInput)){
    message = "Endtime is before startime"
  }else{ #a valid date-time range is given
    message = ""
    isValid <<- TRUE
  }
  
  message
}

ui <- fluidPage(
  conditionalPanel(
    condition = "output.goFullScreen == '0'",  
    fluidRow( #allows to set the position of the panels
      heigth=12,
      column(width=4, #is width/12 of the screen
             tabsetPanel(type = 'tabs',
                         id = "tabs",
                         tabPanel("Forecast",
                                  span(actionButton("fullScreen", ">"),
                                       style = "position:absolute;right:2em;"),
                                  br(), #adds blank line
                                  actionButton("parkingSelect", "Select parking garages"),
                                  checkboxInput("checkboxShowOnly", "Show only selected parking garages in map", FALSE), #FALSE is off on startup
                                  br(),
                                  dateRangeInput("dateRange", "Choose the date range:", #adds calander
                                                 start = startdate, #Starting value beginning of range
                                                 end = enddate, #Starting value end of range
                                                 min = hardStartdate, #Date which can't be selected before
                                                 max = hardEnddate, #Date which can't be selected after
                                                 format = "dd/mm/yyyy",
                                                 separator = "-"),
                                  fluidRow( #places the input boxes next to eacother with set distance
                                    box(width = 12,
                                        splitLayout(
                                          cellWidths = 160,      
                                          textInput("startHourInput", "Start hour", startHour, "45px"),
                                          textInput("endHourInput", "End hour", endHour, "45px")
                                        )
                                    )
                                  ),
                                  textOutput("validDateTime"), #tells the user why a date is invalid
                                  br(),
                                  checkboxGroupInput("checkboxOutputChoice", label = "Show which forecast(s)?", choices = c("Graph", "Bottlenecks", "Statistics"), selected = checkboxesOutput, inline = TRUE), #places the three boxes next to eachother (inline) and already selected
                                  uiOutput("plots")
                         ),
                         tabPanel("Export to CSV"),
                         tabPanel("Select data"),
                         tabPanel("Exit" )
             )
      )
      , column(width=8,
               fixedPanel(top = 0, right = 0, width = "66.666%",leafletOutput("AmsterdamMap",height = "100vh")), #draws the map
               fixedPanel(bottom = 20, right = 40,  uiOutput("slider"), width = "60%"
               ))
    )
  ),
  conditionalPanel(
    condition = "output.goFullScreen == '1'",
    fluidRow( #allows to set the position of the panels
      heigth=12,
      column(width=12, #is width/12 of the screen
             tabsetPanel(type = 'tabs',
                         id = "tabsFS",
                         tabPanel("Forecast",
                                  span(actionButton("fullScreenFS", "<"),
                                       style = "position:absolute;right:2em;"),
                                  actionButton("parkingSelectFS", "Select parking garages"),
                                  br(),
                                  dateRangeInput("dateRangeFS", "Choose the date range:", #adds calander
                                                 start = startdate, #Starting value beginning of range
                                                 end = enddate, #Starting value end of range
                                                 min = startdate, #Date which can't be selected before
                                                 max = enddate, #Date which can't be selected after
                                                 format = "dd/mm/yyyy",
                                                 separator = "-"),
                                  fluidRow( #places the input boxes next to eacother with set distance
                                    box(width = 12,
                                        splitLayout(
                                          cellWidths = 160,      
                                          textInput("startHourInputFS", "Start hour", startHour, "45px"),
                                          textInput("endHourInputFS", "End hour", endHour, "45px")
                                        )
                                    )
                                  ),
                                  textOutput("validDateTimeFS"), #tells the user why a date is invalid
                                  br(),
                                  checkboxGroupInput("checkboxOutputChoiceFS", label = "Show which forecast(s)?", choices = c("Graph", "Bottlenecks", "Statistics"), selected = checkboxesOutput, inline = TRUE), #places the three boxes next to eachother (inline) and already selected
                                  uiOutput("plotsFS")
                         ),
                         tabPanel("Export to CSV"),
                         tabPanel("Select data"),
                         tabPanel("Exit" )
             )
      )          
    )          
  )
)

server <- function(input, output, session) {
  reactiveVals <- reactiveValues( #makes a variable that can be used in all functions of the server
    mapColours = "",
    mapValues = "",
    firstIteration = TRUE,
    updateNextFrame = TRUE,
    forceUpdate = TRUE
  )
  
  observeEvent(input$tabs, {
    
    if (input$tabs=="Export to CSV"){
      if(isValid){
        found = 0 #counter that keeps track of the number of selected locations that have been found untill now
        #The offset of the selected begin and end date/time (in hours) from the begin end date/time is calculated
        beginTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardStartdate, origin = "1970-01-01"), as.integer(hardStartHour)), format="%Y-%m-%d %H"), units = c("hours"))
        endTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRange[2], origin = "1970-01-01"), as.integer(input$endHourInput)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardEnddate, origin = "1970-01-01"), as.integer(hardEndHour)), format="%Y-%m-%d %H"), units = c("hours"))
        if (length(checkboxesActive) > 0){ #when at least one location is selected
          bottlenecks = matrix(nrow=0, ncol=3)
          for (occupiedSpacesIndex in 1:(length(occupiedSpaces[1,]))){
            for (checkboxIndex in 1:length(checkboxesActive)){
              if (toString(checkboxesActive[checkboxIndex]) == toString(colnames(occupiedSpaces)[occupiedSpacesIndex])){
                found=found+1 #the index of a selected location is found
                localOccupiedSpacesIndex = occupiedSpacesIndex #a local copy is made. Since the plots only process when the function is done and have only the call stored, not the variable value at that time, if there is not local copy, only the last location will be used for every plot and graph.
                
                #the user selected range is taken
                data = occupiedSpaces[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                capacityData = capacity[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                
                #the bottlenecks are calculated
                occupancyPercentage = data/capacityData
                occupancyIndex=which(occupancyPercentage>0.95)
                bottleneckIndex = 1
                if (length(occupancyIndex) > 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=3)
                  while (bottleneckIndex<(length(occupancyIndex))){
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    while(as.integer(occupancyIndex[bottleneckIndex]) == (occupancyIndex[bottleneckIndex+1]-1)&bottleneckIndex<(length(occupancyIndex)-1)){
                      bottleneckIndex=bottleneckIndex+1
                    }
                    bottlenecksAdd[1,3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,1]=colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                    bottleneckIndex=bottleneckIndex+1
                  }
                  if(as.integer(occupancyIndex[bottleneckIndex]) != (occupancyIndex[bottleneckIndex-1]+1)){
                    bottlenecksAdd = matrix(nrow=1, ncol=3)
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,1]=colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                  }else{
                    bottlenecks[length(bottlenecks[,3]),3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  }
                  
                }else if(length(occupancyIndex) == 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=3)
                  bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecksAdd[1,3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecksAdd[1,1]=colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                  bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                }
              }
            }
          }
          startpoint = as.character(as.POSIXct(as.integer(bottlenecks[,2]), origin = "1970-01-01"))
          endpoint = as.character(as.POSIXct(as.integer(bottlenecks[,3]), origin = "1970-01-01"))
          bottleNeckTable = data.frame(bottlenecks[,1], startpoint, endpoint)
          colnames(bottleNeckTable) = c("garage", "start point", "end point")
        }
      }
      write.csv(bottleNeckTable,file = file.choose())
      updateTabsetPanel(session, "tabs", selected = "Forecast")
    }
    else if(input$tabs=="Select data"){
      testdata <<- read.csv2(file.choose(), header = TRUE)
      updateTabsetPanel(session, "tabs", selected = "Forecast")
    }
    else if(input$tabs=="Exit"){
      stopApp()
    }
  })
  
  observeEvent(input$tabsFS, {
    
    if (input$tabsFS=="Export to CSV"){
      if(isValid){
        found = 0 #counter that keeps track of the number of selected locations that have been found untill now
        #The offset of the selected begin and end date/time (in hours) from the begin end date/time is calculated
        beginTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardStartdate, origin = "1970-01-01"), as.integer(hardStartHour)), format="%Y-%m-%d %H"), units = c("hours"))
        endTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRange[2], origin = "1970-01-01"), as.integer(input$endHourInput)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardEnddate, origin = "1970-01-01"), as.integer(hardEndHour)), format="%Y-%m-%d %H"), units = c("hours"))
        if (length(checkboxesActive) > 0){ #when at least one location is selected
          bottlenecks = matrix(nrow=0, ncol=3)
          for (occupiedSpacesIndex in 1:(length(occupiedSpaces[1,]))){
            for (checkboxIndex in 1:length(checkboxesActive)){
              if (toString(checkboxesActive[checkboxIndex]) == toString(colnames(occupiedSpaces)[occupiedSpacesIndex])){
                found=found+1 #the index of a selected location is found
                localOccupiedSpacesIndex = occupiedSpacesIndex #a local copy is made. Since the plots only process when the function is done and have only the call stored, not the variable value at that time, if there is not local copy, only the last location will be used for every plot and graph.
                
                #the user selected range is taken
                data = occupiedSpaces[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                capacityData = capacity[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                
                #the bottlenecks are calculated
                occupancyPercentage = data/capacityData
                occupancyIndex=which(occupancyPercentage>0.95)
                bottleneckIndex = 1
                if (length(occupancyIndex) > 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=3)
                  while (bottleneckIndex<(length(occupancyIndex))){
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    while(as.integer(occupancyIndex[bottleneckIndex]) == (occupancyIndex[bottleneckIndex+1]-1)&bottleneckIndex<(length(occupancyIndex)-1)){
                      bottleneckIndex=bottleneckIndex+1
                    }
                    bottlenecksAdd[1,3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,1]=colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                    bottleneckIndex=bottleneckIndex+1
                  }
                  if(as.integer(occupancyIndex[bottleneckIndex]) != (occupancyIndex[bottleneckIndex-1]+1)){
                    bottlenecksAdd = matrix(nrow=1, ncol=3)
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,1]=colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                  }else{
                    bottlenecks[length(bottlenecks[,3]),3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  }
                  
                }else if(length(occupancyIndex) == 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=3)
                  bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecksAdd[1,3]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecksAdd[1,1]=colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                  bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                }
              }
            }
          }
          startpoint = as.character(as.POSIXct(as.integer(bottlenecks[,2]), origin = "1970-01-01"))
          endpoint = as.character(as.POSIXct(as.integer(bottlenecks[,3]), origin = "1970-01-01"))
          bottleNeckTable = data.frame(bottlenecks[,1], startpoint, endpoint)
          colnames(bottleNeckTable) = c("garage", "start point", "end point")
        }
      }
      write.csv(bottleNeckTable,file = file.choose())
      updateTabsetPanel(session, "tabsFS", selected = "Forecast")
    }
    else if(input$tabsFS=="Select data"){
      testdata <<- read.csv2(file.choose(), header = TRUE)
      updateTabsetPanel(session, "tabsFS", selected = "Forecast")
    }
    else if(input$tabsFS=="Exit"){
      stopApp()
    }
  })
  
  observeEvent(reactiveVals$firstIteration, {
    if (reactiveVals$firstIteration){
      isValid <<- FALSE
      updateTextInput(session, inputId = "startHourInput", value = startHour)
      updateTextInput(session, inputId = "endHourInput", value = endHour)
      updateDateRangeInput(session, inputId = "dateRange", start = startdate, end = enddate)
      updateCheckboxGroupInput(session, inputId = "checkboxGroup", selected=unlist(checkboxesActive))
      updateCheckboxGroupInput(session, inputId = "checkboxOutputChoice", selected=unlist(checkboxesOutput))
      updateCheckboxInput(session, inputId = "checkboxShowOnly", value=showOnlySelected)
      
      updateTextInput(session, inputId = "startHourInputFS", value = startHour)
      updateTextInput(session, inputId = "endHourInputFS", value = endHour)
      updateDateRangeInput(session, inputId = "dateRangeFS", start = startdate, end = enddate)
      updateCheckboxGroupInput(session, inputId = "checkboxGroupFS", selected=unlist(checkboxesActive))
      updateCheckboxGroupInput(session, inputId = "checkboxOutputChoiceFS", selected=unlist(checkboxesOutput))
    }  
  })
  
  #initializes checkboxes
  observeEvent(input$parkingSelect,{
    if(!isFullScreen){
      initializeParkingSelectCheckboxes("checkboxAll", "checkboxNone", "checkboxGroup", checkboxesActive)
    }
  })
  
  #initializes checkboxes fullscreen
  observeEvent(input$parkingSelectFS, {
    if(isFullScreen){
      initializeParkingSelectCheckboxes("checkboxAllFS", "checkboxNoneFS", "checkboxGroupFS", checkboxesActive)
    }
  })
  
  #set all checkboxes
  observeEvent(input$checkboxAll, {
    if(!isFullScreen){
      selectAll(input$checkboxAll, "checkboxAll", "checkboxGroup", session)
    }
  })
  
  #set all checkboxes fullscreen
  observeEvent(input$checkboxAllFS, {
    if(isFullScreen){
      selectAll(input$checkboxAllFS, "checkboxAllFS", "checkboxGroupFS", session)
    }
  })
  
  #reset all checkboxes
  observeEvent(input$checkboxNone, {
    if(!isFullScreen){
      selectNone(input$checkboxNone, "checkboxNone", "checkboxGroup", session)
    }
  })
  
  #reset all checkboxes fullscreen
  observeEvent(input$checkboxNoneFS, {
    if(isFullScreen){
      selectNone(input$checkboxNoneFS, "checkboxNoneFS", "checkboxGroupFS", session)
    }
  })
  
  observeEvent(input$checkboxShowOnly, {
    if(!reactiveVals$firstIteration){
      showOnlySelected <<-input$checkboxShowOnly
    }
  })
  
  
  #checks validity of date and time input
  observeEvent(reactiveVals$updateNextFrame, {
    if(reactiveVals$updateNextFrame>0 & reactiveVals$updateNextFrame<=5){
      message = checkValidDateTime(input$startHourInput, input$endHourInput, input$dateRange)
      
      if(isValid){
        message = checkValidDateTime(input$startHourInputFS, input$endHourInputFS, input$dateRangeFS)
      }
      output$validDateTime = renderText({ #the message is given to the ui
        message
      })
      output$validDateTimeFS = renderText({ #the message is given to the ui
        message
      })
      reactiveVals$updateNextFrame = reactiveVals$updateNextFrame +1
    }else{
      reactiveVals$updateNextFrame=0
    }
    if (reactiveVals$updateNextFrame==4){
      reactiveVals$forceUpdate = !reactiveVals$forceUpdate
    }
    
  }, ignoreNULL = FALSE)
  
  
  #copies values from not fullscreen to fullscreen and vice versa
  observeEvent(input$startHourInput, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(!isFullScreen){
      if (input$startHourInput == "min"){
        updateTextInput(session, inputId = "startHourInput", value = hardStartHour)
        updateTextInput(session, inputId = "startHourInputFS", value = hardStartHour)
        updateDateRangeInput(session, inputId = "dateRange", start = hardStartdate)
        updateDateRangeInput(session, inputId = "dateRangeFS", start = hardStartdate)
      }else if(input$startHourInput == "max"){
        updateTextInput(session, inputId = "startHourInput", value = hardEndHour)
        updateTextInput(session, inputId = "startHourInputFS", value = hardEndHour)
        updateDateRangeInput(session, inputId = "dateRange", start = hardEnddate)
        updateDateRangeInput(session, inputId = "dateRangeFS", start = hardEnddate)
      }else{
        if (startHour!=input$startHourInput){
          startHour <<- input$startHourInput
          updateTextInput(session, inputId = "startHourInputFS", value = startHour)}
      }
    }
  })
  
  observeEvent(input$startHourInputFS, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(isFullScreen){
      if (input$startHourInputFS == "min"){
        updateTextInput(session, inputId = "startHourInputFS", value = hardStartHour)
        updateTextInput(session, inputId = "startHourInput", value = hardStartHour)
        updateDateRangeInput(session, inputId = "dateRangeFS", start = hardStartdate)
        updateDateRangeInput(session, inputId = "dateRange", start = hardStartdate)
      }else if(input$startHourInputFS == "max"){
        updateTextInput(session, inputId = "startHourInputFS", value = hardEndHour)
        updateTextInput(session, inputId = "startHourInput", value = hardEndHour)
        updateDateRangeInput(session, inputId = "dateRangeFS", start = hardEnddate)
        updateDateRangeInput(session, inputId = "dateRange", start = hardEnddate)
      }else{
        startHour <<- input$startHourInputFS
        updateTextInput(session, inputId = "startHourInput", value = startHour)
      }
    }
  })
  
  observeEvent(input$endHourInput, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(!isFullScreen){
      if (input$endHourInput == "min"){
        updateTextInput(session, inputId = "endHourInput", value = hardStartHour)
        updateTextInput(session, inputId = "endHourInputFS", value = hardStartHour)
        updateDateRangeInput(session, inputId = "dateRange", end = hardStartdate)
        updateDateRangeInput(session, inputId = "dateRangeFS", end = hardStartdate)
      }else if(input$endHourInput == "max"){
        updateTextInput(session, inputId = "endHourInput", value = hardEndHour)
        updateTextInput(session, inputId = "endHourInputFS", value = hardEndHour)
        updateDateRangeInput(session, inputId = "dateRange", end = hardEnddate)
        updateDateRangeInput(session, inputId = "dateRangeFS", end = hardEnddate)
      }else{
        endHour <<- input$endHourInput
        updateTextInput(session, inputId = "endHourInputFS", value = endHour)
      }
    }
  })
  
  observeEvent(input$endHourInputFS, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(isFullScreen){
      if (input$endHourInputFS == "min"){
        updateTextInput(session, inputId = "endHourInputFS", value = hardStartHour)
        updateTextInput(session, inputId = "endHourInput", value = hardStartHour)
        updateDateRangeInput(session, inputId = "dateRangeFS", end = hardStartdate)
        updateDateRangeInput(session, inputId = "dateRange", end = hardStartdate)
      }else if(input$endHourInput == "max"){
        updateTextInput(session, inputId = "endHourInputFS", value = hardEndHour)
        updateTextInput(session, inputId = "endHourInput", value = hardEndHour)
        updateDateRangeInput(session, inputId = "dateRangeFS", end = hardEnddate)
        updateDateRangeInput(session, inputId = "dateRange", end = hardEnddate)
      }else{
        endHour <<- input$endHourInputFS
        updateTextInput(session, inputId = "endHourInput", value = endHour)
      }
    }
  })
  
  observeEvent(input$dateRange, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(!isFullScreen){
      startdate <<- input$dateRange[1]
      enddate <<- input$dateRange[2]
      updateDateRangeInput(session, inputId = "dateRangeFS", start = startdate, end = enddate)
    }
  })
  
  observeEvent(input$dateRangeFS, {
    reactiveVals$updateNextFrame = 1
    isValid <<- FALSE
    if(isFullScreen){
      startdate <<- input$dateRangeFS[1]
      enddate <<- input$dateRangeFS[2]
      updateDateRangeInput(session, inputId = "dateRange", start = startdate, end = enddate)
    }
  })
  
  observeEvent(input$checkboxGroup, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(!isFullScreen){
      if(!reactiveVals$firstIteration){
        checkboxesActive <<- input$checkboxGroup
        updateCheckboxGroupInput(session, inputId = "checkboxGroupFS", selected=unlist(checkboxesActive))
      }
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$checkboxGroupFS, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    if(isFullScreen){
      if(!reactiveVals$firstIteration){
        checkboxesActive <<- input$checkboxGroupFS
        updateCheckboxGroupInput(session, inputId = "checkboxGroup", selected=unlist(checkboxesActive))
      }
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$checkboxOutputChoice, {
    if(!isFullScreen){
      checkboxesOutput <<- input$checkboxOutputChoice
      updateCheckboxGroupInput(session, inputId = "checkboxOutputChoiceFS", selected=unlist(checkboxesOutput))
    }
  })
  
  observeEvent(input$checkboxOutputChoiceFS, {
    if(isFullScreen){
      checkboxesOutput <<- input$checkboxOutputChoiceFS
      updateCheckboxGroupInput(session, inputId = "checkboxOutputChoice", selected=unlist(checkboxesOutput))
    }
  })
  
  #render map
  output$AmsterdamMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(min(longitudes), min(latitudes), max(longitudes), max(latitudes))
  })
  
  
  observeEvent(reactiveVals$forceUpdate, {
    if(isValid){
      #updates slider
      local({
        localStartDate = input$dateRange[1]
        localEndDate = input$dateRange[2]
        localStartTime = input$startHourInput
        localEndTime = input$endHourInput
        if (is.null(input$mapDateTime)){
          value = as.POSIXct(paste(as.Date(localStartDate, origin = "1970-01-01"), as.integer(localStartTime)), format="%Y-%m-%d %H")
        }else{
          value = input$mapDateTime
        }
        output$slider <- renderUI({
          sliderInput("mapDateTime", "Select date and time:",
                      min = as.POSIXct(paste(as.Date(localStartDate, origin = "1970-01-01"), as.integer(localStartTime)), format="%Y-%m-%d %H"),
                      max = as.POSIXct(paste(as.Date(localEndDate, origin = "1970-01-01"), as.integer(localEndTime)), format="%Y-%m-%d %H"),
                      value= value,
                      step = 3600,
                      ticks = FALSE,
                      timeFormat="%Y-%m-%d %H:%M", width="100%"
          )
        })
      })
    }else{
      local({
        output$slider <- renderUI({})
      })
    }
  })
  
  #plot
  observeEvent(c(reactiveVals$updatedGarages, reactiveVals$forceUpdate), {
    if(isValid){
      found = 0 #counter that keeps track of the number of selected locations that have been found untill now
      #The offset of the selected begin and end date/time (in hours) from the begin end date/time is calculated
      beginTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardStartdate, origin = "1970-01-01"), as.integer(hardStartHour)), format="%Y-%m-%d %H"), units = c("hours"))
      endTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRange[2], origin = "1970-01-01"), as.integer(input$endHourInput)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardEnddate, origin = "1970-01-01"), as.integer(hardEndHour)), format="%Y-%m-%d %H"), units = c("hours"))
      if (length(checkboxesActive) > 0){ #when at least one location is selected
        for (occupiedSpacesIndex in 1:(length(occupiedSpaces[1,]))){
          for (checkboxIndex in 1:length(checkboxesActive)){
            if (toString(checkboxesActive[checkboxIndex]) == toString(colnames(occupiedSpaces)[occupiedSpacesIndex])){
              found=found+1 #the index of a selected location is found
              local({
                localOccupiedSpacesIndex = occupiedSpacesIndex #a local copy is made. Since the plots only process when the function is done and have only the call stored, not the variable value at that time, if there is not local copy, only the last location will be used for every plot and graph.
                
                #the user selected range is taken
                data = occupiedSpaces[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                capacityData = capacity[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                
                #The statistics are calculated
                Min = min(data)
                Max = max(data)
                Mean = mean(data)
                Median = median(data)
                AverageOccupancy = mean(data/capacityData)
                PercentageBottleneck = sum(data/capacityData >= 0.95)/length(data)*100
                
                #the table is made and the title is given to be the location name
                dataTable = data.frame(Min, Max, Mean, Median, AverageOccupancy, PercentageBottleneck)
                rownames(dataTable) = colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                
                tablenumber = found
                tablename <- paste("table", found, sep="") #the tablename is made, this is done since every table needs a distinct name
                output[[tablename]] <- renderTable({t(dataTable)}, rownames=TRUE, colnames = TRUE) #the data is transposed to get a row vector instead of a column vector
                
                #the same is done for the graphs
                plotcolumn = colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                plotname <- paste("plot", found, sep="")
                output[[plotname]] <- renderPlot({
                  plot(data,
                       main = paste("Parking location", plotcolumn, sep = " "),
                       ylim = c(0, max(1.1*max(Max, max(capacityData)),1)),
                       xlab = "Date" ,
                       ylab = "Parking spaces",
                       type = "l",
                       col = "blue"
                  )
                  lines(capacityData,
                        type = "l",
                        col = "red")
                })
                
                #and the bottlenecks
                occupancyPercentage = data/capacityData
                occupancyIndex=which(occupancyPercentage>0.95)
                bottlenecks = matrix(nrow=0, ncol=2)
                bottleneckIndex = 1
                if (length(occupancyIndex) > 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=2)
                  while (bottleneckIndex<(length(occupancyIndex))){
                    bottlenecksAdd[1,1]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    while(as.integer(occupancyIndex[bottleneckIndex]) == (occupancyIndex[bottleneckIndex+1]-1)&bottleneckIndex<(length(occupancyIndex)-1)){
                      bottleneckIndex=bottleneckIndex+1
                    }
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                    bottleneckIndex=bottleneckIndex+1
                  }
                  if(as.integer(occupancyIndex[bottleneckIndex]) != (occupancyIndex[bottleneckIndex-1]+1)){
                    bottlenecksAdd = matrix(nrow=1, ncol=2)
                    bottlenecksAdd[1,1]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                  }else{
                    bottlenecks[length(bottlenecks[,2]),2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  }
                  
                }else if(length(occupancyIndex) == 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=2)
                  bottlenecksAdd[1,1]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                }
                
                startpoint = as.character(as.POSIXct(bottlenecks[,1], origin = "1970-01-01"))
                endpoint = as.character(as.POSIXct(bottlenecks[,2], origin = "1970-01-01"))
                bottleNeckTable = data.frame(startpoint, endpoint)
                colnames(bottleNeckTable) = c("start point", "end point")
                tablenumberb = found
                cap = paste("Bottlenecks", colnames(occupiedSpaces)[localOccupiedSpacesIndex])
                tablenameb <- paste("tableb", found, sep="") #the tablename is made, this is done since every table needs a distinct name
                output[[tablenameb]] <- renderTable({bottleNeckTable}, caption = cap,
                                                    caption.placement = getOption("xtable.caption.placement", "top")) #the data is transposed to get a row vector instead of a column vector
                
              })
            }
          }
        }
      }
    }else{ #no locations are selected
      found = 0
    }
    
    output$plots <- renderUI({
      if (found>0){ #a location is selected
        #every loop only one object can be plotted, but since we want data per location and not first every graph and then every statistics and then every bottleneck, a modulo component is introduced
        plot_output_list <- lapply(1:(3*found), function(i) { #we need to loop over everything 3 times
          if(i%%3 == 1 & is.element( "Graph", input$checkboxOutputChoice)){ #first loop of location and the graph option is selected
            plotname <- paste("plot", (i-1) %/% 3 + 1, sep="")
            plotOutput(plotname, height = 400, width = "100%")
          }else if(i%%3 == 0 & is.element("Statistics", input$checkboxOutputChoice)){ #second loop of location and the statistics option is selected
            tablename <- paste("table", (i-1) %/% 3 + 1, sep="")
            tableOutput(tablename)
          }else if(i%%3 == 2 & is.element("Bottlenecks", input$checkboxOutputChoice)){ #third loop of location and the bottlenecks option is selected
            tablenameb <- paste("tableb", (i-1) %/% 3 + 1, sep="")
            tableOutput(tablenameb)
          }
        })
        do.call(tagList, plot_output_list) #the list off plots is returned
      }else{
        plotOutput("", height = 0, width = 0) #clear all plots
      }
      
    })
  })
  
  observeEvent(reactiveVals$forceUpdate, {
    if(isValid){
      found = 0 #counter that keeps track of the number of selected locations that have been found untill now
      #The offset of the selected begin and end date/time (in hours) from the begin end date/time is calculated
      beginTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRangeFS[1], origin = "1970-01-01"), as.integer(input$startHourInputFS)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardStartdate, origin = "1970-01-01"), as.integer(hardStartHour)), format="%Y-%m-%d %H"), units = c("hours"))
      endTimeDiff = difftime(as.POSIXct(paste(as.Date(input$dateRangeFS[2], origin = "1970-01-01"), as.integer(input$endHourInputFS)), format="%Y-%m-%d %H"), as.POSIXct(paste(as.Date(hardEnddate, origin = "1970-01-01"), as.integer(hardEndHour)), format="%Y-%m-%d %H"), units = c("hours"))
      if (length(checkboxesActive) > 0){ #when at least one location is selected
        for (occupiedSpacesIndex in 1:(length(occupiedSpaces[1,]))){
          for (checkboxIndex in 1:length(checkboxesActive)){
            if (toString(checkboxesActive[checkboxIndex]) == toString(colnames(occupiedSpaces)[occupiedSpacesIndex])){
              found=found+1 #the index of a selected location is found
              local({
                localOccupiedSpacesIndex = occupiedSpacesIndex #a local copy is made. Since the plots only process when the function is done and have only the call stored, not the variable value at that time, if there is not local copy, only the last location will be used for every plot and graph.
                #the user selected range is taken
                data = occupiedSpaces[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                capacityData = capacity[(1+beginTimeDiff):(length(occupiedSpaces[,localOccupiedSpacesIndex])+endTimeDiff), localOccupiedSpacesIndex]
                
                #The statistics are calculated
                Min = min(data)
                Max = max(data)
                Mean = mean(data)
                Median = median(data)
                AverageOccupancy = mean(data/capacityData)
                PercentageBottleneck = sum(data/capacityData >= 0.95)/length(data)*100
                
                #the table is made and the title is given to be the location name
                dataTable = data.frame(Min, Max, Mean, Median, AverageOccupancy, PercentageBottleneck)
                rownames(dataTable) = colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                
                tablenumber = found
                tablename <- paste("tableFS", found, sep="") #the tablename is made, this is done since every table needs a distinct name
                output[[tablename]] <- renderTable({t(dataTable)}, rownames=TRUE, colnames = TRUE) #the data is transposed to get a row vector instead of a column vector
                
                #the same is done for the graphs
                plotcolumn = colnames(occupiedSpaces)[localOccupiedSpacesIndex]
                plotname <- paste("plotFS", found, sep="")
                output[[plotname]] <- renderPlot({
                  plot(data,
                       main = paste("Parking location", plotcolumn, sep = " "),
                       ylim = c(0, max(1.1*max(Max, max(capacityData)),1)),
                       xlab = "Date" ,
                       ylab = "Parking spaces",
                       type = "l",
                       col = "blue"
                  )
                  lines(capacityData,
                        type = "l",
                        col = "red")
                })
                
                #and the bottlenecks
                occupancyPercentage = data/capacityData
                occupancyIndex=which(occupancyPercentage>0.95)
                bottlenecks = matrix(nrow=0, ncol=2)
                bottleneckIndex = 1
                if (length(occupancyIndex) > 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=2)
                  while (bottleneckIndex<(length(occupancyIndex))){
                    bottlenecksAdd[1,1]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    while(as.integer(occupancyIndex[bottleneckIndex]) == (occupancyIndex[bottleneckIndex+1]-1)&bottleneckIndex<(length(occupancyIndex)-1)){
                      bottleneckIndex=bottleneckIndex+1
                    }
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                    bottleneckIndex=bottleneckIndex+1
                  }
                  if(as.integer(occupancyIndex[bottleneckIndex]) != (occupancyIndex[bottleneckIndex-1]+1)){
                    bottlenecksAdd = matrix(nrow=1, ncol=2)
                    bottlenecksAdd[1,1]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                    bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                  }else{
                    bottlenecks[length(bottlenecks[,2]),2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  }
                  
                }else if(length(occupancyIndex) == 1){
                  bottlenecksAdd = matrix(nrow=1, ncol=2)
                  bottlenecksAdd[1,1]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecksAdd[1,2]=as.POSIXct(paste(as.Date(input$dateRange[1], origin = "1970-01-01"), as.integer(input$startHourInput)), format="%Y-%m-%d %H")+3600*(occupancyIndex[bottleneckIndex]-1)
                  bottlenecks = rbind(bottlenecks,bottlenecksAdd)
                }
                
                startpoint = as.character(as.POSIXct(bottlenecks[,1], origin = "1970-01-01"))
                endpoint = as.character(as.POSIXct(bottlenecks[,2], origin = "1970-01-01"))
                bottleNeckTable = data.frame(startpoint, endpoint)
                colnames(bottleNeckTable) = c("start point", "end point")
                tablenumberb = found
                cap = paste("Bottlenecks", colnames(occupiedSpaces)[localOccupiedSpacesIndex])
                tablenameb <- paste("tablebFS", found, sep="") #the tablename is made, this is done since every table needs a distinct name
                output[[tablenameb]] <- renderTable({bottleNeckTable}, caption = cap,
                                                    caption.placement = getOption("xtable.caption.placement", "top")) #the data is transposed to get a row vector instead of a column vector
                
              })
            }
          }
        }
      }
    }else{ #no locations are selected
      found = 0
    }
    
    output$plotsFS <- renderUI({
      if (found>0){ #a location is selected
        #every loop only one object can be plotted, but since we want data per location and not first every graph and then every statistics and then every bottleneck, a modulo component is introduced
        plot_output_list <- lapply(1:(3*found), function(i) { #we need to loop over everything 3 times
          if(i%%3 == 1 & is.element( "Graph", input$checkboxOutputChoiceFS)){ #first loop of location and the graph option is selected
            plotname <- paste("plotFS", (i-1) %/% 3 + 1, sep="")
            plotOutput(plotname, height = 400, width = "100%")
          }else if(i%%3 == 0 & is.element("Statistics", input$checkboxOutputChoiceFS)){ #second loop of location and the statistics option is selected
            tablename <- paste("tableFS", (i-1) %/% 3 + 1, sep="")
            tableOutput(tablename)
          }else if(i%%3 == 2 & is.element("Bottlenecks", input$checkboxOutputChoiceFS)){ #third loop of location and the bottlenecks option is selected
            tablenameb <- paste("tablebFS", (i-1) %/% 3 + 1, sep="")
            tableOutput(tablenameb)
          }
        })
        do.call(tagList, plot_output_list) #the list off plots is returned
      }else{
        plotOutput("", height = 0, width = 0) #clear all plots
      }
      
    })
  })
  
  #clicked on one of the markers, clicks on and off.
  observeEvent(input$AmsterdamMap_marker_click, {
    isValid <<- FALSE
    reactiveVals$updateNextFrame = 1
    isNotActive = sum(is.element(input$AmsterdamMap_marker_click$id, checkboxesActive))==0
    if (isNotActive){
      checkboxesActive <<- c(checkboxesActive, input$AmsterdamMap_marker_click$id)
    }else{
      element = match(TRUE, is.element(checkboxesActive, input$AmsterdamMap_marker_click$id))
      checkboxesActive <<- checkboxesActive[-1*element]
    }
  })
  
  #the range is changed.
  observeEvent(c(input$mapDateTime), {
    if(isValid){
      beginTimeDiff = difftime(as.POSIXct(input$mapDateTime), as.POSIXct(paste(as.Date(hardStartdate, origin = "1970-01-01"), as.integer(hardStartHour)), format="%Y-%m-%d %H"), units = c("hours"))
      occupiedData = occupiedSpaces[1+beginTimeDiff, ]
      capacityData = capacity[1+beginTimeDiff, ]
      #occupancy rate is given here
      reactiveVals$mapValues = round(occupiedData/capacityData,2)
      #here we choose what we define as "lowly, medium, highly occupied"
      occupancyColours = (reactiveVals$mapValues < 0.75)*1 + (reactiveVals$mapValues >= 0.75 & reactiveVals$mapValues < 0.95)*2 + (reactiveVals$mapValues >= 0.95)*3
      reactiveVals$mapColours = c("red", "orange", "green")[match(occupancyColours, c(3, 2, 1))]
    }
  })
  
  #slider.
  observeEvent(c(input$checkboxShowOnly, input$mapDateTime, reactiveVals$forceUpdate), {
    if (length(input$checkboxShowOnly) > 0){
      if (!input$checkboxShowOnly){
        if (isValid){
          if(reactiveVals$mapColours[1]!=""){
            #default function for changable icons.
            icons <- awesomeIcons(markerColor = reactiveVals$mapColours,
                                  text =  as.character(reactiveVals$mapValues),
                                  fontFamily = "calibri"
            )
            
            leafletProxy("AmsterdamMap") %>%
              clearShapes() %>%
              clearMarkers() %>%
              #markers added at given location, label gives name of garage and the mean occupancy rate
              addAwesomeMarkers(lat=as.numeric(latitudes), lng=as.numeric(longitudes), layerId = colnames(occupiedSpaces), label = paste(colnames(occupiedSpaces), reactiveVals$mapValues), icon = icons)
          }else{
            leafletProxy("AmsterdamMap") %>%
              clearShapes() %>%
              clearMarkers() %>%
              addMarkers(lat=as.numeric(latitudes), lng=as.numeric(longitudes), layerId = colnames(occupiedSpaces), label = paste(colnames(occupiedSpaces)))
          }
        }else{
          leafletProxy("AmsterdamMap") %>%
            clearShapes() %>%
            clearMarkers() %>%
            addMarkers(lat=as.numeric(latitudes), lng=as.numeric(longitudes), layerId = colnames(occupiedSpaces), label = paste(colnames(occupiedSpaces)))
        }
      }else{
        locationsToShow = which(colnames(occupiedSpaces) %in% checkboxesActive)
        if (length(locationsToShow > 0)){
          if (isValid){
            icons <- awesomeIcons(markerColor = reactiveVals$mapColours[locationsToShow],
                                  text =  as.character(reactiveVals$mapValues[locationsToShow]),
                                  fontFamily = "calibri"
            )
            
            leafletProxy("AmsterdamMap") %>%
              clearShapes() %>%
              clearMarkers() %>%
              addAwesomeMarkers(lat=as.numeric(latitudes[locationsToShow]), lng=as.numeric(longitudes[locationsToShow]), layerId = colnames(occupiedSpaces[locationsToShow]), label = paste(colnames(occupiedSpaces)[locationsToShow], reactiveVals$mapValues[locationsToShow]), icon = icons)
          }else{
            leafletProxy("AmsterdamMap") %>%
              clearShapes() %>%
              clearMarkers() %>%
              addMarkers(lat=as.numeric(latitudes[locationsToShow]), lng=as.numeric(longitudes[locationsToShow]), layerId = colnames(occupiedSpaces[locationsToShow]), label = paste(colnames(occupiedSpaces)[locationsToShow]))
            
          }
        }else{
          leafletProxy("AmsterdamMap") %>%
            clearShapes() %>%
            clearMarkers()
        }
      }
    }
  })
  
  observeEvent(input$fullScreen, {
    if(!reactiveVals$firstIteration){
      reactiveVals$updateNextFrame = 1
      isFullScreen <<- TRUE
      output$goFullScreen = renderText({"1"})
      outputOptions(output, 'goFullScreen', suspendWhenHidden=FALSE)
    }
  })
  
  observeEvent(input$fullScreenFS, {
    if(!reactiveVals$firstIteration){
      isFullScreen <<- FALSE
      reactiveVals$updateNextFrame = 1
      output$goFullScreen = renderText({"0"})
      outputOptions(output, 'goFullScreen', suspendWhenHidden=FALSE)
    }
    
  })
  
  observeEvent(reactiveVals$firstIteration, {
    if(reactiveVals$firstIteration){
      if (isFullScreen){
        output$goFullScreen <- renderText({"1"})
      }else{
        output$goFullScreen <- renderText({"0"})
      }
    }
    outputOptions(output, 'goFullScreen', suspendWhenHidden=FALSE)
  })
  
  observeEvent(reactiveVals$firstIteration, {
    if(reactiveVals$firstIteration){
      reactiveVals$firstIteration = FALSE
    }
  })
}

shinyApp(ui, server)
