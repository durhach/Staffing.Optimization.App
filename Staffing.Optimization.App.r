##########################################################################
## Staffing Optimization App
## Chris Durham October 2017
## Shiny
##
##########################################################################

## Load Libraries ------------------------------------------------------

options(java.parameters = "-Xmx16000m")

packages.to.load <- c("shiny","tidyverse","plotly","plyr","Hmisc","xlsx","gtools ",
                      "data.table","bit64","RODBC","hillmakeR","dygraphs",
                      "xts","viridis","ggExtra","ggpubr","lpSolveAPI","lpSolve")

if(sum(packages.to.load %in% (.packages()))==length(packages.to.load)){
  print("All required Libraries Loaded!")} else{
    if("shiny" %nin% (.packages())) suppressMessages(library(shiny)) else print("shiny loaded!")
    if("tidyverse" %nin% (.packages())) suppressMessages(library(tidyverse)) else print("tidyverse loaded!")
    if("plotly" %nin% (.packages())) suppressMessages(library(plotly)) else print("plotly loaded!")
    if("plyr" %nin% (.packages())) suppressMessages(library(plyr)) else print("plyr loaded!")
    if("Hmisc" %nin% (.packages())) suppressMessages(library(Hmisc)) else print("Hmisc loaded!")
    if("xlsx" %nin% (.packages())) suppressMessages(library(xlsx)) else print("xlsx loaded!")
    if("gtools" %nin% (.packages())) suppressMessages(library(gtools)) else print("gtools loaded!")
    if("data.table" %nin% (.packages())) suppressMessages(library(data.table)) else print("data.table loaded!")
    if("bit64" %nin% (.packages())) suppressMessages(library(bit64)) else print("bit64 loaded!")
    if("RODBC" %nin% (.packages())) suppressMessages(library(RODBC)) else print("RODBC loaded!")
    if("hillmakeR" %nin% (.packages())) suppressMessages(library(hillmakeR)) else print("hillmakeR loaded!")
    if("xts" %nin% (.packages())) suppressMessages(library(xts)) else print("xts loaded!")
    if("viridis" %nin% (.packages())) suppressMessages(library(viridis)) else print("viridis loaded!")
    if("ggExtra" %nin% (.packages())) suppressMessages(library(ggExtra)) else print("ggExtra loaded!")
    if("ggpubr" %nin% (.packages())) suppressMessages(library(ggpubr)) else print("ggpubr loaded!")
    if("lpSolveAPI" %nin% (.packages())) suppressMessages(library(lpSolveAPI)) else print("lpSolveAPI loaded!")
    if("lpSolve" %nin% (.packages())) suppressMessages(library(lpSolve)) else print("lpSolve loaded!")}

#detach(package:xlsx) detach(package:XLConnect) library(openxlsx)

rm(packages.to.load)

## Set Working Directory & Load Data --------------

base.theme <- readRDS("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/base.theme.RDS")
chart.list <- readRDS("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/chart.list.RDS")
Wday.factors <- readRDS("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/Wday.factors.RDS")
setwd("C:/Users/mhhccd/Documents/Tools/R/Shared Drive R Code/Census.Optimization.App")


## Shiny App Error Logging (Initiate) --------------
   #zz <- file("all.Rout", open="wt")
   #sink(zz, type="message")

   Sys.setenv(TZ='America/New_York') 
   # TZ was broken in R Version 3.4.3 and fixed with R version 3.4.4 (2018-03-15)


## ui -------------------
ui <- fluidPage(
        tabsetPanel(
   # Data Load and Exploration Tab -------
            tabPanel("Data Load and Exploration",
              fluidRow(
                  h4("csv file Date Timestamp Headers need to contain 'EncounterID', 'StartDTS' and 'EndDTS' with format: '%m/%d/%Y %H:%M' for DTS"),
                  column(3,
                   fileInput("dataz", "Choose CSV File", multiple = FALSE, accept =  c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"), width = NULL,
                     buttonLabel = "Choose csv Data File", placeholder = "No file selected"),
      checkboxInput("ts2", "Data includes both a StartDTS and EndDTS (Initial csv Load only!)", TRUE),  
      checkboxInput("outliers", "Include LOS Outliers in the data?", TRUE),  
      dateRangeInput(inputId ="inDateRange", label="Date range input:"),
      selectInput(inputId="op1",label="Choose DataSet Table to View:",
            choices=c("Valid Data","EndDate Year = 2100", "Negative LOS", "Zero LOS", "Outliers","NA DTS"),selected=c("Valid Data"),multiple=F),
      plotOutput("all.data.bp", width = 300, height = 200),
      plotOutput("all.data.bp2", width = 300, height = 200)),
    column(8,offset=1,
           h4("First 1000 rows of dataset; to view different set change date range"),
          tableOutput("contents")
    )
          
  #)
)),#))

   # Data Statistical Week Census Tab -------
 tabPanel("Data Statistical Week Census",
              fluidRow(
                  column(3,
    selectInput(inputId="custom.percentile",label="Choose Custom Census Percentile for Optimization",
                        choices=c(seq(1,100,by=1)),selected=85,multiple=F)),#),
                  column(3,
    selectInput(inputId="stat.week",label="Choose DataSet Week for Optimization",
                        choices=c("All",seq(1,53,by=1)),selected="All",multiple=T)),
                  column(3,
    selectInput(inputId="non.allowed.demand.hours",label="Choose Demand Hours to Zero",
                        choices=c("None",seq(0,23,by=1)),selected="None",multiple=T)),
                  # column(10,
                     plotOutput("census.plot", width = 1200, height = 600)
    
)),#))

   # Staffing Optimization Configurable Parameters Tab -------
 tabPanel("Staffing Optimization Configurable Parameters",
              fluidRow(
                  column(2,
      selectInput(inputId="asl",label="Choose Allowable Shift Lengths",
                        choices=c(seq(1,23,by=1)),selected=4,multiple=T),
      selectInput(inputId="assh",label="Choose Allowable Shift Start Hours",
                        choices=c(seq(0,23,by=1)),selected=c(seq(0,23,by=1)),multiple=T),
      "BackSpace Deletes Allowable Shift Lengths/Start Hours",
      selectInput(inputId="nr",label="Choose # of Patients per Nurse",
                        choices=c(seq(1,5,by=1)),selected=3,multiple=F) 
      
      ),
      column(2,

      selectInput(inputId="num1sh",label="Choose max # of 1 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num2sh",label="Choose max # of 2 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num3sh",label="Choose max # of 3 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num4sh",label="Choose max # of 4 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num5sh",label="Choose max # of 5 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num6sh",label="Choose max # of 6 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F)
      ),
      column(2,
      selectInput(inputId="num7sh",label="Choose max # of 7 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num8sh",label="Choose max # of 8 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num9sh",label="Choose max # of 9 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num10sh",label="Choose max # of 10 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num11sh",label="Choose max # of 11 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),  
      selectInput(inputId="num12sh",label="Choose max # of 12 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F)
      ),
      column(2,
      selectInput(inputId="num13sh",label="Choose max # of 13 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num14sh",label="Choose max # of 14 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),    
      selectInput(inputId="num15sh",label="Choose max # of 15 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num16sh",label="Choose max # of 16 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num17sh",label="Choose max # of 17 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),  
      selectInput(inputId="num18sh",label="Choose max # of 18 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F)),
      column(2,
      selectInput(inputId="num19sh",label="Choose max # of 19 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num20sh",label="Choose max # of 20 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),      
      selectInput(inputId="num21sh",label="Choose max # of 21 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num22sh",label="Choose max # of 22 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F),
      selectInput(inputId="num23sh",label="Choose max # of 23 Hour Shifts",
                        choices=c(seq(1,500,by=1)),selected=500,multiple=F))
                  
              )),#))

   # Optimized Staffing Graphics Tab -------
 tabPanel("Optimized Staffing Graphics",
              fluidRow(
                  column(3,
    selectInput(inputId="optim.graph",label="Choose Optimization Graph to Display",
                        choices=c("Start.Hour.graph","Supply_Demand.graph","Shift.Schedule.Graph"),selected="Shift.Schedule.Graph",multiple=F)),
                  column(3,
    selectInput(inputId="optim.csv",label="Choose Optimization Data to Download",
                        choices=c("Shift Schedule Data","Supply and Demand Data","Start Hour Data"),selected="Shift Schedule",multiple=F)),
                  column(6,
    downloadLink("optim.downloadData", "Download Optimization Data xlsx: IE_Browser_Only")),
             # column(10,
                     plotOutput("staff.optim.plot", width = 1200, height = 600)               
                  
     ))
          
          ))

#custom.percentile <- 85/100


# Server logic ------
server <- function(input, output,session) {

   # Set shiny options ------------------
options(shiny.maxRequestSize=60*1024^2) # This is a number which specifies the maximum web request size, which serves as a size limit for file uploads. If unset, the maximum request size defaults to 5MB. Increases to 30MB.
options(shiny.launch.browser=TRUE) # A boolean which controls the default behavior when an app is run. 
 
   # df -> (Reactive df) Updates User Interface (UI) Date Input prior to app loading -----
 df <- eventReactive(input$dataz, {
    
    df.list <- vector("list", 2) # initiate vector for df output
    
    inFile <- input$dataz # read csv file ( $name,$size,$type,$datapath)

    if (is.null(inFile))
      return(NULL)          # abort if no file is selected

    df2 <- read.csv(inFile$datapath, header = T) # df2 is data.frame to be manipulated
    df.name <- as.character(inFile$name) # df.name is file name for xlsx inputs download

    # convert DTSs to POSIXct and treat data as census or arrival curve (StartDTS + 1 minute)
    df2$StartDTS <- as.POSIXct(df2$StartDTS, format= "%m/%d/%Y %H:%M") 
    df2$EndDTS <- if(input$ts2==TRUE) as.POSIXct(df2$EndDTS, format= "%m/%d/%Y %H:%M") else df2$StartDTS + minutes(1)
    df2 <- unique(df2)
    df2$LOS <- as.numeric(difftime(df2$EndDTS,df2$StartDTS,units="mins"))

    # Remove invalid data that causes problems updating inDateRange
    raw  <- df2 # for invalid data display
    df2  <- df2 [!is.na(df2$StartDTS),]
    df2  <- df2 [!is.na(df2$EndDTS),]
    df2  <- df2 [which(lubridate::year(df2$EndDTS)!='2100'),]

    # Update the UI Date Range
    updateDateRangeInput(session, "inDateRange",
    label = "Date range", start=as.Date(min(df2$StartDTS)), end=as.Date(max(df2$EndDTS)),min=as.Date(min(df2$StartDTS)), max=as.Date(max(df2$EndDTS)))

    df.list[[1]] <- df2
    df.list[[2]] <- df.name
    df.list[[3]] <- raw 
    
    raw.desc <- describe(raw)
    
    Enc.ID.desc <- as.data.frame(unlist(raw.desc[[1]])); colnames(Enc.ID.desc) <- "Enc.ID.Values"
    Enc.ID.desc$Enc.ID.Vars <- rownames(Enc.ID.desc)
    Enc.ID.desc <-Enc.ID.desc %>% select(Enc.ID.Vars,everything())
    Enc.ID.desc <- data.frame(Enc.ID.desc[c(1:4),])
    Enc.ID.desc <- rbind(Enc.ID.desc,c(NA,NA))
    Enc.ID.desc <- rbind(Enc.ID.desc,c(NA,NA))
    
    
    StartDTS.desc <- as.data.frame(unlist(raw.desc[[2]])); colnames(StartDTS.desc) <- "StartDTS.Values"
    StartDTS.desc$StartDTS.Vars <- rownames(StartDTS.desc); StartDTS.desc <-StartDTS.desc %>% select(StartDTS.Vars,everything())
    StartDTS.desc <- data.frame(StartDTS.desc[c(1:4),]) 
    StartDTS.desc <- rbind(StartDTS.desc,c("Min.StartDTS",strftime(min(df2$StartDTS), format="%Y-%m-%d %H:%M:%S")))
    StartDTS.desc <- rbind(StartDTS.desc,c("Max.StartDTS",strftime(max(df2$StartDTS), format="%Y-%m-%d %H:%M:%S")))
    
    EndDTS.desc <- as.data.frame(unlist(raw.desc[[3]])); colnames(EndDTS.desc) <- "EndDTS.Values"
    EndDTS.desc$EndDTS.Vars <- rownames(EndDTS.desc)
    EndDTS.desc <-EndDTS.desc %>% select(EndDTS.Vars,everything())
    EndDTS.desc <- data.frame(EndDTS.desc[c(1:4),])  
    EndDTS.desc <- rbind(EndDTS.desc,c("Min.EndDTS",strftime(min(df2$EndDTS), format="%Y-%m-%d %H:%M:%S")))
    EndDTS.desc <- rbind(EndDTS.desc,c("Max.EndDTS",strftime(max(df2$EndDTS), format="%Y-%m-%d %H:%M:%S")))

    raw.desc.2 <- cbind(Enc.ID.desc,StartDTS.desc)
    raw.desc.2 <- cbind(raw.desc.2,EndDTS.desc)
    df.list[[4]] <- raw.desc.2
    
    Arrival.df <- data.frame(table(lubridate::year(raw$StartDTS),lubridate::week(raw$EndDTS)))
    names(Arrival.df) <- c("Year","Week","Freq")
    Arrival.df$Year.Week <- paste0(Arrival.df$Year,"_",Arrival.df$Week)
    Arrival.df <- Arrival.df %>% arrange(Year,Week)
    Arrival.df <- Arrival.df %>% dplyr::filter(Freq>0)
    Arrival.df$Year.Week <- fct_inorder(Arrival.df$Year.Week)
    Arrival.df <- Arrival.df %>% select(Year.Week,Freq)
    Arrival.Plot <- ggplot(data=Arrival.df,aes(x=Arrival.df$Year.Week,y=Arrival.df$Freq))+
      geom_point(colour="red")+geom_text(aes(label=Arrival.df$Freq),hjust=0, vjust=1)+
      geom_line(group=1,colour="red")+
      labs(title="Raw Data Arrival Frequency Curve",x="Year_Week",y="Arrival Count")+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_y_continuous(limits =c(0,max(Arrival.df$Freq)),
                         breaks=(seq(0,max(Arrival.df$Freq),by=25)),
                         minor_breaks =(seq(0,max(Arrival.df$Freq),by=5)))
    df.list[[5]] <- Arrival.df
    df.list[[6]] <- Arrival.Plot

    
    df.list # list of objects to be passed to other shiny blocks.
      })
 

   # DataTable Display ->  & data view selection ----- 
output$contents <- renderTable({
  
req(df())

raw2 <- df()[[3]]
raw2.2 <- raw2
raw2 <- raw2[!is.na(raw2$StartDTS),]
raw2 <- raw2[!is.na(raw2$EndDTS),]

dataztable <- raw2  # UI updated prior to reading data

# Filter data based on Date Range selected
dataztable <- dataztable[!is.na(dataztable$StartDTS),]
dataztable <- dataztable[!is.na(dataztable$EndDTS),]
dataztable <- dataztable[which(dataztable$StartDTS>=input$inDateRange[1] & dataztable$EndDTS<=input$inDateRange[2]),]

dataztable <- switch(input$op1,
                    "Valid Data" = dataztable[which(lubridate::year(dataztable$EndDTS)!=2100 & dataztable$LOS>0),],
                    "EndDate Year = 2100" =  raw2[which(lubridate::year(raw2$EndDTS)==2100),], 
                    "Negative LOS" = dataztable[which(dataztable$LOS<0),],
                    "Zero LOS" = dataztable[which(dataztable$LOS==0),],
                    "Outliers" = raw2[which(raw2$LOS %in% boxplot.stats(raw2$LOS)$out),],
                    "NA DTS" = raw2.2[is.na(raw2.2$StartDTS) | is.na(raw2.2$EndDTS),])

dataztable <- dataztable %>% arrange(StartDTS,EncounterID)
if(length(dataztable$EncounterID)>0){
dataztable$Row <-  as.integer(nrow(dataztable):1)
dataztable <- dataztable %>% select(Row,everything())}
dataztable$EncounterID <- as.integer(dataztable$EncounterID)
dataztable$StartDTS <- strftime(dataztable$StartDTS, format="%Y-%m-%d %H:%M:%S")
dataztable$EndDTS <- strftime(dataztable$EndDTS, format="%Y-%m-%d %H:%M:%S")

dataztable[c(1:1000),]
  })

   # df3 -> (Reactive df) date filtered with only valid data -----
df3 <- eventReactive(df(), { 
  
df3.list <- vector("list", 3) 

dff <- df()[[1]] 
dff <- dff[which(dff$StartDTS>=input$inDateRange[1] & dff$EndDTS<=input$inDateRange[2]),]
dff <- dff[which(dff$LOS>0),]

if(length(boxplot.stats(dff$LOS)$out>0) & input$outliers == FALSE){
dff <- dff[which(dff$LOS %nin% boxplot.stats(dff$LOS)$out),]}


df3.list[[1]] <- dff
names(df3.list)[[1]] <- "dff"

## Boxplot All Plot Generation

raw3 <- df()[[3]]
n.total <- nrow(raw3)
n.na <- nrow(raw3[is.na(raw3$StartDTS)|is.na(raw3$EndDTS),])
raw3 <- raw3[!is.na(raw3$StartDTS),]; raw3 <- raw3[!is.na(raw3$EndDTS),]
n.2100 <- nrow(raw3[which(lubridate::year(raw3$EndDTS)==2100),])
n.neg <- nrow(raw3[which(raw3$LOS<0),])
n.zero <- nrow(raw3[which(raw3$LOS==0),])

p <- ggplot(raw3, aes(x="",y=LOS)) + geom_boxplot()+labs(title=paste("All Data Length of Stay Boxplot; Observations: ", n.total,sep=""),y="Length of Stay (Minutes)")+
    theme(text = element_text(size=8),axis.text.y = element_text(size=8),axis.title.y = element_text(size=8))+ 
    annotate("text", x = 1, y = -1, label = paste(n.2100,": Year=2100; ",n.neg,": Negative; ",n.zero,": Zero",n.na,": NA; ",sep=" "),size=3,colour="red")

df3.list[[2]] <- p
names(df3.list)[[2]] <- "bp.all"

## Boxplot Date Filtered Plot Generation

q <- if(length(dff$LOS)>0){
ggplot(dff, aes(x="",y=LOS)) + geom_boxplot()+labs(title="Valid Data (no outliers) Length of Stay Boxplot",y="Length of Stay (Minutes)")+
    theme(text = element_text(size=8),axis.text.y = element_text(size=8),axis.title.y = element_text(size=8))+ 
    annotate("text", x = 1, y = max(dff$LOS), label = paste(nrow(dff),"Observations",sep=" "),size=3,colour="red")} else  ggplot()+ggtitle("Plot Loading!")

df3.list[[3]] <- q
names(df3.list)[[3]] <- "bp.filtered"

raw.stats <- data.frame(BP.All.Minimum=boxplot(raw3$LOS)$stats[[1]],
                        BP.All.First.Quartile=boxplot(raw3$LOS)$stats[[2]],
                        BP.All.Median=boxplot(raw3$LOS)$stats[[3]],
                        BP.All.Third.Quartile=boxplot(raw3$LOS)$stats[[4]],
                        BP.All.Maximum=boxplot(raw3$LOS)$stats[[5]],
                        BP.Filtered.Minimum=boxplot(dff$LOS)$stats[[1]],
                        BP.Filtered.First.Quartile=boxplot(dff$LOS)$stats[[2]],
                        BP.Filtered.Median=boxplot(dff$LOS)$stats[[3]],
                        BP.Filtered.Third.Quartile=boxplot(dff$LOS)$stats[[4]],
                        BP.Filtered.Maximum=boxplot(dff$LOS)$stats[[5]])
                        
 raw.stats2 <- data.table::transpose(raw.stats)
 names(raw.stats2) <- "Stat.Values"
 rownames(raw.stats2) <- colnames(raw.stats)
 raw.stats2$Statistic <- rownames(raw.stats2)
 raw.stats2 <- raw.stats2 %>% select(Statistic,Stat.Values)                        

 df3.list[[4]] <- raw.stats2

 df3.list

})

   # all.data.bp -> (Output Graph) Boxplot of all data (no date filters) -----
output$all.data.bp <- renderPlot({

raw4 <- df()[[3]]
n.total.2 <- nrow(raw4)
n.na.2 <- nrow(raw4[is.na(raw4$StartDTS)|is.na(raw4$EndDTS),])
raw4.2 <- raw4[!is.na(raw4$StartDTS),]; raw3 <- raw4[!is.na(raw4$EndDTS),]
n.2100.2 <- nrow(raw4[which(lubridate::year(raw4$EndDTS)==2100),])
n.neg.2 <- nrow(raw4[which(raw4$LOS<0),])
n.zero.2 <- nrow(raw4[which(raw4$LOS==0),])

p <- ggplot(raw4, aes(x="",y=LOS)) + geom_boxplot()+labs(title=paste("All Data Length of Stay Boxplot; Observations: ", n.total.2,sep=""),y="Length of Stay (Minutes)")+
    theme(text = element_text(size=7.5),axis.text.y = element_text(size=8),axis.title.y = element_text(size=8))+ 
    annotate("text", x = 1, y = -2, label = paste(n.2100.2,": Year=2100; ",n.neg.2,": Negative; ",n.zero.2,": Zero",n.na.2,": NA; ",sep=" "),size=3,colour="red")

p

})

   # all.data.bp2 -> (Output Graph) Boxplot of filtered valid data -----
output$all.data.bp2 <- renderPlot({

dff <- df()[[1]] 
dff <- dff[which(dff$StartDTS>=input$inDateRange[1] & dff$EndDTS<=input$inDateRange[2]),]
dff <- dff[which(lubridate::year(dff$EndDTS)!=2100),]
dff <- dff[!is.na(dff$StartDTS),]
dff <- dff[!is.na(dff$EndDTS),]
dff <- dff[which(dff$LOS>0),]

if(length(boxplot.stats(dff$LOS)$out>0) & input$outliers == FALSE){
  dff <- dff[which(dff$LOS %nin% boxplot.stats(dff$LOS)$out),]}

q <- if(length(dff$LOS)>0){
ggplot(dff, aes(x="",y=LOS)) + geom_boxplot()+labs(title="Valid Data (no outliers) Length of Stay Boxplot",y="Length of Stay (Minutes)")+
    theme(text = element_text(size=8),axis.text.y = element_text(size=8),axis.title.y = element_text(size=8))+ 
    annotate("text", x = 1, y = max(dff$LOS), label = paste(nrow(dff),"Observations",sep=" "),size=3,colour="red")} else  ggplot()+ggtitle("Plot Loading!")

q
})

   # df4 -> (Reactive df) Statistical Census object with all statistic values -----
df4 <- eventReactive(c(df3(),input$stat.week,input$custom.percentile,input$non.allowed.demand.hours), {

dff.data.list <- vector("list",2)

dff.data <- df3()[[1]]

setDT(dff.data)
ts <- names(dff.data)[grepl("DTS$",names(dff.data))] 
dff.data[, c(ts):=lapply(.SD, function(x) round_date(x,unit="minutes")), .SDcols=ts]
dff.data <- data.frame(dff.data)
rm(ts)

## Create Census from StartDTS and EndDTS Columns 
dff.data.census <- occupancy(startTimes=dff.data$StartDTS,stopTimes=dff.data$EndDTS, resolution="min",countlast = FALSE)

## Add Date/Time Identifier Columns 
dff.data.census$Date <- as.Date(dff.data.census$times)
dff.data.census$Year <- lubridate::year(dff.data.census$times)
dff.data.census$Month <- lubridate::month(dff.data.census$times)
dff.data.census$Week <- lubridate::isoweek(dff.data.census$times)
dff.data.census$Hour <- lubridate::hour(dff.data.census$times)
dff.data.census$WDay <-   lubridate::wday(dff.data.census$times, label = TRUE, abbr = FALSE)
dff.data.census$Wday.Hr <- ordered(as.factor(paste0(lubridate::wday(dff.data.census$times, label = TRUE, abbr = FALSE),"_",lubridate::hour(dff.data.census$times))), levels = Wday.factors)
dff.data.census$Minute <- lubridate::minute(dff.data.census$times)
dff.data.census$Wday.Hr.Min <- paste(lubridate::wday(dff.data.census$times, label = TRUE, abbr = FALSE),lubridate::hour(dff.data.census$times),lubridate::minute(dff.data.census$times),sep="_") 
dff.data.census$Yr.Wk <- paste(lubridate::year(dff.data.census$times),lubridate::isoweek(dff.data.census$times),sep="_") 
dff.data.census$Yr.Wk.Wday.Hr <-   paste(lubridate::year(dff.data.census$times),lubridate::isoweek(dff.data.census$times),lubridate::wday(dff.data.census$times, label = TRUE, abbr = FALSE),lubridate::hour(dff.data.census$times),sep="_")

dff.select <- if("All" %in% input$stat.week) c(seq(1,53,by=1)) else as.numeric(input$stat.week)
dff.select <- dff.select[dff.select %in% unique(dff.data.census$Week)]
dff.select <- if(length(dff.select)==00) c(seq(1,53,by=1)) else dff.select
                     
dff.data.census <- dff.data.census[which(dff.data.census$Week %in% dff.select),]

if("None" %nin% input$non.allowed.demand.hours){
dff.data.census[which(dff.data.census$Hour %in% c(as.numeric(input$non.allowed.demand.hours))),]$counts <- 0}

## Create dff.data.census statistical values 

custom.percentile.2 <- as.numeric(input$custom.percentile)/100

setDT(dff.data.census)

dff.data.census[, five:=boxplot.stats(counts)$stats[1], by=Wday.Hr]
dff.data.census[, twofive:=boxplot.stats(counts)$stats[2], by=Wday.Hr]
dff.data.census[, median:=boxplot.stats(counts)$stats[3], by=Wday.Hr]
dff.data.census[, sevenfive:=boxplot.stats(counts)$stats[4], by=Wday.Hr]
dff.data.census[, ninefive:=boxplot.stats(counts)$stats[5], by=Wday.Hr]
dff.data.census[, custom.perc:=quantile(counts,custom.percentile.2), by=Wday.Hr]
dff.data.census[, num.outliers:=length(boxplot.stats(counts)$out), by=Wday.Hr]

dff.data.census.2 <- unique(dff.data.census[,c(7,9,14:19)])
names(dff.data.census.2) <- c("Hour","Wday.Hr","five","twofive","median","sevenfive","ninefive","custom")
dff.data.census.2$Wday.Hr <- ordered(dff.data.census.2$Wday.Hr,levels=Wday.factors)
dff.data.census.2 <- dff.data.census.2 %>% arrange(as.numeric(Wday.Hr))

## Statistical Census Plot
census.records <- df3()[[1]]
n.pop <- as.character(nrow(census.records))

dff.data.census.stats.lines <- melt(dff.data.census.2, id.vars = c("Wday.Hr"), measure.vars = c("twofive","median","sevenfive","custom"))
dff.data.census.stats.ribbon <- dff.data.census.2[,c(2:3,7)] 

max.y <- max(dff.data.census.stats.ribbon$ninefive)
breaks.y <- if (max.y<50) {c(5,1)
} else if (max.y<100) {c(10,1)
} else if (max.y<200) {c(20,5)
} else c(25,5)

## plot

 r <- ggplot()+ geom_line(data=dff.data.census.stats.lines,aes(x=Wday.Hr, y=value, group=variable,linetype=variable,colour=variable)) + 
     scale_linetype_manual(name="",values=c("twofive"="dashed","median"="solid","sevenfive"="dashed","custom"="solid"))+
     scale_colour_manual(name="",values=c("twofive"="black","median"="black","sevenfive"="black","custom"="green"))+
     geom_ribbon(data=dff.data.census.stats.ribbon,aes(x=Wday.Hr,ymin=five,ymax=ninefive,group="identity"),fill="cadetblue",size = .5, linetype = "solid",alpha=.25) +
     scale_y_continuous(breaks=seq(0,max.y,by=breaks.y[1]),minor_breaks=seq(0,max.y,by=breaks.y[2]))+labs(y="Census Value",x="Hour of the Week (0:168)")+
     labs(title=paste0("Statistical Census for Week(s) ",input$stat.week),subtitle=paste0("Number Records= ",n.pop))+chart.list+base.theme

 dff.data.list[[1]] <- dff.data.census.2
 dff.data.list[[2]] <-  r
 dff.data.list
})

   # census.plot -> Statistical Census Plot -----------
output$census.plot <- renderPlot({

StatPlot <- df4()[[2]]

StatPlot
      
})

   # df5 -> (Reactive df) Linear Optimization Matrix Solution and Supply & Demand object/Shift Start Hour Object -----------
df5 <- eventReactive(c(df4(),input$asl,input$assh,input$nr,input$num1sh,input$num2sh,input$num3sh,input$num4sh,input$num5sh,input$num6sh,input$num7sh,input$num8sh,input$num9sh,input$num10sh,input$num11sh,input$num12sh,input$num13sh,input$num14sh,input$num15sh,input$num16sh,input$num17sh,input$num18sh,input$num19sh,input$num20sh,input$num21sh,input$num22sh,input$num23sh), {

df5.list <- vector("list",2)

dff.data.4 <- df4()[[1]]

z <- as.numeric(input$asl) ## Selection of allowable shift lengths
z <- sort(z,decreasing=F) 
z.2 <- as.numeric(input$assh) ## Selection of allowable shift start hours
z.2 <- sort(z.2,decreasing=F) 
asl.z.2 <- z.2
z.5 <- z.2 ## Extend allowable shift start hours for the 7 day week
for(i in 2:7){
z.5[c(length(z.5)+1:length(z.2))] <- z.2[c(1:length(z.2))]+(24*(i-1))}
#View(data.frame(z.5))
#rm(i)
z.2 <- z.5; rm(z.5) 

test2 <- 1:168
z.3 <- test2[test2 %nin% c(z.2+1)] ## variable to set SML start hours to 0 for unallowable start hours
shift.limits <- rep(500,23) ## initialize & unconstrained

shift.limits <- c(as.numeric(input$num1sh),as.numeric(input$num2sh),as.numeric(input$num3sh),as.numeric(input$num4sh),
                     as.numeric(input$num5sh),as.numeric(input$num6sh),as.numeric(input$num7sh),as.numeric(input$num8sh), 
                    as.numeric(input$num9sh),as.numeric(input$num10sh),as.numeric(input$num11sh),as.numeric(input$num12sh),
                     as.numeric(input$num13sh),as.numeric(input$num14sh),as.numeric(input$num15sh),as.numeric(input$num16sh),
                     as.numeric(input$num17sh),as.numeric(input$num18sh),as.numeric(input$num19sh),as.numeric(input$num20sh),
                     as.numeric(input$num21sh),as.numeric(input$num22sh),as.numeric(input$num23sh))

                    
## Demand is an atomic vector with 168 hours of demand for Sunday midnight thru Saturday 2300
nurse.ratio <- as.numeric(input$nr) # User specified staffing ratio for selected census patient population
demand <- ceiling(dff.data.4$custom/nurse.ratio) # demand$custom is the user specified percentile from the census code
#demand <- c(rep(45,168))
rm(test2)

## Setup SML
SML2 <- readRDS("C:/Users/mhhccd/Documents/Tools/R/Shared Drive R Code/Hospital_Census/Optimization/Shift.Matrix.List.RDS")

SML <- SML2[c(z)]

for(i in 1:length(SML)){
    SML[[i]][z.3,] <- 0}
rm(i)

## Setup Cost.List
cost.list <- list()
for(i in 1:168){
    p <- rep(i,168)
    cost.list[[i]] <- p
    names(cost.list)[i]<- paste("cost.shift.",i,sep="")}
#cost.list[[2]]
#cost.list[[23]]
rm(i)

for(i in 1:length(z)){
    cost <- if(i==1) rev(c(unlist(cost.list[z[i]],use.names=F))) else c(cost,rev(c(unlist(cost.list[z[i]],use.names=F))))
}

rm(p,i,cost.list)

# Objective Function is the decision variables of the allowable shifts  * the cost per shift of the allowable shifts

for(i in 1:length(z)){
    SML.const.0 <- if(i==1) c(unlist(SML[[i]][,1],use.names=F)) else c(SML.const.0,c(unlist(SML[[i]][,1],use.names=F)))
    SML.const.1 <- if(i==1) c(unlist(SML[[i]][,2],use.names=F)) else c(SML.const.1,c(unlist(SML[[i]][,2],use.names=F)))
    SML.const.2 <- if(i==1) c(unlist(SML[[i]][,3],use.names=F)) else c(SML.const.2,c(unlist(SML[[i]][,3],use.names=F)))
    SML.const.3 <- if(i==1) c(unlist(SML[[i]][,4],use.names=F)) else c(SML.const.3,c(unlist(SML[[i]][,4],use.names=F)))
    SML.const.4 <- if(i==1) c(unlist(SML[[i]][,5],use.names=F)) else c(SML.const.4,c(unlist(SML[[i]][,5],use.names=F)))
    SML.const.5 <- if(i==1) c(unlist(SML[[i]][,6],use.names=F)) else c(SML.const.5,c(unlist(SML[[i]][,6],use.names=F)))
    SML.const.6 <- if(i==1) c(unlist(SML[[i]][,7],use.names=F)) else c(SML.const.6,c(unlist(SML[[i]][,7],use.names=F)))
    SML.const.7 <- if(i==1) c(unlist(SML[[i]][,8],use.names=F)) else c(SML.const.7,c(unlist(SML[[i]][,8],use.names=F)))
    SML.const.8 <- if(i==1) c(unlist(SML[[i]][,9],use.names=F)) else c(SML.const.8,c(unlist(SML[[i]][,9],use.names=F)))
    SML.const.9 <- if(i==1) c(unlist(SML[[i]][,10],use.names=F)) else c(SML.const.9,c(unlist(SML[[i]][,10],use.names=F)))
    SML.const.10 <- if(i==1) c(unlist(SML[[i]][,11],use.names=F)) else c(SML.const.10,c(unlist(SML[[i]][,11],use.names=F)))
    SML.const.11 <- if(i==1) c(unlist(SML[[i]][,12],use.names=F)) else c(SML.const.11,c(unlist(SML[[i]][,12],use.names=F)))
    SML.const.12 <- if(i==1) c(unlist(SML[[i]][,13],use.names=F)) else c(SML.const.12,c(unlist(SML[[i]][,13],use.names=F)))
    SML.const.13 <- if(i==1) c(unlist(SML[[i]][,14],use.names=F)) else c(SML.const.13,c(unlist(SML[[i]][,14],use.names=F)))
    SML.const.14 <- if(i==1) c(unlist(SML[[i]][,15],use.names=F)) else c(SML.const.14,c(unlist(SML[[i]][,15],use.names=F)))
    SML.const.15 <- if(i==1) c(unlist(SML[[i]][,16],use.names=F)) else c(SML.const.15,c(unlist(SML[[i]][,16],use.names=F)))
    SML.const.16 <- if(i==1) c(unlist(SML[[i]][,17],use.names=F)) else c(SML.const.16,c(unlist(SML[[i]][,17],use.names=F)))
    SML.const.17 <- if(i==1) c(unlist(SML[[i]][,18],use.names=F)) else c(SML.const.17,c(unlist(SML[[i]][,18],use.names=F)))
    SML.const.18 <- if(i==1) c(unlist(SML[[i]][,19],use.names=F)) else c(SML.const.18,c(unlist(SML[[i]][,19],use.names=F)))
    SML.const.19 <- if(i==1) c(unlist(SML[[i]][,20],use.names=F)) else c(SML.const.19,c(unlist(SML[[i]][,20],use.names=F)))
    SML.const.20 <- if(i==1) c(unlist(SML[[i]][,21],use.names=F)) else c(SML.const.20,c(unlist(SML[[i]][,21],use.names=F)))
    SML.const.21 <- if(i==1) c(unlist(SML[[i]][,22],use.names=F)) else c(SML.const.21,c(unlist(SML[[i]][,22],use.names=F)))
    SML.const.22 <- if(i==1) c(unlist(SML[[i]][,23],use.names=F)) else c(SML.const.22,c(unlist(SML[[i]][,23],use.names=F)))
    SML.const.23 <- if(i==1) c(unlist(SML[[i]][,24],use.names=F)) else c(SML.const.23,c(unlist(SML[[i]][,24],use.names=F)))

    SML.const.24 <- if(i==1) c(unlist(SML[[i]][,25],use.names=F)) else c(SML.const.24,c(unlist(SML[[i]][,25],use.names=F)))
    SML.const.25 <- if(i==1) c(unlist(SML[[i]][,26],use.names=F)) else c(SML.const.25,c(unlist(SML[[i]][,26],use.names=F)))
    SML.const.26 <- if(i==1) c(unlist(SML[[i]][,27],use.names=F)) else c(SML.const.26,c(unlist(SML[[i]][,27],use.names=F)))
    SML.const.27 <- if(i==1) c(unlist(SML[[i]][,28],use.names=F)) else c(SML.const.27,c(unlist(SML[[i]][,28],use.names=F)))
    SML.const.28 <- if(i==1) c(unlist(SML[[i]][,29],use.names=F)) else c(SML.const.28,c(unlist(SML[[i]][,29],use.names=F)))
    SML.const.29 <- if(i==1) c(unlist(SML[[i]][,30],use.names=F)) else c(SML.const.29,c(unlist(SML[[i]][,30],use.names=F)))
    SML.const.30 <- if(i==1) c(unlist(SML[[i]][,31],use.names=F)) else c(SML.const.30,c(unlist(SML[[i]][,31],use.names=F)))
    SML.const.31 <- if(i==1) c(unlist(SML[[i]][,32],use.names=F)) else c(SML.const.31,c(unlist(SML[[i]][,32],use.names=F)))
    SML.const.32 <- if(i==1) c(unlist(SML[[i]][,33],use.names=F)) else c(SML.const.32,c(unlist(SML[[i]][,33],use.names=F)))
    SML.const.33 <- if(i==1) c(unlist(SML[[i]][,34],use.names=F)) else c(SML.const.33,c(unlist(SML[[i]][,34],use.names=F)))
    SML.const.34 <- if(i==1) c(unlist(SML[[i]][,35],use.names=F)) else c(SML.const.34,c(unlist(SML[[i]][,35],use.names=F)))
    SML.const.35 <- if(i==1) c(unlist(SML[[i]][,36],use.names=F)) else c(SML.const.35,c(unlist(SML[[i]][,36],use.names=F)))
    SML.const.36 <- if(i==1) c(unlist(SML[[i]][,37],use.names=F)) else c(SML.const.36,c(unlist(SML[[i]][,37],use.names=F)))
    SML.const.37 <- if(i==1) c(unlist(SML[[i]][,38],use.names=F)) else c(SML.const.37,c(unlist(SML[[i]][,38],use.names=F)))
    SML.const.38 <- if(i==1) c(unlist(SML[[i]][,39],use.names=F)) else c(SML.const.38,c(unlist(SML[[i]][,39],use.names=F)))
    SML.const.39 <- if(i==1) c(unlist(SML[[i]][,40],use.names=F)) else c(SML.const.39,c(unlist(SML[[i]][,40],use.names=F)))
    SML.const.40 <- if(i==1) c(unlist(SML[[i]][,41],use.names=F)) else c(SML.const.40,c(unlist(SML[[i]][,41],use.names=F)))
    SML.const.41 <- if(i==1) c(unlist(SML[[i]][,42],use.names=F)) else c(SML.const.41,c(unlist(SML[[i]][,42],use.names=F)))
    SML.const.42 <- if(i==1) c(unlist(SML[[i]][,43],use.names=F)) else c(SML.const.42,c(unlist(SML[[i]][,43],use.names=F)))
    SML.const.43 <- if(i==1) c(unlist(SML[[i]][,44],use.names=F)) else c(SML.const.43,c(unlist(SML[[i]][,44],use.names=F)))
    SML.const.44 <- if(i==1) c(unlist(SML[[i]][,45],use.names=F)) else c(SML.const.44,c(unlist(SML[[i]][,45],use.names=F)))
    SML.const.45 <- if(i==1) c(unlist(SML[[i]][,46],use.names=F)) else c(SML.const.45,c(unlist(SML[[i]][,46],use.names=F)))
    SML.const.46 <- if(i==1) c(unlist(SML[[i]][,47],use.names=F)) else c(SML.const.46,c(unlist(SML[[i]][,47],use.names=F)))
    SML.const.47 <- if(i==1) c(unlist(SML[[i]][,48],use.names=F)) else c(SML.const.47,c(unlist(SML[[i]][,48],use.names=F)))
    
    SML.const.48 <- if(i==1) c(unlist(SML[[i]][,49],use.names=F)) else c(SML.const.48,c(unlist(SML[[i]][,49],use.names=F)))
    SML.const.49 <- if(i==1) c(unlist(SML[[i]][,50],use.names=F)) else c(SML.const.49,c(unlist(SML[[i]][,50],use.names=F)))
    SML.const.50 <- if(i==1) c(unlist(SML[[i]][,51],use.names=F)) else c(SML.const.50,c(unlist(SML[[i]][,51],use.names=F)))
    SML.const.51 <- if(i==1) c(unlist(SML[[i]][,52],use.names=F)) else c(SML.const.51,c(unlist(SML[[i]][,52],use.names=F)))
    SML.const.52 <- if(i==1) c(unlist(SML[[i]][,53],use.names=F)) else c(SML.const.52,c(unlist(SML[[i]][,53],use.names=F)))
    SML.const.53 <- if(i==1) c(unlist(SML[[i]][,54],use.names=F)) else c(SML.const.53,c(unlist(SML[[i]][,54],use.names=F)))
    SML.const.54 <- if(i==1) c(unlist(SML[[i]][,55],use.names=F)) else c(SML.const.54,c(unlist(SML[[i]][,55],use.names=F)))
    SML.const.55 <- if(i==1) c(unlist(SML[[i]][,56],use.names=F)) else c(SML.const.55,c(unlist(SML[[i]][,56],use.names=F)))
    SML.const.56 <- if(i==1) c(unlist(SML[[i]][,57],use.names=F)) else c(SML.const.56,c(unlist(SML[[i]][,57],use.names=F)))
    SML.const.57 <- if(i==1) c(unlist(SML[[i]][,58],use.names=F)) else c(SML.const.57,c(unlist(SML[[i]][,58],use.names=F)))
    SML.const.58 <- if(i==1) c(unlist(SML[[i]][,59],use.names=F)) else c(SML.const.58,c(unlist(SML[[i]][,59],use.names=F)))
    SML.const.59 <- if(i==1) c(unlist(SML[[i]][,60],use.names=F)) else c(SML.const.59,c(unlist(SML[[i]][,60],use.names=F)))
    SML.const.60 <- if(i==1) c(unlist(SML[[i]][,61],use.names=F)) else c(SML.const.60,c(unlist(SML[[i]][,61],use.names=F)))
    SML.const.61 <- if(i==1) c(unlist(SML[[i]][,62],use.names=F)) else c(SML.const.61,c(unlist(SML[[i]][,62],use.names=F)))
    SML.const.62 <- if(i==1) c(unlist(SML[[i]][,63],use.names=F)) else c(SML.const.62,c(unlist(SML[[i]][,63],use.names=F)))
    SML.const.63 <- if(i==1) c(unlist(SML[[i]][,64],use.names=F)) else c(SML.const.63,c(unlist(SML[[i]][,64],use.names=F)))
    SML.const.64 <- if(i==1) c(unlist(SML[[i]][,65],use.names=F)) else c(SML.const.64,c(unlist(SML[[i]][,65],use.names=F)))
    SML.const.65 <- if(i==1) c(unlist(SML[[i]][,66],use.names=F)) else c(SML.const.65,c(unlist(SML[[i]][,66],use.names=F)))
    SML.const.66 <- if(i==1) c(unlist(SML[[i]][,67],use.names=F)) else c(SML.const.66,c(unlist(SML[[i]][,67],use.names=F)))
    SML.const.67 <- if(i==1) c(unlist(SML[[i]][,68],use.names=F)) else c(SML.const.67,c(unlist(SML[[i]][,68],use.names=F)))
    SML.const.68 <- if(i==1) c(unlist(SML[[i]][,69],use.names=F)) else c(SML.const.68,c(unlist(SML[[i]][,69],use.names=F)))
    SML.const.69 <- if(i==1) c(unlist(SML[[i]][,70],use.names=F)) else c(SML.const.69,c(unlist(SML[[i]][,70],use.names=F)))
    SML.const.70 <- if(i==1) c(unlist(SML[[i]][,71],use.names=F)) else c(SML.const.70,c(unlist(SML[[i]][,71],use.names=F)))
    SML.const.71 <- if(i==1) c(unlist(SML[[i]][,72],use.names=F)) else c(SML.const.71,c(unlist(SML[[i]][,72],use.names=F)))
    
    SML.const.72 <- if(i==1) c(unlist(SML[[i]][,73],use.names=F)) else c(SML.const.72,c(unlist(SML[[i]][,73],use.names=F)))
    SML.const.73 <- if(i==1) c(unlist(SML[[i]][,74],use.names=F)) else c(SML.const.73,c(unlist(SML[[i]][,74],use.names=F)))
    SML.const.74 <- if(i==1) c(unlist(SML[[i]][,75],use.names=F)) else c(SML.const.74,c(unlist(SML[[i]][,75],use.names=F)))
    SML.const.75 <- if(i==1) c(unlist(SML[[i]][,76],use.names=F)) else c(SML.const.75,c(unlist(SML[[i]][,76],use.names=F)))
    SML.const.76 <- if(i==1) c(unlist(SML[[i]][,77],use.names=F)) else c(SML.const.76,c(unlist(SML[[i]][,77],use.names=F)))
    SML.const.77 <- if(i==1) c(unlist(SML[[i]][,78],use.names=F)) else c(SML.const.77,c(unlist(SML[[i]][,78],use.names=F)))
    SML.const.78 <- if(i==1) c(unlist(SML[[i]][,79],use.names=F)) else c(SML.const.78,c(unlist(SML[[i]][,79],use.names=F)))
    SML.const.79 <- if(i==1) c(unlist(SML[[i]][,80],use.names=F)) else c(SML.const.79,c(unlist(SML[[i]][,80],use.names=F)))
    SML.const.80 <- if(i==1) c(unlist(SML[[i]][,81],use.names=F)) else c(SML.const.80,c(unlist(SML[[i]][,81],use.names=F)))
    SML.const.81 <- if(i==1) c(unlist(SML[[i]][,82],use.names=F)) else c(SML.const.81,c(unlist(SML[[i]][,82],use.names=F)))
    SML.const.82 <- if(i==1) c(unlist(SML[[i]][,83],use.names=F)) else c(SML.const.82,c(unlist(SML[[i]][,83],use.names=F)))
    SML.const.83 <- if(i==1) c(unlist(SML[[i]][,84],use.names=F)) else c(SML.const.83,c(unlist(SML[[i]][,84],use.names=F)))
    SML.const.84 <- if(i==1) c(unlist(SML[[i]][,85],use.names=F)) else c(SML.const.84,c(unlist(SML[[i]][,85],use.names=F)))
    SML.const.85 <- if(i==1) c(unlist(SML[[i]][,86],use.names=F)) else c(SML.const.85,c(unlist(SML[[i]][,86],use.names=F)))
    SML.const.86 <- if(i==1) c(unlist(SML[[i]][,87],use.names=F)) else c(SML.const.86,c(unlist(SML[[i]][,87],use.names=F)))
    SML.const.87 <- if(i==1) c(unlist(SML[[i]][,88],use.names=F)) else c(SML.const.87,c(unlist(SML[[i]][,88],use.names=F)))
    SML.const.88 <- if(i==1) c(unlist(SML[[i]][,89],use.names=F)) else c(SML.const.88,c(unlist(SML[[i]][,89],use.names=F)))
    SML.const.89 <- if(i==1) c(unlist(SML[[i]][,90],use.names=F)) else c(SML.const.89,c(unlist(SML[[i]][,90],use.names=F)))
    SML.const.90 <- if(i==1) c(unlist(SML[[i]][,91],use.names=F)) else c(SML.const.90,c(unlist(SML[[i]][,91],use.names=F)))
    SML.const.91 <- if(i==1) c(unlist(SML[[i]][,92],use.names=F)) else c(SML.const.91,c(unlist(SML[[i]][,92],use.names=F)))
    SML.const.92 <- if(i==1) c(unlist(SML[[i]][,93],use.names=F)) else c(SML.const.92,c(unlist(SML[[i]][,93],use.names=F)))
    SML.const.93 <- if(i==1) c(unlist(SML[[i]][,94],use.names=F)) else c(SML.const.93,c(unlist(SML[[i]][,94],use.names=F)))
    SML.const.94 <- if(i==1) c(unlist(SML[[i]][,95],use.names=F)) else c(SML.const.94,c(unlist(SML[[i]][,95],use.names=F)))
    SML.const.95 <- if(i==1) c(unlist(SML[[i]][,96],use.names=F)) else c(SML.const.95,c(unlist(SML[[i]][,96],use.names=F)))
   
    SML.const.96 <- if(i==1) c(unlist(SML[[i]][,97],use.names=F)) else c(SML.const.96,c(unlist(SML[[i]][,97],use.names=F)))
    SML.const.97 <- if(i==1) c(unlist(SML[[i]][,98],use.names=F)) else c(SML.const.97,c(unlist(SML[[i]][,98],use.names=F)))
    SML.const.98 <- if(i==1) c(unlist(SML[[i]][,99],use.names=F)) else c(SML.const.98,c(unlist(SML[[i]][,99],use.names=F)))
    SML.const.99 <- if(i==1) c(unlist(SML[[i]][,100],use.names=F)) else c(SML.const.99,c(unlist(SML[[i]][,100],use.names=F)))
    SML.const.100 <- if(i==1) c(unlist(SML[[i]][,101],use.names=F)) else c(SML.const.100,c(unlist(SML[[i]][,101],use.names=F)))
    SML.const.101 <- if(i==1) c(unlist(SML[[i]][,102],use.names=F)) else c(SML.const.101,c(unlist(SML[[i]][,102],use.names=F)))
    SML.const.102 <- if(i==1) c(unlist(SML[[i]][,103],use.names=F)) else c(SML.const.102,c(unlist(SML[[i]][,103],use.names=F)))
    SML.const.103 <- if(i==1) c(unlist(SML[[i]][,104],use.names=F)) else c(SML.const.103,c(unlist(SML[[i]][,104],use.names=F)))
    SML.const.104 <- if(i==1) c(unlist(SML[[i]][,105],use.names=F)) else c(SML.const.104,c(unlist(SML[[i]][,105],use.names=F)))
    SML.const.105 <- if(i==1) c(unlist(SML[[i]][,106],use.names=F)) else c(SML.const.105,c(unlist(SML[[i]][,106],use.names=F)))
    SML.const.106 <- if(i==1) c(unlist(SML[[i]][,107],use.names=F)) else c(SML.const.106,c(unlist(SML[[i]][,107],use.names=F)))
    SML.const.107 <- if(i==1) c(unlist(SML[[i]][,108],use.names=F)) else c(SML.const.107,c(unlist(SML[[i]][,108],use.names=F)))
    SML.const.108 <- if(i==1) c(unlist(SML[[i]][,109],use.names=F)) else c(SML.const.108,c(unlist(SML[[i]][,109],use.names=F)))
    SML.const.109 <- if(i==1) c(unlist(SML[[i]][,110],use.names=F)) else c(SML.const.109,c(unlist(SML[[i]][,110],use.names=F)))
    SML.const.110 <- if(i==1) c(unlist(SML[[i]][,111],use.names=F)) else c(SML.const.110,c(unlist(SML[[i]][,111],use.names=F)))
    SML.const.111 <- if(i==1) c(unlist(SML[[i]][,112],use.names=F)) else c(SML.const.111,c(unlist(SML[[i]][,112],use.names=F)))
    SML.const.112 <- if(i==1) c(unlist(SML[[i]][,113],use.names=F)) else c(SML.const.112,c(unlist(SML[[i]][,113],use.names=F)))
    SML.const.113 <- if(i==1) c(unlist(SML[[i]][,114],use.names=F)) else c(SML.const.113,c(unlist(SML[[i]][,114],use.names=F)))
    SML.const.114 <- if(i==1) c(unlist(SML[[i]][,115],use.names=F)) else c(SML.const.114,c(unlist(SML[[i]][,115],use.names=F)))
    SML.const.115 <- if(i==1) c(unlist(SML[[i]][,116],use.names=F)) else c(SML.const.115,c(unlist(SML[[i]][,116],use.names=F)))
    SML.const.116 <- if(i==1) c(unlist(SML[[i]][,117],use.names=F)) else c(SML.const.116,c(unlist(SML[[i]][,117],use.names=F)))
    SML.const.117 <- if(i==1) c(unlist(SML[[i]][,118],use.names=F)) else c(SML.const.117,c(unlist(SML[[i]][,118],use.names=F)))
    SML.const.118 <- if(i==1) c(unlist(SML[[i]][,119],use.names=F)) else c(SML.const.118,c(unlist(SML[[i]][,119],use.names=F)))
    SML.const.119 <- if(i==1) c(unlist(SML[[i]][,120],use.names=F)) else c(SML.const.119,c(unlist(SML[[i]][,120],use.names=F)))

    SML.const.120 <- if(i==1) c(unlist(SML[[i]][,121],use.names=F)) else c(SML.const.120,c(unlist(SML[[i]][,121],use.names=F)))
    SML.const.121 <- if(i==1) c(unlist(SML[[i]][,122],use.names=F)) else c(SML.const.121,c(unlist(SML[[i]][,122],use.names=F)))
    SML.const.122 <- if(i==1) c(unlist(SML[[i]][,123],use.names=F)) else c(SML.const.122,c(unlist(SML[[i]][,123],use.names=F)))
    SML.const.123 <- if(i==1) c(unlist(SML[[i]][,124],use.names=F)) else c(SML.const.123,c(unlist(SML[[i]][,124],use.names=F)))
    SML.const.124 <- if(i==1) c(unlist(SML[[i]][,125],use.names=F)) else c(SML.const.124,c(unlist(SML[[i]][,125],use.names=F)))
    SML.const.125 <- if(i==1) c(unlist(SML[[i]][,126],use.names=F)) else c(SML.const.125,c(unlist(SML[[i]][,126],use.names=F)))
    SML.const.126 <- if(i==1) c(unlist(SML[[i]][,127],use.names=F)) else c(SML.const.126,c(unlist(SML[[i]][,127],use.names=F)))
    SML.const.127 <- if(i==1) c(unlist(SML[[i]][,128],use.names=F)) else c(SML.const.127,c(unlist(SML[[i]][,128],use.names=F)))
    SML.const.128 <- if(i==1) c(unlist(SML[[i]][,129],use.names=F)) else c(SML.const.128,c(unlist(SML[[i]][,129],use.names=F)))
    SML.const.129 <- if(i==1) c(unlist(SML[[i]][,130],use.names=F)) else c(SML.const.129,c(unlist(SML[[i]][,130],use.names=F)))
    SML.const.130 <- if(i==1) c(unlist(SML[[i]][,131],use.names=F)) else c(SML.const.130,c(unlist(SML[[i]][,131],use.names=F)))
    SML.const.131 <- if(i==1) c(unlist(SML[[i]][,132],use.names=F)) else c(SML.const.131,c(unlist(SML[[i]][,132],use.names=F)))
    SML.const.132 <- if(i==1) c(unlist(SML[[i]][,133],use.names=F)) else c(SML.const.132,c(unlist(SML[[i]][,133],use.names=F)))
    SML.const.133 <- if(i==1) c(unlist(SML[[i]][,134],use.names=F)) else c(SML.const.133,c(unlist(SML[[i]][,134],use.names=F)))
    SML.const.134 <- if(i==1) c(unlist(SML[[i]][,135],use.names=F)) else c(SML.const.134,c(unlist(SML[[i]][,135],use.names=F)))
    SML.const.135 <- if(i==1) c(unlist(SML[[i]][,136],use.names=F)) else c(SML.const.135,c(unlist(SML[[i]][,136],use.names=F)))
    SML.const.136 <- if(i==1) c(unlist(SML[[i]][,137],use.names=F)) else c(SML.const.136,c(unlist(SML[[i]][,137],use.names=F)))
    SML.const.137 <- if(i==1) c(unlist(SML[[i]][,138],use.names=F)) else c(SML.const.137,c(unlist(SML[[i]][,138],use.names=F)))
    SML.const.138 <- if(i==1) c(unlist(SML[[i]][,139],use.names=F)) else c(SML.const.138,c(unlist(SML[[i]][,139],use.names=F)))
    SML.const.139 <- if(i==1) c(unlist(SML[[i]][,140],use.names=F)) else c(SML.const.139,c(unlist(SML[[i]][,140],use.names=F)))
    SML.const.140 <- if(i==1) c(unlist(SML[[i]][,141],use.names=F)) else c(SML.const.140,c(unlist(SML[[i]][,141],use.names=F)))
    SML.const.141 <- if(i==1) c(unlist(SML[[i]][,142],use.names=F)) else c(SML.const.141,c(unlist(SML[[i]][,142],use.names=F)))
    SML.const.142 <- if(i==1) c(unlist(SML[[i]][,143],use.names=F)) else c(SML.const.142,c(unlist(SML[[i]][,143],use.names=F)))
    SML.const.143 <- if(i==1) c(unlist(SML[[i]][,144],use.names=F)) else c(SML.const.143,c(unlist(SML[[i]][,144],use.names=F)))
    
    SML.const.144 <- if(i==1) c(unlist(SML[[i]][,145],use.names=F)) else c(SML.const.144,c(unlist(SML[[i]][,145],use.names=F)))
    SML.const.145 <- if(i==1) c(unlist(SML[[i]][,146],use.names=F)) else c(SML.const.145,c(unlist(SML[[i]][,146],use.names=F)))
    SML.const.146 <- if(i==1) c(unlist(SML[[i]][,147],use.names=F)) else c(SML.const.146,c(unlist(SML[[i]][,147],use.names=F)))
    SML.const.147 <- if(i==1) c(unlist(SML[[i]][,148],use.names=F)) else c(SML.const.147,c(unlist(SML[[i]][,148],use.names=F)))
    SML.const.148 <- if(i==1) c(unlist(SML[[i]][,149],use.names=F)) else c(SML.const.148,c(unlist(SML[[i]][,149],use.names=F)))
    SML.const.149 <- if(i==1) c(unlist(SML[[i]][,150],use.names=F)) else c(SML.const.149,c(unlist(SML[[i]][,150],use.names=F)))
    SML.const.150 <- if(i==1) c(unlist(SML[[i]][,151],use.names=F)) else c(SML.const.150,c(unlist(SML[[i]][,151],use.names=F)))
    SML.const.151 <- if(i==1) c(unlist(SML[[i]][,152],use.names=F)) else c(SML.const.151,c(unlist(SML[[i]][,152],use.names=F)))
    SML.const.152 <- if(i==1) c(unlist(SML[[i]][,153],use.names=F)) else c(SML.const.152,c(unlist(SML[[i]][,153],use.names=F)))
    SML.const.153 <- if(i==1) c(unlist(SML[[i]][,154],use.names=F)) else c(SML.const.153,c(unlist(SML[[i]][,154],use.names=F)))
    SML.const.154 <- if(i==1) c(unlist(SML[[i]][,155],use.names=F)) else c(SML.const.154,c(unlist(SML[[i]][,155],use.names=F)))
    SML.const.155 <- if(i==1) c(unlist(SML[[i]][,156],use.names=F)) else c(SML.const.155,c(unlist(SML[[i]][,156],use.names=F)))
    SML.const.156 <- if(i==1) c(unlist(SML[[i]][,157],use.names=F)) else c(SML.const.156,c(unlist(SML[[i]][,157],use.names=F)))
    SML.const.157 <- if(i==1) c(unlist(SML[[i]][,158],use.names=F)) else c(SML.const.157,c(unlist(SML[[i]][,158],use.names=F)))
    SML.const.158 <- if(i==1) c(unlist(SML[[i]][,159],use.names=F)) else c(SML.const.158,c(unlist(SML[[i]][,159],use.names=F)))
    SML.const.159 <- if(i==1) c(unlist(SML[[i]][,160],use.names=F)) else c(SML.const.159,c(unlist(SML[[i]][,160],use.names=F)))
    SML.const.160 <- if(i==1) c(unlist(SML[[i]][,161],use.names=F)) else c(SML.const.160,c(unlist(SML[[i]][,161],use.names=F)))
    SML.const.161 <- if(i==1) c(unlist(SML[[i]][,162],use.names=F)) else c(SML.const.161,c(unlist(SML[[i]][,162],use.names=F)))
    SML.const.162 <- if(i==1) c(unlist(SML[[i]][,163],use.names=F)) else c(SML.const.162,c(unlist(SML[[i]][,163],use.names=F)))
    SML.const.163 <- if(i==1) c(unlist(SML[[i]][,164],use.names=F)) else c(SML.const.163,c(unlist(SML[[i]][,164],use.names=F)))
    SML.const.164 <- if(i==1) c(unlist(SML[[i]][,165],use.names=F)) else c(SML.const.164,c(unlist(SML[[i]][,165],use.names=F)))
    SML.const.165 <- if(i==1) c(unlist(SML[[i]][,166],use.names=F)) else c(SML.const.165,c(unlist(SML[[i]][,166],use.names=F)))
    SML.const.166 <- if(i==1) c(unlist(SML[[i]][,167],use.names=F)) else c(SML.const.166,c(unlist(SML[[i]][,167],use.names=F)))
    SML.const.167 <- if(i==1) c(unlist(SML[[i]][,168],use.names=F)) else c(SML.const.167,c(unlist(SML[[i]][,168],use.names=F)))
    }

rm(i)

shift.starts <- data.frame(matrix(nrow=23,ncol=168*length(z)))
shift.starts[,] <- 0
for(i in 1:length(z)){
shift.starts[z[i],c(seq((i-1)*168+1,(i-1)*168+168))] <- rep(1,168)}

rm(i)

# rm(SML.const.0,SML.const.1,SML.const.2,SML.const.3,SML.const.4,SML.const.5,SML.const.6)
# rm(SML.const.7,SML.const.8,SML.const.9,SML.const.10,SML.const.11,SML.const.5,SML.const.12)
# rm(SML.const.13,SML.const.14,SML.const.15,SML.const.16,SML.const.17,SML.const.5,SML.const.18)
# rm(SML.const.19,SML.const.20,SML.const.21,SML.const.22,SML.const.23,SML.const.5,SML.const.24)

###

#defining parameters

obj.fun <- cost

constr <- matrix(c(

SML.const.0,SML.const.1,SML.const.2,SML.const.3,SML.const.4,SML.const.5,SML.const.6,
SML.const.7,SML.const.8,SML.const.9,SML.const.10,SML.const.11,SML.const.12,SML.const.13,
SML.const.14,SML.const.15,SML.const.16,SML.const.17,SML.const.18,SML.const.19,SML.const.20,
SML.const.21,SML.const.22,SML.const.23,

SML.const.24,SML.const.25,SML.const.26,SML.const.27,SML.const.28,SML.const.29,SML.const.30,
SML.const.31,SML.const.32,SML.const.33,SML.const.34,SML.const.35,SML.const.36,SML.const.37,
SML.const.38,SML.const.39,SML.const.40,SML.const.41,SML.const.42,SML.const.43,SML.const.44,
SML.const.45,SML.const.46,SML.const.47,

SML.const.48,SML.const.49,SML.const.50,SML.const.51,SML.const.52,SML.const.53,SML.const.54,
SML.const.55,SML.const.56,SML.const.57,SML.const.58,SML.const.59,SML.const.60,SML.const.61,
SML.const.62,SML.const.63,SML.const.64,SML.const.65,SML.const.66,SML.const.67,SML.const.68,
SML.const.69,SML.const.70,SML.const.71,

SML.const.72,SML.const.73,SML.const.74,SML.const.75,SML.const.76,SML.const.77,SML.const.78,
SML.const.79,SML.const.80,SML.const.81,SML.const.82,SML.const.83,SML.const.84,SML.const.85,
SML.const.86,SML.const.87,SML.const.88,SML.const.89,SML.const.90,SML.const.91,SML.const.92,
SML.const.93,SML.const.94,SML.const.95,

SML.const.96,SML.const.97,SML.const.98,SML.const.99,SML.const.100,SML.const.101,SML.const.102,
SML.const.103,SML.const.104,SML.const.105,SML.const.106,SML.const.107,SML.const.108,SML.const.109,
SML.const.110,SML.const.111,SML.const.112,SML.const.113,SML.const.114,SML.const.115,SML.const.116,
SML.const.117,SML.const.118,SML.const.119,

SML.const.120,SML.const.121,SML.const.122,SML.const.123,SML.const.124,SML.const.125,SML.const.126,
SML.const.127,SML.const.128,SML.const.129,SML.const.130,SML.const.131,SML.const.132,SML.const.133,
SML.const.134,SML.const.135,SML.const.136,SML.const.137,SML.const.138,SML.const.139,SML.const.140,
SML.const.141,SML.const.142,SML.const.143,

SML.const.144,SML.const.145,SML.const.146,SML.const.147,SML.const.148,SML.const.149,SML.const.150,
SML.const.151,SML.const.152,SML.const.153,SML.const.154,SML.const.155,SML.const.156,SML.const.157,
SML.const.158,SML.const.159,SML.const.160,SML.const.161,SML.const.162,SML.const.163,SML.const.164,
SML.const.165,SML.const.166,SML.const.167,

shift.starts[1,],shift.starts[2,],shift.starts[3,],
shift.starts[4,],shift.starts[5,],shift.starts[6,],shift.starts[7,],shift.starts[8,],
shift.starts[9,],shift.starts[10,],shift.starts[11,],shift.starts[12,],shift.starts[13,],
shift.starts[14,],shift.starts[15,],shift.starts[16,],shift.starts[17,],shift.starts[18,],
shift.starts[19,],shift.starts[20,],shift.starts[21,],shift.starts[22,],shift.starts[23,]), ncol = (length(z)*168), byrow=TRUE)

constr.dir <- c(rep(">=",168),rep("<=",23))

rhs <- c(demand,shift.limits) # demand$custom

#solving model

prod.sol <- lp(direction="min", obj.fun, constr, constr.dir, rhs, compute.sens = TRUE,int.vec=c(1:(length(z)*168)))

#accessing to R output

#prod.sol$objective
#prod.sol$solution
#prod.sol$duals #includes duals of constraints and reduced costs of variables
#prod.sol$duals.from
#prod.sol$duals.to
#prod.sol$sens.coef.from
#prod.sol$constraints

## test.melt; Used to create object for Start.Hour.graph  
optim.solution <- matrix(prod.sol$solution, ncol = 168, byrow=TRUE)
#rowSums(test) # 8  2 10 25  5
rownames(optim.solution) <- c(z)

df5.list[[1]] <- optim.solution

shift.starts.schedule <- data.frame(matrix(nrow=24,ncol=168))
shift.starts.schedule[c(as.numeric(input$asl)),] <- optim.solution
shift.starts.schedule[is.na(shift.starts.schedule)] <- 0
shift.starts.schedule$shift <- as.factor(1:24)
for(i in 1:length(shift.starts.schedule)-1){
    colnames(shift.starts.schedule)[i] <- paste0("Hour.",i-1)}
rm(i)

test.melt <- melt(shift.starts.schedule, id.vars = "shift")
test.melt$value.2 <- as.numeric(NA)
test.melt[which(test.melt$value>0),]$value.2 <- test.melt[which(test.melt$value>0),]$value   
test.melt[which(test.melt$value==0),]$value.2 <- NA  

df5.list[[2]] <- test.melt

SML.List <- SML

for(i in 1:length(input$asl)){
actual <- if(i==1) colSums(optim.solution[i,]*SML.List[[i]]) else actual + colSums(optim.solution[i,]*SML.List[[i]])}

#actual <- colSums(test[1,]*SML[[4]])+colSums(test[2,]*SML[[6]])+colSums(test[3,]*SML[[8]])+colSums(test[4,]*SML[[10]])+colSums(test[5,]*SML[[12]])
demand1 <- ceiling(dff.data.4$custom/as.numeric(input$nr))
extra <- actual-demand1

extra <- data.frame(x=names(actual),y=actual)
extra$demand <- demand1
names(extra)[1:2] <- c("hour","actual")
extra$extra <- extra$actual-extra$demand
extra$hour <- fct_inorder(extra$hour)

extra.melt <- melt(extra,id.vars="hour")
df5.list[[3]] <- extra.melt

df5.list

})

   # df6 -> (Reactive df) used to create objects for Shift.Schedule.graph -----------

df6 <- eventReactive(c(df5()[[1]],df4()[[1]]), {
  
df6list <- vector("list",3)

optim.solution3 <- df5()[[1]] 
dff.data.42 <- df4()[[1]]

p <- which(optim.solution3!=0,arr.ind = T)
p <- data.frame(row=as.numeric(p[,1]),col=as.numeric(p[,2]),shift=as.numeric(rownames(p)))

p <- p %>% arrange(col)
p$rep <- optim.solution3[which(optim.solution3!=0)]
p$start.hour <- p$col - 1

#p.final; Used to build #p.melt & p.summary.melt 
p.final <- data.frame(matrix(nrow=length(p$row),ncol=168))
p.final[,] <- NA

q <- 1

for(i in 1:length(p$row)){
    p.optim.solution3 <- seq(p[i,2],p[i,2]+p[i,3]-1,by=1)
    p.optim.solution3[p.optim.solution3>168] <- p.optim.solution3[p.optim.solution3>168]-168
    p.final[i,c(p.optim.solution3)] <- p[i,4]
    q <- q+1}
rm(i)

p.final[is.na(p.final)] <- 0

demand2 <- ceiling(dff.data.42$custom/as.numeric(input$nr))

p.final[c(length(p.final[,1])+1),] <- colSums(p.final)
rownames(p.final)[length(p.final[,1])] <- "Shift.Count"
p.final[c(length(p.final[,1])+1),] <- demand2
rownames(p.final)[length(p.final[,1])] <- "Demand"
p.final[c(length(p.final[,1])+1),] <- p.final[c(length(p.final[,1])-1),]-p.final[c(length(p.final[,1])),]
rownames(p.final)[length(p.final[,1])] <- "Extra"
p.final$shift <- NULL
p.final$shift.length <- NULL

df6list[[1]] <- p.final

## p.melt; Upper Shift Schedule Graph 

optim.solution4 <- p.final 

new_col_name <- NA
for(j in 1:168){
    new_col_name[j]<- paste("H",j-1,sep="_")}

p.short <- optim.solution4[-c(length(optim.solution4[,1]),length(optim.solution4[,1])-1,length(optim.solution4[,1])-2),]
p.short$shift <- 1:length(p.short$X1)
p.short$shift.length <- rowSums(p.short[,c(1:168)])
names(p.short)[1:168] <- new_col_name


p.melt <- melt(data=p.short, id.vars = c("shift","shift.length"))
p.melt$value.2 <- as.numeric(NA)
p.melt[which(p.melt$value>0),]$value.2 <- p.melt[which(p.melt$value>0),]$value   
p.melt[which(p.melt$value==0),]$value.2 <- NA  

p.melt <- p.melt %>% arrange(-shift.length)

df6list[[2]] <- p.melt

## p.summary.melt; Lower Shift Schedule Graph 

optim.solution5 <- p.final 

new_col_name <- NA
for(j in 1:168){
    new_col_name[j]<- paste("H",j-1,sep="_")}

p.summary <- rbind(optim.solution5[c(length(optim.solution5[,1])-2,length(optim.solution5[,1])-1,length(optim.solution5[,1])),])
p.summary$row <- 1:length(p.summary$X1)

names(p.summary)[1:168] <- new_col_name

p.summary.melt <- melt(data=p.summary, id.vars = c("row"))
p.summary.melt$value.2 <- as.numeric(NA)
p.summary.melt[which(p.summary.melt$value>0),]$value.2 <- p.summary.melt[which(p.summary.melt$value>0),]$value   
p.summary.melt[which(p.summary.melt$value==0),]$value.2 <- NA 

df6list[[3]] <- p.summary.melt

df6list

})

   # optim_graphs -> Staffing Plots & data object used to create plots ------

optim_graphs <- eventReactive(c(df5(),df6(),input$optim.csv), {

optim.graphs.list <- vector("list",6)
  
    test.melt2 <- df5()[[2]]        # Start.Hour.graph
    extra.melt2 <- df5()[[3]]       # Supply_Demand.graph
    p.melt2 <- df6()[[2]]           # Shift Schedule Upper Graph
    p.summary.melt2 <- df6()[[3]]   # Shift Schedule Upper Graph
    
days <- c(24,48,72,96,120,144)
days.labels <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
days.labels.position <- c(12,36,60,84,108,132,156)

Start.Hour.graph <- ggplot(test.melt2,aes(x=variable,y=as.factor(shift),fill=value))+
     geom_tile(color= "grey",size=0.1) + geom_text(aes(label = value.2),size=2) +
     geom_vline(xintercept=days,colour="black",linetype="solid",size=1.5)+
     annotate("text", x = days.labels.position, y = max(as.numeric(test.melt2$shift))+1.5, vjust= +1, label = days.labels ,size=5,colour="black") + 
     scale_fill_gradient(low = "white", high = "red")+
     labs(title="Nurse Shift Schedule Start Hours", y="Shift Duration",x="# of Nurses Starting specified shift at this Hour")+
     theme(axis.text.x = element_text(hjust=0.5,vjust=0,angle=90, size=8))

Supply_Demand.graph <- ggplot(data=extra.melt2,aes(x=hour,y=value,group=variable))+geom_line(aes(colour=extra.melt2$variable))+ geom_point(aes(colour=extra.melt2$variable))+
    scale_y_continuous(limits=c(-0.25,max(extra.melt2$value)),breaks=c(seq(0,max(extra.melt2$value),by=5)),minor_breaks = c(seq(0,max(extra.melt2$value),by=1)))+
    geom_text(aes(label = value,colour=extra.melt2$variable),vjust = -1,size=3)+labs(title="Nurse Supply, Demand & Excess with optimized Schedule",y="Nurses",x="Hours")+
    geom_vline(xintercept=days,colour="black",linetype="solid",size=1.5)+
    annotate("text", x = days.labels.position, y = -0.125, vjust= 0, label = days.labels ,size=5,colour="black") + 
    theme(axis.text.x = element_text(hjust=0.5,vjust=0,angle=90, size=8))

p1 <- ggplot(data=p.melt2,aes(x=variable,y=as.factor(shift),fill=value))+
    geom_tile(color= "lightskyblue4",size=0.1) +
    geom_text(aes(label = value.2),size=1.5,colour="black")+
    geom_vline(xintercept=days,colour="black",linetype="solid",size=1.5)+
    annotate("text", x = days.labels.position, y = max(as.numeric(p.melt2$shift))+1.5, vjust= +1, label = days.labels ,size=5,colour="black") + 
    scale_fill_gradient(low = "grey90", high = "lightcoral")+ theme(legend.position="none")+
    ggtitle(paste("Optimized Nurse Schedule with a ",as.numeric(input$nr)," to 1 Nurse Ratio; Allowable Shift Lengths: ",paste(as.numeric(c(input$asl)), collapse=", "), "; \nand Allowable Shift Start Hours: ", paste(rev(sort(as.numeric(c(input$assh)),decreasing=T)), collapse=", ")," hours replicated across each Weekday"))+
    theme(axis.text.x = element_text(hjust=0.5,vjust=0,angle=90, size=8))+labs(x="Hours of the Week (0:167; Sunday thru Saturday)",y="Shifts")

p2 <- ggplot(data=p.summary.melt2,aes(x=variable,y=as.factor(row)))+
  geom_tile(fill="white",color= "grey",size=0.1)+
  geom_text(aes(label = value.2),size=1.5,colour="black")+ scale_y_discrete(labels=c("1" = "Supply", "2" = "Demand",
  "3" = "Surplus"))+ylab("")+ theme(axis.text.y = element_text(angle = 70, size = 8))+
    theme(axis.text.x = element_text(hjust=0.5,vjust=0,angle=90, size=8))+labs(x="Hours of the Week (0:167; Sunday thru Saturday)",y="Shifts")

Shift.Schedule.Graph <- ggarrange(p1,p2,ncol=1,nrow=2,heights=c(4,1))
#widths = c(2, 1) would make the first column twice as wide as the second column.

optim29 <- df6()[[2]]
optim29 <- optim29[,c(1,3,4)]
optim29$variable <- fct_inorder(optim29$variable)
optim29 <- optim29 %>% arrange(shift,variable)
optim29.cast <- dcast(data=optim29, formula=shift~variable)

optim27 <- df5()[[3]] 
optim27$hour <- fct_inorder(optim27$hour)
optim27 <- optim27 %>% arrange(variable,hour)
optim27.cast <- dcast(data=optim27, formula=variable~hour)

optim26 <- df5()[[2]]   
optim26 <- optim26[,c(1:3)]
optim26$variable <- fct_inorder(optim26$variable)
optim26 <- optim26 %>% arrange(shift,variable)
optim26.cast <- dcast(data=optim26, formula=shift~variable)  

optim.graphs.list[[1]] <- Start.Hour.graph
optim.graphs.list[[2]] <- Supply_Demand.graph
optim.graphs.list[[3]] <- Shift.Schedule.Graph
optim.graphs.list[[4]] <- optim29.cast # Shift Schedule Upper Graph Data
optim.graphs.list[[5]] <- optim27.cast # Supply & Demand Graph Data
optim.graphs.list[[6]] <- optim26.cast # Start Hour Graph Data
optim.graphs.list       
})
    

   # staff.optim.plot -> optim.plot; Selects the plot to be displayed ------------------------
output$staff.optim.plot <- renderPlot({
  
optim.plot <- switch(input$optim.graph,
                     "Start.Hour.graph"=optim_graphs()[[1]],
                     "Supply_Demand.graph"=optim_graphs()[[2]],
                     "Shift.Schedule.Graph"=optim_graphs()[[3]])

optim.plot

})


   # App Input Data for xlsx download -------------------
inputparameters <- eventReactive(c(df(),input$ts2,input$outliers,input$inDateRange,input$custom.percentile,
                                  input$stat.week,input$non.allowed.demand.hours,input$asl,input$assh,
                                  input$nr,input$num1sh,input$num2sh,input$num3sh,input$num4sh,input$num5sh,
                                  input$num6sh,input$num7sh,input$num8sh,input$num9sh,input$num10sh,
                                  input$num11sh,input$num12sh,input$num13sh,input$num14sh,input$num15sh,
                                  input$num16sh,input$num17sh,input$num18sh,input$num19sh,input$num20sh,
                                  input$num21sh,input$num22sh,input$num23sh), {
                                    
   Stat.Week2 =                paste(unlist(input$stat.week),collapse=',')
   NotAllowedDemandHours2 =    paste(unlist(input$non.allowed.demand.hours),collapse=',')
   AllowableShiftLengths2 =    paste(unlist(input$asl),collapse=',')   
   AllowableShiftStartHours2 = paste(unlist(input$assh),collapse=',')                                  
                                    
   Staffing.Inputs <- data.frame(
   
   File.Name =                df()[[2]], # File Name
   Data.Type =                if(input$ts2==FALSE) "Arrival Curve (EndDTS = StartDTS + 1 minute)" else "StartDTS & EndDTS included in the data", # Arrival or Census curve
   Outliers =                 if(input$outliers==TRUE) "Outliers included!" else "NO Outliers included.", # Include Outliers or not
   StartDTS =                 strftime(input$inDateRange[[1]], format="%Y-%m-%d %H:%M:%S"), # Minimum selected StartDTS
   EndDTS =                   strftime(input$inDateRange[[2]], format="%Y-%m-%d %H:%M:%S"), # Maximum selected EndDTS
   Custom.Percentile =        input$custom.percentile, # CustomPercentile
   Stat.Week =                Stat.Week2,  # StatWeek
   NotAllowedDemandHours =    NotAllowedDemandHours2, # NotAllowedDemandHours
   AllowableShiftLengths =    AllowableShiftLengths2,  # AllowableShiftLengths
   AllowableShiftStartHours = AllowableShiftStartHours2,  # AllowableShiftStartHours   
   StaffingRatio =            input$nr,  # StaffingRatio  
   
   Shift.Limits.1 =           input$num1sh,   # Shift 1 Limits
   Shift.Limits.2 =           input$num2sh,   # Shift 2 Limits   
   Shift.Limits.3 =           input$num3sh,   # Shift 3 Limits                                
   Shift.Limits.4 =           input$num4sh,   # Shift 4 Limits                                
   Shift.Limits.5 =           input$num5sh,   # Shift 5 Limits
   Shift.Limits.6 =           input$num6sh,   # Shift 6 Limits
   Shift.Limits.7 =           input$num7sh,   # Shift 7 Limits
   Shift.Limits.8 =           input$num8sh,   # Shift 8 Limits
   Shift.Limits.9 =           input$num9sh,   # Shift 9 Limits   
   Shift.Limits.10 =          input$num10sh,   # Shift 10 Limits                                
   Shift.Limits.11 =          input$num11sh,   # Shift 11 Limits                                
   Shift.Limits.12 =          input$num12sh,   # Shift 12 Limits
   Shift.Limits.13 =          input$num13sh,   # Shift 13 Limits                                
   Shift.Limits.14 =          input$num14sh,   # Shift 14 Limits                                
   Shift.Limits.15 =          input$num15sh,   # Shift 15 Limits
   Shift.Limits.16 =          input$num16sh,   # Shift 16 Limits                                
   Shift.Limits.17 =          input$num17sh,   # Shift 17 Limits                                
   Shift.Limits.18 =          input$num18sh,   # Shift 18 Limits
   Shift.Limits.19 =          input$num19sh,   # Shift 19 Limits   
   Shift.Limits.20 =          input$num20sh,   # Shift 20 Limits 
   Shift.Limits.21 =          input$num21sh,   # Shift 21 Limits                                
   Shift.Limits.22 =          input$num22sh,   # Shift 22 Limits
   Shift.Limits.23 =          input$num23sh)   # Shift 23 Limits                                

 Staffing.Inputs2 <- data.table::transpose(Staffing.Inputs)
 names(Staffing.Inputs2) <- "Input.Values"
 rownames(Staffing.Inputs2) <- colnames(Staffing.Inputs)
 Staffing.Inputs2$Input.Names <- rownames(Staffing.Inputs2)
 Staffing.Inputs2 <- Staffing.Inputs2 %>% select(Input.Names,Input.Values)

 Staffing.Inputs2 
   })
   # App xlsx download -------------------                     
 output$optim.downloadData <- downloadHandler(

    filename = function() { paste("Staffing.Optimization_",inputparameters()$File.Name,"_",as.Date(input$inDateRange[[1]]),"_to_",as.Date(input$inDateRange[[2]]),".xlsx")},

    content = function(file){
         
      # Create xlsx
      Results_Workbook <- createWorkbook(type='xlsx')    
      formatt <- CellStyle(Results_Workbook) + Alignment(h="ALIGN_CENTER") + Border(color="black", position=c("BOTTOM", "LEFT", "TOP", "RIGHT"),pen=c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN"))
      format1 <- rep(list(formatt), 169) 
      names(format1) <- seq(from=1, to=169, by = 1)

      format.1.1 <- format1[c(1:4)] # Raw Data
      format.1.2 <- format1[c(6:11)] # Variable summary
      format.1.3 <- format1[c(13:14)] # Arrival Data
      format.1.4 <- format1[c(16:17)] # BP All summary

      format.3 <- format1[c(1:2)] # Inputs
      format.4 <- format1[c(1:8)] # Stat Census
      format.2 <- format1 # Shift Schedule, Start Hour, Supply & Demand
      
      csTableColNames <- CellStyle(Results_Workbook) + Font(Results_Workbook, isBold=TRUE) + Alignment(h="ALIGN_CENTER") +  Border(color="black", position=c("BOTTOM", "LEFT", "TOP", "RIGHT"),pen=c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN"))
      
      # add sheet 1 raw data, variable summary, and arrival data/plot
      sheet.1 <- createSheet(Results_Workbook, sheetName = "Raw.Data")
      addDataFrame(df()[[3]][c(1:1000),], sheet=sheet.1, startRow=1, # Raw Data
                   startColumn=1,row.names=FALSE,colStyle=format.1.1, colnamesStyle=csTableColNames)#
      autoSizeColumn(sheet.1, colIndex=c(1:4))
      gc()
      addDataFrame(df()[[4]], sheet=sheet.1, startRow=1, # EncID, StartDTS, & EndDTS data Summary
                   startColumn=6,row.names=FALSE,colStyle=format.1.2, colnamesStyle=csTableColNames)
      autoSizeColumn(sheet.1, colIndex=c(6:11))
      gc()
      addDataFrame(df()[[5]], sheet=sheet.1, startRow=1, # Arrival Frequency Table 
                   startColumn=13,row.names=FALSE,colStyle=format.1.3, colnamesStyle=csTableColNames)
      autoSizeColumn(sheet.1, colIndex=c(13:14))
      gc()
      addDataFrame(df3()[[4]], sheet=sheet.1, startRow=1, # Boxplot Summary Data
                   startColumn=16,row.names=FALSE,colStyle=format.1.4, colnamesStyle=csTableColNames)
      setColumnWidth(sheet.1,colIndex=c(16:17),colWidth=20)

      plot.1 <- df()[[6]]; plot.1
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/arrival.curve.jpeg",plot=plot.1, device="jpeg",width=14,height=8.5,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/arrival.curve.jpeg", sheet = sheet.1, scale = 1,startRow = 14, startColumn = 16)
      gc()
      
      plot.2 <- df3()[[2]]; plot.2
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/bp.all.data.jpeg",plot=plot.2, device="jpeg",width=4.25,height=7,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/bp.all.data.jpeg", sheet = sheet.1, scale = 0.75,startRow = 9, startColumn = 6)
      gc()
      
      plot.3 <- df3()[[3]]; plot.3
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/bp.filtered.data.jpeg",plot=plot.3, device="jpeg",width=4.25,height=7,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/bp.filtered.data.jpeg", sheet = sheet.1, scale = 0.75,startRow = 9, startColumn = 9)
    
      #add sheet 2 Model Inputs    
      sheet.2 <- createSheet(Results_Workbook, sheetName = "Setup.Inputs")
      addDataFrame(inputparameters(), sheet=sheet.2, startRow=1, 
                   startColumn=1,row.names=FALSE,colStyle=format.3, colnamesStyle=csTableColNames )
      autoSizeColumn(sheet.2, colIndex=1:ncol(inputparameters()))
      gc()
      
      # add sheet 3 Statistical Census data & plot
      sheet.3 <- createSheet(Results_Workbook, sheetName = "Stat.Census")
      addDataFrame(df4()[[1]], sheet=sheet.3, startRow=1, # Stat.Census
                   startColumn=1,row.names=FALSE,colStyle=format.4, colnamesStyle=csTableColNames )
      autoSizeColumn(sheet.3 , colIndex=1:ncol(df4()[[1]]))
      gc()

      plot.4 <- df4()[[2]]; plot.4
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/stat.census.jpeg",plot=plot.4, device="jpeg",width=14,height=8.5,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/stat.census.jpeg", sheet = sheet.3, scale = 1,startRow = 1, startColumn = 10)
      gc()
      
      # add sheet 4 Shift Schedule data & plot
      sheet.4 <- createSheet(Results_Workbook, sheetName = "Shift.Schedule")
      addDataFrame(optim_graphs()[[4]], sheet=sheet.4, startRow=1, # Shift.Schedule
                   startColumn=1,row.names=FALSE,colStyle=format.2, colnamesStyle=csTableColNames )
      autoSizeColumn(sheet.4 , colIndex=1:ncol(optim_graphs()[[4]]))
      gc()
      
      plot.5 <- optim_graphs()[[3]]; plot.5
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/shift.schedule.jpeg",plot=plot.5, device="jpeg",width=14,height=8.5,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/shift.schedule.jpeg", sheet = sheet.4, scale = 1,startRow = nrow(optim_graphs()[[4]])+3, startColumn = 1)
      gc()
      
      # add sheet 5 Start Hour data & plot
      sheet.5 <- createSheet(Results_Workbook, sheetName = "Start.Hour")
      addDataFrame(optim_graphs()[[6]], sheet=sheet.5, startRow=1, # Start Hour
                   startColumn=1,row.names=FALSE,colStyle=format.2, colnamesStyle=csTableColNames )
      autoSizeColumn(sheet.5 , colIndex=1:ncol(optim_graphs()[[6]]))
      gc()
      
      plot.6 <- optim_graphs()[[1]]; plot.6
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/start.hour.jpeg",plot=plot.6, device="jpeg",width=14,height=8.5,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/start.hour.jpeg", sheet = sheet.5, scale = 1,startRow = 27, startColumn = 1)
      gc()
      
      # add sheet 6 Supply & Demand data & plot
      sheet.6 <- createSheet(Results_Workbook, sheetName = "Supply.Demand")
      addDataFrame(optim_graphs()[[5]], sheet=sheet.6, startRow=1, # Supply & Demand
                   startColumn=1,row.names=FALSE,colStyle=format.2, colnamesStyle=csTableColNames )
      autoSizeColumn(sheet.6, colIndex=1:ncol(optim_graphs()[[5]]))
      gc()
      
      plot.7 <- optim_graphs()[[2]]; plot.7
      ggsave("C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/supply.demand.jpeg",plot=plot.7, device="jpeg",width=14,height=8.5,units="in")   
      addPicture(file = "C:/Users/mhhccd/Dropbox/Work.Synched/Staffing.Optimization.App/supply.demand.jpeg", sheet = sheet.6, scale = 1,startRow = 6, startColumn = 1)
      gc()
      
      saveWorkbook(Results_Workbook,file)
    } 

  )



} # Close for server <- function(input, output,session) { --------

shinyApp(ui, server)

## Close Shiny App debugging -------
#sink(type="message")
#close(zz)