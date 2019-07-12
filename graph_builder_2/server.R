library(shiny)
library(ggplot2)
library(Rmisc)

shinyServer(function(input, output) {
  options(scipen=100000)
  old_wd <- getwd()
  
  observeEvent(input$about, {
  showModal(modalDialog(
  title = "Graph builder info",
  "This app utilizes normalized read count tables obtained from the DESeq2 (1.12.4) dds() function applied to the raw count data.
   The pipeline to get the raw counts is as follows: 
   raw reads are first filtered using the fastq-mcf utility from ea-utils package (1.1.2), 
   then two-pass mapped to the P. axillaris reference genome (1.6.2) with STAR (2.5.0b) 
   and finally counted according to the gene models (v4) via htseq-count utility from HTSeq (0.6.1).
   Where replicates are available, the graphs are built using the mean value of the read counts over them, 
   along with showing the standart deviation over replicates as error bars."
  ))})
  
  observeEvent(input$sbmenu, {
  if (input$sbmenu == "growth") {
    
observeEvent(input$growth_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["growth",]), duration = NULL, closeButton = TRUE, id = "not")
})
    
  growthfile <- read.table("files/growth", header = T, row.names = 1, check.names=FALSE)       #Read data
  
  time <- c( rep("1", 3), rep("2", 3), rep("4", 3), 
             rep("1", 3), rep("2", 3), rep("4", 3), 
             rep("1", 3), rep("2", 3), rep("4", 3),
             rep("1", 3), rep("2", 3), rep("3", 3), rep("4", 6),
             rep("1", 3), rep("2", 3), rep("3", 3), rep("4", 6),
             rep("1", 3), rep("2", 3), rep("3", 3), rep("4", 6),
             rep("2", 3), rep("3", 3), rep("4", 3), 
             rep("2", 3), rep("3", 3), rep("4", 3), 
             rep("2", 3), rep("3", 3), rep("4", 3))
  
  count <- as.data.frame(matrix(0, nrow = 99, ncol = 3))
  colnames(count) <- c("count","condition", "time")
  observeEvent(input$plot1, {
  output$plot1 <- renderPlot({
  isolate({
      count$count <- as.numeric(as.character(c(t(growthfile[input$id1,]))))     
      count$condition <- colnames(growthfile)
      count$time <- time           
  #This part below is responsible for plot building
  pal <- c("#e8d800", "#00ffff", "#91cf60", "#1a9850", "#ea02cf", "#fc8d59","#d73027","#67a9cf","#2166ac")
  countstat <- summarySE(count, measurevar="count", groupvars=c("condition", "time"))
  countstat$tissue <- c(rep("style",10), rep("tube",3),rep("style",7), rep("tube",3),rep("style",4), rep("tube",3))
  p <- ggplot(countstat, aes(x=time, y=count, color=condition, group=condition)) + 
    geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) + geom_line(size=1) + geom_point(size=4) +
    scale_colour_manual(values=pal) + theme(axis.text=element_text(size=12))
  })   #end of isolate
    if (input$style1 == 1) {
      p + facet_grid(tissue ~ .)          #This part interactively changes plot data grouping type
    }
    else if (input$style1 == 2) {
      p + facet_grid(. ~ tissue) 
    }
    else if (input$style1 == 0) {
      p
    }
   })  #end of renderPlot
  })   #end of observeEvent1

output$dow1 <- downloadHandler(
  filename = 'images.zip',
  content = function(fname) {
    on.exit(setwd(old_wd))
    if (input$mtype1 == "list") {
      candidate <- unlist(strsplit(input$list1, split=" "))   #Here plots are built and archived for download
    }
    else if (input$mtype1 == "file") {
      inframe <- input$file1
      tempo <- read.csv(inframe$datapath)
      candidate <- tempo[,1]
    }
 withProgress(message = "Creating archive:", value = 0, {
   fs <- c()
   tmpdir <- tempdir()
   setwd(tempdir())
   pal <- c("#e8d800", "#00ffff", "#91cf60", "#1a9850", "#ea02cf", "#fc8d59","#d73027","#67a9cf","#2166ac")
  for(j in candidate) {               #Start of big for loop
      count$count <- as.numeric(as.character(c(t(growthfile[j,]))))
      count$condition <- colnames(growthfile)
      count$time <- time
  countstat <- summarySE(count, measurevar="count", groupvars=c("condition", "time"))
  countstat$tissue <- c(rep("style",10), rep("tube",3),rep("style",7), rep("tube",3),rep("style",4), rep("tube",3))
  pi <- ggplot(countstat, aes(x=time, y=count, color=condition, group=condition)) + 
    geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) + geom_line(size=1) + geom_point(size=4) +
    scale_colour_manual(values=pal) + theme(axis.text=element_text(size=12))
  
  if (input$savestyle1 == 1) {
    pi <- pi + facet_grid(tissue ~ .)
  }                                        #Applying plot style preferences here
  else if (input$savestyle1 == 2) {
    pi <- pi + facet_grid(. ~ tissue) 
  }
  
  graphname <- paste(j,".png",sep="")
  fs <- append(fs, graphname)              #Writing to ID-defined file
  png(file=graphname, width = 960, height = 480, units = "px")    
  print(pi)
  dev.off()
                                           #Updating progress bar
 incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
    }  #end of big for loop
  zip(zipfile=fname, files=fs)             #Archiving
    }) #end of withProgress
   }   #end of content  
  )    #end of downloadHandler
} #end of growth condition
    else if (input$sbmenu == "color") {
      
observeEvent(input$color_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["color",]), duration = NULL, closeButton = TRUE, id = "not")
})  

  colorfile <- read.table("files/color", header = T, row.names = 1, check.names=FALSE)       #Read data 
  count <- as.data.frame(matrix(0, nrow = 18, ncol = 2))
  colnames(count) <- c("count","condition")
  
  observeEvent(input$plot2, {
  output$plot2 <- renderPlot({
  isolate({
    count$count <- as.numeric(as.character(c(t(colorfile[input$id2,]))))     
    count$condition <- colnames(colorfile)
    #This part below is responsible for plot building
    pal <- c("#fffeff", "#ffffff", "#e692ec", "#eba7f0", "#8f1a98", "#a31ead")
    countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
    p <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
      geom_bar(stat="identity", width=0.5) +
      geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
      scale_colour_manual(values=c(rep("black", 6))) + 
      theme(axis.text=element_text(size=13)) +
      scale_fill_manual(values=pal)
}) #end of isolate
    p
}) #end of renderPlot
}) #end of observeEvent2
  output$dow2 <- downloadHandler(
    filename = 'images.zip',
    content = function(fname) {
    on.exit(setwd(old_wd))
      if (input$mtype2 == "list") {
        candidate <- unlist(strsplit(input$list2, split=" "))   #Here plots are built and archived for download
      }
      else if (input$mtype2 == "file") {
   inframe <- input$file2
   tempo <- read.csv(inframe$datapath)
   candidate <- tempo[,1]
      }
  withProgress(message = "Creating archive:", value = 0, {
  fs <- c()
  tmpdir <- tempdir()
  setwd(tempdir())
  pal <- c("#fffeff", "#ffffff", "#e692ec", "#eba7f0", "#8f1a98", "#a31ead")
  for(j in candidate) { #Start of big for loop
    count$count <- as.numeric(as.character(c(t(colorfile[j,]))))     
    count$condition <- colnames(colorfile)
    countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
    pi <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
      geom_bar(stat="identity", width=0.5) +
      geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
      scale_colour_manual(values=c(rep("black", 6))) + 
      theme(axis.text=element_text(size=13)) +
      scale_fill_manual(values=pal)
    
    graphname <- paste(j,".png",sep="")
    fs <- append(fs, graphname)              #Writing to ID-defined file
    png(file=graphname, width = 960, height = 480, units = "px")    
    print(pi)
    dev.off()
  incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
        }  #end of big for loop
  zip(zipfile=fname, files=fs)             #Archiving
        }) #end of withProgress
      }   #end of content  
  )    #end of downloadHandler
} #end of color condition
else if (input$sbmenu == "cali") {
  
observeEvent(input$cali_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["cali",]), duration = NULL, closeButton = TRUE, id = "not")
})  
  
      califile <- read.table("files/cali", header = T, row.names = 1, check.names=FALSE)       #Read data 
      count <- as.data.frame(matrix(0, nrow = 9, ncol = 2))
      ord <- unique(colnames(califile))
      colnames(count) <- c("count","condition")
      observeEvent(input$plot3, {
        output$plot3 <- renderPlot({
          isolate({
            count$count <- as.numeric(as.character(c(t(califile[input$id3,]))))     
            count$condition <- colnames(califile)
            #This part below is responsible for plot building
            pal <- c("#e59cce", "#d140d1", "#eceed9")
            countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
            p <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
              geom_bar(stat="identity", width=0.5) +
              geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
              scale_colour_manual(values=c(rep("black", 3))) + 
              scale_x_discrete(limits=ord) +
              theme(axis.text=element_text(size=13)) +
              scale_fill_manual(values=pal)
      }) #end of isolate
          p
      }) #end of renderPlot
      }) #end of observeEvent2
      output$dow3 <- downloadHandler(
        filename = 'images.zip',
        content = function(fname) {
          on.exit(setwd(old_wd))
          if (input$mtype3 == "list") {
            candidate <- unlist(strsplit(input$list3, split=" "))   #Here plots are built and archived for download
          }
          else if (input$mtype3 == "file") {
            inframe <- input$file3
            tempo <- read.csv(inframe$datapath)
            candidate <- tempo[,1]
          }
          withProgress(message = "Creating archive:", value = 0, {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            pal <- c("#e59cce", "#d140d1", "#eceed9")
            ord <- unique(colnames(califile))
            for(j in candidate) { #Start of big for loop
              count$count <- as.numeric(as.character(c(t(califile[j,]))))     
              count$condition <- colnames(califile)
              countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
              pi <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
                geom_bar(stat="identity", width=0.5) +
                geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
                scale_colour_manual(values=c(rep("black", 3))) + 
                scale_x_discrete(limits=ord) +
                theme(axis.text=element_text(size=13)) +
                scale_fill_manual(values=pal)
              
              graphname <- paste(j,".png",sep="")
              fs <- append(fs, graphname)              #Writing to ID-defined file
              png(file=graphname, width = 960, height = 480, units = "px")    
              print(pi)
              dev.off()
              incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
            }  #end of big for loop
            zip(zipfile=fname, files=fs)             #Archiving
          }) #end of withProgress
        }   #end of content  
      )    #end of downloadHandler
} # end of cali condition
    else if (input$sbmenu == "scent") {

observeEvent(input$scent_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["scent",]), duration = NULL, closeButton = TRUE, id = "not")
})  

      scentfile <- read.table("files/scent", header = T, row.names = 1, check.names=FALSE)       #Read data 
      count <- as.data.frame(matrix(0, nrow = 15, ncol = 5))
      colnames(count) <- c("count","condition")
      ord <- unique(colnames(scentfile))
      
      observeEvent(input$plot4, {
        output$plot4 <- renderPlot({
          isolate({
            count$count <- as.numeric(as.character(c(t(scentfile[input$id4,]))))     
            count$condition <- colnames(scentfile)
            #This part below is responsible for plot building
            pal <- c("#ffafaf","#eba7f0","#ffffff","#ff4646","#a31ead")
            countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
            p <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
              geom_bar(stat="identity", width=0.5) +
              geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
              scale_x_discrete(limits=ord) +
              scale_colour_manual(values=c(rep("black", 5))) + 
              theme(axis.text=element_text(size=13)) +
              scale_fill_manual(values=pal)
          }) #end of isolate
          p
        }) #end of renderPlot
      }) #end of observeEvent2
      output$dow4 <- downloadHandler(
        filename = 'images.zip',
        content = function(fname) {
          on.exit(setwd(old_wd))
          if (input$mtype4 == "list") {
            candidate <- unlist(strsplit(input$list4, split=" "))   #Here plots are built and archived for download
          }
          else if (input$mtype4 == "file") {
            inframe <- input$file4
            tempo <- read.csv(inframe$datapath)
            candidate <- tempo[,1]
          }
          withProgress(message = "Creating archive:", value = 0, {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            pal <- c("#ffafaf","#eba7f0","#ffffff","#ff4646","#a31ead")
            ord <- unique(colnames(scentfile))
            for(j in candidate) { #Start of big for loop
              count$count <- as.numeric(as.character(c(t(scentfile[j,]))))     
              count$condition <- colnames(scentfile)
              countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
              pi <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
                geom_bar(stat="identity", width=0.5) +
                geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) + 
                scale_x_discrete(limits=ord) +
                scale_colour_manual(values=c(rep("black", 5))) + 
                theme(axis.text=element_text(size=13)) +
                scale_fill_manual(values=pal)
              
              graphname <- paste(j,".png",sep="")
              fs <- append(fs, graphname)              #Writing to ID-defined file
              png(file=graphname, width = 960, height = 480, units = "px")    
              print(pi)
              dev.off()
              incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
            }  #end of big for loop
            zip(zipfile=fname, files=fs)             #Archiving
          }) #end of withProgress
        }   #end of content  
      )    #end of downloadHandler
    } # end of scent condition
else if (input$sbmenu == "scentold") {
  
observeEvent(input$scentold_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["scentold",]), duration = NULL, closeButton = TRUE, id = "not")
})  
  
      scentoldfile <- read.table("files/scentold", header = T, row.names = 1, check.names=FALSE)       #Read data 
      count <- as.data.frame(matrix(0, nrow = 16, ncol = 2))
      colnames(count) <- c("count","condition")
      ord <- unique(colnames(scentoldfile))
      
      observeEvent(input$plot5, {
        output$plot5 <- renderPlot({
          isolate({
            count$count <- as.numeric(as.character(c(t(scentoldfile[input$id5,]))))     
            count$condition <- colnames(scentoldfile)
            #This part below is responsible for plot building
            pal <- c("#fffeff","#fffeff","#fffeff","#fffeff","#ff4646","#a31ead","#d233e0")
            countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
            p <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
              geom_bar(stat="identity", width=0.5) +
              geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
              scale_x_discrete(limits=ord) +
              scale_colour_manual(values=c(rep("black", 7))) + 
              theme(axis.text=element_text(size=13)) +
              scale_fill_manual(values=pal)
          }) #end of isolate
          p
        }) #end of renderPlot
      }) #end of observeEvent2
      output$dow5 <- downloadHandler(
        filename = 'images.zip',
        content = function(fname) {
          on.exit(setwd(old_wd))
          if (input$mtype5 == "list") {
            candidate <- unlist(strsplit(input$list5, split=" "))   #Here plots are built and archived for download
          }
          else if (input$mtype4 == "file") {
            inframe <- input$file5
            tempo <- read.csv(inframe$datapath)
            candidate <- tempo[,1]
          }
          withProgress(message = "Creating archive:", value = 0, {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            pal <- c("#eba7f0","#ffffff","#ff4646","#fffeff","#ff4646","#a31ead","#d233e0")
            ord <- unique(colnames(scentoldfile))
            for(j in candidate) { #Start of big for loop
              count$count <- as.numeric(as.character(c(t(scentoldfile[j,]))))     
              count$condition <- colnames(scentoldfile)
              countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
              pi <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
                geom_bar(stat="identity", width=0.5) +
                geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) + 
                scale_x_discrete(limits=ord) +
                scale_colour_manual(values=c(rep("black", 7))) + 
                theme(axis.text=element_text(size=13)) +
                scale_fill_manual(values=pal)
              
              graphname <- paste(j,".png",sep="")
              fs <- append(fs, graphname)              #Writing to ID-defined file
              png(file=graphname, width = 960, height = 480, units = "px")    
              print(pi)
              dev.off()
              incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
            }  #end of big for loop
            zip(zipfile=fname, files=fs)             #Archiving
          }) #end of withProgress
        }   #end of content  
      )    #end of downloadHandler
    } # end of scentold condition
    else if (input$sbmenu == "colorold") {
      
observeEvent(input$colorold_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["colorold",]), duration = NULL, closeButton = TRUE, id = "not")
})  
      
      coloroldfile <- read.table("files/colorold", header = T, row.names = 1, check.names=FALSE)       #Read data 
      count <- as.data.frame(matrix(0, nrow = 18, ncol = 2))
      colnames(count) <- c("count","condition")
      ord <- unique(colnames(coloroldfile))
      
      observeEvent(input$plot6, {
        output$plot6 <- renderPlot({
          isolate({
            count$count <- as.numeric(as.character(c(t(coloroldfile[input$id6,]))))     
            count$condition <- colnames(coloroldfile)
            #This part below is responsible for plot building
            pal <- c("#fffeff","#ff4646","#a883c9","#ff4646","#b49bcc")
            countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
            p <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
              geom_bar(stat="identity", width=0.5) +
              geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
              scale_x_discrete(limits=ord) +
              scale_colour_manual(values=c(rep("black", 5))) + 
              theme(axis.text=element_text(size=13)) +
              scale_fill_manual(values=pal)
          }) #end of isolate
          p
        }) #end of renderPlot
      }) #end of observeEvent2
      output$dow6 <- downloadHandler(
        filename = 'images.zip',
        content = function(fname) {
          on.exit(setwd(old_wd))
          if (input$mtype6 == "list") {
            candidate <- unlist(strsplit(input$list6, split=" "))   #Here plots are built and archived for download
          }
          else if (input$mtype6 == "file") {
            inframe <- input$file6
            tempo <- read.csv(inframe$datapath)
            candidate <- tempo[,1]
          }
          withProgress(message = "Creating archive:", value = 0, {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            pal <- c("#fffeff","#ff4646","#a883c9","#ff4646","#b49bcc")
            ord <- unique(colnames(coloroldfile))
            for(j in candidate) { #Start of big for loop
              count$count <- as.numeric(as.character(c(t(coloroldfile[j,]))))     
              count$condition <- colnames(coloroldfile)
              countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
              pi <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
                geom_bar(stat="identity", width=0.5) +
                geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) + 
                scale_x_discrete(limits=ord) +
                scale_colour_manual(values=c(rep("black", 5))) + 
                theme(axis.text=element_text(size=13)) +
                scale_fill_manual(values=pal)
              
              graphname <- paste(j,".png",sep="")
              fs <- append(fs, graphname)              #Writing to ID-defined file
              png(file=graphname, width = 960, height = 480, units = "px")    
              print(pi)
              dev.off()
              incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
            }  #end of big for loop
            zip(zipfile=fname, files=fs)             #Archiving
          }) #end of withProgress
        }   #end of content  
      )    #end of downloadHandler
    } # end of colorold condition   
    else if (input$sbmenu == "colornew") {

observeEvent(input$colornew_det, {
info <- read.table("files/info", sep = ":", row.names = 1)
if(exists("not")){removeNotification(not)}
showNotification(as.character(info["colornew",]), duration = NULL, closeButton = TRUE, id = "not")
})  

      colornewfile <- read.table("files/colornew", header = T, row.names = 1, check.names=FALSE)       #Read data 
      count <- as.data.frame(matrix(0, nrow = 18, ncol = 2))
      colnames(count) <- c("count","condition")
      ord <- unique(colnames(colornewfile))
      
      observeEvent(input$plot7, {
        output$plot7 <- renderPlot({
          isolate({
            count$count <- as.numeric(as.character(c(t(colornewfile[input$id7,]))))     
            count$condition <- colnames(colornewfile)
            #This part below is responsible for plot building
            pal <- c("#ea48dd","#ea48dd","#fffeff","#fffeff","#b503a6","#b503a6")
            countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
            p <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
              geom_bar(stat="identity", width=0.5) +
              geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) +  
              scale_x_discrete(limits=ord) +
              scale_colour_manual(values=c(rep("black", 6))) + 
              theme(axis.text=element_text(size=13)) +
              scale_fill_manual(values=pal)
          }) #end of isolate
          p
        }) #end of renderPlot
      }) #end of observeEvent2
      output$dow7 <- downloadHandler(
        filename = 'images.zip',
        content = function(fname) {
          on.exit(setwd(old_wd))
          if (input$mtype7 == "list") {
            candidate <- unlist(strsplit(input$list7, split=" "))   #Here plots are built and archived for download
          }
          else if (input$mtype7 == "file") {
            inframe <- input$file7
            tempo <- read.csv(inframe$datapath)
            candidate <- tempo[,1]
          }
          withProgress(message = "Creating archive:", value = 0, {
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            pal <- c("#ea48dd","#ea48dd","#fffeff","#fffeff","#b503a6","#b503a6")
            ord <- unique(colnames(colornewfile))
            for(j in candidate) { #Start of big for loop
              count$count <- as.numeric(as.character(c(t(colornewfile[j,]))))     
              count$condition <- colnames(colornewfile)
              countstat <- summarySE(count, measurevar="count", groupvars=c("condition"))
              pi <- ggplot(countstat, aes(x=condition, y=count, color=condition, fill=condition, group=condition)) + 
                geom_bar(stat="identity", width=0.5) +
                geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.1) + 
                scale_x_discrete(limits=ord) +
                scale_colour_manual(values=c(rep("black", 6))) + 
                theme(axis.text=element_text(size=13)) +
                scale_fill_manual(values=pal)
              
              graphname <- paste(j,".png",sep="")
              fs <- append(fs, graphname)              #Writing to ID-defined file
              png(file=graphname, width = 960, height = 480, units = "px")    
              print(pi)
              dev.off()
              incProgress(1/length(candidate), detail = paste("Drawing plot", grep(j, candidate), "out of", length(candidate))) 
            }  #end of big for loop
            zip(zipfile=fname, files=fs)             #Archiving
          }) #end of withProgress
        }   #end of content  
      )    #end of downloadHandler
    } # end of colornew condition 
}) #end of observeEvent
})

