library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
  
  observeEvent(input$apply, { 
  filen <- paste("files/", input$plant1, input$plant2, input$tissue, input$stage1, input$stage2, sep="")  
  output$tableout <- DT::renderDataTable({
  infile <- read.table(filen)    
  isolate({
  cnames <- as.character(unlist(infile[1,]))
  colnames(infile) <- cnames                             #DeSeq table preprocessing chunk
  infile = infile[-1,]
  infile[,2:4] <- sapply(infile[,2:4], as.character)
  infile[,2:4] <- sapply(infile[,2:4], as.numeric)
  proc <- data.frame()
if (input$revs == FALSE) {
  for (i in 1:nrow(infile)) {
    if (isTRUE( infile$Padj[i]<input$padj & infile$log2FoldChange[i]> input$ulfc & infile$baseMean[i]>input$basemean)) {
      proc[nrow(proc)+1,1] <- infile[i,1]
      proc[nrow(proc),2] <- infile[i,2]
      proc[nrow(proc),3] <- infile[i,3]
      proc[nrow(proc),4] <- infile[i,4]
    }
    else if (isTRUE( infile$Padj[i]<input$padj & infile$log2FoldChange[i]< input$dlfc & infile$baseMean[i]>input$basemean)) {
      proc[nrow(proc)+1,1] <- infile[i,1]
      proc[nrow(proc),2] <- infile[i,2]
      proc[nrow(proc),3] <- infile[i,3]
      proc[nrow(proc),4] <- infile[i,4]
    }
  }                                 #Cycles above and below filter based on selected in UI thresholds
}
else if (input$revs == TRUE) {
  for (i in 1:nrow(infile)) {
    if (isTRUE( infile$Padj[i]<input$padj & infile$log2FoldChange[i]< input$ulfc & infile$log2FoldChange[i]> input$dlfc & infile$baseMean[i]>input$basemean)) {
      proc[nrow(proc)+1,1] <- infile[i,1]
      proc[nrow(proc),2] <- infile[i,2]
      proc[nrow(proc),3] <- infile[i,3]
      proc[nrow(proc),4] <- infile[i,4]
    }
  }
}
  colnames(proc) <- cnames          #Section below writes to file with name based on UI selections
  output$dow <- downloadHandler(
    filename = function() { paste(filen, "_proc.txt", sep="") },
    content = function(file) {
      write.table(proc, file, col.names=TRUE, row.names = FALSE)
    }
  )     
  DT::datatable(proc)
      })
    })
  }) 
})

