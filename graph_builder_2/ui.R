library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "GBuilder"),
  dashboardSidebar(
    sidebarMenu( id = "sbmenu",
      menuItem("Stage 4 limb RNAseq - 2019", tabName = "s4limb"),
      menuItem("Color experiment - 2017", tabName = "colornew"),       
      menuItem("Growth experiment - 2016-2017", tabName = "growth"),
      menuItem("Color experiment (petu) - 2015", tabName = "color"),
      menuItem("Color experiment (cali) - 2015", tabName = "cali"),
      menuItem("Scent experiment - 2015", tabName = "scent"),
      menuItem("Scent experiment - 2014", tabName = "scentold"),
      menuItem("Color experiment - 2013", tabName = "colorold"),
      actionButton("about", "About")
      ) #menuItem close
  ), #sidebarMenu close
  dashboardBody(
tabItems(
    tabItem(tabName = "growth",
fluidPage(
  box(width = 4,
  fluidRow(
    column(width = 12,
      textInput("id1", "Gene ID:", value = "", width = NULL))),
  fluidRow(
    column(width = 12,
      radioButtons("style1", "Data grouping (regarding tubes/styles):", c("None (combined)" = 0, "Horizontal split" = 1, "Vertical split" = 2), inline = TRUE))),
  fluidRow(
    column(width = 12, align="center",
      actionButton(inputId = "plot1", label = "Plot" ))),
  fluidRow(
    column(width = 12,
      radioButtons("mtype1", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
  fluidRow(
  conditionalPanel( condition = "input.mtype1 == 'list'", 
    column(width = 12,
      textInput("list1", "List:", value = "", width = NULL)))),
  fluidRow(
  conditionalPanel( condition = "input.mtype1 == 'file'", 
    column(width = 12,
      fileInput("file1", "File:",
        accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
  fluidRow(
    column(width = 12,
      radioButtons("savestyle1", "Data grouping for plots:", c("None (combined)" = 0, "Horizontal split" = 1, "Vertical split" = 2), inline = TRUE))),
  fluidRow(
    column(width = 12, align = "center", 
      downloadButton('dow1', 'Download')))),
  box(width = 8, plotOutput("plot1"),
      fluidRow(
        column(width = 12, align = "center", 
               actionButton("growth_det", "Details"))))
  ) #fluidPage close
), #1st tab close
tabItem(tabName = "color",
  fluidPage(
  box(width = 4,
  fluidRow(
    column(width = 12,
      textInput("id2", "Gene ID:", value = "", width = NULL))),
  fluidRow(
    column(width = 12, align="center",
      actionButton(inputId = "plot2", label = "Plot" ))),
  fluidRow(
    column(width = 12,
      radioButtons("mtype2", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
  fluidRow(
    conditionalPanel( condition = "input.mtype2 == 'list'", 
      column(width = 12,
        textInput("list2", "List:", value = "", width = NULL)))),
  fluidRow(
    conditionalPanel( condition = "input.mtype2 == 'file'", 
       column(width = 12,
         fileInput("file2", "File:",
            accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
  fluidRow(
    column(width = 12, align = "center", 
      downloadButton('dow2', 'Download')))),
  box(width = 8, plotOutput("plot2"),
      fluidRow(
        column(width = 12, align = "center", 
               actionButton("color_det", "Details"))))
  ) #fluidPage close
), # 2nd tab close
tabItem(tabName = "cali",
        fluidPage(
          box(width = 4,
              fluidRow(
                column(width = 12,
                       textInput("id3", "Gene ID:", value = "", width = NULL))),
              fluidRow(
                column(width = 12, align="center",
                       actionButton(inputId = "plot3", label = "Plot" ))),
              fluidRow(
                column(width = 12,
                       radioButtons("mtype3", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
              fluidRow(
                conditionalPanel( condition = "input.mtype3 == 'list'", 
                                  column(width = 12,
                                         textInput("list3", "List:", value = "", width = NULL)))),
              fluidRow(
                conditionalPanel( condition = "input.mtype3 == 'file'", 
                                  column(width = 12,
                                         fileInput("file3", "File:",
                                                   accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
              fluidRow(
                column(width = 12, align = "center", 
                       downloadButton('dow3', 'Download')))),
          box(width = 8, plotOutput("plot3"),
              fluidRow(
                column(width = 12, align = "center", 
                       actionButton("cali_det", "Details"))))
        ) #fluidPage close
), # 3rd tab close
tabItem(tabName = "scent",
        fluidPage(
          box(width = 4,
              fluidRow(
                column(width = 12,
                       textInput("id4", "Gene ID:", value = "", width = NULL))),
              fluidRow(
                column(width = 12, align="center",
                       actionButton(inputId = "plot4", label = "Plot" ))),
              fluidRow(
                column(width = 12,
                       radioButtons("mtype4", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
              fluidRow(
                conditionalPanel( condition = "input.mtype4 == 'list'", 
                                  column(width = 12,
                                         textInput("list4", "List:", value = "", width = NULL)))),
              fluidRow(
                conditionalPanel( condition = "input.mtype4 == 'file'", 
                                  column(width = 12,
                                         fileInput("file4", "File:",
                                                   accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
              fluidRow(
                column(width = 12, align = "center", 
                       downloadButton('dow4', 'Download')))),
          box(width = 8, plotOutput("plot4"),
              fluidRow(
                column(width = 12, align = "center", 
                       actionButton("scent_det", "Details"))))
        ) #fluidPage close
), # 4th tab close
tabItem(tabName = "scentold",
        fluidPage(
          box(width = 4,
              fluidRow(
                column(width = 12,
                       textInput("id5", "Gene ID:", value = "", width = NULL))),
              fluidRow(
                column(width = 12, align="center",
                       actionButton(inputId = "plot5", label = "Plot" ))),
              fluidRow(
                column(width = 12,
                       radioButtons("mtype5", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
              fluidRow(
                conditionalPanel( condition = "input.mtype5 == 'list'", 
                                  column(width = 12,
                                         textInput("list5", "List:", value = "", width = NULL)))),
              fluidRow(
                conditionalPanel( condition = "input.mtype5 == 'file'", 
                                  column(width = 12,
                                         fileInput("file5", "File:",
                                                   accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
              fluidRow(
                column(width = 12, align = "center", 
                       downloadButton('dow5', 'Download')))),
          box(width = 8, plotOutput("plot5"),
              fluidRow(
                column(width = 12, align = "center", 
                       actionButton("scentold_det", "Details"))))
        ) #fluidPage close
), # 5th tab close
tabItem(tabName = "colorold",
        fluidPage(
          box(width = 4,
              fluidRow(
                column(width = 12,
                       textInput("id6", "Gene ID:", value = "", width = NULL))),
              fluidRow(
                column(width = 12, align="center",
                       actionButton(inputId = "plot6", label = "Plot" ))),
              fluidRow(
                column(width = 12,
                       radioButtons("mtype6", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
              fluidRow(
                conditionalPanel( condition = "input.mtype6 == 'list'", 
                                  column(width = 12,
                                         textInput("list6", "List:", value = "", width = NULL)))),
              fluidRow(
                conditionalPanel( condition = "input.mtype6 == 'file'", 
                                  column(width = 12,
                                         fileInput("file6", "File:",
                                                   accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
              fluidRow(
                column(width = 12, align = "center", 
                       downloadButton('dow6', 'Download')))),
          box(width = 8, plotOutput("plot6"),
              fluidRow(
                column(width = 12, align = "center", 
                       actionButton("colorold_det", "Details"))))
        ) #fluidPage close
), # 6th tab close
tabItem(tabName = "colornew",
        fluidPage(
          box(width = 4,
              fluidRow(
                column(width = 12,
                       textInput("id7", "Gene ID:", value = "", width = NULL))),
              fluidRow(
                column(width = 12, align="center",
                       actionButton(inputId = "plot7", label = "Plot" ))),
              fluidRow(
                column(width = 12,
                       radioButtons("mtype7", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
              fluidRow(
                conditionalPanel( condition = "input.mtype7 == 'list'", 
                                  column(width = 12,
                                         textInput("list7", "List:", value = "", width = NULL)))),
              fluidRow(
                conditionalPanel( condition = "input.mtype7 == 'file'", 
                                  column(width = 12,
                                         fileInput("file7", "File:",
                                                   accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
              fluidRow(
                column(width = 12, align = "center", 
                       downloadButton('dow7', 'Download')))),
          box(width = 8, plotOutput("plot7"),
              fluidRow(
                column(width = 12, align = "center", 
                       actionButton("colornew_det", "Details"))))
        ) #fluidPage close
), # 7th tab close
tabItem(tabName = "s4limb",
        fluidPage(
          box(width = 4,
              fluidRow(
                column(width = 12,
                       textInput("id8", "Gene ID:", value = "", width = NULL))),
              fluidRow(
                column(width = 12, align="center",
                       actionButton(inputId = "plot8", label = "Plot" ))),
              fluidRow(
                column(width = 12,
                       radioButtons("mtype8", "Multiple plot generation:", c("From space-delimited pasted list" = "list", "From column file" = "file"), inline = FALSE))),
              fluidRow(
                conditionalPanel( condition = "input.mtype8 == 'list'", 
                                  column(width = 12,
                                         textInput("list8", "List:", value = "", width = NULL)))),
              fluidRow(
                conditionalPanel( condition = "input.mtype8 == 'file'", 
                                  column(width = 12,
                                         fileInput("file8", "File:",
                                                   accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv"))))),
              fluidRow(
                column(width = 12, align = "center", 
                       downloadButton('dow8', 'Download')))),
          box(width = 8, plotOutput("plot8"),
              fluidRow(
                column(width = 12, align = "center", 
                       actionButton("s4limb_det", "Details"))))
        ) #fluidPage close
) # 7th tab close
) #tabItems close
) #dashboardBody close
) #dashboardPage close