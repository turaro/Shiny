library(shiny)

shinyUI(fluidPage(
sidebarPanel(  
  fluidRow(
    column(width = 6,
      radioButtons("plant1", "Plant 1:", c("Axillaris" = "ax", "Exserta" = "ex", "Parodii" = "s7"))),
    column(width = 6,
      radioButtons("plant2", "Plant 2:", c("Axillaris" = "ax", "Exserta" = "ex", "Parodii" = "s7")))),
        helpText("Picking 2 similar accessions will enable different time point analysis option."),  
  fluidRow(
    column(width = 12,
      radioButtons("tissue", "Tissue:", c("Tube" = "tu", "Style" = "st"), inline = TRUE))),
  fluidRow(
    column(width = 6,
      radioButtons("stage1", "Stage of growth:", c("Early" = "ea", "Medium" = "me", "Late" = "la"))),
  conditionalPanel( condition = "input.plant1 == input.plant2", 
    column(width = 6,
      radioButtons("stage2", "Stage to compare:", c("None" = "", "Early" = "ea", "Medium" = "me", "Late" = "la"))))),
  fluidRow(
    column(width = 12,
      sliderInput("ulfc", "Log2FoldChange lower threshold:", min = 0, max = 15, value = 1, step= 0.5))),
  fluidRow(
    column(width = 12,
      sliderInput("dlfc", "Log2FoldChange upper threshold:", min = -15, max = 0, value = -1, step= 0.5))),
  fluidRow(
    column(width = 12,
      checkboxInput("revs", "Reversed log2fold selection", FALSE))),
  fluidRow(
    column(width = 6,
      numericInput("basemean", "BaseMean value cutoff", value = "100", width = NULL)),
    column(width = 6,
      numericInput("padj", "P-adj value cutoff", value = "0.05", width = NULL))),
  fluidRow(
    column(width = 6, align = "center",
      actionButton(inputId = "apply", label = "Apply" )),
    column(width = 6, align = "center", 
      downloadButton('dow', 'Download')))),
mainPanel(
  tabPanel('Results', DT::dataTableOutput('tableout'))
          )
)
)