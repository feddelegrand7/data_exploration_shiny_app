
library(shiny)
library(tidyverse)
library(arsenal)
library(survival)
library(ggfortify)
source("helper.R")

# increase max upload file size
options(shiny.maxRequestSize = 10*1024^2)

ui <- fluidPage(
  titlePanel("Basic Data Exploration App"),
  sidebarLayout(
    sidebarPanel(
      style = "min-height: 80vh; position: relative;",
      width = 3,
      actionButton("mockstudy", "Use sample dataset..."),
      fileInput("inputfile", NULL, buttonLabel = "...or upload a dataset", multiple = FALSE),
      textOutput("inputfiletext"),
      tags$a("NEWS file", href = "NEWS.md", target = "_blank", style = "bottom: 1vh; position: absolute;")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data Viewer",
          dataTableOutput("datatable")
        ),
        tabPanel(
          "Summary Statistics",
          fluidRow(
            column(4, selectInput("tableby.y", "By-Variable", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("tableby.x", "X-Variables", choices = " ", multiple = TRUE, selectize = FALSE)),
            column(4)
          ),
          tableOutput("tableby")
        ),
        tabPanel(
          "Data Quality",
          tags$br(),
          tabsetPanel(
            tabPanel(
              "Univariate",
              fluidRow(numericInput("nshow1", "N Records to Show:", value = 10)),
              fluidRow(tableOutput("univ.table"))
            ),
            tabPanel(
              "Pairwise",
              fluidRow(numericInput("nshow2", "N Records to Show:", value = 10)),
              fluidRow(tableOutput("pair.table"))
            )
          )
        ),
        tabPanel(
          "Plotting",
          fluidRow(
            column(
              3,
              selectInput("ggplot.y", "Y-Variable", choices = " ", multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.x", "X-Variable", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(
              3,
              selectInput("ggplot.plottype", "Plot Type", choices = PLOTTYPES, multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.facet", "By-Variable", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(
              3,
              selectInput("ggplot.color", "Color", choices = " ", multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.fill", "Fill", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(
              3,
              selectInput("ggplot.scale_y", "Y-Scale Transformation", choices = SCALETYPES("y"), multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.scale_x", "X-Scale Transformation", choices = SCALETYPES("x"), multiple = FALSE, selectize = FALSE)
            )
          ),
          plotOutput("ggplotplot")
        ),
        tabPanel(
          "Survival Analysis",
          fluidRow(
            column(4, selectInput("surv.time", "Follow-Up Time", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("surv.event", "Follow-Up Status", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("surv.x", "X-Variables", choices = " ", multiple = FALSE, selectize = FALSE))
          ),
          plotOutput("survplot")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # This allows you to toggle back and forth between uploaded data and mockstudy
  whichData <- reactiveValues(inputDat = NULL, mockStud = 0, fp = NULL)

  observeEvent(list(input$inputfile, input$mockstudy), {
    if(!identical(input$inputfile$datapath, whichData$inputDat))
    {
      whichData$fp <- input$inputfile$datapath
      whichData$inputDat <- input$inputfile$datapath
    } else if(input$mockstudy != whichData$mockStud)
    {
      whichData$fp <- "data/mockstudy.csv"
      whichData$mockStud <- input$mockstudy
    }
  })

  # eventReactive avoids it being called when the app loads
  inputData <- eventReactive(whichData$fp,  read_my_file(whichData$fp))

  columnNames <- reactive(colnames(inputData()))

  ################## Update all dropdowns across the app ##################

  observeEvent(columnNames(), {
    cn <- function(a = NULL)
    {
      out <- c(" ", columnNames())
      if(!is.null(a)) names(out) <- c(a, out[-1])
      out
    }
    updateSelectInput(session, "tableby.y", choices = cn())
    updateSelectInput(session, "tableby.x", choices = cn())
    updateSelectInput(session, "ggplot.y", choices = cn())
    updateSelectInput(session, "ggplot.x", choices = cn())
    updateSelectInput(session, "ggplot.facet", choices = cn("(No By-Variable)"))
    updateSelectInput(session, "ggplot.color", choices = cn("(No Color)"))
    updateSelectInput(session, "ggplot.fill", choices = cn("(No Fill)"))
    updateSelectInput(session, "surv.time", choices = cn())
    updateSelectInput(session, "surv.event", choices = cn("(All Events)"))
    updateSelectInput(session, "surv.x", choices = cn("(No X-Variables)"))
  })

  ################## Update side nav ##################

  output$inputfiletext <- renderText({
    dat <- inputData()
    paste0("File of extension '", attr(dat, "extension"), "' detected: ",
           nrow(dat), " rows and ", ncol(dat), " columns.")
  })

  ################## Update data viewer tab ##################

  output$datatable <- renderDataTable({
    inputData()
  })

  ################## Update summary statistics tab ##################

  output$tableby <- renderTable({
    do_the_tableby(input$tableby.y, input$tableby.x, isolate(inputData()))
  })

  ################## Update data quality tab ##################

  univ.tab <- reactive({
    univariate(inputData())
  })

  output$univ.table <- renderTable({
    head(univ.tab(), input$nshow1)
  })

  pair.tab <- reactive({
    pairwise(inputData())
  })

  output$pair.table <- renderTable({
    head(pair.tab(), input$nshow2)
  })

  ################## Update plotting tab ##################

  output$ggplotplot <- renderPlot({
    do_the_ggplot(
      y = input$ggplot.y,
      x = input$ggplot.x,
      color = input$ggplot.color,
      fill = input$ggplot.fill,
      facet = input$ggplot.facet,
      scale_y = input$ggplot.scale_y,
      scale_x = input$ggplot.scale_x,
      type = input$ggplot.plottype,
      dat = isolate(inputData())
    )
  })

  ################## Update survival tab ##################

  output$survplot <- renderPlot({
    do_the_survplot(
      input$surv.time,
      input$surv.event,
      input$surv.x,
      isolate(inputData())
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

