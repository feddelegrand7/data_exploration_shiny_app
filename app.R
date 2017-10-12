
library(shiny)
library(tidyverse)
library(arsenal)
library(survival)
library(ggfortify)
source("helper.R")

ui <- fluidPage(
  titlePanel("Basic Data Exploration App"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("inputfile", "Upload Dataset...", multiple = FALSE),
      textOutput("inputfiletext")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Statistics",
          fluidRow(
            column(4, selectInput("tableby.y", "By-Variable", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("tableby.x", "X-Variables", choices = " ", multiple = TRUE, selectize = FALSE)),
            column(4)
          ),
          verbatimTextOutput("tableby")
        ),
        tabPanel("Plotting",
          fluidRow(
            column(4,
              selectInput("ggplot.y", "Y-Variable", choices = " ", multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.x", "X-Variable", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(4,
              selectInput("ggplot.plottype", "Plot Type", choices = PLOTTYPES, multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.facet", "By-Variable", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(4,
              selectInput("ggplot.color", "Color", choices = " ", multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.fill", "Fill", choices = " ", multiple = FALSE, selectize = FALSE)
            )
          ),
          fluidRow(column(12, uiOutput("ggplot")))
        ),
        tabPanel("Survival Analysis",
          fluidRow(
            column(4, selectInput("surv.time", "Follow-Up Time", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("surv.event", "Follow-Up Status", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("surv.x", "X-Variables", choices = " ", multiple = TRUE, selectize = FALSE))
          ),
          plotOutput("surv.plot")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  inputData <- eventReactive(input$inputfile, {
    read_my_file(input$inputfile$datapath)
  })

  columnNames <- reactive({
    colnames(inputData())
  })

  ################## Update all dropdowns across the app ##################

  observeEvent(columnNames(), {
    cn <- c(" ", columnNames())
    updateSelectInput(session, "tableby.y", choices = cn)
    updateSelectInput(session, "tableby.x", choices = cn)
    updateSelectInput(session, "ggplot.y", choices = cn)
    updateSelectInput(session, "ggplot.x", choices = cn)
    updateSelectInput(session, "ggplot.facet", choices = cn)
    updateSelectInput(session, "ggplot.color", choices = cn)
    updateSelectInput(session, "ggplot.fill", choices = cn)
    updateSelectInput(session, "surv.time", choices = cn)
    updateSelectInput(session, "surv.event", choices = cn)
    updateSelectInput(session, "surv.x", choices = cn)
  })

  ################## Update side nav ##################

  output$inputfiletext <- renderText({
    dat <- inputData()
    paste0("File of extension '", attr(dat, "extension"), "' detected: ",
           nrow(dat), " rows and ", ncol(dat), " columns.")
  })

  ################## Update summary statistics tab ##################

  output$tableby <- renderPrint({
    do_the_tableby(input$tableby.y, input$tableby.x, isolate(inputData()))
  })

  ################## Update plotting tab ##################

  did_the_ggplot <- reactive({
    do_the_ggplot(
      y = input$ggplot.y,
      x = input$ggplot.x,
      color = input$ggplot.color,
      fill = input$ggplot.fill,
      facet = input$ggplot.facet,
      type = input$ggplot.plottype,
      dat = isolate(inputData())
    )
  })

  output$ggplot <- renderUI({
    if(is.null(did_the_ggplot()$p)) htmlOutput("ggplottext") else plotOutput("ggplotplot")
  })

  output$ggplottext <- renderUI({
    div(did_the_ggplot()$text, style = "color:red;")
  })

  output$ggplotplot <- renderPlot({
    did_the_ggplot()$plot
  })

  ################## Update survival tab ##################
  output$surv.plot <- renderPlot({
    do_the_survplot(input$surv.time, input$surv.event, input$surv.x, isolate(inputData()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

