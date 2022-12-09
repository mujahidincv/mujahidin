library(shiny)
library(readr)
library(shinydashboard)
library(ggplot2)
library(plotly)


ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title="My Dashbord",
                                    dropdownMenu(type="messages",
                                                 messageItem(from="husain",
                                                             "selamat datang"),
                                                 messageItem(from = "program dashbord",
                                                             "haii")
                                    ),
                                    dropdownMenu(type="notifications",
                                                 notificationItem(text=" Nama : Ahmad husain mustari "),
                                                 notificationItem(text="Nim: 210907502030")
                                                 
                                    ),
                                    dropdownMenu(type="tasks",
                                                 taskItem(value=80,
                                                          text="Avance del dashboardHeader",
                                                          color="blue"),
                                                 taskItem(value=20,
                                                          text="Avance del dashboardSidebar",
                                                          color="red")
                                    )
                                    
                    ),
                    dashboardSidebar(
                      sidebarSearchForm("searchText","buttonSearch","search?",icon = shiny::icon("apple"))
                      ,
                      
                      sidebarMenu(id="sidebarID",
                                  menuItem("menu utama", tabName = "menuku"),
                                  menuSubItem("pengolahan data",
                                  ),
                                  menuItem("settings",id = "chartsID",
                                           menuSubItem("help", tabName = "datos", icon = shiny::icon("eye"))
                                          
                                  )
                      )
                      
                      
                    ),
                    
                    dashboardBody(
                      
                      ui <- fluidPage(
                        fileInput("upload", NULL, accept = c(".csv", ".tsv")),
                        numericInput("n", "Rows", value = 5, min = 1, step = 1),
                        tableOutput("head")
                      ),
                      ui <- fluidPage(
                        selectInput("datasetku", label = "Dataset", choices = ls("package:datasets")),
                        verbatimTextOutput("summary"),
                        tableOutput("table")
                      ),
                      
                      
                      
                      fluidRow(
                        box(title = "Show Data",
                            dataTableOutput("dataku1")
                        ),
                        
                        box(title = "Summary",
                            verbatimTextOutput("summaryku")
                        ),
                        
                      
                        box(title = "plot",
                            plotlyOutput("plotliku"))
                      ), 
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(plotOutput("plot1", height = 250)),
                                  
                                  box(
                                    title = "Controls",
                                    sliderInput("slider", "Number of observations:", 1, 99, 30)
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "widgets",
                                h2("Widgets tab content")
                        )
                      )
                    ),
                    tabItems(tabItem(tabName = "datos", 
                                     DT::dataTableOutput("datos")
                                     
                                     
                    ),
                    tabItem(tabName = "montos", 
                            
                            fluidRow(
                              column(width=9,  
                                     valueBox("9 Meses","Periodo: Marzo-Diciembre",icon=icon("eye"),color="yellow"),
                                     valueBoxOutput("valuebox"),
                                     infoBox("Dato abiertos", "100%"),
                              ),
                              fluidRow(box(title="GRÁFICO",plotOutput("montos"), width=9, status="primary", solidHeader=TRUE)))
                            
                    ))
                    
)
server <- function(input, output) { 
  showNotification("create : husain", duration = NULL, type = 'error')
  server <- function(input, output, session) {
    data <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ","),
             tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    })
    
    output$head <- renderTable({
      head(data(), input$n)
    })
  }
  
  dataku<-function(){
    read_delim("www/husain.csv", escape_double = FALSE, trim_ws = TRUE)
  }
  
  output$dataku1 <- renderDataTable({
    dataku()
  },options = list(lengthMenu = c(10, 20, 15, 100), pageLength = 10, scrollX=T))
  
  
  output$summaryku <- renderPrint({
    summary(dataku())
  })
  
  output$plotliku<- renderPlotly({
    data<-dataku ()
    fig <- plot_ly(data, labels = ~Immigrants, values = ~  Age, type = 'pie')
    fig <- fig %>% layout(title = 'Diagram lingkaran',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  output$plotliku<- renderPlotly({
    data<-dataku()
    fig <- plot_ly(
      x = data[["tahun"]],
      y = data[["satuan"]],
      name = "Data ANTM",
      type = "scatter")
    fig
  })
  
  redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))
  output$datos<-DT::renderDataTable(data120)
  output$montos<-renderPlot({montos})
  output$valuebox<-renderInfoBox({valueBox(redondeo(sum((data120)[5])),"Monto Soles Millones", 
                                           icon=icon("money"),color="red")})
}

shinyApp(ui, server)
  
 