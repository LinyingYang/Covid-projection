#
# This is a Shiny web application on MatrixDS. 
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shiny_project <- ""
prod <- paste0("/srv/shiny-server/", shiny_project)
dev <- paste0("~/shiny-server/", shiny_project)
if (!dir.exists(prod) & !dir.exists(dev)) {
  message(" using getwd() for shiny_path")
  shiny_path <- getwd()
} else {
  .libPaths(c(.libPaths(), "/srv/.R/library"))
  options(java.parameters = "-Xmx8048m")
  if (dir.exists(prod)) message("prod"); shiny_path <- prod
  if (dir.exists(dev)) message("dev"); shiny_path <- dev
}

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reticulate)
library(gridExtra)
library(grid)
library(RGraphics)


library(viridis)
library(plotly)
library(reshape2)


ui <- shinyUI(
  
  navbarPage("Covid-19 ICU and Floor Projection",
             tabPanel("Census Data Input",
                      sidebarPanel(
                        h4("General Census Data"),
                        p("If you would like the model to use your hospital's census as an input, please upload two CSV files that combined contain the following four covariates for dates since March 13th, 2020:"),
                        p("1. ICU COVID Census"),
                        p("2. ICU non-COVID Census"),
                        p("3. Floor COVID Census"),
                        p("4. General Medicine Floor non-COVID Census"),
                        p("5. Cumulative COVID Admits up until each Day"),
                        p("If you do not provide your own hospital's census, then the model will only use the inputs in the `Calculator` tab. To ease the usage, please use the provided two CSV file templates given below."),
                        p(strong(em("Please note:"))),
                        p(strong("1. Do not change the `Date` column. All uploaded files must start from March 13th, 2020, which is the default of our model. Please enter 0s into the first rows if you do not have data for the earlier dates.")),
                        p(strong("2. All input values should be non-empty up to a certain date (e.g. if you have the data input up until March 20th, all cells from March 13th to March 20th should be non-empty).")),
                        h5(a(href='https://drive.google.com/file/d/1g-dwzuVIoXtrHbcmQO40eygiqqKz3Lda/view?usp=sharing', "Click here for Template 1.",
                             target = '_blank')),
                        h5(a(href='https://drive.google.com/file/d/1kBVabOdon7wh5TawmUD6Nov0yGRQozKH/view?usp=sharing', "Click here for Template 2.",
                             target = '_blank')),
                        hr(),
                        fileInput("file1", "Please load the CSV file with the date and the first four covariates (use Template 1).",
                                  accept = c(".csv")
                        ),
                        hr(),
                        fileInput("file2", "Please load the CSV file with the date and cumulative number of COVID admits up until the date (use Template 2).",
                                  accept = c(".csv")
                        ),
                        hr(),
                        p("If you inputted your own CSV files, the two data frames are displayed on the right hand of the screen. Please check that the data are outputted correctly before continuing.")
                      ),
                      
                      mainPanel(
                        tableOutput("contents1"),
                        tableOutput("contents2")
                      )
             ),
             tabPanel("Calculator",
                      sidebarPanel(
                        fluidRow(
                          column(12, 
                                 h4("General Parameters"),
                                 numericInput( "n_days","Number of Days to Project:",
                                               min = 1,
                                               value = 35, step = 1),
                                 numericInput( "doubling_time","Doubling Time for New COVID Admits:",
                                               min = 1,
                                               value = 6, step = 0.5),
                                 numericInput( "icu_capacity","Capacity of ICU:",
                                               min = 1,
                                               value = 100, step = 1),
                                 numericInput( "floor_capacity","Capacity of General Medicine Floor:",
                                               min = 1,
                                               value = 220, step = 1),
                                 numericInput( "ventilator_cap","Ventilator Capacity:",
                                               min = 1,
                                               value = 75, step = 1),
                                 sliderInput( "ventilator_percent","Fraction of Non-COVID ICU Patients in need of Ventilators:",
                                              min = 0, max = 1,
                                              value = 0.5, step = 0.01),
                                 hr(),
                                 h4("Hospital Starting Status Parameters"),
                                 numericInput( "starting_total","Cumulative Number of Admitted COVID Patients by Day 0:",
                                               min = 1,
                                               value = 10, step = 1),
                                 numericInput( "icu_census_covid_0","ICU Census of COVID patients at Day 0 (note if census data was inputted, these parameters are irrelevant):",
                                               min = 0, max = 10,
                                               value = 1, step = 1),
                                 numericInput( "floor_census_covid_0","General Medicine Floor Census of COVID patients at Day 0 (note if census data was inputted, these parameters are irrelevant):",
                                               min = 0, max = 10,
                                               value = 2, step = 1),
                                 numericInput( "icu_census_noncovid_mean","Mean Daily ICU Census of non-COVID patients:",
                                               min = 0, max = 100,
                                               value = 80, step = 1),
                                 numericInput( "floor_census_noncovid_mean","Mean Daily General Medicine Floor Census of non-COVID patients:",
                                               min = 0, max = 10,
                                               value = 176, step = 1),
                                 hr(),
                                 h4("COVID Patient Population Parameters"),
                                 p("Input the % breakdown of COVID patients who go through the five different paths listed below. Note these must sum to 1."),
                                 sliderInput( "cohort_fraction_0","Floor Only:",
                                              min = 0, max = 1,
                                              value = 0.704, step = 0.001),
                                 sliderInput( "cohort_fraction_1",paste("Floor to ICU to Floor:"),
                                              min = 0, max = 1,
                                              value = 0.13, step = 0.001),
                                 sliderInput( "cohort_fraction_2","Floor to ICU:",
                                              min = 0, max = 1,
                                              value = 0.018, step = 0.001),
                                 sliderInput( "cohort_fraction_3","ICU to Floor:",
                                              min = 0, max = 1,
                                              value = 0.13, step = 0.001),
                                 sliderInput( "cohort_fraction_4","ICU Only:",
                                              min = 0, max = 1,
                                              value = 0.018, step = 0.001),
                                 hr(),
                                 h4("Length of Stay Parameters"),
                                 p("Input the average length of stay (in days) in each unit for various COVID patient cohorts. Note the second `Floor` column is for patients who are coming from the ICU."),
                                 p(strong(em("Floor Only"))),
                                 fluidRow(
                                   column(4, "Floor"),
                                   column(4, "ICU"),
                                   column(4, "Floor")
                                 ),
                                 fluidRow(
                                   column(4,
                                          numericInput("los_matrix_0_0", "", 5, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_0_1", "", 0, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_0_2", "", 0, min = 0)
                                   )
                                 ),
                                 p(strong(em("Floor to ICU to Floor"))),
                                 fluidRow(
                                   column(4, "Floor"),
                                   column(4, "ICU"),
                                   column(4, "Floor")
                                 ),
                                 
                                 fluidRow(
                                   column(4,
                                          numericInput("los_matrix_1_0", "", 4, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_1_1", "", 9, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_1_2", "", 4, min = 0)
                                   )
                                 ),
                                 p(strong(em("Floor to ICU"))),
                                 fluidRow(
                                   column(4, "Floor"),
                                   column(4, "ICU"),
                                   column(4, "Floor")
                                 ),
                                 
                                 fluidRow(
                                   column(4,
                                          numericInput("los_matrix_2_0", "", 6, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_2_1", "", 9, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_2_2", "", 0, min = 0)
                                   )
                                 ),
                                 p(strong(em("ICU to Floor"))),
                                 fluidRow(
                                   column(4, "Floor"),
                                   column(4, "ICU"),
                                   column(4, "Floor")
                                 ),
                                 
                                 fluidRow(
                                   column(4,
                                          numericInput("los_matrix_3_0", "", 0, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_3_1", "", 9, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_3_2", "", 4, min = 0)
                                   )
                                 ),
                                 p(strong(em("ICU Only"))),
                                 fluidRow(
                                   column(4, "Floor"),
                                   column(4, "ICU"),
                                   column(4, "Floor")
                                 ),
                                 
                                 fluidRow(
                                   column(4,
                                          numericInput("los_matrix_4_0", "", 0, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_4_1", "", 11, min = 0)
                                   ),
                                   column(4, 
                                          numericInput("los_matrix_4_2", "", 0, min = 0)
                                   )
                                 )
                          )
                        ),
                        width = 3
                      ),
                      mainPanel( # There will be four tabs as outputs
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graphical Representation", plotOutput("plots", width ="800px", height ="700px")),
                                    tabPanel("Tabular Representation", dataTableOutput("table")),
                                    tabPanel("Sensitivity Analysis", plotOutput("sensitivity", width ="1020px", height ="1000px")),
                                    tabPanel("Analysis of Cumulative COVID Admissions", plotOutput("admission", width ="900px", height ="650px")) 
                        )
                      )
             ),
             
             
             tabPanel("Documentation",
                      fluidPage( # Can uncomment if you want the images again
                        mainPanel(
                          strong("Contact:"),
                          img(src = "email.png", height = 40, width = 'auto'),
                          hr(),
                          strong("Created by:"), 
                          p("Teng Zhang, Kelly McFarlane, Jacqueline Vallon, Linying Yang, Jin Xie, Jose Blanchet, Peter Glynn, Kristan Staudenmayer, Kevin Schulman, and David Scheinker"),
                          br(),
                          img(src = "SURF.png", height = 60, width = 'auto'),
                          img(src = "CERC.png", height = 60, width = 'auto'),
                          img(src = "matrixds_logo.png", height = 60, width = 'auto')
                          
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  output$contents1 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile1 <- input$file1
    
    if (is.null(inFile1))
      return(NULL)
    
    # data <- inFile$datapath
    
    data1 <- read.csv(inFile1$datapath, header = TRUE)
    colnames(data1) <- c("Date", "ICU COVID Census", "ICU non-COVID Census", "Floor COVID Census", "AAU Gen Med Floor non-COVID Census")
    data1 <- data1[rowSums(is.na(data1))==0, ]
    
    data1
  })
  
  output$contents2 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    # data <- inFile$datapath
    
    data2 <- read.csv(inFile2$datapath, header = TRUE)
    data2 <- data2[,1:2]
    colnames(data2) <- c("Date", "Total COVID Admit")
    data2 <- data2[rowSums(is.na(data2))==0, ]
    data2
  })
  
  output$plots <- renderPlot({
    #read in the inputs
    n_days_model = input$n_days
    starting_total_model = input$starting_total
    doubling_time_model = input$doubling_time
    
    icu_census_covid_0_model = input$icu_census_covid_0
    floor_census_covid_0_model = input$floor_census_covid_0
    
    icu_census_noncovid_mean_model = input$icu_census_noncovid_mean
    floor_census_noncovid_mean_model = input$floor_census_noncovid_mean
    
    cohort_fraction_0_model = input$cohort_fraction_0
    cohort_fraction_1_model = input$cohort_fraction_1
    cohort_fraction_2_model = input$cohort_fraction_2
    cohort_fraction_3_model = input$cohort_fraction_3
    cohort_fraction_4_model = input$cohort_fraction_4
    
    
    los_matrix_0_0_model = input$los_matrix_0_0
    los_matrix_0_1_model = input$los_matrix_0_1
    los_matrix_0_2_model = input$los_matrix_0_2
    
    los_matrix_1_0_model = input$los_matrix_1_0
    los_matrix_1_1_model = input$los_matrix_1_1
    los_matrix_1_2_model = input$los_matrix_1_2
    
    los_matrix_2_0_model = input$los_matrix_2_0
    los_matrix_2_1_model = input$los_matrix_2_1
    los_matrix_2_2_model = input$los_matrix_2_2
    
    los_matrix_3_0_model = input$los_matrix_3_0
    los_matrix_3_1_model = input$los_matrix_3_1
    los_matrix_3_2_model = input$los_matrix_3_2
    
    los_matrix_4_0_model = input$los_matrix_4_0
    los_matrix_4_1_model = input$los_matrix_4_1
    los_matrix_4_2_model = input$los_matrix_4_2
    
    icu_capacity_model = input$icu_capacity
    floor_capacity_model = input$floor_capacity
    
    ventilator_cap_model = input$ventilator_cap
    ventilator_percent_model = input$ventilator_percent
    
    inFile1 <- input$file1
    
    if (is.null(inFile1)){
      data1 <- NULL
    }
    else{
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      data1 <- data1[rowSums(is.na(data1))==0, ]
      colnames(data1) <- c("Date", "ICU_COVID_Census", "ICU_non_COVID_Census", "Floor_COVID_Census", "Floor_non_COVID_Census")
    }
    
    #setwd("~/Desktop/SURF-covid19-master/shiny-server")
    #use_python("/Users/jacquelinevallon/anaconda3/bin/python", required = T)
    source_python("des_simulator.py") # call in the python code that is located in the shiny-server folder
    
    # run the argument "run_simulation" which outputs a matrix of the projected numbers
    simulation_data <- run_simulation(n_days = as.integer(n_days_model),
                                      df_input_census = data1,
                                      icu_census_covid_0 = as.integer(icu_census_covid_0_model), floor_census_covid_0 = as.integer(floor_census_covid_0_model),
                                      icu_census_noncovid_mean = as.integer(icu_census_noncovid_mean_model), floor_census_noncovid_mean = as.integer(floor_census_noncovid_mean_model),
                                      starting_total = as.integer(starting_total_model),
                                      doubling_time = as.integer(doubling_time_model),
                                      cohort_fraction_0 = cohort_fraction_0_model,
                                      cohort_fraction_1 = cohort_fraction_1_model,
                                      cohort_fraction_2 = cohort_fraction_2_model,
                                      cohort_fraction_3 = cohort_fraction_3_model,
                                      cohort_fraction_4 = cohort_fraction_4_model,
                                      los_matrix_0_0 = as.integer(los_matrix_0_0_model), los_matrix_0_1 = as.integer(los_matrix_0_1_model), los_matrix_0_2 = as.integer(los_matrix_0_2_model),
                                      los_matrix_1_0 = as.integer(los_matrix_1_0_model), los_matrix_1_1 = as.integer(los_matrix_1_1_model), los_matrix_1_2 = as.integer(los_matrix_1_2_model),
                                      los_matrix_2_0 = as.integer(los_matrix_2_0_model), los_matrix_2_1 = as.integer(los_matrix_2_1_model), los_matrix_2_2 = as.integer(los_matrix_2_2_model),
                                      los_matrix_3_0 = as.integer(los_matrix_3_0_model), los_matrix_3_1 = as.integer(los_matrix_3_1_model), los_matrix_3_2 = as.integer(los_matrix_3_2_model), 
                                      los_matrix_4_0 = as.integer(los_matrix_4_0_model), los_matrix_4_1 = as.integer(los_matrix_4_1_model), los_matrix_4_2 = as.integer(los_matrix_4_2_model))
    
    # run the arugment "sensitivity_calculation" which outputs eight sensitivity plots
    sensitivity_data <- sensitivity_calculation(#n_days = as.integer(n_days_model),
      df_input_census = data1,
      icu_census_covid_0 = as.integer(icu_census_covid_0_model), floor_census_covid_0 = as.integer(floor_census_covid_0_model),
      icu_census_noncovid_mean = as.integer(icu_census_noncovid_mean_model), floor_census_noncovid_mean = as.integer(floor_census_noncovid_mean_model),
      starting_total = as.integer(starting_total_model),
      doubling_time = as.integer(doubling_time_model),
      cohort_fraction_0 = cohort_fraction_0_model,
      cohort_fraction_1 = cohort_fraction_1_model,
      cohort_fraction_2 = cohort_fraction_2_model,
      cohort_fraction_3 = cohort_fraction_3_model,
      cohort_fraction_4 = cohort_fraction_4_model,
      los_matrix_0_0 = as.integer(los_matrix_0_0_model), los_matrix_0_1 = as.integer(los_matrix_0_1_model), los_matrix_0_2 = as.integer(los_matrix_0_2_model),
      los_matrix_1_0 = as.integer(los_matrix_1_0_model), los_matrix_1_1 = as.integer(los_matrix_1_1_model), los_matrix_1_2 = as.integer(los_matrix_1_2_model),
      los_matrix_2_0 = as.integer(los_matrix_2_0_model), los_matrix_2_1 = as.integer(los_matrix_2_1_model), los_matrix_2_2 = as.integer(los_matrix_2_2_model),
      los_matrix_3_0 = as.integer(los_matrix_3_0_model), los_matrix_3_1 = as.integer(los_matrix_3_1_model), los_matrix_3_2 = as.integer(los_matrix_3_2_model), 
      los_matrix_4_0 = as.integer(los_matrix_4_0_model), los_matrix_4_1 = as.integer(los_matrix_4_1_model), los_matrix_4_2 = as.integer(los_matrix_4_2_model),
      icu_capacity = icu_capacity_model, floor_capacity = floor_capacity_model,
      ventilator_capacity = as.integer(ventilator_cap_model),
      icu_non_covid_ventilator_percentage = ventilator_percent_model)
    
    
    simulation_data <- simulation_data %>% mutate(date = as.Date("03/13/20", "%m/%d/%y")+Day) # fix date as March 13th as in Excel model
    #df <- df %>% mutate(date =Sys.Date()+Day) # make the date update each day
    colnames(simulation_data) <- c("Day", "ICU_COVID_Census", "ICU_non_COVID_Census", "Floor_COVID_Census", "Floor_non_COVID_Census", "date")
    
    simulation_data <- simulation_data %>% mutate(Total_ICU_Census = ICU_COVID_Census+ICU_non_COVID_Census, Total_Floor_Census=Floor_COVID_Census+Floor_non_COVID_Census, Ventilators = ICU_COVID_Census + ventilator_percent_model*ICU_non_COVID_Census)
    
    colnames(simulation_data) <- c("Day", "ICU COVID Census", "ICU non-COVID Census", "Floor COVID Census", "Floor non-COVID Census", "Date", "Total ICU Census", "Total Floor Census", "Ventilators")
    #simulation_data <- as.data.frame(simulation_data)
    
    df_melt_icu <- melt(simulation_data[, c(2, 3, 6, 7)], id.vars="Date")
    
    plot1 = ggplot(df_melt_icu, aes(Date, value, col=variable)) + 
      geom_line(size = 1)+
      ggtitle("Projected ICU Census")+
      labs(y="Number of Patients", x = "Date")+
      scale_color_manual(values=c("royalblue4", "sienna3", "gray48", "black"))+
      theme_bw()+theme(legend.text=element_text(size=12), plot.title = element_text(face = "bold", size = 15), axis.title=element_text(size=14), axis.text=element_text(size=12), legend.title = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" ), legend.position = "right", panel.background = element_blank(), panel.grid.minor = element_blank(), panel.border=element_blank(),panel.grid.major = element_blank(), axis.line = element_line(color="black"))
    
    plot1 = plot1 + geom_point(data = simulation_data, aes(x=Date, y=Ventilators, color="Ventilator Demand"))
    
    plot1 = plot1 + geom_hline(aes(yintercept= icu_capacity_model, linetype = "ICU Capacity"), colour= 'goldenrod', size=1) +
      geom_hline(aes(yintercept= ventilator_cap_model, linetype = "Ventilator Capacity"), colour= 'darkgrey', size=1) +
      scale_linetype_manual(name = "limit", values = c(2, 2), 
                            guide = guide_legend(override.aes = list(color = c("goldenrod", "grey"))))
    
    df_melt_floor <- melt(simulation_data[, c(4, 5, 6, 8)], id.vars="Date")
    
    plot2 = ggplot(df_melt_floor, aes(Date,value, col=variable)) + 
      geom_line(size = 1)+
      ggtitle("Projected General Medicine IP Floor Census")+
      labs(y="Number of Patients", x = "Date")+
      scale_color_manual(values=c("royalblue4", "sienna3", "gray48"))+
      geom_hline(aes(yintercept=floor_capacity_model, linetype="Gen Med Team Capacity"), color = "goldenrod", size=1)+
      scale_linetype_manual(name = "", values = c(2, 2))+
      theme_bw()+theme(legend.text=element_text(size=12), plot.title = element_text(face = "bold", size = 15), axis.title=element_text(size=14), axis.text=element_text(size=12),legend.title = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" ), legend.position = "right", panel.background = element_blank(), panel.grid.minor = element_blank(), panel.border=element_blank(),panel.grid.major = element_blank(), axis.line = element_line(color="black"))
    
    
    t4 <- textGrob("")
    t1 <- splitTextGrob(paste('Starting from March 13th, the number of days until ICU capacity is met is', sensitivity_data[[1]]$ICU_Cap_Days,"days."), just="right", gp=gpar(fontsize=15))
    t2 <- splitTextGrob(paste('Starting from March 13th, the number of days until the General Medicine Floor capacity is met is', sensitivity_data[[1]]$Floor_Cap_Days,"days."), just="right", gp=gpar(fontsize=15))
    t5 <- splitTextGrob(paste('Starting from March 13th, the number of days until the Ventilator capacity is met is', sensitivity_data[[1]]$Ventilator_Cap_Days,"days."), just="right", gp=gpar(fontsize=15))
    t3 <- textGrob("")
    
    grid.arrange(t4, t1, t2, t5, t3, plot1, plot2, nrow = 7, ncol = 1, widths=c(4), heights=c(0.3, 0.3, 0.3,0.3, 0.3, 4, 4)) 
  })
  
  output$table <- renderDataTable({
    #read in the inputs
    n_days_model = input$n_days
    starting_total_model = input$starting_total
    doubling_time_model = input$doubling_time
    
    icu_census_covid_0_model = input$icu_census_covid_0
    floor_census_covid_0_model = input$floor_census_covid_0
    
    icu_census_noncovid_mean_model = input$icu_census_noncovid_mean
    floor_census_noncovid_mean_model = input$floor_census_noncovid_mean
    
    cohort_fraction_0_model = input$cohort_fraction_0
    cohort_fraction_1_model = input$cohort_fraction_1
    cohort_fraction_2_model = input$cohort_fraction_2
    cohort_fraction_3_model = input$cohort_fraction_3
    cohort_fraction_4_model = input$cohort_fraction_4
    
    los_matrix_0_0_model = input$los_matrix_0_0
    los_matrix_0_1_model = input$los_matrix_0_1
    los_matrix_0_2_model = input$los_matrix_0_2
    
    los_matrix_1_0_model = input$los_matrix_1_0
    los_matrix_1_1_model = input$los_matrix_1_1
    los_matrix_1_2_model = input$los_matrix_1_2
    
    los_matrix_2_0_model = input$los_matrix_2_0
    los_matrix_2_1_model = input$los_matrix_2_1
    los_matrix_2_2_model = input$los_matrix_2_2
    
    los_matrix_3_0_model = input$los_matrix_3_0
    los_matrix_3_1_model = input$los_matrix_3_1
    los_matrix_3_2_model = input$los_matrix_3_2
    
    los_matrix_4_0_model = input$los_matrix_4_0
    los_matrix_4_1_model = input$los_matrix_4_1
    los_matrix_4_2_model = input$los_matrix_4_2
    
    inFile1 <- input$file1
    
    if (is.null(inFile1)){# if no file is given by the user about the census, the python code will mark this empty and use the default parameters
      data1 <- NULL
    }
    else{
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      data1 <- data1[rowSums(is.na(data1))==0, ]
      colnames(data1) <- c("Date", "ICU_COVID_Census", "ICU_non_COVID_Census", "Floor_COVID_Census", "Floor_non_COVID_Census")
    }
    
    source_python("des_simulator.py")
    
    simulation_data <- run_simulation(n_days = as.integer(n_days_model),
                                      df_input_census = data1,
                                      icu_census_covid_0 = as.integer(icu_census_covid_0_model), floor_census_covid_0 = as.integer(floor_census_covid_0_model),
                                      icu_census_noncovid_mean = as.integer(icu_census_noncovid_mean_model), floor_census_noncovid_mean = as.integer(floor_census_noncovid_mean_model),
                                      starting_total = as.integer(starting_total_model),
                                      doubling_time = as.integer(doubling_time_model),
                                      cohort_fraction_0 = cohort_fraction_0_model,
                                      cohort_fraction_1 = cohort_fraction_1_model,
                                      cohort_fraction_2 = cohort_fraction_2_model,
                                      cohort_fraction_3 = cohort_fraction_3_model,
                                      cohort_fraction_4 = cohort_fraction_4_model,
                                      los_matrix_0_0 = as.integer(los_matrix_0_0_model), los_matrix_0_1 = as.integer(los_matrix_0_1_model), los_matrix_0_2 = as.integer(los_matrix_0_2_model),
                                      los_matrix_1_0 = as.integer(los_matrix_1_0_model), los_matrix_1_1 = as.integer(los_matrix_1_1_model), los_matrix_1_2 = as.integer(los_matrix_1_2_model),
                                      los_matrix_2_0 = as.integer(los_matrix_2_0_model), los_matrix_2_1 = as.integer(los_matrix_2_1_model), los_matrix_2_2 = as.integer(los_matrix_2_2_model),
                                      los_matrix_3_0 = as.integer(los_matrix_3_0_model), los_matrix_3_1 = as.integer(los_matrix_3_1_model), los_matrix_3_2 = as.integer(los_matrix_3_2_model), 
                                      los_matrix_4_0 = as.integer(los_matrix_4_0_model), los_matrix_4_1 = as.integer(los_matrix_4_1_model), los_matrix_4_2 = as.integer(los_matrix_4_2_model))
    
    simulation_data <- simulation_data %>% mutate(date = as.Date("03/13/20", "%m/%d/%y")+Day)
    #df <- df %>% mutate(date = as.character(Sys.Date()+Day))
    colnames(simulation_data) <- c("Day", "ICU_COVID_Census", "ICU_non_COVID_Census", "Floor_COVID_Census", "Floor_non_COVID_Census", "date")
    
    simulation_data <- simulation_data %>% mutate(Total_ICU_Census = ICU_COVID_Census+ICU_non_COVID_Census, Total_Floor_Census=Floor_COVID_Census+Floor_non_COVID_Census)
    
    final_data <- simulation_data %>% select(Day, ICU_COVID_Census, ICU_non_COVID_Census, Total_ICU_Census, Floor_COVID_Census, Floor_non_COVID_Census, Total_Floor_Census)
    
    colnames(final_data) <- c("Day", "ICU COVID Census", "ICU non-COVID Census", "Total ICU Census", "Floor COVID Census", "Floor non-COVID Census", "Total Floor Census")
    
    round_df <- function(x, digits) { # changes how the numbers ar shown on the table; round to nearest integer
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    
    final_data <- round_df(final_data, 0)
    final_data  
  })
  
  output$sensitivity <- renderPlot({ # output the sensitivity analysis
    #read in the inputs
    n_days_model = input$n_days
    starting_total_model = input$starting_total
    doubling_time_model = input$doubling_time
    
    icu_census_covid_0_model = input$icu_census_covid_0
    floor_census_covid_0_model = input$floor_census_covid_0
    
    icu_census_noncovid_mean_model = input$icu_census_noncovid_mean
    floor_census_noncovid_mean_model = input$floor_census_noncovid_mean
    
    cohort_fraction_0_model = input$cohort_fraction_0
    cohort_fraction_1_model = input$cohort_fraction_1
    cohort_fraction_2_model = input$cohort_fraction_2
    cohort_fraction_3_model = input$cohort_fraction_3
    cohort_fraction_4_model = input$cohort_fraction_4
    
    los_matrix_0_0_model = input$los_matrix_0_0
    los_matrix_0_1_model = input$los_matrix_0_1
    los_matrix_0_2_model = input$los_matrix_0_2
    
    los_matrix_1_0_model = input$los_matrix_1_0
    los_matrix_1_1_model = input$los_matrix_1_1
    los_matrix_1_2_model = input$los_matrix_1_2
    
    los_matrix_2_0_model = input$los_matrix_2_0
    los_matrix_2_1_model = input$los_matrix_2_1
    los_matrix_2_2_model = input$los_matrix_2_2
    
    los_matrix_3_0_model = input$los_matrix_3_0
    los_matrix_3_1_model = input$los_matrix_3_1
    los_matrix_3_2_model = input$los_matrix_3_2
    
    los_matrix_4_0_model = input$los_matrix_4_0
    los_matrix_4_1_model = input$los_matrix_4_1
    los_matrix_4_2_model = input$los_matrix_4_2
    
    icu_capacity_model = input$icu_capacity
    floor_capacity_model = input$floor_capacity
    
    ventilator_cap_model = input$ventilator_cap
    ventilator_percent_model = input$ventilator_percent
    
    
    inFile1 <- input$file1
    
    if (is.null(inFile1)){ # if no file is given by the user about the census, the python code will mark this empty and use the default parameters
      data1 <- NULL
    }
    else{
      data1 <- read.csv(inFile1$datapath, header = TRUE)
      data1 <- data1[rowSums(is.na(data1))==0, ]
      colnames(data1) <- c("Day", "ICU_COVID_Census", "ICU_non_COVID_Census", "Floor_COVID_Census", "Floor_non_COVID_Census")
    }
    
    
    source_python("des_simulator.py")
    
    # The sensitivity data outputs 8 different data frames in a list, so the code below individually assigns these to the corresponding name
    sensitivity_data <- sensitivity_calculation(#n_days = as.integer(n_days_model),
      df_input_census = data1,
      icu_census_covid_0 = as.integer(icu_census_covid_0_model), floor_census_covid_0 = as.integer(floor_census_covid_0_model),
      icu_census_noncovid_mean = as.integer(icu_census_noncovid_mean_model), floor_census_noncovid_mean = as.integer(floor_census_noncovid_mean_model),
      starting_total = as.integer(starting_total_model),
      doubling_time = as.integer(doubling_time_model),
      cohort_fraction_0 = cohort_fraction_0_model,
      cohort_fraction_1 = cohort_fraction_1_model,
      cohort_fraction_2 = cohort_fraction_2_model,
      cohort_fraction_3 = cohort_fraction_3_model,
      cohort_fraction_4 = cohort_fraction_4_model,
      los_matrix_0_0 = as.integer(los_matrix_0_0_model), los_matrix_0_1 = as.integer(los_matrix_0_1_model), los_matrix_0_2 = as.integer(los_matrix_0_2_model),
      los_matrix_1_0 = as.integer(los_matrix_1_0_model), los_matrix_1_1 = as.integer(los_matrix_1_1_model), los_matrix_1_2 = as.integer(los_matrix_1_2_model),
      los_matrix_2_0 = as.integer(los_matrix_2_0_model), los_matrix_2_1 = as.integer(los_matrix_2_1_model), los_matrix_2_2 = as.integer(los_matrix_2_2_model),
      los_matrix_3_0 = as.integer(los_matrix_3_0_model), los_matrix_3_1 = as.integer(los_matrix_3_1_model), los_matrix_3_2 = as.integer(los_matrix_3_2_model), 
      los_matrix_4_0 = as.integer(los_matrix_4_0_model), los_matrix_4_1 = as.integer(los_matrix_4_1_model), los_matrix_4_2 = as.integer(los_matrix_4_2_model),
      icu_capacity = icu_capacity_model, floor_capacity = floor_capacity_model,
      ventilator_capacity = as.integer(ventilator_cap_model),
      icu_non_covid_ventilator_percentage = ventilator_percent_model)
    
    sensitivity_dt <- sensitivity_data[[2]]
    sensitivity_dt$ICU_Cap_Days <- as.numeric(sensitivity_dt$ICU_Cap_Days)
    sensitivity_dt$Floor_Cap_Days <- as.numeric(sensitivity_dt$Floor_Cap_Days)
    
    colnames(sensitivity_dt) <- c("Doubling Time", "Days until demand exceeds ICU capacity", "Days until demand exceeds Floor capacity", "Days until demand exceeds Ventilator capacity")
    
    sensitivity_los_minus_floor <- sensitivity_data[[4]]
    sensitivity_los_plus_floor <- sensitivity_data[[7]]
    
    sensitivity_los_minus_icu <- sensitivity_data[[3]]
    sensitivity_los_plus_icu <- sensitivity_data[[6]]
    
    sensitivity_los_minus_ventilator <- sensitivity_data[[5]]
    sensitivity_los_plus_ventilator <- sensitivity_data[[8]]
    
    colnames(sensitivity_los_minus_floor) <- c("Floor LOS", "ICU LOS", "Floor LOS")
    rownames(sensitivity_los_minus_floor) <- c("Floor Only", "Floor to ICU to Floor", "Floor to ICU", "ICU to Floor", "ICU Only")
    
    colnames(sensitivity_los_plus_floor) <- c("Floor LOS", "ICU LOS", "Floor LOS")
    rownames(sensitivity_los_plus_floor) <- c("Floor Only", "Floor to ICU to Floor", "Floor to ICU", "ICU to Floor", "ICU Only")
    
    colnames(sensitivity_los_minus_icu) <- c("Floor LOS", "ICU LOS", "Floor LOS")
    rownames(sensitivity_los_minus_icu) <- c("Floor Only", "Floor to ICU to Floor", "Floor to ICU", "ICU to Floor", "ICU Only")
    
    colnames(sensitivity_los_plus_icu) <- c("Floor LOS", "ICU LOS", "Floor LOS")
    rownames(sensitivity_los_plus_icu) <- c("Floor Only", "Floor to ICU to Floor", "Floor to ICU", "ICU to Floor", "ICU Only")
    
    colnames(sensitivity_los_minus_ventilator) <- c("Floor LOS", "ICU LOS", "Floor LOS")
    rownames(sensitivity_los_minus_ventilator) <- c("Floor Only", "Floor to ICU to Floor", "Floor to ICU", "ICU to Floor", "ICU Only")
    
    colnames(sensitivity_los_plus_ventilator) <- c("Floor LOS", "ICU LOS", "Floor LOS")
    rownames(sensitivity_los_plus_ventilator) <- c("Floor Only", "Floor to ICU to Floor", "Floor to ICU", "ICU to Floor", "ICU Only")
    
    
    
    sensitivity_los_minus_floor[is.na(sensitivity_los_minus_floor)] <- ""
    sensitivity_los_plus_floor[is.na(sensitivity_los_plus_floor)] <- ""
    sensitivity_los_minus_icu[is.na(sensitivity_los_minus_icu)] <- ""
    sensitivity_los_plus_icu[is.na(sensitivity_los_plus_icu)] <- ""
    sensitivity_los_minus_ventilator[is.na(sensitivity_los_minus_ventilator)] <- ""
    sensitivity_los_plus_ventilator[is.na(sensitivity_los_plus_ventilator)] <- ""
    
    
    blank_space <- textGrob("")
    
    p1 <- tableGrob(sensitivity_dt, rows=NULL)
    
    header <- splitTextGrob("Doubling Time", gp=gpar(fontsize=15, fontface="bold"))
    dt_explanation <- splitTextGrob('For each doubling time, the table outlines the number of days until demand exceeds the ICU Capacity, Floor Capacity, or Ventilator Capacity starting from March 13th.', just="right", gp=gpar(fontsize=15))
    
    p2 <- tableGrob(sensitivity_los_minus_floor)
    #print(p2)
    
    p3 <- tableGrob(sensitivity_los_plus_floor)
    header2 <- splitTextGrob(paste("Impact of a One Day LOS Decrease or Increase on the Days until Demand Exceeds Floor Capacity"), gp=gpar(fontsize=15, fontface="bold"))
    explanation2 <- splitTextGrob(paste('For each patient path, the table outlines the number of days until demand exceeds the Floor Capacity starting from March 13th if each LOS in the different units is individually decreased (left table) or increased (right table) by one day. For example, if patients who only go to the Floor (first row) and their Floor LOS decreases by one day (left table), then it will take', sensitivity_los_minus_floor[1,1], 'days to reach Floor Capacity, keeping all other parameters the same.'), just="right", gp=gpar(fontsize=15))
    
    
    p4 <- tableGrob(sensitivity_los_minus_icu)
    p5 <- tableGrob(sensitivity_los_plus_icu)
    header3 <- splitTextGrob(paste("Impact of a One Day LOS Decrease or Increase on the Days until Demand Exceeds ICU Capacity"), gp=gpar(fontsize=15, fontface="bold"))
    explanation3 <- splitTextGrob(paste('For each patient path, the table outlines the number of days until demand exceeds the ICU Capacity starting from March 13th if each LOS in the different units is individually decreased (left table) or increased (right table) by one day. For example, if patients who only go to the Floor (first row) and their Floor LOS decreases by one day (left table), then it will take', sensitivity_los_minus_icu[1,1], 'days to reach ICU Capacity, keeping all other parameters the same.'), just="right", gp=gpar(fontsize=15))
    
    
    p6 <- tableGrob(sensitivity_los_minus_ventilator)
    p7 <- tableGrob(sensitivity_los_plus_ventilator)
    header4 <- splitTextGrob(paste("Impact of a One Day LOS Decrease or Increase on the Days until Demand Exceeds Ventilator Capacity"), gp=gpar(fontsize=15, fontface="bold"))
    explanation4 <- splitTextGrob(paste('For each patient path, the table outlines the number of days until demand exceeds the Ventilator Capacity starting from March 13th if each LOS in the different units is individually decreased (left table) or increased (right table) by one day. For example, if patients who only go to the Floor (first row) and their Floor LOS decreases by one day (left table), then it will take', sensitivity_los_minus_ventilator[1,1], 'days to reach Ventilator Capacity, keeping all other parameters the same.'), just="right", gp=gpar(fontsize=15))
    
    
    
    table_title1 <- splitTextGrob(paste("- One Day for LOS"), gp=gpar(fontsize=12, fontface="italic"), just="center")
    table_title2 <- splitTextGrob(paste("+ One Day for LOS"), gp=gpar(fontsize=12, fontface="italic"))
    
    table_title3 <- splitTextGrob(paste("- One Day for LOS"), gp=gpar(fontsize=12, fontface="italic"), just="center")
    table_title4 <- splitTextGrob(paste("+ One Day for LOS"), gp=gpar(fontsize=12, fontface="italic"))
    
    table_title5 <- splitTextGrob(paste("- One Day for LOS"), gp=gpar(fontsize=12, fontface="italic"), just="center")
    table_title6 <- splitTextGrob(paste("+ One Day for LOS"), gp=gpar(fontsize=12, fontface="italic"))
    
    grid.arrange(blank_space, header, dt_explanation, p1,blank_space, header2, explanation2, blank_space, blank_space, table_title1, table_title2, p2, p3, blank_space, header3, explanation3, blank_space, blank_space, table_title3, table_title4, p4, p5, blank_space, header4, explanation4, blank_space, blank_space, table_title5, table_title6, p6, p7, layout_matrix = matrix(c(1,1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,7,7,8,8, 9, 9, 10, 11, 12, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 20, 21, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 29, 30, 31), ncol=2, byrow=TRUE), 
                 heights = unit(c(0.001, 0.3, 0.3, 1.5, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 2, 0.3, 0.3, 0.3,0.3,0.3, 0.3, 2, 0.3, 0.3, 0.3,0.3,0.3, 0.3, 2), c("in","in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in", "in")))
    
  })
  
  output$admission <- renderPlot({ # output the regression analysis
    inFile2 <- input$file2 # uses the cumulative admission CSV as an input into the code 
    source_python("des_simulator.py")
    
    if (is.null(inFile2)){ # if the file is not given, then there is no output on this page
      data2 <- NULL
      blankspace  <- textGrob("")
      text <- splitTextGrob(paste("If you would like to see an analysis of the doubling time as estimated from your hospital's cumulative COVID admissions, please input the CSV file in the `Census Tab` with the cumulative COVID admissions."), gp=gpar(fontsize=15, fontface="italic"), just="center")
      grid.arrange(blankspace, text, heights = c(0.03, 0.3))
    }
    else{ # if a file is given by the user, then output the text, table, and plot
      data2 <- read.csv(inFile2$datapath, header = TRUE)
      colnames(data2) <- c("Date", "Total COVID Admit")
      data2 <- data2[,1:2]
      blankspace  <- textGrob("")
      data2$`Total COVID Admit` <- data2$`Total COVID Admit`+0
      df = arrival_fitting(data2)[[1]]
      df1 = arrival_fitting(data2)[[2]]
      
      text <- splitTextGrob(paste("Using the inputted cumulative COVID admissions CSV file and regressing the doubling time on the", df[1, 2], "most recent datapoints (default: set to 7 days), the model corresponds to a doubling time of", round(df[2, 2],0), "days. The fitted cumulative number of admissions on the first day is", round(df[3, 2],1),"patients. The table below summarizes the information of the fitted line to the cumulative COVID admissions, along with a graph as a visual representation of the fit. The regression is based on a simple linear regression (https://en.wikipedia.org/wiki/Simple_linear_regression) of the log(cumulative COVID admissions) over the past", df[1, 2], "days."), gp=gpar(fontsize=15), just="center")
      names(df) <- NULL
      mytheme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 1.2)))
      
      round_df <- function(x, digits) { # changes how the numbers ar shown on the table; round to nearest integer
        numeric_columns <- sapply(x, mode) == 'numeric'
        x[numeric_columns] <-  round(x[numeric_columns], digits)
        x
      }
      
      df <- round_df(df, 0)
      
      table_df <- tableGrob(df, rows=NULL, theme = mytheme)
      
      colnames(df1) <- c("Date", "Cumulative COVID Admit", "Fitted")
      df_melt <- melt(df1[, c(1:3)], id.vars="Date")
      df_melt<- df_melt[rowSums(is.na(df_melt))==0, ]
      df_melt$Date <- as.Date(df_melt$Date, "%d-%b")
      
      
      plot <- ggplot(df_melt, aes(Date, value)) + 
        geom_line(size = 1, aes(col=variable, linetype=variable))+
        #geom_point(size = 1)+
        ggtitle("Cumulative COVID Admissions and Fitted Values")+
        labs(y="Cumulative COVID Admissions", x = "Date")+
        scale_color_manual(values=c("royalblue4", "dimgray"))+
        theme_bw()+theme(legend.text=element_text(size=12), plot.title = element_text(face = "bold", size = 15), axis.title=element_text(size=14), axis.text=element_text(size=12),legend.title = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" ), legend.position = "right", panel.background = element_blank(), panel.grid.minor = element_blank(), panel.border=element_blank(),panel.grid.major = element_blank(), axis.line = element_line(color="black"))
      
      plot <- plot + geom_point(data = df_melt %>% filter(variable == "Cumulative COVID Admit"), aes(Date, value), col = "royalblue4")
      
      grid.arrange(blankspace, text, blankspace, table_df, plot, heights = unit(c(0.005, 1, 0.1, 2, 5),c("in","in","in", "in","in")))
    }
    
  })
  
}

shinyApp(ui, server)
