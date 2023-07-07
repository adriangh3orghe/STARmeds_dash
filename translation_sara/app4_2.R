#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load R packages####
library("shiny") #for web app
library("tidyverse") #for data manipulation and graphs
library("ggrepel") #for dynamic labels that avoid overlap
library("magrittr") #for pipe operator
library("rsconnect") #for link with shinyapps
library("DT") #for data tables
library("shiny.i18n")
library("ggplot2")
library(shinydashboard)
library(bslib)
library(htmlTable)
library(rmarkdown)
library(png)
library(plotly)

#Initial declarations ####

#i18n <- Translator$new(translation_json_path='translation.json')
#i18n <- Translator$new(translation_csvs_path = "C:/Users/svalente/Desktop/shiny")
#i18n <- Translator$new(translation_csvs_path = "/Users/adriangheorghe/Documents/starmeds_shiny")
i18n <- Translator$new(translation_csvs_path = ".", translation_csv_config = "config.yml")


i18n$set_translation_language('en')

#function for displaying a table with title and caption
dt_output = function(id, dt_title, dt_caption) {
    fluidRow(column(10, h2(dt_title), 
                    p(), 
                    p(dt_caption),
                    p()),
             DTOutput(id))
}

# Define UI for web application ####
ui <- fluidPage(
  theme = bs_theme(bootswatch = "yeti"),
  
  # Use i18n in UI
  usei18n(i18n),
  selectInput(
    inputId = "selected_language",
    label = i18n$t("Change language"),
    choices = setNames(
      i18n$get_languages(),
      c("English", "Indonesian") # Set labels for the languages
    ),
    selected = i18n$get_key_translation()
  ),
  
  navbarPage("STARMeds PMS dashboard",
             tabPanel(i18n$t("How to use?"),
                      #page title
                      titlePanel(i18n$t("Introduction")),
                      #list of left hand tabs
                      p(i18n$t("This interactive calculator allows you to estimate the costs required to conduct your planned post-market surveillance study based on input data that you provide and explicit assumptions. The results are meant to be indicative and to be used only as a guide.")),
                      
                      p(i18n$t("The tool assumes that a generic PMS study entails three phases: preparing the study, collecting the medicine samples, and reporting the results.")),
                      
                      p(i18n$t("The following three tabs ask you for information that captures expected resources to be used in each of these three phases.")),
                      
                      p(i18n$t("The Results section produces a summary of expected costs based on the information you have provided.")),
                      
                      p(i18n$t("Please report any issues at"), tags$a(href="bla@bla.com", "support@starmeds.id")),
                      
                      p(tags$a(href="https://starmeds.id/", "STARmeds website"))
             ),
             
             tabPanel(i18n$t("Toolkit"),
                      
                      navlistPanel(
                        "Inputs",
                        tabPanel(i18n$t("Study preparation"),
                                 dt_output("prep_hr_tab", i18n$t("Human resources"), i18n$t("Double click table cells to edit staff positions and characteristics.")),
                                 actionButton("add_btn1", i18n$t("Add row"), class = "btn-info"),
                                 actionButton("del_btn1", i18n$t("Delete row"), class = "btn-warning"),
                                 textOutput("prep_hr_total"),
                                 dt_output("prep_other_tab", i18n$t("Other costs"), i18n$t("Double click table cells to edit other cost items.")),
                                 actionButton("add_btn2", i18n$t("Add row"), class = "btn-info"),
                                 actionButton("del_btn2", i18n$t("Delete row"), class = "btn-warning")
                        ),
                        tabPanel(i18n$t("Fieldwork"),
                                 textInput(inputId = "fw_medlist", 
                                           label = i18n$t("Input below the TYPES of meds to be collected, separated by '; '. For example: paracetamol 500mg tab; ibuprofen 400mg caps"), 
                                           width = "100%"),
                                 textOutput("fw_medlist_value"),
                                 p(),
                                 textInput(inputId = "fw_outletlist", 
                                           label = i18n$t("Input below the TYPES of outlets from where meds will be collected, separated by '; '. For example: street pharmacy; online pharmacy; clinics"), 
                                           width = "100%"),
                                 textOutput("fw_outletlist_value"),
                                 p(),
                                 dt_output("fw_sampleframe", i18n$t("Medicine samples to be collected"),
                                           i18n$t("The table below will update automatically based on values inputted above. Once you are happy with the structure, double click each table cell to populate it with the target number of medicine samples to be collected for each medicine X outlet combination")),
                                 textOutput("fw_colsum_text"),
                                 p(),
                                 dt_output("fw_medcost", i18n$t("Cost of buying sampled medicines"),
                                           i18n$t("The table below will update automatically based on values inputted above. Once you are happy with the structure, double click each table cell to populate it with the average cost of buying sampled medicines")),
                                 p(),
                                 dt_output("fw_perdayperoutlet", i18n$t("Sample collection rate"),
                                           i18n$t("The table below will update automatically based on values inputted above. Once you are happy with the structure, double click each table cell to populate it with the target number of medicine samples to be collected for each medicine X outlet combination")),
                                 p(),
                                 sliderInput("integer", i18n$t("Estimated number of fieldwork days:"),
                                             min = 0, max = 200,
                                             value = 50),
                                 p(),
                                 numericInput("obs", i18n$t("Estimated cost per fieldwork day (1 to 10 million):"), 10, min = 1, max = 10000000),
                                 textOutput("fw_total_cost"),
                        ),
                        tabPanel(i18n$t("Analysis and reporting"),
                                 dt_output("analysis_hr", i18n$t("Human resources"), i18n$t("Double click table cells to edit staff positions and characteristics.")),
                                 actionButton("add_btn3", i18n$t("Add row"), class = "btn-info"),
                                 actionButton("del_btn3", i18n$t("Delete row"), class = "btn-warning"),
                                 dt_output("analysis_lab", i18n$t("Laboratory costs"), i18n$t("Double click table cells to edit laboratory tests and unit costs.")),
                                 actionButton("add_btn4", i18n$t("Add row"), class = "btn-info"),
                                 actionButton("del_btn4", i18n$t("Delete row"), class = "btn-warning"),
                                 dt_output("analysis_other", i18n$t("Other costs"), i18n$t("Double click table cells to edit other cost items.")),
                                 actionButton("add_btn5", i18n$t("Add row"), class = "btn-info"),
                                 actionButton("del_btn5", i18n$t("Delete row"), class = "btn-warning")
                                 ),
                        "Results",
                        tabPanel(i18n$t("Summary of costs"),
                                 dt_output("res_sumbycost", i18n$t("Costs by item"), 
                                           i18n$t("This table is informed by information provided in previous tabs.")),
                                 dt_output("res_sumbycostphase", i18n$t("Costs by phase"), 
                                           i18n$t("This table is informed by information provided in previous tabs.")),
                                 dt_output("res_sumbycostitem", i18n$t("Costs by type of item"), 
                                           i18n$t("This table is informed by information provided in previous tabs."))),
                        tabPanel(i18n$t("Value for money"),
                                 textOutput("totalcostpersamplecollected"),
                                 dt_output("res_costperOOSsample", i18n$t("Estimated cost per out-of-specification sample detected"), 
                                           i18n$t("In the table below, change the expected prevalence (%) of out-of-specification (OOS) samples to obtain estimated costs per OOS sample detected.")),
                                 actionButton("add_btn6", i18n$t("Add row"), class = "btn-info"),
                                 actionButton("del_btn6", i18n$t("Delete row"), class = "btn-warning"),),
                        tabPanel(i18n$t("Visuals"),
                                 plotlyOutput("res_pie_cost_phase"),
                                 plotlyOutput("res_pie_cost_item"))
                      ),    
                      downloadButton("downloadBtn1", "Export report - ENG"),
                      downloadButton("downloadBtn2", "Export report - IND"),
             )
  ),
)

# Define server logic ####
server <- function(input, output) {
  # Change the language according to user input
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })

    #Intro
  output$tab_intro_title <- renderText({
    paste(i18n$t("About STARmeds Toolkit"), input$text)
  })
  
    #Prep - hr costs
  
  #define as dynamic object and load with initial data
  prep.hr <- reactiveValues(data=NULL)
  
  prep.hr.fun <- reactive({
    prep_hr_base <- data.frame(
      Positions = c(i18n$t("Senior Investigator"),i18n$t("Data Manager")),
      N_FTE = c(0.1, 1),
      Months = c(3, 3),
      Monthly_salary = c(200, 75),
      Salary = c(NA, NA)
    ) %>%
      #function to calculate the last column (Salary) as product of previous columns
      mutate(Salary = N_FTE * Monthly_salary * Months)
    #colnames(prep_hr_base) <- c(i18n$t("Positions"), "N_FTE",i18n$t("Months"), i18n$t("Monthly salary"), "Salary")
    prep_hr_base
  })
    
    #recalculate each time data change using function above
    observe({prep.hr$data <- prep.hr.fun()})
    
    #show table and make it editable at cell level
    #with the exception of last column which is calculated using function dat1()
    output$prep_hr_tab <- renderDT({prep.hr$data %>%
        datatable(editable = list(target = "cell", disable = list(columns = c(5))),
                  colnames = c(i18n$t("Positions"), i18n$t("FTE"),
                               i18n$t("Months"), i18n$t("Monthly salary"),
                               i18n$t("Salary")))
    })

    #allow the editing of the initial data on hr
    observeEvent(input$prep_hr_tab_cell_edit, {
        info <- input$prep_hr_tab_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        
        # Without this line the table does not change but with it it jumps to row 1 after an edit.
        prep.hr$data[i, j] <<- (DT::coerceValue(v, prep.hr$data[i, j]))
        prep.hr$data[,"Salary"] <<- prep.hr$data[,"Monthly_salary"] * prep.hr$data[,"N_FTE"] * prep.hr$data[,"Months"]  ## update the calculated column
    })
    
    #add row button
    observeEvent(input$add_btn1, {
      prep.hr$data[nrow(prep.hr$data) + 1, ] <- NA
    })
    
    #delete row button
    observeEvent(input$del_btn1, {
      if (!is.null(input$prep_hr_tab_rows_selected)) {
        prep.hr$data <<- prep.hr$data[-input$prep_hr_tab_rows_selected,]
      }
    })
    
    #show text below the table with sum of salary costs
    prep_hr_cost_sum_value <- reactive(sum(prep.hr$data[,"Salary"]))
    prep_hr_cost_sum <- reactive(c(i18n$t("Preparation"), i18n$t("Human resources"), prep_hr_cost_sum_value()))
    
    output$prep_hr_total <- renderText({
      paste0(i18n$t("Total HR cost is "), prep_hr_cost_sum_value(), ".")
    })
    
    #Prep - other costs
    prep.other <- reactiveValues(data=NULL)
    
    prep.other.fun <- reactive({
        prep_other_base <- data.frame(
            Cost.items = c(i18n$t("Meetings"), i18n$t("Travel"), i18n$t("Consultants")),
            Costs = c(1000, 2000, 4000))
        prep_other_base
    })
    
    observe({prep.other$data <- prep.other.fun()})
    
    observeEvent(input$add_btn2, {
      prep.other$data[nrow(prep.other$data) + 1, ] <- NA
    })
    
    observeEvent(input$del_btn2, {
      if (!is.null(input$prep_other_tab_rows_selected)) {
        prep.other$data <<- prep.other$data[-input$prep_other_tab_rows_selected,]
      }
    })
    
    output$prep_other_tab <- renderDT({prep.other$data %>%
            datatable(editable = list(target = "cell"),
                      colnames = c(i18n$t("Cost item"), i18n$t("Costs")))
    })
    
    prep_other_cost <- reactive(matrix(cbind(i18n$t("Preparation"), as.matrix(prep.other$data, ncol = 2)), 
                               ncol = 3,
                               nrow = nrow(prep.other$data),
                               dimnames = NULL))
    
    #Fieldwork - types of medicines
    
    x <- reactive(input$fw_medlist)
    y <- reactive(strsplit(as.character(x()), "; ")[[1]])
    z <- reactive(length(y()))
    
    output$fw_medlist_value <- renderText({
        paste0(i18n$t("You have entered "), z(), i18n$t(" unique types of medicines."))
    })
    
    #Fieldwork - types of outlets
    
    t <- reactive(input$fw_outletlist)
    u <- reactive(strsplit(as.character(t()), "; ")[[1]])
    v <- reactive(length(u()))
    
    output$fw_outletlist_value <- renderText({
        paste0(i18n$t("You have entered "), v(), i18n$t(" unique types of outlets."))
    })
    
    #Fieldwork - dynamic sampling frame
    fw.samplingframe <- reactiveValues(data = NULL)
    
    fw.samplingframe.fun <- reactive({
        m <- matrix(data = as.numeric(NA), 
                    nrow = z(),
                    ncol = v(),
                    dimnames = list(y(), u()))
        m
    })
  
    observe({fw.samplingframe$data <- fw.samplingframe.fun()})
    
    output$fw_sampleframe <- renderDT({fw.samplingframe$data %>%
            datatable(editable = list(target = "cell"))
        })
    
    observeEvent(input$fw_sampleframe_cell_edit, {
      info <- input$fw_sampleframe_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      fw.samplingframe$data[i, j] <<- (DT::coerceValue(v, fw.samplingframe$data[i, j]))
    })
    
    #Fieldwork - dynamic sample collection rate
    fw.samplecollectionrate <- reactiveValues(data = NULL)
    q <- reactive(colSums(fw.samplingframe$data))
    
    output$fw_colsum_text <- renderText(q())
    
    fw.samplecollectionrate.fun <- reactive({
      m <- data.frame(
        Samples.day = rep(1, v()),
        Est.samples = q()) %>%
        mutate(Est.days = round(Est.samples/Samples.day, 1))
      m
      
      })
    
    observe({fw.samplecollectionrate$data <- fw.samplecollectionrate.fun()})
    
    output$fw_perdayperoutlet <- renderDT({fw.samplecollectionrate$data %>%
        datatable(editable = list(target = "cell", 
                                  disable = list(columns = c(3))),
                  colnames = c(i18n$t("Samples per day"), i18n$t("Target number of samples"), i18n$t("Estimated days")))
    })
    
    observeEvent(input$fw_perdayperoutlet_cell_edit, {
      info <- input$fw_perdayperoutlet_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      fw.samplecollectionrate$data[i, j] <<- (DT::coerceValue(v, fw.samplecollectionrate$data[i, j]))
      fw.samplecollectionrate$data[,3] <<- round(fw.samplecollectionrate$data[,2]/fw.samplecollectionrate$data[,1], 1)
    })
    
    #Fieldwork - dynamic table of sampled medicines cost
    fw.samplecost <- reactiveValues(data = NULL)
    r <- reactive(rowSums(fw.samplingframe$data))
    
    fw.samplecost.fun <- reactive({
      m <- data.frame(
        Medicines = y(),
        Med.sampled = r(),
        Cost.med = rep(1, z())) %>%
        mutate(Est.costmed = round(Med.sampled*Cost.med, 1))
      m
      
    })
    
    observe({fw.samplecost$data <- fw.samplecost.fun()})
    
    output$fw_medcost <- renderDT({fw.samplecost$data %>%
        datatable(editable = list(target = "cell", 
                                  disable = list(columns = c(1, 2, 4))),
                  colnames = c(i18n$t("Medicine"), i18n$t("Target number of samples"), 
                               i18n$t("Price per sample"), i18n$t("Purchase cost")))
    })
    
    observeEvent(input$fw_medcost_cell_edit, {
      info <- input$fw_medcost_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      fw.samplecost$data[i, j] <<- (DT::coerceValue(v, fw.samplecost$data[i, j]))
      fw.samplecost$data[,4] <<- round(fw.samplecost$data[,2]*fw.samplecost$data[,3], 1)
    })
    
    total_days <- reactive({ input$integer })
    
    cost_day <- reactive({ input$obs })
    
    output$fw_total_cost <- renderText({
      paste0(i18n$t("The total estimated cost of fieldwork is"),(" "), total_days()*cost_day(), ("."))
    })
    
    fw_mat <- reactive(matrix(cbind(i18n$t("Fieldwork"), i18n$t("Fieldwork"), total_days()*cost_day()), 
                              ncol = 3, nrow = 1,
                              dimnames = NULL))
    
    #Analysis - hr costs
    analysis.hr <- reactiveValues(data=NULL)
    
    analysis.hr.fun <- reactive({
      analysis_hr_base <- data.frame(
        Positions = c(i18n$t("Senior Investigator"), i18n$t("Data Manager")),
        N_FTE = c(0.1, 1),
        Months = c(3, 3),
        Monthly_salary = c(200, 75),
        Salary = c(NA, NA))
      analysis_hr_base$Salary <- analysis_hr_base$N_FTE * analysis_hr_base$Monthly_salary *
        analysis_hr_base$Months
      analysis_hr_base
    })
    
    observe({analysis.hr$data <- analysis.hr.fun()})
    
    observeEvent(input$add_btn3, {
      analysis.hr$data[nrow(analysis.hr$data) + 1, ] <- NA
    })
    
    observeEvent(input$del_btn3, {
      if (!is.null(input$analysis_hr_rows_selected)) {
        analysis.hr$data <<- analysis.hr$data[-input$analysis_hr_rows_selected,]
      }
    })
    
    output$analysis_hr <- renderDT({analysis.hr$data %>%
        datatable(editable = list(target = "cell", disable = list(columns = c(5))),
                  colnames = c(i18n$t("Positions"), i18n$t("FTE"),
                               i18n$t("Months"), i18n$t("Monthly salary"),
                               i18n$t("Salary")))
    })
    
    observeEvent(input$analysis_hr_cell_edit, {
      info <- input$analysis_hr_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      analysis.hr$data[i, j] <<- (DT::coerceValue(v, analysis.hr$data[i, j]))
      analysis.hr$data[,"Salary"] <<- analysis.hr$data[,i18n$t("Monthly_salary")] * analysis.hr$data[,"N_FTE"] * analysis.hr$data[,i18n$t("Months")]  ## update the calculated column
    })
    
    analysis_hr_cost_sum <- reactive(sum(analysis.hr$data[,"Salary"]))
    analysis_hr <- reactive(c(i18n$t("Analysis"), i18n$t("Human resources"), analysis_hr_cost_sum()))
    
    #Analysis - lab costs
    
    #define as dynamic object and load with initial data
    analysis.lab <- reactiveValues(data=NULL)
    
    analysis.lab.fun <- reactive({
      analysis_lab_base <- data.frame(
        Test = c(i18n$t("API concentration"), i18n$t("Dissolution")),
        N_samples = c(0, 0),
        Unit_cost = c(0, 0),
        Cost = c(NA, NA)
      ) %>%
        #function to calculate the last column (Cost) as product of previous columns
        mutate(Cost = N_samples * Unit_cost)
      colnames(analysis_lab_base) <- c(i18n$t("Test"), i18n$t("N_samples"), i18n$t("Unit_cost"), i18n$t("Cost"))
      analysis_lab_base
    })
    
    #recalculate each time data change using function above
    observe({analysis.lab$data <- analysis.lab.fun()})
    
    observeEvent(input$add_btn4, {
      analysis.lab$data[nrow(analysis.lab$data) + 1, ] <- NA
    })
    
    observeEvent(input$del_btn4, {
      if (!is.null(input$analysis_lab_rows_selected)) {
        analysis.lab$data <<- analysis.lab$data[-input$analysis_lab_rows_selected,]
      }
    })
    
    #show table and make it editable at cell level
    #with the exception of last column which is calculated using function dat4.1()
    output$analysis_lab <- renderDT({analysis.lab$data %>%
        datatable(editable = list(target = "cell", disable = list(columns = c(4))),
                  colnames = c(i18n$t("Test"), i18n$t("N_samples"), i18n$t("Unit cost"), i18n$t("Cost")))
    })
    
    #allow the editing of the initial data on hr
    observeEvent(input$analysis_lab_cell_edit, {
      info <- input$analysis_lab_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      analysis.lab$data[i, j] <<- (DT::coerceValue(v, analysis.lab$data[i, j]))
      analysis.lab$data[,4] <<- analysis.lab$data[,2] * analysis.lab$data[,3]  ## update the calculated column
    })
    
    #show text below the table with sum of lab costs
    analysis_lab_cost_sum <- reactive(sum(analysis.lab$data[, 4]))
    analysis_lab <- reactive(c(i18n$t("Analysis"), i18n$t("Laboratory"), analysis_lab_cost_sum()))
    
    output$analysis_lab_total <- renderText({
      paste0(i18n$t("Total laboratory cost is "), analysis_lab_cost_sum(), ".")
    })
    
    #Analysis - other costs
    analysis.other <- reactiveValues(data=NULL)
    
    analysis.other.fun <- reactive({
      analysis_other_base <- data.frame(
        Cost.items = c(i18n$t("Meetings"), i18n$t("Travel"), i18n$t("Consultants")),
        Costs = c(1000, 2000, 4000))
      analysis_other_base
    })
    
    observe({analysis.other$data <- analysis.other.fun()})
    output$analysis_other <- renderDT({analysis.other$data %>%
        datatable(editable = list(target = "cell"),
                  colnames = c(i18n$t("Cost item"), i18n$t("Cost")))
    })
    
    analysis_other_mat <- reactive(matrix(cbind(i18n$t("Analysis"), as.matrix(analysis.other$data, ncol = 2)), 
                               ncol = 3, 
                               nrow = nrow(analysis.other$data),
                               dimnames = NULL))
    
    #Results - dynamic summary table by cost item
    res.cost.detail <- reactiveValues(data = NULL)
    
    res.cost.detail.fun <- reactive({
      m <- matrix(data = rbind(prep_hr_cost_sum(),
                               prep_other_cost(),
                               fw_mat(),
                               analysis_hr(),
                               analysis_lab(),
                               analysis_other_mat()),
                  ncol = 3)
      colnames(m) <- c("Phase", "Item", "Cost")
      m <- as.data.frame(m, row.names = FALSE)
      m$Cost <- as.numeric(m$Cost)
      m$Percentage <- round(m$Cost/sum(m$Cost)*100, 1)
      m
    })
    
    observe({res.cost.detail$data <- res.cost.detail.fun()})
    
    output$res_sumbycost <- renderDT({res.cost.detail$data %>%
        datatable(colnames = c(i18n$t("Phase"), i18n$t("Item"), i18n$t("Cost"), i18n$t("Percentage")))
      })
    
  #  output$res_sumbycost <- renderTable({
      # Render the first output table
   #   outputTable1()
    #})
    
    res.cost.phase <- reactiveValues(data = NULL)
    res.cost.phase.fun <- reactive({
      x <- res.cost.detail$data
      #x$prop_cost <- round(res.cost.detail$data$Cost*100/sum(res.cost.detail$data$Cost), 1)
      x <- data.frame(x) %>%
        group_by(Phase) %>%
        summarise(Cost = sum(Cost)) %>%
        mutate(Percentage = round(Cost*100/sum(res.cost.detail$data$Cost), 1))
      
      csum = rev(cumsum(rev(x$Percentage)))
      pos = x$Percentage/2 + lead(csum, 1)
      pos = ifelse(is.na(pos), csum/2, pos)
      #x <- aggregate(x$prop_cost, list(x$Phase), FUN=sum)
      #colnames(x) <- c("phase", "sum_prop")
      #x$csum <- rev(cumsum(rev(x$sum_prop)))
      #x$pos <- x$sum_prop/2 + lead(x$csum, 1)
      #x$pos <- ifelse(is.na(x$pos), x$sum_prop/2, x$pos)
      
      x
    })
    
    observe({res.cost.phase$data <- res.cost.phase.fun()})
    output$res_sumbycostphase <- renderDT({res.cost.phase$data %>%
        select(Phase, Cost, Percentage) %>%
        datatable(colnames = c(i18n$t("Phase"), i18n$t("Cost"), i18n$t("Percentage")))})
    
    res.cost.item <- reactiveValues(data = NULL)
    res.cost.item.fun <- reactive({
      x <- res.cost.detail$data
      #x$prop_cost <- round(res.cost.detail$data$Cost*100/sum(res.cost.detail$data$Cost), 1)
      x <- data.frame(x) %>%
        group_by(Item) %>%
        summarise(Cost = sum(Cost)) %>%
        mutate(Percentage = round(Cost*100/sum(res.cost.detail$data$Cost), 1))
      
      csum = rev(cumsum(rev(x$Percentage)))
      pos = x$Percentage/2 + lead(csum, 1)
      pos = ifelse(is.na(pos), csum/2, pos)
      #x <- aggregate(x$prop_cost, list(x$Phase), FUN=sum)
      #colnames(x) <- c("phase", "sum_prop")
      #x$csum <- rev(cumsum(rev(x$sum_prop)))
      #x$pos <- x$sum_prop/2 + lead(x$csum, 1)
      #x$pos <- ifelse(is.na(x$pos), x$sum_prop/2, x$pos)
      
      x
    })
    
    observe({res.cost.item$data <- res.cost.item.fun()})
    output$res_sumbycostitem <- renderDT({res.cost.item$data %>%
        select(Item, Cost, Percentage) %>%
        datatable(colnames = c(i18n$t("Item"), i18n$t("Cost"), i18n$t("Percentage")))})
    
    #Value for money
    totalcost <- reactive(sum(res.cost.detail$data[,"Cost"]))
    totalcost_samplecollected <- reactive(round(totalcost()/sum(fw.samplingframe$data), 1))
    
    output$totalcostpersamplecollected <- renderText({
      paste0(i18n$t("The study aims to collect "), sum(fw.samplingframe$data), 
             i18n$t(" medicine samples at a total cost of "), totalcost(),
             i18n$t(", leading to an estimated cost per sample collected of "), totalcost_samplecollected())
    })
    
    #table with expected OOS prevalence
    prevOOS <- reactiveValues(data=NULL)
    
    prevOOS.fun <- reactive({
        Prevalence <- c(1, 2, 3, 5, 10)
        Est_OOS <- Prevalence * sum(fw.samplingframe$data)/100
        Cost_OOS <- round(totalcost()/Est_OOS, 0)
      prevOOS_base <- as.data.frame(cbind(Prevalence, Est_OOS, Cost_OOS))
      colnames(prevOOS_base) <- c(i18n$t("Prevalence"), "Est_OOS", "Cost_OOS")
      prevOOS_base
    })
    
    #recalculate each time data change using function above
    observe({prevOOS$data <- prevOOS.fun()})
    
    #show table and make it editable at cell level
    #with the exception of the last two columns
    output$res_costperOOSsample <- renderDT({prevOOS$data %>%
        datatable(editable = list(target = "cell", disable = list(columns = c(2, 3))),
                  colnames = c(i18n$t("Prevalence"), i18n$t("Estimated out-of-specification samples"), i18n$t("Cost per out-of-specification sample")))
    })
    
    #allow the editing of the initial data
    observeEvent(input$res_costperOOSsample_cell_edit, {
      info <- input$res_costperOOSsample_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      prevOOS$data[i, j] <<- (DT::coerceValue(v, prevOOS$data[i, j]))
      prevOOS$data[,"Est_OOS"] <<- round(prevOOS$data[,"Prevalence"] * sum(fw.samplingframe$data)/100, 0) ## update the calculated column
      prevOOS$data[,"Cost_OOS"] <<- round(totalcost()/prevOOS$data[,"Est_OOS"], 0) ## update the calculated column
      
    })
    
    #add row button
    observeEvent(input$add_btn6, {
      prevOOS$data[nrow(prevOOS$data) + 1, ] <- NA
    })
    
    #delete row button
    observeEvent(input$del_btn6, {
      if (!is.null(input$res_costperOOSsample_rows_selected)) {
        prevOOS$data <<- prevOOS$data[-input$res_costperOOSsample_rows_selected,]
      }
    })
    
    #Visuals
    output$res_pie_cost_phase <- renderPlotly(
      plot <- plot_ly(res.cost.phase.fun(), labels = ~Phase, values = ~Cost, type = 'pie') %>% 
        layout(title = i18n$t("Costs by phase"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    )
    
    output$res_pie_cost_item <- renderPlotly(
      plot <- plot_ly(res.cost.item.fun(), labels = ~Item, values = ~Cost, type = 'pie') %>% 
        layout(title = i18n$t("Costs by item"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    )

    # Download handler
    output$downloadBtn1 <- downloadHandler(
      filename = "output_data_ENG.pdf",  # Specify the filename for the downloaded PDF file
      content = function(file) {
        # Create the R Markdown content
        rmarkdown::render("output_report_eng.Rmd", output_file = file)
      }
    )
    
    output$downloadBtn2 <- downloadHandler(
      filename = "output_data_IND.pdf",  # Specify the filename for the downloaded PDF file
      content = function(file) {
        # Create the R Markdown content
        rmarkdown::render("output_report_ind.Rmd", output_file = file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
