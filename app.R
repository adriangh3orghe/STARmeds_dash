#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load R packages
library("shiny") #for web app
library("tidyverse") #for data manipulation and graphs
library("ggrepel") #for dynamic labels that avoid overlap
library("magrittr") #for pipe operator
library("rsconnect") #for link with shinyapps
library("DT") #for data tables

#function for displaying a table with title and caption
dt_output = function(id, dt_title, dt_caption) {
    fluidRow(column(10, h2(dt_title), 
                    p(), 
                    p(dt_caption),
                    p()),
             DTOutput(id))
}

#initialise data
prep_hr_base <- data.frame(
  Positions = c("Senior Investigator", "Data Manager"),
  N_FTE = c(0.1, 1),
  Months = c(3, 3),
  Monthly_salary = c(200, 75),
  Salary = c(NA, NA))

#preliminary lists for language support
languages <- c("ENG", "FRA")
table_lang <- data.frame(id = "tab_intro",
                            lang = languages,
                            text = c("Intro", "Preface"))

# Define UI for web application
ui <- fluidPage(
    
    #page title
    titlePanel("STARMeds PMS dashboard"),
    
    #language selection
    selectInput("select_lang", "Select language:",
                c("English" = "ENG",
                  "French" = "FRA")),
    
    #list of left hand tabs
    navlistPanel(
      tabPanel(textOutput("tab_intro_title"), 
                 p("This interactive calculator allows you to estimate the costs required to conduct your planned post-market surveillance study based on input data that you provide and explicit assumptions. The results are meant to be indicative and to be used only as a guide."),
                 
                 p("The tool assumes that a generic PMS study entails three phases: preparing the study; collecting the medicine samples; and reporting the results. The following three tabs ask you for information that captures expected resources to be used in each of these three phases."),
                 
                 p("The final tab “Results” produces a summary of expected costs based on the information you have provided.")),
        "Inputs",
        tabPanel("Study preparation",
                 dt_output("prep_hr", "Human resources", "Double click table cells to edit staff positions and characteristics."),
                 actionButton("add_btn1", "Add row", class = "btn-info"),
                 actionButton("del_btn1", "Delete row", class = "btn-warning"),
                 textOutput("prep_hr_total"),
                 dt_output("prep_other", "Other costs", "Double click table cells to edit other cost items."),
                 actionButton("add_btn2", "Add row", class = "btn-info"),
                 actionButton("del_btn2", "Delete row", class = "btn-warning")),
        tabPanel("Fieldwork",
                 textInput(inputId = "fw_medlist", 
                           label = "Input below the TYPES of meds to be collected, separated by '; '", 
                           width = "100%",
                           placeholder = "Example: paracetamol 500mg tab; ibuprofen 400mg caps"),
                 textOutput("fw_medlist_value"),
                 p(),
                 textInput(inputId = "fw_outletlist", 
                           label = "Input below the TYPES of outlets from where meds will be collected, separated by '; '", 
                           width = "100%",
                           placeholder = "Example: street pharmacy; online pharmacy; clinics"),
                 textOutput("fw_outletlist_value"),
                 p(),
                 dt_output("fw_sampleframe", "Medicine samples to be collected",
                           "The table below will update automatically based on values inputted above. Once you are happy with the structure, double click each table cell to populate it with the target number of medicine samples to be collected for each medicine X outlet combination")
                 ),
        tabPanel("Analysis and reporting",
                 dt_output("analysis_hr", "Human resources", "Double click table cells to edit staff positions and characteristics."),
                 actionButton("add_btn3", "Add row", class = "btn-info"),
                 actionButton("del_btn3", "Delete row", class = "btn-warning"),
                 dt_output("analysis_lab", "Laboratory costs", "Double click table cells to edit tests and unit costs."),
                 actionButton("add_btn4", "Add row", class = "btn-info"),
                 actionButton("del_btn4", "Delete row", class = "btn-warning"),
                 dt_output("analysis_other", "Other costs", "Double click table cells to edit other cost items."),
                 actionButton("add_btn5", "Add row", class = "btn-info"),
                 actionButton("del_btn5", "Delete row", class = "btn-warning")),
        "Results",
        tabPanel("Tables",
                 dt_output("res_sumbycost", "Costs by item", 
                           "This table is informed by information provided in previous tabs."),
                 dt_output("res_sumbycostphase", "Costs by phase", 
                           "This table is informed by information provided in previous tabs."),
                 dt_output("res_sumbycostitem", "Costs by item", 
                           "This table is informed by information provided in previous tabs.")),
        tabPanel("Visuals",
                 plotOutput("res_pie_cost_phase"),
                 plotOutput("res_pie_cost_item"))
    )
)

# Define server logic
server <- function(input, output) {
    
    #change language
  output$tab_intro_title <- renderText({
    switch(input$select_lang, "ENG"="Introduction", "FRA"="Preface") 
  })
  
    #Prep - hr costs
  
    #define as dynamic object and load with initial data
    df1 <- reactiveValues(data=prep_hr_base)
    
    #function to calculate the last column (Salary) as product of previous columns
    dat1 <- reactive({
      prep_hr_base %>%
        mutate(Salary = N_FTE * Monthly_salary * Months)
    })
    
    #recalculate each time data change using function above
    observe({df1$data <- dat1()})
    
    #show table and make it editable at cell level
    #with the exception of last column which is calculated using function dat1()
    output$prep_hr <- renderDT({df1$data %>%
            datatable(editable = list(target = "cell", disable = list(columns = c(5))))
    })
    
    #allow the editing of the initial data on hr
    observeEvent(input$prep_hr_cell_edit, {
        info <- input$prep_hr_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        
        # Without this line the table does not change but with it it jumps to row 1 after an edit.
        df1$data[i, j] <<- (DT::coerceValue(v, df1$data[i, j]))
        df1$data[,"Salary"] <<- df1$data[,"Monthly_salary"] * df1$data[,"N_FTE"] * df1$data[,"Months"]  ## update the calculated column
    })
    
    #add row button
    observeEvent(input$add_btn1, {
      df1$data [nrow(df1$data) + 1, ] <- NA
    })
    
    #delete row button
    observeEvent(input$del_btn1, {
      if (!is.null(input$prep_hr_rows_selected)) {
        df1$data <<- df1$data[-input$prep_hr_rows_selected,]
      }
    })
    
    #show text below the table with sum of salary costs
    prep_hr_cost_sum <- reactive(sum(df1$data[,"Salary"]))
    prep_hr <- reactive(c("Preparation", "Human resources", prep_hr_cost_sum()))
    
    output$prep_hr_total <- renderText({
      paste0("Total HR cost is ", prep_hr_cost_sum(), ".")
    })
    
    #Prep - other costs
    df2 <- reactiveValues(data=NULL)
    
    dat2 <- reactive({
        prep_other_base <- data.frame(
            `Cost items` = c("Meetings", "Travel", "Consultants"),
            Costs = c(1000, 2000, 4000))
        prep_other_base
    })
    
    observe({df2$data <- dat2()})
    
    observeEvent(input$add_btn2, {
      df2$data [nrow(df2$data) + 1, ] <- NA
    })
    
    observeEvent(input$del_btn2, {
      if (!is.null(input$prep_other_rows_selected)) {
        df2$data <<- df2$data[-input$prep_other_rows_selected,]
      }
    })
    
    output$prep_other <- renderDT({df2$data %>%
            datatable(editable = list(target = "cell"))
    })
    
    df2_mat <- reactive(matrix(cbind("Preparation", as.matrix(df2$data, ncol = 2)), 
                               ncol = 3,
                               nrow = nrow(df2$data),
                               dimnames = NULL))
    
    #Fieldwork - types of medicines
    
    x <- reactive(input$fw_medlist)
    y <- reactive(strsplit(as.character(x()), "; ")[[1]])
    z <- reactive(length(y()))
    
    output$fw_medlist_value <- renderText({
        paste0("You have entered ", z(), " unique types of medicines.")
    })
    
    #Fieldwork - types of outlets
    
    t <- reactive(input$fw_outletlist)
    u <- reactive(strsplit(as.character(t()), "; ")[[1]])
    v <- reactive(length(u()))
    
    output$fw_outletlist_value <- renderText({
        paste0("You have entered ", v(), " unique types of outlets.")
    })
    
    #Fieldwork - dynamic sampling frame
    df3 <- reactiveValues(data = NULL)
    
    dat3 <- reactive({
        m <- matrix(data = NA, 
                    nrow = z(),
                    ncol = v(),
                    dimnames = list(y(), u()))
        m
    })
  
    observe({df3$data <- dat3()})
    
    output$fw_sampleframe <- renderDT({df3$data %>%
            datatable(editable = list(target = "cell"))
        })
    
    #Analysis - hr costs
    df4 <- reactiveValues(data=NULL)
    
    dat4 <- reactive({
      analysis_hr_base <- data.frame(
        Positions = c("Senior Investigator", "Data Manager"),
        N_FTE = c(0.1, 1),
        Months = c(3, 3),
        Monthly_salary = c(200, 75),
        Salary = c(NA, NA))
      analysis_hr_base$Salary <- analysis_hr_base$N_FTE * analysis_hr_base$Monthly_salary *
        analysis_hr_base$Months
      analysis_hr_base
    })
    
    observe({df4$data <- dat4()})
    
    output$analysis_hr <- renderDT({df4$data %>%
        datatable(editable = list(target = "cell", disable = list(columns = c(5))))
    })
    
    observeEvent(input$analysis_hr_cell_edit, {
      info <- input$analysis_hr_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      df4$data[i, j] <<- (DT::coerceValue(v, df4$data[i, j]))
      df4$data[,"Salary"] <<- df4$data[,"Monthly_salary"] * df4$data[,"N_FTE"] * df4$data[,"Months"]  ## update the calculated column
    })
    
    analysis_hr_cost_sum <- reactive(sum(df4$data[,"Salary"]))
    analysis_hr <- reactive(c("Analysis", "Human resources", analysis_hr_cost_sum()))
    
    #Analysis - other costs
    df6 <- reactiveValues(data=NULL)
    
    dat6 <- reactive({
      analysis_other_base <- data.frame(
        `Cost items` = c("Meetings", "Travel", "Consultants"),
        Costs = c(1000, 2000, 4000))
      analysis_other_base
    })
    
    observe({df6$data <- dat6()})
    output$analysis_other <- renderDT({df6$data %>%
        datatable(editable = list(target = "cell"))
    })
    
    df6_mat <- reactive(matrix(cbind("Analysis", as.matrix(df6$data, ncol = 2)), 
                               ncol = 3, 
                               nrow = nrow(df6$data),
                               dimnames = NULL))
    
    #Results - dynamic summary table by cost item
    df7 <- reactiveValues(data = NULL)
    
    dat7 <- reactive({
      m <- matrix(data = rbind(prep_hr(),
                               df2_mat(),
                               analysis_hr(),
                               df6_mat()),
                  ncol = 3)
      colnames(m) <- c("Phase", "Cost.item", "Cost")
      m <- as.data.frame(m, row.names = FALSE)
      m$Cost <- as.numeric(m$Cost)
      m$Percentage <- round(m$Cost/sum(m$Cost)*100, 1)
      m
    })
    
    observe({df7$data <- dat7()})
    
    output$res_sumbycost <- renderDT({df7$data})
    
    
    df8 <- reactiveValues(data = NULL)
    dat8 <- reactive({
      x <- df7$data
      x$prop_cost <- round(df7$data$Cost*100/sum(df7$data$Cost), 1)
      x <- aggregate(x$prop_cost, list(x$Phase), FUN=sum)
      colnames(x) <- c("phase", "sum_prop")
      x$csum <- rev(cumsum(rev(x$sum_prop)))
      x$pos <- x$sum_prop/2 + lead(x$csum, 1)
      x$pos <- ifelse(is.na(x$pos), x$sum_prop/2, x$pos)
      x
    })
    
    observe({df8$data <- dat8()})
    output$res_sumbycostphase <- renderDT({df8$data})
    
    df9 <- reactiveValues(data = NULL)
    dat9 <- reactive({
      x <- df7$data
      x$prop_cost <- round(df7$data$Cost*100/sum(df7$data$Cost), 1)
      x <- aggregate(x$prop_cost, list(x$Cost.item), FUN=sum)
      colnames(x) <- c("cost.item", "sum_prop")
      x$csum <- rev(cumsum(rev(x$sum_prop)))
      x$pos <- x$sum_prop/2 + lead(x$csum, 1)
      x$pos <- ifelse(is.na(x$pos), x$sum_prop/2, x$pos)
      x
    })
    
    observe({df9$data <- dat9()})
    output$res_sumbycostitem <- renderDT({df9$data})
    
    #Visuals
    
    output$res_pie_cost_phase <- renderPlot({
      
      ggplot(dat8(),
             aes(x = "" , y = sum_prop, fill = fct_inorder(phase))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        labs(title = "Cost breakdown by phase") +
        scale_fill_brewer(palette = "Pastel1") +
        geom_label_repel(dat8(),
                         mapping=aes(y = pos, label = paste0(sum_prop, "%")),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = "Phase")) +
        theme_void()
    })
    
    output$res_pie_cost_item <- renderPlot({
      
      ggplot(dat9(),
             aes(x = "" , y = sum_prop, fill = fct_inorder(cost.item))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        labs(title = "Cost breakdown by cost item") +
        scale_fill_brewer(palette = "Pastel1") +
        geom_label_repel(dat9(),
                         mapping=aes(y = pos, label = paste0(sum_prop, "%")),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = "Cost item")) +
        theme_void()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
