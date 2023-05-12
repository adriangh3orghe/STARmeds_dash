#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pk_list <-c("shiny", "tidyverse", "ggrepel", "rsconnect", "DT")
lapply(pk_list, require, character.only = TRUE)

#functions for showing and editing data tables
dt_output = function(id, dt_title, dt_caption) {
    fluidRow(column(10, h2(dt_title), p(), p(dt_caption)),
             DTOutput(id))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("STARMeds PMS dashboard"),
    
    navlistPanel(
        tabPanel("Introduction",
                 p("This interactive calculator allows you to estimate the costs required to conduct your planned post-market surveillance study based on input data that you provide and explicit assumptions. The results are meant to be indicative and to be used only as a guide."),
                 
                 p("The tool assumes that a generic PMS study entails three phases: preparing the study; collecting the medicine samples; and reporting the results. The following three tabs ask you for information that captures expected resources to be used in each of these three phases."),
                 
                 p("The final tab “Results” produces a summary of expected costs based on the information you have provided.")),
        "Inputs",
        tabPanel("Study preparation",
                 dt_output("prep_hr", "Human resources", "Double click table cells to edit staff positions and characteristics."),
                 textOutput("prep_hr_total"),
                 dt_output("prep_other", "Other costs", "Double click table cells to edit other cost items.")),
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
                 dt_output("analysis_lab", "Laboratory costs", "Double click table cells to edit tests and unit costs."),
                 dt_output("analysis_other", "Other costs", "Double click table cells to edit other cost items.")),
        "Results",
        tabPanel("Tables",
                 dt_output("res_sumbycost", "Costs by item", "This table is informed by information provided in previous tabs."),
                 plotOutput("cost_piechart")),
        tabPanel("Visuals")
    )
)

# Define server logic
server <- function(input, output) {
    
    #Prep - hr costs
    df1 <- reactiveValues(data=NULL)
    
    dat1 <- reactive({
        prep_hr_base <- data.frame(
            Positions = c("Senior Investigator", "Data Manager"),
            N_FTE = c(0.1, 1),
            Months = c(3, 3),
            Monthly_salary = c(200, 75),
            Salary = c(NA, NA))
        prep_hr_base$Salary <- prep_hr_base$N_FTE * prep_hr_base$Monthly_salary *
            prep_hr_base$Months
        prep_hr_base
    })
    
    observe({df1$data <- dat1()})
    
    output$prep_hr <- renderDT({df1$data %>%
            datatable(editable = list(target = "cell", disable = list(columns = c(5))))
    })
    
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
      colnames(m) <- c("Phase", "Cost item", "Cost")
      m <- as.data.frame(m, row.names = FALSE)
      m$Cost <- as.numeric(m$Cost)
      m$Percentage <- round(m$Cost/sum(m$Cost)*100, 1)
      m
    })
    
    observe({df7$data <- dat7()})
    
    output$res_sumbycost <- renderDT({df7$data})
    
    #Visuals
    cost_byphase <- reactive({df7$data %>%
      group_by(Phase) %>%
      summarise(sum_cost = sum(Cost))
      })

}

# Run the application 
shinyApp(ui = ui, server = server)
