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
library("shiny.i18n")
library("ggplot2")
library(shinydashboard)
library(bslib)
library(htmlTable)
library(rmarkdown)
library(png)


#i18n <- Translator$new(translation_json_path='translation.json')
#i18n <- Translator$new(translation_csvs_path = "C:/Users/svalente/Desktop/shiny")
#i18n <- Translator$new(translation_csvs_path = "/Users/adriangheorghe/Documents/starmeds_shiny")
i18n <- Translator$new(translation_csvs_path = "/Users/sara/Documents/GitHub/STARmeds_dash/translation_sara")

i18n$set_translation_language('en')

#function for displaying a table with title and caption
dt_output = function(id, dt_title, dt_caption) {
    fluidRow(column(10, h2(dt_title), 
                    p(), 
                    p(dt_caption),
                    p()),
             DTOutput(id))
}


# Define UI for web application
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
                                 
                                 p(i18n$t("The tool assumes that a generic PMS study entails three phases: preparing the study, collecting the medicine samples, and reporting the results. The following three tabs ask you for information that captures expected resources to be used in each of these three phases.")),
                                 
                                 p(i18n$t("The final tab “Results” produces a summary of expected costs based on the information you have provided.")),
                      
                                 p(i18n$t("Please report any issues at"), tags$a(href="bla@bla.com", "support@starmeds.id")),
                      
                                 p(tags$a(href="https://starmeds.id/", "STARmeds website"))
                  ),
             
             tabPanel("Toolkit",
  #page title
  #titlePanel(i18n$t("STARMeds PMS dashboard")),
    #list of left hand tabs

    navlistPanel(
    "Inputs",
        tabPanel(i18n$t("Study preparation"),
                 dt_output("prep_hr", i18n$t("Human resources"), i18n$t("Double click table cells to edit staff positions and characteristics.")),
                 actionButton("add_btn1", i18n$t("Add row"), class = "btn-info"),
                 actionButton("del_btn1", i18n$t("Delete row"), class = "btn-warning"),
                 textOutput("prep_hr_total"),
                 dt_output("prep_other", i18n$t("Other costs"), i18n$t("Double click table cells to edit other cost items.")),
                 actionButton("add_btn2", i18n$t("Add row"), class = "btn-info"),
                 actionButton("del_btn2", i18n$t("Delete row"), class = "btn-warning")),
        tabPanel(i18n$t("Fieldwork"),
                 textInput(inputId = "fw_medlist", 
                           label = "Input below the TYPES of meds to be collected, separated by '; '", 
                           width = "100%",
                           placeholder = "Example: paracetamol 500mg tab; ibuprofen 400mg caps"),
                 textOutput("fw_medlist_value"),
                 p(),
                 textInput(inputId = "fw_outletlist", 
                           label = i18n$t("Input below the TYPES of outlets from where meds will be collected, separated by '; '"), 
                           width = "100%",
                           placeholder = "Example: street pharmacy; online pharmacy; clinics"),
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
                 dt_output("analysis_lab", i18n$t("Laboratory costs"), i18n$t("Double click table cells to edit tests and unit costs.")),
                 actionButton("add_btn4", i18n$t("Add row"), class = "btn-info"),
                 actionButton("del_btn4", i18n$t("Delete row"), class = "btn-warning"),
                 dt_output("analysis_other", i18n$t("Other costs"), i18n$t("Double click table cells to edit other cost items.")),
                 actionButton("add_btn5", i18n$t("Add row"), class = "btn-info"),
                 actionButton("del_btn5", i18n$t("Delete row"), class = "btn-warning")),
        "Results",
        tabPanel(i18n$t("Tables"),
                 dt_output("res_sumbycost", i18n$t("Costs by item"), 
                           i18n$t("This table is informed by information provided in previous tabs.")),
                 dt_output("res_sumbycostphase", i18n$t("Costs by phase"), 
                           i18n$t("This table is informed by information provided in previous tabs.")),
                 dt_output("res_sumbycostitem", i18n$t("Costs by item"), 
                           i18n$t("This table is informed by information provided in previous tabs."))),
        tabPanel(i18n$t("Visuals"),
                 plotOutput("res_pie_cost_phase"),
                 plotOutput("res_pie_cost_item"))
    ),    
  downloadButton("downloadBtn1", "Export report - ENG"),
  downloadButton("downloadBtn2", "Export report - IND"),
             )
  ),
)

# Define server logic
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
    df1 <- reactiveValues(data=NULL)
    
    dat1 <- reactive({
      prep_hr_base <- data.frame(
        Positions = c(i18n$t("Senior Investigator"),i18n$t("Data Manager")),
        N_FTE = c(0.1, 1),
        Months = c(3, 3),
        Monthly_salary = c(200, 75),
        Salary = c(NA, NA)
      ) %>%
        #function to calculate the last column (Salary) as product of previous columns
        mutate(Salary = N_FTE * Monthly_salary * Months)
      colnames(prep_hr_base) <- c(i18n$t("Positions"), "N_FTE",i18n$t("Months"), i18n$t("Monthly salary"), "Salary")
      prep_hr_base
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
    prep_hr <- reactive(c(i18n$t("Preparation"), i18n$t("Human resources"), prep_hr_cost_sum()))
    
    output$prep_hr_total <- renderText({
      paste0(i18n$t("Total HR cost is "), prep_hr_cost_sum(), ".")
    })
    
    #Prep - other costs
    df2 <- reactiveValues(data=NULL)
    
    dat2 <- reactive({
        prep_other_base <- data.frame(
            `Cost items` = c(i18n$t("Meetings"), i18n$t("Travel"), i18n$t("Consultants")),
            Costs = c(1000, 2000, 4000))
        colnames(prep_other_base) <- c(i18n$t("Cost items"), i18n$t("Costs"))
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
    
    df2_mat <- reactive(matrix(cbind(i18n$t("Preparation"), as.matrix(df2$data, ncol = 2)), 
                               ncol = 3,
                               nrow = nrow(df2$data),
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
        paste0(i18n$t("You have entered "), v(), i18n$t(" unique types of outlets"))
    })
    
    #Fieldwork - dynamic sampling frame
    df3 <- reactiveValues(data = NULL)
    
    dat3 <- reactive({
        m <- matrix(data = as.numeric(NA), 
                    nrow = z(),
                    ncol = v(),
                    dimnames = list(y(), u()))
        m
    })
  
    observe({df3$data <- dat3()})
    
    output$fw_sampleframe <- renderDT({df3$data %>%
            datatable(editable = list(target = "cell"))
        })
    
    observeEvent(input$fw_sampleframe_cell_edit, {
      info <- input$fw_sampleframe_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      df3$data[i, j] <<- (DT::coerceValue(v, df3$data[i, j]))
    })
    
    #Fieldwork - dynamic sample collection rate
    df3.1 <- reactiveValues(data = NULL)
    q <- reactive(colSums(df3$data))
    
    output$fw_colsum_text <- renderText(q())
    
    dat3.1 <- reactive({
      m <- data.frame(
        Samples.day = rep(1, v()),
        Est.samples = q()) %>%
        mutate(Est.days = round(Est.samples/Samples.day, 1))
      m
      
      })
    
    observe({df3.1$data <- dat3.1()})
    
    output$fw_perdayperoutlet <- renderDT({df3.1$data %>%
        datatable(editable = list(target = "cell", 
                                  disable = list(columns = c(3))))
    })
    
    observeEvent(input$fw_perdayperoutlet_cell_edit, {
      info <- input$fw_perdayperoutlet_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      df3.1$data[i, j] <<- (DT::coerceValue(v, df3.1$data[i, j]))
      df3.1$data[,3] <<- round(df3.1$data[,2]/df3.1$data[,1], 1)
    })
    
    #Fieldwork - dynamic table of sampled medicines cost
    df3.2 <- reactiveValues(data = NULL)
    r <- reactive(rowSums(df3$data))
    
    dat3.2 <- reactive({
      m <- data.frame(
        Medicines = y(),
        Med.sampled = r(),
        Cost.med = rep(1, z())) %>%
        mutate(Est.costmed = round(Med.sampled*Cost.med, 1))
      m
      
    })
    
    observe({df3.2$data <- dat3.2()})
    
    output$fw_medcost <- renderDT({df3.2$data %>%
        datatable(editable = list(target = "cell", 
                                  disable = list(columns = c(1, 2, 4))))
    })
    
    observeEvent(input$fw_medcost_cell_edit, {
      info <- input$fw_medcost_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # Without this line the table does not change but with it it jumps to row 1 after an edit.
      df3.2$data[i, j] <<- (DT::coerceValue(v, df3.2$data[i, j]))
      df3.2$data[,4] <<- round(df3.2$data[,2]*df3.2$data[,3], 1)
    })
    
    total_days <- reactive({ input$integer })
    
    cost_day <- reactive({ input$obs })
    
    output$fw_total_cost <- renderText({
      paste0(i18n$t("The total estimated cost is"),(" "), total_days()*cost_day()," .")
    })
    
    #Analysis - hr costs
    df4 <- reactiveValues(data=NULL)
    
    dat4 <- reactive({
      analysis_hr_base <- data.frame(
        Positions = c(i18n$t("Senior Investigator"), i18n$t("Data Manager")),
        N_FTE = c(0.1, 1),
        Months = c(3, 3),
        Monthly_salary = c(200, 75),
        Salary = c(NA, NA))
      analysis_hr_base$Salary <- analysis_hr_base$N_FTE * analysis_hr_base$Monthly_salary *
        analysis_hr_base$Months
      colnames(analysis_hr_base) <- c(i18n$t("Positions"), "N_FTE",i18n$t("Months"), i18n$t("Monthly salary"), "Salary")
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
      df4$data[,"Salary"] <<- df4$data[,i18n$t("Monthly_salary")] * df4$data[,"N_FTE"] * df4$data[,i18n$t("Months")]  ## update the calculated column
    })
    
    analysis_hr_cost_sum <- reactive(sum(df4$data[,"Salary"]))
    analysis_hr <- reactive(c(i18n$t("Analysis"), i18n$t("Human resources"), analysis_hr_cost_sum()))
    
    #Analysis - other costs
    df6 <- reactiveValues(data=NULL)
    
    dat6 <- reactive({
      analysis_other_base <- data.frame(
        `Cost items` = c(i18n$t("Meetings"), i18n$t("Travel"), i18n$t("Consultants")),
        Costs = c(1000, 2000, 4000))
      colnames(analysis_other_base) <- c(i18n$t("Cost items"), i18n$t("Costs"))
      analysis_other_base
    })
    
    observe({df6$data <- dat6()})
    output$analysis_other <- renderDT({df6$data %>%
        datatable(editable = list(target = "cell"))
    })
    
    df6_mat <- reactive(matrix(cbind(i18n$t("Analysis"), as.matrix(df6$data, ncol = 2)), 
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
      m <- as.data.frame(m, row.names = FALSE)
      colnames(m) <- c("Phase", "Cost.item", "Cost")
      m$Cost <- as.numeric(m$Cost)
      m$Percentage <- round(m$Cost/sum(m$Cost)*100, 1)
      m
    })
    
    observe({df7$data <- dat7()})
    
    output$res_sumbycost <- renderDT({df7$data})
    
  #  output$res_sumbycost <- renderTable({
      # Render the first output table
   #   outputTable1()
    #})
    
    df8 <- reactiveValues(data = NULL)
    dat8 <- reactive({
      x <- df7$data
      x$prop_cost <- round(df7$data$Cost*100/sum(df7$data$Cost), 1)
      x <- aggregate(x$prop_cost, list(x$Phase), FUN=sum)
      colnames(x) <- c("Phase", "sum_prop")
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
      library(ggplot2)
      
      # Create the plot
      plot2 <- 
      ggplot(dat8(),
             aes(x = "" , y = sum_prop, fill = fct_inorder(phase))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        labs(title = i18n$t("Cost breakdown by phase")) +
        scale_fill_brewer(palette = "Pastel1") +
        geom_label_repel(dat8(),
                         mapping=aes(y = pos, label = paste0(sum_prop, "%")),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = i18n$t("Phase"))) +
        theme_void()
      # Display the plot
      print(plot2)
      
      # Save the plot using ggsave and print it
      ggsave(filename = "res_pie_cost_phase.png", plot = print(plot2), dpi = 300)
      
      # Return the plot for rendering (optional)
      plot
    }
    )
    
    
    
    output$res_pie_cost_item <- renderPlot({
      library(ggplot2)
      
      # Create the plot
      plot <- ggplot(dat9(),
                     aes(x = "", y = sum_prop, fill = fct_inorder(cost.item))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        labs(title = i18n$t("Cost breakdown by cost item")) +
        scale_fill_brewer(palette = "Pastel1") +
        geom_label_repel(
          dat9(),
          mapping = aes(y = pos, label = paste0(sum_prop, "%")),
          size = 4.5, nudge_x = 1, show.legend = FALSE
        ) +
        guides(fill = guide_legend(title = i18n$t("Cost item"))) +
        theme_void()
      
      # Display the plot
      print(plot)
      
      # Save the plot using ggsave and print it
      ggsave(filename = "res_pie_cost_item.png", plot = print(plot), dpi = 300)
      
      # Return the plot for rendering (optional)
      plot
    })
    
      #ggsave("myplotplot.png", plot = plot1, device = 'png')

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
