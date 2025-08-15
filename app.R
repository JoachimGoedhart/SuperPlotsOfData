# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SuperPlotsOfData: Shiny app for plotting and comparing data from different replicates
# Created by Joachim Goedhart (@joachimgoedhart), first version 2020
# Uses tidy data as input with a column that defines conditions and a column with measured values
# A third column can be selected that indicates the replicates (as numbers or other unique strings)
# Raw data is displayed with user-defined visibility (alpha)
# Summary statistics are displayed with user-defined visibility (alpha)
# A plot and a table with stats are generated
# Several colorblind safe palettes are available
# Ordering of the conditions is 'as is', based on median or alphabetical
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Copyright (C) 2020  Joachim Goedhart
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Improvements:
# display of p-values (use of scientific notation if smaller than 0.001
# Needs fixing, since doesn't work for multiple conditions)
# Implement measures of reproducibility/repeatability




library(shiny)
library(plyr)
library(tidyverse)
library(ggbeeswarm)
library(readxl)
library(DT)
library(RCurl)
library(broom)

#Uncomment for sinaplot
#library(ggforce)

source("geom_flat_violin.R")
source("themes.R")
source("function_tidy_df.R")
source("repeatability.R")

###### Functions ##########


# Function that returns the p-value from Shapiro-Wilk test
f = function(x){
  if (length(x)<3) {return(NA)}

  # Return NA in case all numbers are identical
  if (length(unique(x))<2) {return(NA)}
  st = shapiro.test(x)
  return(st$p.value)
  }

#Custom stats for ggplot2: https://stackoverflow.com/questions/6717675/how-can-one-write-a-function-to-create-custom-error-bars-for-use-with-ggplot2/6717697

add_SD <- function(x) {
  avg <- mean(x)
  sd <- sd(x)
  triplet <- data.frame(avg, avg-sd, avg+sd)
  names(triplet) <- c("y","ymin","ymax") #this is what ggplot is expecting
  return (triplet)
}

add_CI <- function(x) {
  avg <- mean(x)
  sd <- sd(x)
  n <- length(x)
  sem <-  sd / sqrt(n)
  CI_lo = avg - qt((1+0.95)/2, n - 1) * sem
  CI_hi = avg + qt((1+0.95)/2, n - 1) * sem
  triplet <- data.frame(avg, CI_lo, CI_hi)
  names(triplet) <- c("y","ymin","ymax") #this is what ggplot is expecting
  return (triplet)
}

add_sem <- function(x) {
  avg <- mean(x)
  sd <- sd(x)
  n <- length(x)
  sem <-  sd / sqrt(n)
  triplet <- data.frame(avg, avg-sem, avg+sem)
  names(triplet) <- c("y","ymin","ymax") #this is what ggplot is expecting
  return (triplet)
}



##### Global variables #####

i=0
#Number of bootstrap samples
nsteps=1000

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

alpha=1-Confidence_level
lower_percentile=(1-Confidence_level)/2
upper_percentile=1-((1-Confidence_level)/2)

#Code to generate vectors in R to use these palettes

#From Paul Tol: https://personal.sron.nl/~pault/
Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')

Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')

Greyscale <- c('grey30','grey40','grey50','grey60','grey70')

#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Read a text file (comma separated values)
df_tidy_example <- read.csv("combined.csv", na.strings = "", stringsAsFactors = TRUE)
df_tidy_example2 <- read.csv("SystBloodPressure_tidy.csv", na.strings = "", stringsAsFactors = TRUE)

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

###### UI: User interface #########

ui <- fluidPage(

  titlePanel("SuperPlotsOfData - Plots Data and its Replicates"),
  sidebarLayout(
    sidebarPanel(width=3,

                 conditionalPanel(
                   condition = "input.tabs=='Data upload'",
                   radioButtons(
                     "data_input", h4("Data upload"),
                     choices =
                       list(
                          # "Example data (tidy format)" = 1,
                         "Example data (tidy)" = 1,
                         "Example data (tidy)" = 2,
                         "Upload file" = 3,
                         "Paste data" = 4,
                         "URL (csv files only)" = 5
                       )
                     ,
                     selected =  1),
                   conditionalPanel(
                     condition = "input.data_input=='2'"

                   ),
                   conditionalPanel(
                     condition = "input.data_input=='1'",
                     p("Data S1 published in the original SuperPlots paper:"),a("https://doi.org/10.1083/jcb.202001064", href="https://doi.org/10.1083/jcb.202001064")
                   ),
                   conditionalPanel(
                     condition = "input.data_input=='2'",
                     p("Data from Table 1 of:"),a("Bland & Altman (1999)", href="https://doi.org/10.1177/096228029900800204")
                   ),

                   conditionalPanel(
                     condition = "input.data_input=='3'",
                     h5("Upload file: "),
                     fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
                     # selectInput("file_type", "Type of file:",
                     #             list("text (csv)" = "text",
                     #                  "Excel" = "Excel"
                     #             ),
                     #             selected = "text"),

                     selectInput("upload_delim", label = "Select Delimiter (for text file):", choices =list("Comma" = ",",
                                                                                                            "Tab" = "\t",
                                                                                                            "Semicolon" = ";",
                                                                                                            "Space" = " "))),


                     # selectInput("sheet", label = "Select sheet (for excel workbook):", choices = " "),

                     # conditionalPanel(
                     #   condition = "input.file_type=='text'",
                     #
                     #   radioButtons(
                     #     "upload_delim", "Delimiter",
                     #     choices =
                     #       list("Comma" = ",",
                     #            "Tab" = "\t",
                     #            "Semicolon" = ";",
                     #            "Space" = " "),
                     #     selected = ",")),
                     #
                     # actionButton("submit_datafile_button",
                     #              "Submit datafile")),
                   conditionalPanel(
                     condition = "input.data_input=='4'",
                     h5("Paste data below:"),
                     tags$textarea(id = "data_paste",
                                   placeholder = "Add data here",
                                   rows = 10,
                                   cols = 20, ""),
                     actionButton("submit_data_button", "Submit data"),
                     radioButtons(
                       "text_delim", "Delimiter",
                       choices =
                         list("Tab (from Excel)" = "\t",
                              "Space" = " ",
                              "Comma" = ",",
                              "Semicolon" = ";"),
                       selected = "\t")),

                   ### csv via URL as input
                   conditionalPanel(
                     condition = "input.data_input=='5'",
                     #         textInput("URL", "URL", value = "https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"),
                     textInput("URL", "URL", value = ""),
                     NULL
                   ),

                   hr(),
                   h4('Data conversion'),
                   checkboxInput(inputId = "toggle_tidy", label = "Convert to tidy", value = FALSE),
                   conditionalPanel(
                     condition = "input.toggle_tidy==true",

                     ########### Ask for number of rows and labels (optional) ############
                     numericInput("n_conditions", "Number of rows that specify parameters:", value = 1,min = 1,max=10,step = 1),
                     textInput("labels", "Labels for parameters (separated by comma):", value = ""),

                     NULL),

                   hr(),
                   h4('Data selection for plotting'),
                   selectInput("x_var", "Data for the x-axis:", choices = "Treatment", selected="Treatment"),

                   selectInput("y_var", "Data for the y-axis:", choices = "Speed", selected="Speed"),

                   selectInput("g_var", "Groups/Replicates:", choices = list("Replicate", "-"), selected="Replicate"),

                   hr(),
                   # selectInput("filter_column", "Filter based on this parameter:", choices = ""),
                   selectInput("use_these_conditions", "Select and order:", "", multiple = TRUE),
                   hr(),


                   h4('Data properties'),
                   checkboxInput(inputId = "x_cont",
                                 label = "Continuous x-axis data",
                                 value = FALSE),
                   checkboxInput(inputId = "paired",
                                 label = "All data are paired/connected",
                                 value = FALSE),
                   hr(),
                   checkboxInput(inputId = "info_data",
                                 label = "Show information on data formats",
                                 value = FALSE),

                   conditionalPanel(
                     condition = "input.info_data==true",
                    p("The data has to be organized in a 'tidy' format. This means that all measured values must be present in a single column ('Speed' in the example data). The labels for the conditions are present in another column ('Treatment' in the example data). When different replicates are present, this information is stored in a third column ('Replicate' in the example data). The selection of a column with replicates is optional. The order of the columns is arbitrary. For more information, see:"),
                    a("A tutorial on preparing data for superplots", href = "https://thenode.biologists.com/converting-excellent-spreadsheets-part2"),br(),
                    a("A basic intro on converting spreadsheet data", href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/"),br(),
                    a("The original paper by Hadley Wickham 'Tidy data'", href = "http://dx.doi.org/10.18637/jss.v059.i10")

                   ),
                   NULL
                 ),

      conditionalPanel(
        condition = "input.tabs=='Plot'",

        radioButtons(inputId = "jitter_type", label = h4("Data display"), choices = list("Data & distribution" = "quasirandom", "Jittered data" = "random","No offset"="no_jitter", "Distribution only"="violin"), selected = "quasirandom"),

        checkboxInput(inputId = "change_size", label = "Change size", value = FALSE),
        conditionalPanel(condition = "input.change_size == true",
                         numericInput("dot_size", "Size:", value = 3.5),
        ),

        sliderInput(inputId = "alphaInput", label = "Visibility of the data", 0, 1, 0.7),

        h4("Replicates"),
        radioButtons(inputId = "summary_replicate", label = "Statistics per replicate:", choices = list("Mean" = "mean", "Median" = "median"), selected = "mean"),
        sliderInput(inputId = "alphaStats", label = "Visibility of the stats", 0, 1, 1),
        radioButtons(inputId = "connect", label = "Connect the dots (treat as paired data):", choices = list("No" = "blank", "Dotted line" = "dotted", "Dashed line"= "dashed", "Solid line" ="solid"), selected = "blank"),


        # checkboxInput(inputId = "connect", label = "Connect the dots (paired data)", FALSE),
        # conditionalPanel(condition = "input.connect == true",
        #                  checkboxInput(inputId = "solid", label = "Solid line", FALSE)
        # ),
        #
        checkboxInput(inputId = "add_shape", label = "Identify by shape", value = FALSE),
        checkboxInput(inputId = "add_n", label = "Size reflects 'n' (new feature)", value = FALSE),

        checkboxInput(inputId = "show_distribution", label = "Distribution per replicate", value = FALSE),

        radioButtons("adjustcolors", "By color:",
                     choices =
                       list("Greyscale" = 1,
                            "Viridis" = 2,
                            "Okabe&Ito; CUD" = 6,
                            "Tol; bright" = 3,
                            "Tol; light" = 4,
                            "User defined"=5),
                     selected =  6),

        conditionalPanel(condition = "input.adjustcolors == 5",
                         textInput("user_color_list", "Names or hexadecimal codes separated by a comma (applied to conditions in alphabetical order):", value = "turquoise2,#FF2222,lawngreen"),

                         h5("",a("Click here for more info on color names", href = "http://www.endmemo.com/program/R/color.php", target="_blank"))

        ),
        selectInput("split_direction", label = "Split replicates:", choices = list("No", "Horizontal", "Vertical"), selected = "No"),

        # h4("Comparing conditions"),
        radioButtons(inputId = "summary_condition", label = "Error bars:", choices = list("Mean & S.D." = "mean_SD", "Mean & 95%CI" = "mean_CI", "Mean & s.e.m." = "mean_sem", "none"="none"), selected = "none"),

        conditionalPanel(condition = "input.summary_condition != 'none'",
                         sliderInput("alphaInput_summ", "Visibility of the error bar:", 0, 1, 1)
        ),


        checkboxInput(inputId = "show_table", label = "Table with quantitative comparison", value = FALSE),

        # conditionalPanel(condition = "input.show_table == true",
        selectInput("zero", "Select reference condition:", choices = ""
                    # )
        ),




        h4("Plot Layout"),

        radioButtons(inputId = "ordered", label= "Order of the conditions:", choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"), selected = "none"),


        checkboxInput(inputId = "rotate_plot", label = "Rotate plot 90 degrees", value = FALSE),

        checkboxInput(inputId = "no_grid", label = "Remove gridlines", value = FALSE),

        checkboxInput(inputId = "change_scale", label = "Change scale", value = FALSE),

          conditionalPanel(condition = "input.change_scale == true", checkboxInput(inputId = "scale_log_10", label = "Log scale", value = FALSE),

        textInput("range", "Range of values (min,max)", value = "")),

      # checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (minimum n=10)"), value = FALSE),
      # conditionalPanel(
      #   condition = "input.add_CI == true && input.summary_replicate !='box'",
      #   checkboxInput(inputId = "ugly_errors", label = "Classic error bars", value = FALSE)),


            ########## Choose color from list
            # selectInput("colour_list", "Colour:", choices = ""),


        checkboxInput(inputId = "dark", label = "Dark Theme", value = FALSE),
      # conditionalPanel(
      #   condition = "input.dark == true",checkboxInput(inputId = "dark_classic", label = "Classic Dark Theme", value = FALSE)),

        numericInput("plot_height", "Height (# pixels): ", value = 480),
        numericInput("plot_width", "Width (# pixels):", value = 480),

        h4("Labels/captions"),

        checkboxInput(inputId = "add_title",
                        label = "Add title",
                        value = FALSE),
        conditionalPanel(
        condition = "input.add_title == true",
        textInput("title", "Title:", value = "")
        ),

        checkboxInput(inputId = "label_axes",
              label = "Change labels",
              value = FALSE),
        conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = ""),
              textInput("lab_y", "Y-axis:", value = "")),
        checkboxInput(inputId = "adj_fnt_sz",
              label = "Change font size",
              value = FALSE),
       conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_title", "Plot title:", value = 24),
              numericInput("fnt_sz_labs", "Axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Axis labels:", value = 18)
              ),
        checkboxInput(inputId = "add_legend",
              label = "Add legend",
              value = FALSE),
        NULL

    ),


      conditionalPanel(
        condition = "input.tabs=='About'",

        #Session counter: https://gist.github.com/trestletech/9926129
        h4("About"),  "There are currently",
        verbatimTextOutput("count"),
        "session(s) connected to this app.",
        hr(),
        h4("Find our other dataViz apps at:"),a("https://huygens.science.uva.nl/", href = "https://huygens.science.uva.nl/")
      ),

      conditionalPanel(
        condition = "input.tabs=='Data Summary'",
        h4("Data summary") ,
        # checkboxGroupInput("stats_select", label = h5("Statistics for replicates:"),
        #                    choices = list("mean", "sd", "sem","95CI mean", 'p(Shapiro-Wilk)', "median", "MAD", "IQR", "Q1", "Q3"),
        #                    selected = "sem"),
        # actionButton('select_all1','select all'),
        # actionButton('deselect_all1','deselect all'),
        numericInput("digits", "Digits:", 2, min = 0, max = 5),
        hr(),
        htmlOutput("legend", width="200px", inline =FALSE),


#        ,
#        selectInput("stats_hide2", "Select columns to hide", "", multiple = TRUE, choices=list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median")
        NULL)

    ),
    mainPanel(

       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"),
                  dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"),
                           downloadButton("downloadPlotSVG", "Download svg-file"),
                           # downloadButton("downloadPlotEPS", "Download eps-file"),
                           downloadButton("downloadPlotPNG", "Download png-file"),
                           actionButton("settings_copy", icon = icon("clone"),
                                         label = "Clone current setting"),
                           # actionButton("legend_copy", icon = icon("clone"),
                                        # label = "Copy Legend"),

                                        div(`data-spy`="affix", `data-offset-top`="10", plotOutput("coolplot", height="100%"),
                                            # htmlOutput("LegendText", width="200px", inline =FALSE),
#                                            htmlOutput("HTMLpreset"),
conditionalPanel(condition = "input.show_table == true", h3("Difference with the selected reference")), (tableOutput('toptable')))




                  ),
                  tabPanel("Data Summary",
                           h3("Table 1: Statistics for individual replicates"),dataTableOutput('data_summary'),
                           h3("Table 2: Statistics for conditions"),dataTableOutput('data_summary_condition') ,
                           h3("Table 3: Statistics for comparison of means between conditions"),dataTableOutput('data_difference'),
                           h3("Table 4: Statistics for repeatability"),dataTableOutput('data_repeats'),
                           NULL
                           ),
                  tabPanel("About", includeHTML("about.html")
                           )
        )
    )
  )
)


server <- function(input, output, session) {


  observe({
    showNotification("New feature: conditions can be (de)selected and ordered. Feedback or suggestions to improve the app are appreciated. For contact information, see the 'About' tab. ", duration = 10, type = "warning")
  })

  fraction_significant <- 0
  x_var.selected <- "Treatment"
  y_var.selected <- "Speed"
  g_var.selected <- "Replicate"


  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################

df_upload <- reactive({

    if (input$data_input == 2) {
      data <- df_tidy_example2
      x_var.selected <<- "Condition"
      y_var.selected <<- "BP"
      g_var.selected <<- "N"
    }  else if (input$data_input == 1) {
        data <- df_tidy_example
        x_var.selected <<- "Treatment"
        y_var.selected <<- "Speed"
        g_var.selected <<- "Replicate"


    } else if (input$data_input == 3) {
      x_var.selected <<- "none"
      y_var.selected <<- "none"
      g_var.selected <<- "-"
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Click 'Browse...' to select a datafile or drop file onto 'Browse' button"))
      # } else if (input$submit_datafile_button == 0) {
      #   return(data.frame(x = "Press 'submit datafile' button"))
      } else {

        filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
        fileext <- tolower(filename_split[length(filename_split)])
        # if (fileext=="xls" || fileext=="xlsx") {
        #   updateSelectInput(session, 'selectInput', selected = 'Excel')
        # }





        # isolate({

        #### Read Tidy Data #####
        if (input$toggle_tidy == FALSE) {


              if (fileext=="xls" || fileext=="xlsx") {
                  data <- readxl::read_excel(file_in$datapath)
              } else if (fileext == "txt" || fileext=="csv") {
                  data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A"), stringsAsFactors = TRUE)
              }

        #### Read wide Data and convert #####
        } else if (input$toggle_tidy == TRUE) {

            if (fileext == "txt" || fileext=="csv") {
                df <- read.csv(file=file_in$datapath, sep = input$upload_delim, header = FALSE, stringsAsFactors = FALSE)
            } else if (fileext=="xls" || fileext=="xlsx") {
                df <- readxl::read_excel(file_in$datapath, col_names = FALSE)
            }

            labels <- gsub("\\s","", strsplit(input$labels,",")[[1]])
            data <- tidy_df(df, n = input$n_conditions, labels = labels)
        }

      }
    } else if (input$data_input == 5) {
      x_var.selected <<- "none"
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"))
      } else if (url.exists(input$URL) == FALSE) {
         return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read.csv(file=input$URL, stringsAsFactors = TRUE)}

      #Read the data from textbox
    } else if (input$data_input == 4) {
      x_var.selected <<- "none"
      y_var.selected <<- "none"
      g_var.selected <<- "-"
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          # isolate({
            if (input$toggle_tidy == FALSE) {
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
            } else if (input$toggle_tidy == TRUE) {
              df <- read_delim(input$data_paste,
                                 delim = input$text_delim,
                                 col_names = FALSE)
              labels <- gsub("\\s","", strsplit(input$labels,",")[[1]])
              data <- tidy_df(df, n = input$n_conditions, labels = labels)
            }
          # })
        }
      }
    }


    #Replace space and dot of header names by underscore
    data <- data %>%
      select_all(~gsub("\\s+|\\.", "_", .))

    return(data)
})


##### REMOVE SELECTED COLUMNS #########
df_filtered <- reactive({

  if (!is.null(input$use_these_conditions) && input$x_var != "none") {

    x_var <- input$x_var
    use_these_conditions <- input$use_these_conditions

    observe({print(use_these_conditions)})

    #Remove the columns that are selected (using filter() with the exclamation mark preceding the condition)
    # https://dplyr.tidyverse.org/reference/filter.html
    df <- df_upload() %>% filter(.data[[x_var[[1]]]] %in% !!use_these_conditions)


  } else {df <- df_upload()}

})

##### CONVERT TO TIDY DATA ##########


##### Get Variables from the input ##############

observe({
        var_names  <- names(df_upload())
        varx_list <- c("none", var_names)

        # Get the names of columns that are factors. These can be used for coloring the data with discrete colors
        nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) ||
                                   is.logical(x) ||
                                   is.character(x),
                                 df_upload()))
        nms_var <- names(Filter(function(x) is.integer(x) ||
                                  is.numeric(x) ||
                                  is.double(x),
                                df_upload()))

        vary_list <- c("none",nms_var)

        facet_list <- c("-",var_names)

        # updateSelectInput(session, "colour_list", choices = nms_fact)
        updateSelectInput(session, "y_var", choices = vary_list, selected = y_var.selected)
        updateSelectInput(session, "x_var", choices = varx_list, selected = x_var.selected)
        updateSelectInput(session, "g_var", choices = facet_list, selected = g_var.selected)
        updateSelectInput(session, "h_facet", choices = facet_list)
        updateSelectInput(session, "v_facet", choices = facet_list)
        # updateSelectInput(session, "filter_column", choices = varx_list, selected="none")

 #       if (input$add_bar == TRUE) {
#          updateSelectInput(session, "alphaInput", min = 0.3)
#       }

    })


  ########### When x_var is selected for tidy data, get the list of conditions

  observeEvent(input$x_var != 'none' && input$y_var != 'none', {

    if (input$x_var != 'none') {

      filter_column <- input$x_var

      if (filter_column == "") {filter_column <- NULL}

      koos <- df_upload() %>% select(for_filtering = !!filter_column)

      conditions_list <- levels(factor(koos$for_filtering))
      # observe(print((conditions_list)))
      updateSelectInput(session, "use_these_conditions", choices = conditions_list)
    }

  })



###### When a bar is added, make sure that the data is still visible
observeEvent(input$paired, {
  if (input$paired==TRUE)  {
    # update jitter options
    updateRadioButtons(session, "jitter_type", choices = list("No offset"="no_jitter", "Distribution only"="violin"))
    # update pairing options
    updateRadioButtons(session, "connect", choices = list("Dotted line" = "dotted", "Dashed line"= "dashed", "Solid line" ="solid"))

  } else if (input$paired==FALSE)  {
    updateRadioButtons(session, "jitter_type", choices = list("Data & distribution" = "quasirandom", "Jittered data" = "random","No offset"="no_jitter", "Distribution only"="violin"))
    # update pairing options
    updateRadioButtons(session, "connect", choices = list("No" = "blank", "Dotted line" = "dotted", "Dashed line"= "dashed", "Solid line" ="solid"))
  }
})

observeEvent(input$tabs, {
  if (input$tabs=='Data Summary')  {
    df <- df_summ_per_replica()
    # Get the list of p-values
    p <- (df[,10]) %>% unlist(use.names = F)
    # Determine the fraction of p-values that is significant, based on a threshold of 0.05
    fraction_significant <<- length(which(p<0.05))/length(p)
    if (fraction_significant>0.5 && input$summary_replicate =='mean') {

    showNotification("The majority of replicates has a distribution that deviates from normality, as inferred from a p< 0.05 from a Shapiro-wilk test. Consider using the median as a measure of location for the replicates", duration = 10, type = "error")
    }
  }
})


observeEvent(input$connect, {
  if (input$connect!='blank')  {
    showNotification("Connecting or 'pairing' the data changes the p-value and the 95% confidence interval for the difference", duration = 10, type = "message")
  }
})

observeEvent(input$summary_replicate, {
  if (input$summary_replicate=="median")  {
    showNotification("Selecting the median as the measure of location for the replicates does not change the p-value and the difference, as these are calculated from the mean values", duration = 10, type = "message")
  }
})

########### GET INPUT VARIABLEs FROM HTML ##############

observe({



  query <- parseQueryString(session$clientData$url_search)

  ############ ?data ################

  if (!is.null(query[['data']])) {
    presets_data <- query[['data']]
    presets_data <- unlist(strsplit(presets_data,";"))
    observe(print((presets_data)))

    updateRadioButtons(session, "data_input", selected = presets_data[1])
    # updateCheckboxInput(session, "tidyInput", value = presets_data[2])

    # updateSelectInput(session, "x_var", selected = presets_data[3])
    # updateSelectInput(session, "y_var", selected = presets_data[4])
    # updateSelectInput(session, "g_var", selected = presets_data[5])

    x_var.selected <<- presets_data[3]
    y_var.selected <<- presets_data[4]
    g_var.selected <<- presets_data[5]

    if (presets_data[1] == "1" || presets_data[1] == "2") {
      updateTabsetPanel(session, "tabs", selected = "Plot")
    }
  }






  ############ ?vis ################

  if (!is.null(query[['vis']])) {

  presets_vis <- query[['vis']]
  presets_vis <- unlist(strsplit(presets_vis,";"))
  observe(print((presets_vis)))

  #radio, slider, radio, check, slider
  updateRadioButtons(session, "jitter_type", selected = presets_vis[1])
  updateCheckboxInput(session, "show_distribution", value = presets_vis[2])
  updateSliderInput(session, "alphaInput", value = presets_vis[3])
  updateRadioButtons(session, "summary_replicate", selected = presets_vis[4])
  updateCheckboxInput(session, "connect", value = presets_vis[5])
  updateCheckboxInput(session, "show_table", value = presets_vis[6])
  updateCheckboxInput(session, "add_shape", value = presets_vis[7])

  updateSliderInput(session, "alphaInput_summ", value = presets_vis[8])
  updateRadioButtons(session, "ordered", selected = presets_vis[9])

  #For backward compatibility with links in the paper
  if (length(presets_vis)<10) {dotsize <- 3.5} else {dotsize <- presets_vis[10]}
  updateNumericInput(session, "dot_size", value= dotsize)
  #select zero


  updateRadioButtons(session, "summary_condition", selected = presets_vis[11])
#  updateTabsetPanel(session, "tabs", selected = "Plot")
  }


  ############ ?layout ################

  if (!is.null(query[['layout']])) {

    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    observe(print((presets_layout)))

    updateSelectInput(session, "split_direction", selected = presets_layout[1])

    updateCheckboxInput(session, "rotate_plot", value = presets_layout[2])
    updateCheckboxInput(session, "no_grid", value = (presets_layout[3]))

    updateCheckboxInput(session, "change_scale", value = presets_layout[4])
    updateCheckboxInput(session, "scale_log_10", value = presets_layout[5])
     updateTextInput(session, "range", value= presets_layout[6])
     # updateCheckboxInput(session, "color_data", value = presets_layout[6])
     updateCheckboxInput(session, "dark", value = presets_layout[7])
     updateRadioButtons(session, "adjustcolors", selected = presets_layout[8])
     updateCheckboxInput(session, "add_legend", value = presets_layout[9])
     if (length(presets_layout)>10) {
       updateNumericInput(session, "plot_height", value= presets_layout[10])
       updateNumericInput(session, "plot_width", value= presets_layout[11])
     }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }

  ############ ?color ################

  if (!is.null(query[['color']])) {

    presets_color <- query[['color']]
    observe(print((presets_color)))
    presets_color <- unlist(strsplit(presets_color,";"))

    color_list <- gsub("_", "#", presets_color[2])

    observe(print((color_list)))


    # updateSelectInput(session, "colour_list", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= color_list)
  }

    ############ ?label ################

  if (!is.null(query[['label']])) {

    presets_label <- query[['label']]
    presets_label <- unlist(strsplit(presets_label,";"))
    observe(print((presets_label)))


    updateCheckboxInput(session, "add_title", value = presets_label[1])
    updateTextInput(session, "title", value= presets_label[2])

    updateCheckboxInput(session, "label_axes", value = presets_label[3])
    updateTextInput(session, "lab_x", value= presets_label[4])
    updateTextInput(session, "lab_y", value= presets_label[5])

    updateCheckboxInput(session, "adj_fnt_sz", value = presets_label[6])
    updateNumericInput(session, "fnt_sz_ttl", value= presets_label[7])
    updateNumericInput(session, "fnt_sz_labs", value= presets_label[8])
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[9])
    # updateNumericInput(session, "fnt_sz_cand", value= presets_label[10])
    updateCheckboxInput(session, "add_legend", value = presets_label[11])
    }

  ############ ?url ################

  if (!is.null(query[['url']])) {
    updateRadioButtons(session, "data_input", selected = 5)
    updateTextInput(session, "URL", value= query[['url']])
    observe(print((query[['url']])))
    updateTabsetPanel(session, "tabs", selected = "Plot")
  }
})

########### RENDER URL ##############

output$HTMLpreset <- renderText({
  url()
  })

######### GENERATE URL with the settings #########

url <- reactive({

  base_URL <- paste(sep = "", session$clientData$url_protocol, "//",session$clientData$url_hostname, ":",session$clientData$url_port, session$clientData$url_pathname)

  # data <- c(input$data_input, "", input$x_var, input$y_var, input$h_facet, input$v_facet)
  data <- c(input$data_input, "", input$x_var, input$y_var, input$g_var)


  vis <- c(input$jitter_type, input$show_distribution, input$alphaInput, input$summary_replicate, input$connect, input$show_table, input$add_shape, input$alphaInput_summ, input$ordered, input$dot_size, input$summary_condition)
  layout <- c(input$split_direction, input$rotate_plot, input$no_grid, input$change_scale, input$scale_log_10, input$range, input$dark,
              input$adjustcolors, input$add_legend, input$plot_height, input$plot_width)

  #Hide the standard list of colors if it is'nt used
   if (input$adjustcolors != "5") {
     color <- c("x", "none")
   } else if (input$adjustcolors == "5") {



     stripped <- gsub("#", "_", input$user_color_list)

     color <- c("X", stripped)
   }

  # label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_ttl, input$fnt_sz_ax, input$add_legend)

  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_title, input$fnt_sz_labs, input$fnt_sz_ax, "", input$add_legend)



  #replace FALSE by "" and convert to string with ; as seperator
  data <- sub("FALSE", "", data)
  data <- paste(data, collapse=";")
  data <- paste0("data=", data)

  vis <- sub("FALSE", "", vis)
  vis <- paste(vis, collapse=";")
  vis <- paste0("vis=", vis)

  layout <- sub("FALSE", "", layout)
  layout <- paste(layout, collapse=";")
  layout <- paste0("layout=", layout)

  color <- sub("FALSE", "", color)
  color <- paste(color, collapse=";")
  color <- paste0("color=", color)

  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label)

  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}

  parameters <- paste(data, vis,layout,color,label,url, sep="&")

  preset_URL <- paste(base_URL, parameters, sep="?")

 observe(print(parameters))
 observe(print(preset_URL))
 return(preset_URL)
  })

############# Pop-up that displays the URL to 'clone' the current settings ################

observeEvent(input$settings_copy , {
  showModal(urlModal(url=url(), title = "Use the URL to launch SuperPlotsOfData with the current setting"))
})

observeEvent(input$legend_copy , {
  showModal(urlModal(url=Fig_legend(), title = "Legend text"))
})


######## ORDER the Conditions #######

df_sorted <- reactive({

  klaas <-  df_selected()

   if(input$ordered == "median") {
     klaas$Condition <- reorder(klaas$Condition, klaas$Value, median, na.rm = TRUE)

   } else if (input$ordered == "none") {

    if (!is.null(input$use_these_conditions)) {
      # Set order based on input
     klaas$Condition <- factor(klaas$Condition, levels = input$use_these_conditions)}
     else {
      klaas$Condition <- factor(klaas$Condition, levels=unique(klaas$Condition))
     }

   } else if (input$ordered == "alphabet") {
     klaas$Condition <- factor(klaas$Condition, levels=unique(sort(klaas$Condition)))
   }

    return(klaas)

})

######## Extract the data for display & summary stats #######

df_selected <- reactive({

    df_temp <- df_filtered()
    x_choice <- input$x_var
    y_choice <- input$y_var
    g_choice <- input$g_var


    #Prevent error if y parameter is not selected
    if (input$y_var =='none') {
      koos <- df_temp %>% dplyr::select(Condition = !!x_choice)
      koos$Value <- 1
      koos$Replica <- as.factor("1")
      return(koos)
    }

    if (g_choice == "-") {
      if (input$x_var =='none') {
        koos <- df_temp %>% dplyr::select(Value = !!y_choice) %>% filter(!is.na(Value))
        koos$Replica <- as.factor("1")
        koos$Condition <- as.factor("1")
      } else {
        koos <- df_temp %>% dplyr::select(Condition = !!x_choice , Value = !!y_choice) %>% filter(!is.na(Value))
        koos$Replica <- as.factor("1")
      }
    } else {
      #Prevent error if x parameter is not selected
      if (input$x_var =='none') {
        koos <- df_temp %>% dplyr::select(Value = !!y_choice, Replica = !!g_choice) %>% filter(!is.na(Value))
        koos$Condition <- as.factor("1")
        return(koos)
      } else {
      koos <- df_temp %>% dplyr::select(Condition = !!x_choice , Value = !!y_choice, Replica = !!g_choice) %>% filter(!is.na(Value))
      }
    }


    #Convert Condition and Replica into factors
    koos <- koos %>% mutate_at(vars(Condition, Replica), list(factor))

    return(koos)
})

########### When x_var is selected for tidy data, get the list of conditions

observeEvent(input$x_var != 'none', {

  if (input$x_var != 'none') {

    koos <- df_sorted()
    conditions_list <- as.factor(koos$Condition)
    # observe(print((conditions_list)))
    updateSelectInput(session, "zero", choices = conditions_list)
  }
})



#### DISPLAY UPLOADED DATA (as provided) ##################

output$data_uploaded <- renderDataTable(

#    observe({ print(input$tidyInput) })
  df_filtered(),
  rownames = FALSE,
  options = list(pageLength = 10,
                  lengthMenu = c(10, 100, 1000, 10000), columnDefs = list(list(className = 'dt-left', targets = '_all'))),
  editable = FALSE,selection = 'none'
)


########### Caluclate summary stats for each REPLICATE ############

df_summ_per_replica <- reactive({

  koos <- df_sorted() %>%
  # koos <- df_selected() %>%
    group_by(Condition, Replica) %>%
    dplyr::summarise(n = n(),
            mean = mean(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            median= median(Value, na.rm = TRUE),
            'p(Shapiro-Wilk)' = f(Value)) %>%
      mutate(sem = sd / sqrt(n),
             '95%CI_lo' = mean - qt((1+Confidence_level)/2, n - 1) * sem,
             '95%CI_hi' = mean + qt((1+Confidence_level)/2, n - 1) * sem)



   # koos <- koos %>% select(Replica,n,mean,sd,sem,'95%CI_lo','95%CI_hi',median,'p(Shapiro-Wilk)') %>% mutate_at(c(3:9), round, input$digits) %>% mutate_at(10,round,3)
})

df_summ_per_replica_rounded <- reactive({

  koos <- df_summ_per_replica() %>% select(Replica,n,mean,sd,sem,'95%CI_lo','95%CI_hi',median,'p(Shapiro-Wilk)') %>% mutate_at(c(3:9), round, input$digits) %>% mutate_at(10,round,3)

})


######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("SuperPlotsOfData_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotSVG <- downloadHandler(
  filename <- function() {
    paste("SuperPlotsOfData_", Sys.time(), ".svg", sep = "")
  },
  content <- function(file) {
    svg(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/svg" # MIME type of the image
)

output$downloadPlotEPS <- downloadHandler(
  filename <- function() {
    paste("SuperPlotsOfData_", Sys.time(), ".eps", sep = "")
  },
  content <- function(file) {
    cairo_ps(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()

  },
  contentType = "application/eps" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("SuperPlotsOfData_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
)



######## PREPARE PLOT FOR DISPLAY ##########


plotdata <- reactive({


####### Read the order from the ordered dataframe #############
    koos <- df_sorted()

    stats <- sym(as.character(input$summary_replicate))

#   observe({ print(koos) })

    custom_order <-  levels(factor(koos$Condition))
#    custom_labels <- levels(factor(koos$label))

    if (input$dark) {line_color="grey80"} else {line_color="gray20"}

  ########## Define alternative color palettes ##########

     # observe({ print(head(df_selected())) })

    newColors <- NULL
    if (input$adjustcolors == 3) {
      newColors <- Tol_bright
    } else if (input$adjustcolors == 4) {
      newColors <- Tol_light
    } else if (input$adjustcolors == 6) {
      Okabe_Ito[8] <- line_color
      newColors <- Okabe_Ito
    } else if (input$adjustcolors == 5) {
      newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
    }




        kleur <- 'Replica'

        if (input$add_shape) {
          vorm <- sym('Replica')
        } else {vorm <- NULL}



    klaas <- df_selected()
    klaas <- as.data.frame(klaas)

    #### Used to convert integers to factors, compatible with a discrete color scale
    klaas[,kleur] <- as.factor(klaas[,kleur])

    #Determine the number of colors that are necessary
    max_colors <- nlevels(as.factor(klaas[,kleur]))


        #If unsufficient colors available, repeat
        if(length(newColors) < max_colors) {
          newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
        }



############## GENERATE PLOT LAYERS #############

    #########################  For continuous x-axis variables
    if (input$x_cont) {
      klaas$Condition <- as.numeric(as.character(klaas$Condition))
    }


    #########################
    p <- ggplot(data=klaas, aes(x=Condition))

    if (!input$x_cont) {
      # Setting the order of the x-axis
      p <- p + scale_x_discrete(limits=custom_order)
    }

    data_width = 0.4

    if (input$show_distribution) data_width=data_width/2
  ##### plot selected data summary (bottom layer) ####

   #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "quasirandom") {
      ##### This is the way to do it, as aes_string() is deprecated. #########
      p <- p + geom_quasirandom(data=klaas, aes(x=Condition, y=Value, color = Replica, shape = !!vorm, fill = Replica), width=data_width, cex=input$dot_size, alpha=input$alphaInput, groupOnX=TRUE)
      #p <- p + geom_quasirandom(data=klaas, aes_string(x='Condition', y='Value', color = kleur, shape = vorm, fill = kleur), width=data_width, cex=input$dot_size, alpha=input$alphaInput, groupOnX=TRUE)
    } else if (input$jitter_type == "random") {

      ##### This is the way to do it, as aes_string() is deprecated. #########
      p <- p + geom_jitter(data=klaas, aes(x=Condition, y=Value, color = Replica, shape = !!vorm, fill = Replica), width=data_width*0.8, height=0.0, cex=input$dot_size, alpha=input$alphaInput)
      # p <- p + geom_jitter(data=klaas, aes_string(x='Condition', y='Value', color = kleur, shape = vorm, fill = kleur), width=data_width*0.8, height=0.0, cex=input$dot_size, alpha=input$alphaInput)
    } else if (input$jitter_type == "no_jitter") {
      p <- p + geom_jitter(data=klaas, aes(x=Condition, y=Value, color = Replica, shape = !!vorm, fill = Replica), width=0, height=0.0, cex=input$dot_size, alpha=input$alphaInput)
    } else if (input$jitter_type == "violin") {
      p <- p + geom_violin(data=klaas, aes(x=Condition, y=Value, group=Condition),width=data_width*2, fill='grey50', color=NA, alpha=input$alphaInput)
    }

    #Add lines when all data is paired
    if (input$paired == TRUE && input$jitter_type != "violin") {
      #Need to add another column that defines pairing
      klaas <- klaas %>% group_by(Condition) %>% mutate (id=row_number()) %>% ungroup()
      p <-  p + geom_line(data=klaas, aes(x=Condition, y=Value, color= Replica, group = id), linewidth = .2, linetype=input$connect, alpha=input$alphaInput)
    }

    if (input$summary_condition=="mean_SD" && input$split_direction=="No") {
      p <-  p + geom_errorbar(data = df_summary_condition(), aes(x=Condition, ymin=mean, ymax=mean), width=data_width*1.2, color=line_color, size=2, alpha=input$alphaInput_summ)
      p <-  p + geom_errorbar(data = df_summary_condition(), aes(x=Condition, ymin=mean-sd, ymax=mean+sd), width=data_width*0.8, color=line_color, size=2, alpha=input$alphaInput_summ)
    } else if (input$summary_condition=="mean_SD" && input$split_direction!="No") {
      p <- p + stat_summary(data=klaas, aes(x=Condition, y=Value, group = Replica),
                            fun.data = add_SD,
                            geom = "errorbar", width=data_width*1.2, color=line_color, size=2, alpha=input$alphaInput_summ)
    }

    if (input$summary_condition=="mean_CI" && input$split_direction=="No") {
      p <-  p + geom_errorbar(data = df_summary_condition(), aes(x=Condition, ymin=mean, ymax=mean), width=data_width*1.2, color=line_color, size=2, alpha=input$alphaInput_summ)
      p <-  p + geom_errorbar(data = df_summary_condition(), aes(x=Condition, ymin=`95%CI_lo`, ymax=`95%CI_hi`), width=data_width*0.8, color=line_color, size=2, alpha=input$alphaInput_summ)
    } else if (input$summary_condition=="mean_CI" && input$split_direction!="No") {

      p <- p + stat_summary(data=klaas, aes(x=Condition, y=Value, group = Replica),
                            fun.data = add_CI,
                            geom = "errorbar", width=data_width*1.2, color=line_color, size=2, alpha=input$alphaInput_summ)
    }

    if (input$summary_condition=="mean_sem" && input$split_direction=="No") {
      p <-  p + geom_errorbar(data = df_summary_condition(), aes(x=Condition, ymin=mean, ymax=mean), width=data_width*1.2, color=line_color, size=2, alpha=input$alphaInput_summ)
      p <-  p + geom_errorbar(data = df_summary_condition(), aes(x=Condition, ymin=mean-sem, ymax=mean+sem), width=data_width*0.8, color=line_color, size=2, alpha=input$alphaInput_summ)
    } else if (input$summary_condition=="mean_sem" && input$split_direction!="No") {
      p <- p + stat_summary(data=klaas, aes(x=Condition, y=Value, group = Replica),
                            fun.data = add_sem,
                            geom = "errorbar", width=data_width*1.2, color=line_color, size=2, alpha=input$alphaInput_summ)
    }

    #Add line to depict paired replicates
      p <-  p + geom_line(data=df_summ_per_replica(), aes(x=Condition, y=!!stats, group = Replica, color=Replica), linewidth = 1, linetype=input$connect)

    #Distinguish replicates by symbol
    # if (input$add_shape)
    #   p <-  p + stat_summary(data=klaas, aes_string(x='Condition', y='Value', group = 'Replica', fill = kleur, shape = vorm), color=line_color, fun = stats, geom = "point", stroke = 1, size = 8)
    # if (!input$add_shape)
    #   p <-  p + stat_summary(data=klaas, aes_string(x='Condition', y='Value', group = 'Replica', fill = kleur), color=line_color, shape=21, fun = stats, geom = "point", stroke = 1, size = 8)

      #Distinguish replicates by symbol
      if (input$add_shape)
        p <-  p + geom_point(data=df_summ_per_replica(), aes(x=Condition, y=.data[[input$summary_replicate]], group = Replica, fill = Replica, shape = !!vorm), alpha=input$alphaStats, color=line_color, stroke = 1, size = 8)
       if (!input$add_shape) {

         if (!input$add_n)
           # p <-  p + geom_point(data=df_summ_per_replica(), aes_string(x='Condition', y=stats, group = 'Replica', fill = kleur), alpha=input$alphaStats, color=line_color, shape=21, stroke = 1, size = 8)
          p <-  p + geom_point(data=df_summ_per_replica(), aes(x=Condition, y=.data[[input$summary_replicate]], group = Replica, fill = Replica), alpha=input$alphaStats, color=line_color, shape=21, stroke = 1, size = 8)



         if (input$add_n)
          p <-  p + geom_point(data=df_summ_per_replica(), aes(x=Condition, y=.data[[input$summary_replicate]], group = Replica, fill = Replica, size=n), alpha=input$alphaStats, color=line_color, shape=21, stroke = 1)
         p <- p + scale_size_area(max_size = 8)
       }

    #Show distribution for each replicate
    if  (input$show_distribution) {
      p <- p + geom_flat_violin(data=klaas, aes(x=Condition, y=Value,  fill=Replica),color=NA,scale = "width", width=0.7,position = position_nudge(x = .22, y = 0), trim=FALSE, alpha = 0.75*input$alphaInput)
    }

########### Do some formatting of the lay-out ###########

    p <- p+ theme_light(base_size = 16)
    if (input$dark) {p <- p+ theme_light_dark_bg(base_size = 16)}


     # if log-scale checked specified
     if (input$scale_log_10)
       p <- p + scale_y_log10()

    #Adjust scale if range (min,max) is specified
    if (input$range != "" &&  input$change_scale == TRUE) {
         rng <- as.numeric(strsplit(input$range,",")[[1]])

         #If min>max invert the axis
         if (rng[1]>rng[2]) {p <- p+ scale_y_reverse()}

    #Autoscale if rangeis NOT specified
     } else if (input$range == "" || input$change_scale == FALSE) {
       rng <- c(NULL,NULL)
     }

     p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
      #### If selected, rotate plot 90 degrees CW ####
     if (input$rotate_plot == TRUE) { p <- p + coord_flip(ylim=c(rng[1],rng[2]))}

    # if title specified
    if (input$add_title)
      p <- p + ggtitle(input$title)

     # if labels specified
    if (input$label_axes) {

      x_label = input$lab_x
      y_label = input$lab_y

     } else {
       # if labels not specified, use label from input
         y_label <- paste(input$y_var)
         x_label <- paste(input$x_var)
       }

    p <- p + labs(x = x_label, y = y_label)

     # # if font size is adjusted
     if (input$adj_fnt_sz) {
       p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
       p <- p + theme(axis.title = element_text(size=input$fnt_sz_labs))
       p <- p + theme(plot.title = element_text(size=input$fnt_sz_title))
     }


    # #remove legend (if selected)
    if (input$add_legend == FALSE) {
      p <- p + theme(legend.position="none")
    }

    #Remove strips with labels of facets when legend is present
    if (input$add_legend == TRUE && input$split_direction != "No") {
      p <- p + theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )
    }


     #remove gridlines (if selected)
     if (input$no_grid == TRUE) {
       p <- p+ theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
     }


   if (input$adjustcolors == 1) {

     p <- p + scale_fill_grey(start=0.3, end=0.7)
     p <- p + scale_color_grey(start=0.3, end=0.7)
   }

    if (input$adjustcolors == 2) {

      p <- p + scale_fill_viridis_d(begin=0.3, end=0.7)
      p <- p + scale_color_viridis_d(begin=0.3, end=0.7)
    }


    if (input$adjustcolors > 2) {
    #Adjust colors
    p <- p+ scale_color_manual(values=newColors)
    p <- p+ scale_fill_manual(values=newColors)
    }


       # if (input$add_shape) {
         p <- p + scale_shape_manual(values = c(21:25))
       # }


     if (input$split_direction =="Horizontal") {

       p <- p+ facet_grid(.~Replica)
     } else if (input$split_direction =="Vertical") {
       p <- p+ facet_grid(Replica~.)
     }


    ### Output the plot ######
    return(p)

  }) #close plotdata


##### Render the plot ############

output$coolplot <- renderPlot(width = width, height = height, {
  plot(plotdata())
}
)


#### Export the data in tidy format ###########

output$downloadData <- downloadHandler(
  filename = function() {
    paste("PlotsOfData_Tidy", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_selected(), file, row.names = FALSE)
  }
)


#### Combine the statistics in one table and filter ###########

df_filtered_stats <- reactive({

  digits <- as.numeric(input$digits)

  #Combine the numbers from the 95% CI for the mean to show the interval
  klaas <- df_summ_per_replica()
  # %>% mutate(mean_CI_lo = round(mean_CI_lo, digits), mean_CI_hi = round(mean_CI_hi, digits)) %>% unite("95CI mean", c("mean_CI_lo","mean_CI_hi"), sep=" , ")


    # Round down to the number of selected digits
    # klaas <- klaas %>% mutate_at(c(3:5, 7:11), round, input$digits)
    klaas <- klaas %>% mutate_at(c(4:5, 7:14), round, input$digits) %>% mutate_at(c(6), round, 4)
    # observe({ print((klaas)) })

  ##### Show the statistics selected by the user ############
  if (!is.null(input$stats_select)) {
    columns = input$stats_select
    if ('95CI mean' %in% columns) {columns <- c(columns, '95%CI_lo', '95%CI_hi')}
    columns <- c("Condition", "n", columns)
    df <- klaas %>% select(one_of(columns))
  } else if (is.null(input$stats_select)) {
    df <- klaas %>% select("Condition", "n")}
})


df_difference <- reactive({

  df <- df_selected()

  ## When multiple replicates are present, use the df with summaries per replicate as input for calculation of differences
  if (length(unique(df$Replica)) > 1) {
    if (input$summary_replicate =="median")  {
      df <- df_summ_per_replica() %>% dplyr::rename(Value=mean) #Even when median is selected as summary, the mean is used for the t-test
    } else if (input$summary_replicate =="mean") {
      df <- df_summ_per_replica() %>% dplyr::rename(Value=mean)}
  }

  # Select only the relevant data from the dataframe
  df <- df %>% dplyr::select(Condition, Value, Replica)

  # If no replicates are defined, treat each samples as a replicate. This will ensure proper behaviour in the t-test
  if (length(unique(df$Replica)) == 1) {
    df <- df %>% group_by(Condition) %>% mutate(Replica = row_number(Condition))
  }

  #Makes every condition the same length by filling up with NA. This simplifies testing/comparison
  # df <- df %>% spread(Condition, Value) %>% gather(Condition, Value, -Replica, na.rm = FALSE)

  df <- df %>% pivot_wider(names_from = "Condition", values_from = "Value") %>% pivot_longer(!Replica, names_to="Condition", values_to = "Value", values_drop_na = FALSE)


  # select the control condition for comparisons
  control_condition <- input$zero

  #Get the reference values
  df_controls <- df %>% filter(Condition==!!control_condition)
  #Rename the column names for the control dataframe
  df_controls <- df_controls %>% select(Replica, control_value = Value, cond = Condition)

  if(length(na.omit(df_controls$control_value))<3) {
    return(df_difference <- data.frame('Error'='n<3 for the control condition'))
  }

  #Remove the Reference from the dataframe and add the reference values to a new column
  df_diff <- df %>%
    filter (Condition != !!control_condition) %>%
    select(Condition,Replica,Value) %>% full_join(df_controls, by='Replica') %>% unite('Condition' ,c("cond","Condition"), sep = " vs ")

  # df_diff$Condition <- paste(df_diff$Condition, "vs", control_condition)

  observe({print(df_diff)})
  if (input$connect !='blank') {connect = TRUE} else {connect=FALSE}
  # Generate a dataframe that summarizes the differences between the control condition and others.
  df_difference <- df_diff  %>%
    group_by(Condition) %>% do(tidy(t.test(.$Value, .$control_value, paired = connect)))

  # observe({print(df_difference)})

  df_difference <- df_difference  %>% dplyr::select(Condition, difference=estimate, `95%CI_lo`=conf.low, `95%CI_hi`=conf.high,p.value)

  df_difference <- df_difference %>% mutate_at(c(2:4), round, input$digits)  %>% mutate_at(c(5), round, 8)

  #Use scientific notation if smaller than 0.001
  # Needs fixing, since doesn't work for multiple conditions
  # if (df_difference$p.value<0.001 && df_difference$p.value>=1e-10) {
  # df_difference$p.value  <- formatC(df_difference$p.value, format = "e", digits = 2)
  # #Any p-value lower than 1e-10 is noted as <1e-10
  # } else if (df_difference$p.value<1e-10) {
  #   df_difference$p.value <- "<1e-10"
  # }

  return(df_difference)

})

df_summary_condition <- reactive({

  df <- df_selected()

  ## When multiple replicates are present, use the df with summaries per replicate as input for calculation of differences
  if (length(unique(df$Replica)) > 1) {
    if (input$summary_replicate =="median")  {
      df <- df_summ_per_replica() %>% dplyr::rename(Value=median)
    } else if (input$summary_replicate =="mean") {
      df <- df_summ_per_replica() %>% dplyr::rename(Value=mean)}
  }

  df <- df %>% group_by(Condition) %>% dplyr::summarise(n = n(),
                                                 mean = mean(Value),
                                                 sd = sd(Value))  %>%
    mutate(sem = sd / sqrt(n),
           `95%CI_lo` = mean - qt((1+Confidence_level)/2, n - 1) * sem,
           `95%CI_hi` = mean + qt((1+Confidence_level)/2, n - 1) * sem,
           NULL)


  # ## Need to add the repeats for each subject
  # df_id <- df_selected() %>% group_by(Condition, Replica) %>% mutate(replicates=row_number()) %>% ungroup()
  # ## Calculate measures of repeatability
  # df_repeatability <- df_id %>% repeatability(values=Value, replicates=replicates , groups=Condition)
  # df_repeatability <- df_repeatability %>%
  #   select(-c(Replica, replicates, TSS, SSw, SSb, MSw, MSb))
  #
  # observe({print(head(df_repeatability))})


  return(df)
})

df_summary_condition_rounded <- reactive({
  df <- df_summary_condition() %>% mutate_at(c(3:7), round, input$digits)
})

df_repeats_rounded <- reactive({

  ## Need to add the repeats for each subject
  df <- df_selected() %>% group_by(Condition, Replica) %>% dplyr::mutate(replicates=row_number()) %>% ungroup()
  # df <- df_selected()

  ## Calculate paramaters
  df <- df %>% repeatability(values=Value, replicates=replicates , groups=Condition) %>%
     dplyr::select(-c(Replica, replicates, TSS, SSw, SSb, MSw, MSb))

  df <- df %>% mutate_at(c(4:9), round, input$digits)

})

#### A predefined selection of stats for the table  ###########

observeEvent(input$summary_replicate, {
  if (input$summary_replicate=="mean")  {
    updateSelectInput(session, "stats_select", selected = list("mean", "sd", '95CI mean', 'p(Shapiro-Wilk)'))
  }
  else if (input$summary_replicate=="median")  {
    updateSelectInput(session, "stats_select", selected = list("median", "MAD"))
  }
})

observeEvent(input$select_all1, {
  updateSelectInput(session, "stats_select", selected = list("mean", "sd", "sem", '95CI mean','p(Shapiro-Wilk)', "median", "MAD", "IQR", "Q1", "Q3"))
  })

observeEvent(input$deselect_all1, {
  updateSelectInput(session, "stats_select", selected = "")
})


#### Render the data summary as a table ###########

output$data_summary <- renderDataTable(
 datatable(
   df_summ_per_replica_rounded(),
#  colnames = c(ID = 1),
  selection = 'none',
  extensions = c('Buttons', 'ColReorder'),
  options = list(dom = 'Bfrtip', pageLength = 100,
             buttons = c('copy', 'csv','excel', 'pdf'),
    editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    )
  )
#   %>% formatRound(n, digits=0)
)


#### Render the data summary as a table ###########

output$data_summary_condition <- renderDataTable(
  datatable(
    df_summary_condition_rounded(),
    #  colnames = c(ID = 1),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip', pageLength = 100,
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    )
  )
)

#### Render the data summary as a table ###########

output$data_difference <- renderDataTable(
  datatable(
    df_difference(),
    #  colnames = c(ID = 1),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip', pageLength = 100,
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    )
  )
)

#### Render the data summary as a table ###########

output$data_repeats <- renderDataTable(
  datatable(
    df_repeats_rounded(),
    #  colnames = c(ID = 1),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip', pageLength = 100,
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    )
  )
)


############## Render the data summary as a table ###########

output$toptable <- renderTable({

  if (input$show_table == F) return(NULL)
  df <- as.data.frame(df_difference())

})

output$legend <- renderText({

  df <- df_difference()

  HTML_Legend <- c('<h4>Explanation of the statistics</h4>')
  HTML_Legend <- append(HTML_Legend, paste('<p><u>Table 1</u>: Summary of the statistics for each of the replicates. A high p-value for the Shapiro-Wilk test for normality suggests that the data distribution is normal.</p>', sep=""))

   if (fraction_significant>0.5 && input$summary_replicate == 'mean') {
     HTML_Legend <- append(HTML_Legend, paste('<p>Since the majority of the replicates shows a low p-value, consider using the <b>median</b> instead of the mean as a summary of the replicates.</p>', sep=""))
   }
    HTML_Legend <- append(HTML_Legend, paste('<p><u>Table 2</u>: Summary of the statistics for each condition which is calculated from the <b>',input$summary_replicate,'</b> of the replicates.</p>', sep=""))


    HTML_Legend <- append(HTML_Legend, paste('<p><u>Table 3</u>: Statistics for the comparison of the conditions to "',input$zero,'" based on the <b>mean</b> of the replicates.</br>', sep=""))


    HTML_Legend <- append(HTML_Legend, paste('The difference is a point estimate of the size of the effect and the 95% confidence interval is an interval estimate. ', sep=""))


    if (input$connect!='blank') {
      HTML_Legend <- append(HTML_Legend, paste('The replicates are paired between conditions and a paired t-test is used to calculate the p-value. ', sep=""))
    } else if (input$connect=='blank') {
      HTML_Legend <- append(HTML_Legend, paste("The replicates are <b>not</b> paired and Welch's t-test is performed to calculate the p-value. ", sep=""))
    }

    HTML_Legend <- append(HTML_Legend, paste('<p><u>Table 4</u>: Statistics for the repeatability for each of the conditions. For explanation of the parameters see <a href="https://doi.org/10.1593/tlo.09268">Barnhart & Barboriak, 2009</a></br></p>', sep=""))


    if (length(df$Condition)>1) {
      HTML_Legend <- append(HTML_Legend, paste("</br>The p-values are <b>not corrected</b> for multiple comparisons. Consider alternative statistical analyses.", sep=""))

    }
    return(HTML_Legend)


})






      ########### Update count #########
      # Reactively update the client.
      output$count <- renderText({
        vals$count
      })


    # When a session ends, decrement the counter.
    session$onSessionEnded(function(){

      isolate(vals$count <- vals$count - 1)
      # End R-session when browser closed
#      stopApp()
    })


######## The End; close server ########################

} #close "server"

shinyApp(ui = ui, server = server)
