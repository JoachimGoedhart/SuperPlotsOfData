# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SuperPlotsOfData: Shiny app for plotting and comparing data from different replicas
# Created by Joachim Goedhart (@joachimgoedhart), first version 2020
# Uses tidy data as input with a column that defines conditions and a column with measured values
# A third column can be selected that indicates the replicas (as numbers or other unique strings)
# Raw data is displayed with user-defined visibility (alpha)
# Summary statistics are displayed with user-defined visibility (alpha)
# A plot and a table with stats are generated
# Several colorblind safe palettes are available
# Ordering of the categorial data is 'as is, based on median or alphabetical
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

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)
library(readxl)
library(DT)
library(RCurl)
library(broom)

#Uncomment for sinaplot
#library(ggforce)

source("geom_flat_violin.R")

###### Functions ##########


# Function that returns the p-value from Shapiro-Wilk test
f = function(x){
  if (length(x)<3) {return(NA)}
  st = shapiro.test(x)
  return(st$p.value)
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

#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Read a text file (comma separated values)
df_tidy_example <- read.csv("combined.csv", na.strings = "")
df_tidy_example2 <- read.csv("another_example.csv", na.strings = "")

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

###### UI: User interface #########

ui <- fluidPage(
  
  titlePanel("SuperPlotsOfData - Plots Data and its Replicas"),
  sidebarLayout(
    sidebarPanel(width=3,
                 
                 conditionalPanel(
                   condition = "input.tabs=='Data upload'",
                   h4("Data upload"),
                   radioButtons(
                     "data_input", "",
                     choices = 
                       list(
                          "Example data (tidy format)" = 1,
                         "Example data (combined.csv)" = 2,
                         "Upload file" = 3,
                         "Paste data" = 4,
                         "URL (csv files only)" = 5
                       )
                     ,
                     selected =  2),
                   conditionalPanel(
                     condition = "input.data_input=='1'"
                     
                   ),
                   conditionalPanel(
                     condition = "input.data_input=='2'",
                     p("Dataset combined.csv from the SuperPlots paper") 
                   ),
                   conditionalPanel(
                     condition = "input.data_input=='3'",
                     h5("Upload file: "),
                     fileInput("upload", "", multiple = FALSE),
                     selectInput("file_type", "Type of file:",
                                 list("text (csv)" = "text",
                                      "Excel" = "Excel"
                                 ),
                                 selected = "text"),
                     conditionalPanel(
                       condition = "input.file_type=='text'",
                       
                       radioButtons(
                         "upload_delim", "Delimiter",
                         choices = 
                           list("Comma" = ",",
                                "Tab" = "\t",
                                "Semicolon" = ";",
                                "Space" = " "),
                         selected = ",")),
                     
                     actionButton("submit_datafile_button",
                                  "Submit datafile")),
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
                   
                   selectInput("x_var", "Conditions to compare:", choices = "Treatment", selected="Treatment"),
                   selectInput("y_var", "Variables:", choices = "Speed", selected="Speed"),
                   selectInput("g_var", "Groups/Replicas:", choices = list("Replicate", "-"), selected="Replicate"),
                   
                   hr(),
                   
                   checkboxInput(inputId = "info_data",
                                 label = "Show information on data formats",
                                 value = FALSE),
                   
                   conditionalPanel(
                     condition = "input.info_data==true",
                    p("The data needs to be organized in 'tidy' format, in which each variable is a column. This means that all measured values must be present in a single column ('Speed' in the example data). The labels for the conditions are present in another column ('Treatment' in the example data). When different replicates are present, this information is stored in a third column ('Replicate' in the example data). The selection of a column with replicates is optional. The order of the columns is arbitrary. For more information on the tidy format, see:"),
                    a("A basic tutorial for converting wide data to tidy format", href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/"),br(),
                    a("The original paper by Hadley Wickham 'Tidy data'", href = "http://dx.doi.org/10.18637/jss.v059.i10")
                    
                   ),
                   NULL
                 ),
                 
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        
        radioButtons(inputId = "jitter_type", label = "Data offset", choices = list("Quasirandom" = "quasirandom", "Random" = "random"), selected = "quasirandom"),
        
        checkboxInput(inputId = "violin", label = "Display data distribution", value = FALSE),
        
        sliderInput(inputId = "alphaInput", label = "Visibility of the data", 0, 1, 0.7),

        radioButtons(inputId = "summaryInput", label = "Summary statistics for replicates:", choices = list("Mean" = "mean", "Median" = "median"), selected = "mean"),
        

        checkboxInput(inputId = "connect", label = "Connect the dots (paired data)", FALSE),
        
        checkboxInput(inputId = "show_table", label = "Display table with effect size", value = FALSE),
        
          conditionalPanel(condition = "input.show_table == true", selectInput("zero", "Select reference condition:", choices = "")),

        sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),
        
        h4("Plot Layout"),
        
        radioButtons(inputId = "ordered", label= "Order of the conditions:", choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"), selected = "none"),

        selectInput("split_direction", label = "Split replicas:", choices = list("No", "Horizontal", "Vertical"), selected = "No"),

        checkboxInput(inputId = "rotate_plot", label = "Rotate plot 90 degrees", value = FALSE),

        checkboxInput(inputId = "no_grid", label = "Remove gridlines", value = FALSE),

        checkboxInput(inputId = "change_scale", label = "Change scale", value = FALSE),
        
          conditionalPanel(condition = "input.change_scale == true", checkboxInput(inputId = "scale_log_10", label = "Log scale", value = FALSE),

        textInput("range", "Range of values (min,max)", value = "")),

      # checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (minimum n=10)"), value = FALSE),
      # conditionalPanel(
      #   condition = "input.add_CI == true && input.summaryInput !='box'",
      #   checkboxInput(inputId = "ugly_errors", label = "Classic error bars", value = FALSE)),


            ########## Choose color from list
            # selectInput("colour_list", "Colour:", choices = ""),

          radioButtons("adjustcolors", "Color palette:",
                       choices = 
                          list("Standard" = 1,
                               "Okabe&Ito; CUD" = 6,
                               "Tol; bright" = 2,
                               "Tol; muted" = 3,
                               "Tol; light" = 4,
                               "User defined"=5),
                           selected =  6),
      
              conditionalPanel(condition = "input.adjustcolors == 5",
                 textInput("user_color_list", "Names or hexadecimal codes separated by a comma (applied to conditions in alphabetical order):", value = "turquoise2,#FF2222,lawngreen"), 
                 
              h5("",a("Click here for more info on color names", href = "http://www.endmemo.com/program/R/color.php", target="_blank"))
                 
              ),

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
              numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Size axis labels:", value = 18)),
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
        checkboxGroupInput("stats_select", label = h5("Statistics for replicates:"), 
                           choices = list("mean", "sd", "sem","95CI mean", 'p(Shapiro-Wilk)', "median", "MAD", "IQR", "Q1", "Q3"),
                           selected = "sem"),
        actionButton('select_all1','select all'),
        actionButton('deselect_all1','deselect all'),
        numericInput("digits", "Digits:", 2, min = 0, max = 5),
        hr(),


#        ,
#        selectInput("stats_hide2", "Select columns to hide", "", multiple = TRUE, choices=list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median")
        NULL)   
      
    ),
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"),
                  dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"),
                           # downloadButton("downloadPlotSVG", "Download svg-file"), 
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
                           h3("Statistics for individual replicates"),dataTableOutput('data_summary'),
                           h3("Statistics for conditions"),dataTableOutput('data_summary_cluster') , 
                           h3("Statistics for differences between conditions"),dataTableOutput('data_difference')
                           ),
                  tabPanel("About", includeHTML("about.html")
                           )
        )
    )
  )         
)


server <- function(input, output, session) {
  observe({
    showNotification("The SuperPlotsOfData webtool is still in development, and therefore updates may change the appearance of the plot and may have different features. Any feedback or suggestions to improve the app are highly appreciated. For contact information, see the 'About' tab", duration = 100, type = "warning")
  })
  

  x_var.selected <- "Treatment"
  y_var.selected <- "Speed"
  g_var.selected <- "Replicate"
  
  
  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################

df_upload <- reactive({
    
    if (input$data_input == 1) {
      data <- df_tidy_example2
      x_var.selected <<- "Condition"
      y_var.selected <<- "Activity"
      g_var.selected <<- "Replicate" 
    }  else if (input$data_input == 2) {
        data <- df_tidy_example
        x_var.selected <<- "Treatment"
        y_var.selected <<- "Speed"
        g_var.selected <<- "Replicate" 
        
        
    } else if (input$data_input == 3) {
      y_var.selected <<- "none"
      g_var.selected <<- "-" 
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A"))
          } else if (input$file_type == "Excel") {
            data <- read_excel(file_in$datapath)
          } 
        })
      }
    } else if (input$data_input == 5) {
      
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"))
      } else if (url.exists(input$URL) == FALSE) {
         return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read.csv(input$URL)}
    
      #Read the data from textbox
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
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
  
  if (!is.null(input$data_remove)) {
    columns = input$data_remove
    df <- df_upload() %>% select(-one_of(columns))
  } else if (is.null(input$data_remove)) {
  df <- df_upload()}
  
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
        
        facet_list <- c("-",nms_fact)

        updateSelectInput(session, "colour_list", choices = nms_fact)
        updateSelectInput(session, "y_var", choices = vary_list, selected = y_var.selected)
        updateSelectInput(session, "x_var", choices = varx_list, selected = x_var.selected)
        updateSelectInput(session, "g_var", choices = facet_list, selected = g_var.selected)
        updateSelectInput(session, "h_facet", choices = facet_list)
        updateSelectInput(session, "v_facet", choices = facet_list)
        
 #       if (input$add_bar == TRUE) {
#          updateSelectInput(session, "alphaInput", min = 0.3)
#       }

    })


###### When a bar is added, make sure that the data is still visible
observeEvent(input$add_bar, {
  if (input$add_bar==TRUE)  {
    updateSliderInput(session, "alphaInput", min=0.2, max=1)

  } else if (input$add_bar==FALSE)  {
    updateSliderInput(session, "alphaInput", min=0, max=1)
    
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
  updateCheckboxInput(session, "violin", value = presets_vis[2])
  updateSliderInput(session, "alphaInput", value = presets_vis[3])
  updateRadioButtons(session, "summaryInput", selected = presets_vis[4])
  updateCheckboxInput(session, "connect", value = presets_vis[5])
  updateCheckboxInput(session, "show_table", value = presets_vis[6])
  
  #select zero
  
  updateSliderInput(session, "alphaInput_summ", value = presets_vis[8])
  updateRadioButtons(session, "ordered", selected = presets_vis[9])
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
     # updateCheckboxInput(session, "color_stats", value = presets_layout[7])
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
    presets_color <- unlist(strsplit(presets_color,";"))

    updateSelectInput(session, "colour_list", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= presets_color[2])
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
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[8])
    updateCheckboxInput(session, "add_legend", value = presets_label[9])
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
 
 
  vis <- c(input$jitter_type, input$violin, input$alphaInput, input$summaryInput, input$connect, input$show_table, input$zero, input$alphaInput_summ, input$ordered)
  layout <- c(input$split_direction, input$rotate_plot, input$no_grid, input$change_scale, input$scale_log_10, input$range, "7",
              input$adjustcolors, input$add_legend, input$plot_height, input$plot_width)

  #Hide the standard list of colors if it is'nt used
   if (input$adjustcolors != "5") {
     color <- c(input$colour_list, "none")
   } else if (input$adjustcolors == "5") {
     color <- c(input$colour_list, input$user_color_list)
   }
  
  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_ttl, input$fnt_sz_ax, input$add_legend)

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
      klaas$Condition <- factor(klaas$Condition, levels=unique(klaas$Condition))

   } else if (input$ordered == "alphabet") {
     klaas$Condition <- factor(klaas$Condition, levels=unique(sort(klaas$Condition)))
   }  
  
    return(klaas)
  
})

######## Extract the data for display & summary stats #######  

df_selected <- reactive({

    df_temp <- df_upload() 
    x_choice <- input$x_var
    y_choice <- input$y_var
    g_choice <- input$g_var
    
    #Prevent error if y parameter is not selected
    if (input$y_var =='none') {
      koos <- df_temp %>% select(Condition = !!x_choice)
      koos$Value <- 1
      koos$Replica <- as.factor("1")
      return(koos)
    }
    
    if (g_choice == "-") {

    koos <- df_temp %>% select(Condition = !!x_choice , Value = !!y_choice) %>% filter(!is.na(Value))
    koos$Replica <- as.factor("1")
    } else {
      
      koos <- df_temp %>% select(Condition = !!x_choice , Value = !!y_choice, Replica = !!g_choice) %>% filter(!is.na(Value))
      
    }
    
    #Convert Condition and Replica into factors
    koos <- koos %>% mutate_at(vars(Condition, Replica), funs(factor))

    return(koos)
})

########### When x_var is selected for tidy data, get the list of conditions

observeEvent(input$x_var != 'none', {
  
  if (input$x_var != 'none') {
    
    koos <- df_selected()
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
  

########### Caluclate stats for the MEAN ############

df_summary_replica <- reactive({
  koos <- df_selected()

  koos %>%
    group_by(Condition, Replica) %>% 
    summarise(n = n(),
            mean = mean(Value, na.rm = TRUE),
#            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE)) %>%
      mutate(sem = sd / sqrt(n - 1),
             `95%CI_lo` = mean + qt((1-Confidence_level)/2, n - 1) * sem,
             `95%CI_hi` = mean - qt((1-Confidence_level)/2, n - 1) * sem)

  })

############ Caluclate stats for the MEDIAN ##########

df_summary_median <- reactive({
    
    kees <- df_selected()

    df <- kees %>%
                  group_by(Condition, Replica) %>%
                    summarise(
#                            n= n(),
                      'p(Shapiro-Wilk)' = f(Value),
                         median= median(Value, na.rm = TRUE),
                            MAD= mad(Value, na.rm = TRUE, constant=1),
                            IQR= IQR(Value, na.rm = TRUE),
                            Q1=quantile(Value, probs=0.25),
                            Q3=quantile(Value, probs=0.75))
    

    return(df)
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
    
    stats <- as.character(input$summaryInput)
    
#   observe({ print(koos) })
    
    custom_order <-  levels(factor(koos$Condition))
#    custom_labels <- levels(factor(koos$label))
  
  ########## Define alternative color palettes ##########
  
     # observe({ print(head(df_selected())) })    
    
    newColors <- NULL
    
    if (input$adjustcolors == 2) {
      newColors <- Tol_bright
    } else if (input$adjustcolors == 3) {
      newColors <- Tol_muted
    } else if (input$adjustcolors == 4) {
      newColors <- Tol_light
    } else if (input$adjustcolors == 6) {
      newColors <- Okabe_Ito
    } else if (input$adjustcolors == 5) {
      newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
    }


        kleur <- 'Replica'

  

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

    
    p <- ggplot(data=klaas, aes_string(x='Condition', y='Value')) 
    
    # Setting the order of the x-axis
    p <- p + scale_x_discrete(limits=custom_order)
    
    data_width = 0.4
    
    if (input$violin) data_width=data_width/2
  ##### plot selected data summary (bottom layer) ####

   #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "quasirandom") {
      p <- p + geom_quasirandom(aes_string(x='Condition', y='Value', color = kleur), shape = 16, width=data_width, cex=3.5, alpha=input$alphaInput)
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(aes_string(x='Condition', y='Value', color = kleur), width=data_width*0.8, height=0.0, shape = 16, cex=3.5, alpha=input$alphaInput)
      
    }
    if (input$connect) {
      p <-  p + stat_summary(aes_string(group = 'Replica'), color="grey20", fun = stats, geom = "line", size = 1, alpha=input$alphaInput_summ, linetype='dotted') 

        p <-  p + stat_summary(aes_string(group = 'Replica', fill=kleur), color="grey20", fun = stats, geom = "point", stroke = 1, shape = 21, size = 8, alpha=input$alphaInput_summ) 
      # } else if (input$color_data == FALSE) {
        # p <-  p + stat_summary(aes_string(group = 'Replica'), fill = 'grey', fun = input$summaryInput, geom = "point", stroke = 2, shape = 21, size = 10, alpha=input$alphaInput_summ) 
        
        
    }
    else if (!input$connect)
    p <-  p + stat_summary(aes_string(group = 'Replica', color=kleur), fun = stats, geom = "point", stroke = 0, shape = 16, size = 10, alpha=input$alphaInput_summ) 
    

    if  (input$violin) {
      p <- p + geom_flat_violin(aes_string(x='Condition',  fill=kleur),color=NA,scale = "width", width=0.7,position = position_nudge(x = .22, y = 0), trim=FALSE, alpha = 0.75*input$alphaInput)
    }
    
########### Do some formatting of the lay-out ###########

     p <- p+ theme_light(base_size = 16)
    
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
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    
    # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
    }
     
    # #remove legend (if selected)
    if (input$add_legend == FALSE) {
      p <- p + theme(legend.position="none")
    }

     #remove gridlines (if selected)
     if (input$no_grid == TRUE) {  
       p <- p+ theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
     }
     
          
   if (input$adjustcolors >1) {
       p <- p+ scale_color_manual(values=newColors)
       p <- p+ scale_fill_manual(values=newColors)
   }
    
     
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
  klaas <- df_summary_replica() 
  # %>% mutate(mean_CI_lo = round(mean_CI_lo, digits), mean_CI_hi = round(mean_CI_hi, digits)) %>% unite("95CI mean", c("mean_CI_lo","mean_CI_hi"), sep=" , ")

    koos <- df_summary_median() 
    
  klaas  <- full_join(klaas, koos,by=c("Condition","Replica"))

    # Round down to the number of selected digits
    # klaas <- klaas %>% mutate_at(c(3:5, 7:11), round, input$digits)
    klaas <- klaas %>% mutate_at(c(4:8, 10:14), round, input$digits) %>% mutate_at(c(9), round, 4) 
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
  
  df <- df_summary_replica() %>% rename(Value=mean)
  df_cluster <- df_summary_cluster()
  if (max(df_cluster$n == 1)) {
    df <- df_selected()}

  # df_summary <- df %>% group_by(Condition) %>% summarise(n = n(),
  #                                                mean = mean(Value))
  
  # observe({print(head(df))})

  # df <- df %>% ungroup()
  
  # select the control condition for comparisons
  control_condition <- input$zero
  
  #Get the reference values
  df_controls <- df %>% filter(Condition==!!control_condition)
  df_controls <- df_controls %>% select(Replica, control_value = Value, cond = Condition)

  
  
  #Remove the Reference from the dataframe and add the reference values to a new column
  df_diff <- df %>% filter (Condition!=!!control_condition) %>% select(Condition,Replica,Value) %>% full_join(df_controls, by='Replica')  %>% unite('Condition' ,c("cond","Condition"), sep = " vs ")

  df_difference <- df_diff  %>% 
    group_by(Condition) %>% do(tidy(t.test(.$Value, .$control_value, paired = input$connect)))
  
  observe({print(df_difference)})  
  
  df_difference <- df_difference  %>% select(Condition, difference=estimate, `95%CI_lo`=conf.low, `95%CI_hi`=conf.high,p.value)
  
  df_difference <- df_difference %>% mutate_at(c(2:4), round, input$digits)  %>% mutate_at(c(5), round, 8)
  
  observe({print(df_difference$p.value)})
  
  #Use scientific notation if smaller than 0.001
  if (df_difference$p.value<0.001) {
  df_difference$p.value  <- formatC(df_difference$p.value, format = "e", digits = 2)
  }

  return(df_difference)
  
})

df_summary_cluster <- reactive({
  
  df <- df_summary_replica()
  df$Value <- df$mean
  df <- df %>% group_by(Condition) %>% summarise(n = n(),
                                                 mean = mean(Value),
                                                 sd = sd(Value))  %>%
    mutate(sem = sd / sqrt(n - 1),
           # mean_CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
           # mean_CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem,
           NULL)
  
  # observe({print(df_stats)})
  
  
  df <- df %>% mutate_at(c(3:5), round, input$digits)
  
  # observe({print(df)})
  return(df)
})



#### A predefined selection of stats for the table  ###########

observeEvent(input$summaryInput, {
  if (input$summaryInput=="mean")  {
    updateSelectInput(session, "stats_select", selected = list("mean", "sd", '95CI mean', 'p(Shapiro-Wilk)'))
  }
  else if (input$summaryInput=="median")  {
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
  df_filtered_stats(),
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

output$data_summary_cluster <- renderDataTable(
  datatable(
    df_summary_cluster(),
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


############## Render the data summary as a table ###########

output$toptable <- renderTable({
  
  if (input$show_table == F) return(NULL)
  df <- as.data.frame(df_difference())
  
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