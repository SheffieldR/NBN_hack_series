


passwordTextInput <- function (inputId, label, value = "") {
  tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, 
                                                         type = "password", value = value,class="passTextInput"))
}

dat <- read.csv("data/data.csv",
                stringsAsFactors = F)

metadata <- read.csv("data/metadata.csv", 
                     stringsAsFactors = FALSE)


vars <- as.list(names(dat)[!names(dat) %in% c("species","order","family")])
names(vars) <- paste(metadata$cat[match(vars, metadata$ms.vname)], 
                     "-", metadata$list.vname[match(vars, metadata$ms.vname)])
vars <- vars[order(names(vars))]
  
h1("my title")

shinyUI(fluidPage(theme = "bootstrap.css",

                  shinyjs::useShinyjs(),
  h1(strong("sex roles in birds data exploration app"), style = "font-family: 'Arial Black', cursive;
        font-weight: 700; line-height: 1.7; 
        color: #82BDA9;
     text-shadow: -1px 0 #336655, 0 1px #336655, 1px 0 #336655, 0 -1px #336655;"),
  img(src = "bird_sil_copy.png"),
  
  br(""),
  
  navbarPage("", id = "inNavpage",
             tabPanel("Single Variable",
                      
                      h3("Explore single variables"),
                      br("Use this panel to exlore the distribution of individual variables. 
                        You can subset the data by family. Bins in histograms can be altered 
                        using the slider. 
                         Tick the", strong("log"), "box to view logged values"),
                      br(""),
  
                      sidebarLayout(
                        sidebarPanel(h2(strong(em("Input select"))),
                                     selectInput("variable", "select variable", selected = "all", 
                                                 choices = vars),
                                     
                                     br(),
                                     h3(strong(em("subset"))),
                                     uiOutput("taxoDat")
                                     ),
    
                        mainPanel(tabsetPanel(
                          tabPanel("Plot", plotlyOutput("plot1"),
                                  helpText("use slider to select number of histogram bins"),
                                  sliderInput("bins", label = "", min = 1, max = 100, value = 15),
                                  checkboxInput("log", label = "log", value = FALSE)),
                          tabPanel("Summary", dataTableOutput("summary")),
                          tabPanel("Data", dataTableOutput("data"))
                          ))
                        )
                      ),
  
  #### CROSS-VARIABLE PANEL ##########################################################
  
            tabPanel("Cross Variable", 
                     h3("Explore cross-variable relationships"),
                     br("Use this panel to exlore the cross variable distribution. 
                        Tick the", strong("log"), "box under each variable to view logged values. 
                        Only works on numeric data"),
                     br(""),
                     
                     sidebarLayout(
                       sidebarPanel(h2(strong(em("Inputs select"))),
                                    selectInput("var1", "select variable 1", choices = vars,
                                                selected = vars[[1]]),
                                    helpText("only works on numerical variables"),
                                    checkboxInput("log1", label = "log", value = FALSE),
                                     selectInput("var2", "select variable 2", choices = vars,
                                                selected = vars[[1]]),
                                    checkboxInput("log2", label = "log", value = FALSE),
                                    br(""),
                                    helpText("only works on image plots for categorical variables"),
                                    checkboxInput("rel", label = "within category relative proportion", value = FALSE),
                                    uiOutput("relVar")
                                    ),
                     
                       mainPanel(plotlyOutput("plot2")))
                     ),
  
  #### DOWNLOAD PANEL ###############################################################
  
            tabPanel("Download", 
                     h2("Data Download"),
                     h4("Use this panel to select and download data"),
                     br(),
                     
                     sidebarLayout(
                       sidebarPanel(h3(strong(em("select data"))),
                                    selectInput("varOut", h4("select variables"), 
                                                choices = c("all", vars), multiple = T, selectize = T),
                                    helpText("multiple variables allowed"),
                                    
                                    br(),
                                    
                                    checkboxGroupInput("varGroup", label = h4("select groups of variables"), 
                                                       choices = as.list(unique(metadata$cat)[!unique(metadata$cat) 
                                                                                           %in% c("ref", "qc", 
                                                                                                  "id", NA, "taxo")])),
                                    br(),
                                    h4(strong(em("subset"))),
                                    uiOutput("taxoDat2"),
                                    helpText("Note: to select individual families, first delete",
                                             "already selected", code("all")),
                                    br(),
                                    br(),
                                    passwordTextInput("password","Password"),
                                    #submitButton("Submit"),
                                    h3("download selected data"),
                                    downloadButton('downloadData', 'Download')
                                    ),
                       
                       mainPanel(
                         column(8, 
                                dataTableOutput('var_out')),
 
                         column(3, 
                                h4(strong("families")),
                                htmlOutput('fam_out')))
                   
                       )
                     )
                     
  
  
  )
  
  ))


