# user interface file for PWS Shiny application

# first load libraries and data
# source("dependencies.R", local = TRUE)
source("dependencies_joined_data.R", local = TRUE)

# shiny app begins 
shinyUI(navbarPage("PWS", inverse = TRUE,
     
    tabPanel("Comparisons", sidebarLayout(
      
      sidebarPanel(
        selectInput("variable_select", label = h3("Select variable"), choices = pws_numeric_vars, selected = "Water_Area"),
        radioButtons("radio_log", label = h3("Log variable?"), choices = list("No" = 0, "Yes" = 1), selected = 0), br(),
        em("About selected variable"),br(),
        strong("Data type:"), textOutput("selected_var_type"), br(),
        strong("Definition:"), textOutput("selected_var_defn"), br(),
        strong("Source:"), textOutput("selected_var_source")),
      
      mainPanel(tabsetPanel(
        tabPanel("Boxplots",
                 h3("All data"), plotOutput("boxplot_all", height="300px"),
                 h3("US removed"), plotOutput("boxplot_wOut_US", height="300px"),
                 h3("Only US"), plotOutput("boxplot_Only_US"), height = "300px"),
        tabPanel("Dotplots",
                 h3("All data"), plotOutput("dot_all", height="300px"),
                 h3("US removed"), plotOutput("dot_wOut_US", height="300px"),
                 h3("Only US"), plotOutput("dot_Only_US"), height = "300px"),
        tabPanel("Rank Plots",
                 h3("All data"), plotOutput("rankplot_all", height="300px"),
                 h3("US removed"), plotOutput("rankplot_wOut_US", height="300px"),
                 h3("Only US"), plotOutput("rankplot_Only_US"), height = "300px"),
        tabPanel("Scatterplots",
                 h3("All data"), plotOutput("scatter_all", height="300px"),
                 h3("US removed"), plotOutput("scatter_wOut_US", height = "300px"),
                 h3("Only US"), plotOutput("scatter_Only_US", height = "300px")),
        tabPanel("Histograms",
                 h3("All data"), plotOutput("hist_all", height="300px"),
                 h3("US removed"), plotOutput("hist_wOut_US", height = "300px"),
                 h3("Only US"), plotOutput("hist_Only_US", height = "300px"),
                 h3("Stacked"), plotOutput("hist_all_stacked", height = "300px")),
        tabPanel("Ecdfs",
                 h3("All data"), plotOutput("ecdf_all", height="300px"),
                 h3("US removed"), plotOutput("ecdf_wOut_US", height = "300px"),
                 h3("Only US"), plotOutput("ecdf_Only_US", height = "300px")),
        tabPanel("Data Summary",
                 tabsetPanel(
                   tabPanel("All data",
                            h4("Summary stats"),tableOutput("data_selected_var_summary"),br(),
                            h4("K-S test stat and p-value:"), verbatimTextOutput("ks_test"),
                            h4("Shapiro Wilk test for normality"), verbatimTextOutput("normality_all"),
                            h4("t-test"),verbatimTextOutput("t_test"),
                            h4("variance test"), verbatimTextOutput("var_test"),
                            h4("homog of variance test"), verbatimTextOutput("homog_var_test")),
                   tabPanel("US removed",
                            h4("Summary stats"),tableOutput("data_selected_var_summary_wOut_US"),br(),
                            h4("K-S test stat and p-value:"), verbatimTextOutput("ks_test_wOut_US"),
                            h4("Shapiro Wilk test for normality"), verbatimTextOutput("normality_wOut_US"),
                            h4("t-test"), verbatimTextOutput("t_test_wOut_US"),
                            h4("variance test"), verbatimTextOutput("var_test_wOut_US"),
                            h4("homog of variance test"), verbatimTextOutput("homog_var_test_wOut_US")),
                   tabPanel("Only US",
                            h4("Summary stats"),tableOutput("data_selected_var_summary_Only_US"),br(),
                            h4("K-S test stat and p-value:"), verbatimTextOutput("ks_test_Only_US"),
                            h4("Shapiro Wilk test for normality"), verbatimTextOutput("normality_Only_US"),
                            h4("t-test"), verbatimTextOutput("t_test_Only_US"),
                            h4("variance test"), verbatimTextOutput("var_test_Only_US"),
                            h4("homog of variance test"), verbatimTextOutput("homog_var_test_Only_US"))
                 )),
        tabPanel("Data",
                 fluidRow(column(6, h4("PWS YES"), dataTableOutput("data_selected_var_pwsYES")),
                          column(6, h4("PWS NO"), dataTableOutput("data_selected_var_pwsNO"))))
        )))),
    
    tabPanel("All PWS data", dataTableOutput("data_pws_datatable")),
    
    tabPanel("Variable definitions", 
             p("Defintions and sources of variables"),
             dataTableOutput("field_defns_table")),
    
    tabPanel("P values at a glance", 
             p("P values of K-S tests to compare distributions of numeric variables between cities with and without PWS"),
             dataTableOutput("pvalue_glance"))
))
