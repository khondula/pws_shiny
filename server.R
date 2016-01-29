# shiny server file 

# run the dependencies file to load in packages and data
# use local = TRUE argument to load in new data files on the server
# see reference for session handling on shiny server here: http://shiny.rstudio.com/articles/scoping.html
source("dependencies_joined_data.R", local = TRUE)

# begin shiny here
shinyServer(function(input, output) {

  # make an output object that is the whole data table for "All PWS Data" tab
  output$data_pws_datatable <- renderDataTable({data_pws})
  
  ########################################
  # subset data based on selected variable
  ########################################
  
  # make a reactive object that subsets columns in the original data based on a selected input variable chosen from drop-down menu
  data_selected_var <- reactive({
    # select the appropriate columns with selected var in 2nd position
    data_selected_var <- data_pws[,c("PWS_Final",input$variable_select, "country_name", "city_name")]
    # log transform if radio button is set to 1 for yes
    if(input$radio_log==1){data_selected_var[,2] <- log(data_selected_var[,2])}
    # rename columns for standardization
    names(data_selected_var) <- c("PWS_Final", "Selected_var", "country_name", "city_name")
    # return data frame
    return(data_selected_var)
    })
  
  # make a reactive object that subsets columns in the original data based on a selected input variable and the condition that country is not US
  data_selected_var_wOut_US <- reactive({
    data_selected_var <- data_pws[data_pws$USA==0,c("PWS_Final",input$variable_select, "country_name", "city_name")]
    if(input$radio_log==1){data_selected_var[,2] <- log(data_selected_var[,2])}
    names(data_selected_var) <- c("PWS_Final", "Selected_var", "country_name", "city_name")
    return(data_selected_var)
  })

  # make a reactive object that subsets columns in the original data based on a selected input variable and the condition that country is ONLY the US
  data_selected_var_Only_US <- reactive({
    data_selected_var <- data_pws[data_pws$USA==1,c("PWS_Final",input$variable_select, "country_name", "city_name")]
    if(input$radio_log==1){data_selected_var[,2] <- log(data_selected_var[,2])}
    names(data_selected_var) <- c("PWS_Final", "Selected_var", "country_name", "city_name")
    return(data_selected_var)
  })
  
  # get information from field definitions about selected variable
  output$selected_var_type <- renderPrint({
    return(All_Field_defs[All_Field_defs$Field==input$variable_select,"Data_Type"])
  })
  output$selected_var_defn <- renderPrint({
    return(All_Field_defs[All_Field_defs$Field==input$variable_select,"Definition"])
  })
  output$selected_var_source <- renderPrint({
    return(All_Field_defs[All_Field_defs$Field==input$variable_select,"Source"])
  })
  
  # make a data table of all the field definitions
  output$field_defns_table <- renderDataTable({All_Field_defs})

  #############
  # make plots
  #############
  
  # make boxplots of selected variable to compare distributions for PWS_Final yes/no
  # each figure is the same except for the input data frame being a different reactive object
  output$boxplot_all <- renderPlot({
      ggplot(data_selected_var(), aes(x = PWS_Final, y = Selected_var, fill = PWS_Final)) +
      geom_boxplot() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  output$boxplot_wOut_US <- renderPlot({
    ggplot(data_selected_var_wOut_US(), aes(x = PWS_Final, y = Selected_var, fill = PWS_Final)) +
      geom_boxplot() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  output$boxplot_Only_US <- renderPlot({
    ggplot(data_selected_var_Only_US(), aes(x = PWS_Final, y = Selected_var, fill = PWS_Final)) +
      geom_boxplot() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })

  # make histograms of selected variable
   output$hist_all <- renderPlot({
     ggplot(data_selected_var(), aes(fill = PWS_Final, x = Selected_var)) +
       geom_histogram(position = "dodge") +
       theme(text = element_text(size=20))   
   })
   output$hist_wOut_US <- renderPlot({
     ggplot(data_selected_var_wOut_US(), aes(fill = PWS_Final, x = Selected_var)) +
       geom_histogram(position = "dodge") +
       theme(text = element_text(size=20))   
   })
   output$hist_Only_US <- renderPlot({
     ggplot(data_selected_var_Only_US(), aes(fill = PWS_Final, x = Selected_var)) +
       geom_histogram(position = "dodge")  +
       theme(text = element_text(size=20))   
   })
   
  # make empirical cumulative distribution function plots of selected variable
  output$ecdf_all <- renderPlot({
    ggplot(data_selected_var(), aes(color = PWS_Final, x = Selected_var)) +
      stat_ecdf() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  output$ecdf_wOut_US <- renderPlot({
    ggplot(data_selected_var_wOut_US(), aes(color = PWS_Final, x = Selected_var)) +
      stat_ecdf() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  output$ecdf_Only_US <- renderPlot({
    ggplot(data_selected_var_Only_US(), aes(color = PWS_Final, x = Selected_var)) +
      stat_ecdf() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  
  # make scatterplots selected variable
  output$scatter_all <- renderPlot({
    ggplot(data_selected_var(), aes(x = PWS_Final, y = Selected_var, fill = PWS_Final)) +
      geom_point() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
    })
  output$scatter_wOut_US <- renderPlot({
    ggplot(data_selected_var_wOut_US(), aes(x = PWS_Final, y = Selected_var, fill = PWS_Final)) +
      geom_point() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  output$scatter_Only_US <- renderPlot({
    ggplot(data_selected_var_Only_US(), aes(x = PWS_Final, y = Selected_var, fill = PWS_Final)) +
      geom_point() + guides(fill = FALSE) +
      theme(text = element_text(size=20))
  })
  
  # make dotplots selected variable
  output$dot_all <- renderPlot({
    ggplot(data_selected_var(), aes(x = Selected_var, fill = PWS_Final)) +
      geom_dotplot() + guides(fill = FALSE) + facet_grid(~PWS_Final) +
      theme(text = element_text(size=20))
  })
  output$dot_wOut_US <- renderPlot({
    ggplot(data_selected_var_wOut_US(), aes(x = Selected_var, fill = PWS_Final)) +
      geom_dotplot() + guides(fill = FALSE) + facet_grid(~PWS_Final) +
    theme(text = element_text(size=20))
  })
  output$dot_Only_US <- renderPlot({
    ggplot(data_selected_var_Only_US(), aes(x = Selected_var, fill = PWS_Final)) +
      geom_dotplot() + guides(fill = FALSE) + facet_grid(~PWS_Final) +
    theme(text = element_text(size=20))
  })
  
  # make rank order scatterplots
  output$rankplot_all <- renderPlot({
    ggplot(data = data_selected_var(), aes(x = rank(Selected_var), y = Selected_var, color = PWS_Final)) +
      geom_point() + guides(fill = FALSE) + facet_grid(~PWS_Final) +
      theme(text = element_text(size=20))  
  })
  output$rankplot_wOut_US <- renderPlot({
    ggplot(data = data_selected_var_wOut_US(), aes(x = rank(Selected_var), y = Selected_var, color = PWS_Final)) +
      geom_point() + guides(fill = FALSE) + facet_grid(~PWS_Final) +
      theme(text = element_text(size=20))  
  })
  output$rankplot_Only_US <- renderPlot({
    ggplot(data = data_selected_var_Only_US(), aes(x = rank(Selected_var), y = Selected_var, color = PWS_Final)) +
      geom_point() + guides(fill = FALSE) + facet_grid(~PWS_Final) +
      theme(text = element_text(size=20))  
  })
  
  #####################
  # do some statistics
  #####################
  
  # Tukey fivenum summary, mean, sample size
  output$data_selected_var_summary <- renderTable({
    data.agg <- cbind(
      aggregate(data_selected_var()[,2], by = list(data_selected_var()$PWS_Final), FUN = fivenum)$x,
      aggregate(data_selected_var()[,2], by = list(data_selected_var()$PWS_Final), FUN = mean, na.rm = TRUE)$x,
      aggregate(data_selected_var()[,2], by = list(data_selected_var()$PWS_Final), FUN = function(x) length(x)-sum(is.na(x)))$x)
    data.agg <- as.data.frame(data.agg)
    names(data.agg) <- c("min", "lowerhinge", "median", "upperhinge", "max", "mean", "sample_size")
    row.names(data.agg) <- c("PWS_NO", "PWS_YES")
    return(((data.agg)))
  })
  output$data_selected_var_summary_wOut_US <- renderTable({
    data.agg <- cbind(
      aggregate(data_selected_var_wOut_US()[,2], by = list(data_selected_var_wOut_US()$PWS_Final), FUN = fivenum)$x,
      aggregate(data_selected_var_wOut_US()[,2], by = list(data_selected_var_wOut_US()$PWS_Final), FUN = mean, na.rm = TRUE)$x,
      aggregate(data_selected_var_wOut_US()[,2], by = list(data_selected_var_wOut_US()$PWS_Final), FUN = function(x) length(x)-sum(is.na(x)))$x)
    data.agg <- as.data.frame(data.agg)
    names(data.agg) <- c("min", "lowerhinge", "median", "upperhinge", "max", "mean", "sample_size")
    row.names(data.agg) <- c("PWS_NO", "PWS_YES")
    return(((data.agg)))
  })
  output$data_selected_var_summary_Only_US <- renderTable({
    data.agg <- cbind(
      aggregate(data_selected_var_Only_US()[,2], by = list(data_selected_var_Only_US()$PWS_Final), FUN = fivenum)$x,
      aggregate(data_selected_var_Only_US()[,2], by = list(data_selected_var_Only_US()$PWS_Final), FUN = mean, na.rm = TRUE)$x,
      aggregate(data_selected_var_Only_US()[,2], by = list(data_selected_var_Only_US()$PWS_Final), function(x) length(x)-sum(is.na(x)))$x)
    data.agg <- as.data.frame(data.agg)
    names(data.agg) <- c("min", "lowerhinge", "median", "upperhinge", "max", "mean", "sample_size")
    row.names(data.agg) <- c("PWS_NO", "PWS_YES")
    return(((data.agg)))
  })
  
  # kolgomorov smirnov test to compare distribution
  output$ks_test <- renderPrint({
    ks_test_result <- ks.test(data_selected_var()[data_selected_var()$PWS_Final=="NO",2],
                              data_selected_var()[data_selected_var()$PWS_Final=="YES",2])
    return(c(ks_test_result$statistic, ks_test_result$p.value))
  })
  output$ks_test_wOut_US <- renderPrint({
    ks_test_result <- ks.test(data_selected_var_wOut_US()[data_selected_var_wOut_US()$PWS_Final=="NO",2],
                              data_selected_var_wOut_US()[data_selected_var_wOut_US()$PWS_Final=="YES",2])
    return(c(ks_test_result$statistic, ks_test_result$p.value))
  })
  output$ks_test_Only_US <- renderPrint({
    ks_test_result <- ks.test(data_selected_var_Only_US()[data_selected_var_Only_US()$PWS_Final=="NO",2],
                              data_selected_var_Only_US()[data_selected_var_Only_US()$PWS_Final=="YES",2])
    return(c(ks_test_result$statistic, ks_test_result$p.value))
  })
  
  # t test on the mean
  output$t_test <- renderPrint({
    t.test(data_selected_var()[,2]~data_selected_var()$PWS_Final)
  })
  output$t_test_wOut_US <- renderPrint({
    t.test(data_selected_var_wOut_US()[,2]~data_selected_var_wOut_US()$PWS_Final)
  })
  output$t_test_Only_US <- renderPrint({
    t.test(data_selected_var_Only_US()[,2]~data_selected_var_Only_US()$PWS_Final)
  })
  
  # test for normality
  output$normality_all <- renderPrint({
    shapiro.test(data_selected_var()[,2])
  })
  output$normality_wOut_US <- renderPrint({
    shapiro.test(data_selected_var_wOut_US()[,2])
  })
  output$normality_Only_US <- renderPrint({
    shapiro.test(data_selected_var_Only_US()[,2])
  })
  
  # test for differences in variance
  output$var_test <- renderPrint({
    var.test(data_selected_var()[,2]~data_selected_var()$PWS_Final)
  })
  output$var_test_wOut_US <- renderPrint({
    var.test(data_selected_var_wOut_US()[,2]~data_selected_var_wOut_US()$PWS_Final)
  })
  output$var_test_Only_US <- renderPrint({
    var.test(data_selected_var_Only_US()[,2]~data_selected_var_Only_US()$PWS_Final)
  })
  
  # test for homogeneity of variance (non-parametric?)
  output$homog_var_test <- renderPrint({
    fligner.test(data_selected_var()[,2]~data_selected_var()$PWS_Final)
  })
  output$homog_var_test_wOut_US <- renderPrint({
    fligner.test(data_selected_var_wOut_US()[,2]~data_selected_var_wOut_US()$PWS_Final)
  })
  output$homog_var_test_Only_US <- renderPrint({
    fligner.test(data_selected_var_Only_US()[,2]~data_selected_var_Only_US()$PWS_Final)
  })
  
  # make subsets of the data for the selected variable to show tables of PWS yes and PWS no side by side
  output$data_selected_var_pwsYES <- renderDataTable({subset(data_selected_var(), PWS_Final == "YES")[,2:4]})
  output$data_selected_var_pwsNO <- renderDataTable({subset(data_selected_var(), PWS_Final == "NO")[,2:4]})
  
  output$pvalue_glance <- renderDataTable({
    pvalues_glance
  })
  

  }
)

            
  