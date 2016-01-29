## Purpose and features

Files associated with RShiny application for SESYNC Pursuit Payments for Watershed Services[1]. The main purpose of this application is to easily visualize and compare the distribution of many variables between cities that do and do not have payments for watershed services programs. 

#### Comparisons

The user chooses *variable_select* from the list of numeric variable column names. The output displays a series of plots comparing those variables for `PWS_yes` and `PWS_no` cities. There are separate plots for subsets of the data excluding the US and US only. 

The Data Summary tab displays outputs of some basic statistical tests such as mean, sample size, t-test, test for normality. 

The Data tab displays the data associated with that variable.

The user can also select whether to log transform the data. 

The sidebar displays information from the `All_Field_defs.csv` file about the selected variable.

#### All PWS Data

displays data in `city_join.csv`

#### Variable definitions

Displays all information from the `All_Field_defs.csv` file

#### P values at a glance

Displays the `pvalues_glance.csv` table which is generated from the functions in the `k-s_tests.R` file. This file has an example function of extracting the p-value from a statistical comparison function that is applied to all of the numeric variables in the data. 

## Inputs

* `All_Field_defs.csv` Data dictionary file to describe data type, definition, and source for each variable
* `pvalues_glance.csv` Output of statistical comparisons made across all numeric variables in a data set
* `city_join.csv` Main data file. `dependencies_joined_data.R` removes the first column and renames the `CWM_City` column to `city_name`. 

## R packages dependencies

The `dependencies_joined_data.R` file installs and loads the following packages which support the application

* `shiny`
* `ggplot2`
* `readr`

## References

[1] SESYNC Pursuit Description http://sesync.org/project/graduate-student-pursuit-rfp/enabling-payments-for-watershed-services

[2] McDonald et al. 2014 Water on an urban planet: Urbanization and the reach of urban water infrastructure. Global Environmental Change 27:96-105. doi:10.1016/j.gloenvcha.2014.04.022

[3] Urban Water Blueprint website http://water.nature.org/waterblueprint/index.html


## Acknowledgements

This work was supported by the National Socio-Environmental Synthesis Center (SESYNC) under funding received from the National Science Foundation DBI-1052875.