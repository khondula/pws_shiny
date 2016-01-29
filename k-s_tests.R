# for each numeric variable, compare the distributions (k-s test) of cities with and without PWS

# identify which colums are numeric variables
pws_numeric_vars <- names(data_pws)[sapply(names(data_pws), function(x) is.numeric(data_pws[,x]))]

# function to get p value from k-s test comparing a variable distribution in PWS NO and PWS YES
get_ks_test_p <- function(pws_var){
  value <- ks.test(data_pws[data_pws$PWS_Final=="NO",pws_var], data_pws[data_pws$PWS_Final=="YES",pws_var])$p.value
 return(value)
}

# same function as above but first excludes the US cities
get_ks_test_p_wOut_US <- function(pws_var){
  data_pws <- subset(data_pws, country_name != "United States")
  value <- ks.test(data_pws[data_pws$PWS_Final=="NO",pws_var], data_pws[data_pws$PWS_Final=="YES",pws_var])$p.value
  return(value)
}


# apply the functions above to the numeric variables
pws_ks_p <- sapply(pws_numeric_vars, function(x) get_ks_test_p(x))
pws_ks_p_wOut_US <- sapply(pws_numeric_vars, function(x) get_ks_test_p_wOut_US(x))

pvalues_glance <- as.data.frame(t(rbind(pws_ks_p, pws_ks_p_wOut_US)))
pvalues_glance$variable <- pws_numeric_vars
pvalues_glance$signif <- pvalues_glance$pws_ks_p < 0.05
pvalues_glance$signif_wOut_US <- pvalues_glance$pws_ks_p_wOut_US < 0.05
pvalues_glance <- pvalues_glance[,c(3,1,4,2,5)]

write.csv(pvalues_glance, file = "pvalues_glance.csv", row.names = FALSE)

# significantly different distributions are:
# water area (all cities -- not significantly different w/out US included)
# gov effectiveness (only with excluding US)
# Regulatory quality (only with excluding US)
# Voice and accountability (only with excluding US)
# Rule of law (only with excluding US)
# control of corruption (only with excluding US)
# conservation spending (only with excluding US)
# Conservation A21 (only with excluding US)
# doing business rank
# enforcing contracts rank (only with excluding US)
# 2016 gdp per capita (only with excluding US)
# property rights GCR (only with excluding US)

