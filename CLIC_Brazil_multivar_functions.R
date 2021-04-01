### Function for variable summaries

summ_tab <- function(df,var_list, precision){
  vars.dat <- data.frame(as.list(var_list))
  vars.dat <- data.frame(t(vars.dat[]))
  
  med.list <- lapply(df[var_list], function(x) median(x))
  med.dat <- data.frame(matrix(unlist(med.list), nrow=length(med.list), byrow=T))
  
  q1.list <- lapply(df[var_list], function(x) quantile(x ,probs = 0.25))
  q1.dat <- data.frame(matrix(unlist(q1.list), nrow=length(q1.list), byrow=T))
  
  q3.list <- lapply(df[var_list], function(x) quantile(x ,probs = 0.75))
  q3.dat <- data.frame(matrix(unlist(q3.list), nrow=length(q3.list), byrow=T))
  
  summary.dat <- cbind(vars.dat,med.dat,q1.dat,q3.dat)
  
  names(summary.dat)[1] <- "term"
  names(summary.dat)[2] <- "median"
  names(summary.dat)[3] <- "q1"
  names(summary.dat)[4] <- "q3"
  rownames(summary.dat) <- c()
  
  summary.dat$median <- formatC(summary.dat$median, format = "f", digits = precision)
  summary.dat$q1 <- formatC(summary.dat$q1, format = "f", digits = precision)
  summary.dat$q3 <- formatC(summary.dat$q3, format = "f", digits = precision)
  
  summary.dat$dist <- paste0(summary.dat$median," (",summary.dat$q1," - ",summary.dat$q3,")")
  summary.dat[c(1,5)]  
}

### Function for univariate model 
### Pass data frame, base model  and covariate list 
### Produces a 2 column table with median and IQR for each 
uni_tab <- function(df,base_mod,var_list, precision_p, precision_e){
  
  ## Testing addition of each individual covariate to this model 
  ## And getting univariate effect sdize for the 
  
  for (i in 1:length(var_list)) { 
    print(var_list[i])
    ## testing  variables
    mod2 <- lm(paste0(base_mod," ",covar_all[i]), data = df )  
    summary(mod2)
    # an_result <- anova(mod2,mod1)
    # print(paste0("anova for geo region and mid day model plus  ",var_list[i]))
    # print(an_result)
    ### To get regression parameters 
    tmp1.dat <- broom::tidy(mod2)
    tmp1.dat$estimate <- formatC(tmp1.dat$estimate, format = "f", digits = precision_e)
    tmp1.dat$p.value <- formatC(tmp1.dat$p.value, format = "f", digits = , precision_p)
    ### Confidence intervals
    tmp1.dat <- cbind(tmp1.dat,as.data.frame(confint(mod2, level = 0.95)))
    names(tmp1.dat)[6] <- "CI_2.5"
    names(tmp1.dat)[7] <- "CI_97.5"
    tmp1.dat$CI_2.5 <- formatC(tmp1.dat$CI_2.5, format = "f", digits =  precision_e)
    tmp1.dat$CI_97.5 <- formatC(tmp1.dat$CI_97.5, format = "f", digits = , precision_e)
    
    ### Select required covariate 
    tmp1.dat <- tmp1.dat[which( (grepl(var_list[i], tmp1.dat$term, fixed=TRUE))==TRUE),]
    
    if(i == 1){tmp.dat <- tmp1.dat} 
    if(i > 1) {tmp.dat <- rbind(tmp.dat,tmp1.dat)}
  }
  tmp.dat
}

### Function to generate a multivariate table

multi_tab <- function(df,multi_mod,precision_p,precision_e){
  library("tidyverse")
  library("dplyr")
  library("broom")
  mod_mult <- lm(multi_mod  , data = df)
  ### To get regression parameters 
  tmp1.dat <- broom::tidy(mod_mult)
  tmp1.dat$estimate <- formatC(tmp1.dat$estimate, format = "f", digits = precision_e)
  tmp1.dat$p.value <- formatC(tmp1.dat$p.value, format = "f", digits = precision_p)
  ### Confidence intervals
  tmp1.dat <- cbind(tmp1.dat,as.data.frame(confint(mod_mult, level = 0.95)))
  names(tmp1.dat)[6] <- "CI_2.5"
  names(tmp1.dat)[7] <- "CI_97.5"
  
  tmp1.dat$CI_2.5 <- formatC(tmp1.dat$CI_2.5, format = "f", digits = precision_e)
  tmp1.dat$CI_97.5 <- formatC(tmp1.dat$CI_97.5, format = "f", digits = precision_e)
  
  tmp1.dat 
  
}

## Function to merge models 

merge_sum_uni_mult <- function (summary.dat,univar.dat,multivar.dat) {
  ### Merge sumamry and univar
  
  tab1.dat <- merge(summary.dat,univar.dat,by="term",all.x=TRUE)
  
  ### Merge tab1 and multivar into one dataframe
  
  final_table.dat <- merge(tab1.dat,multivar.dat,by="term",all.x=TRUE)
  
  ### Sort by row number 
  
  
  ### tweaking the format 
  
  #final_table.dat <- final_table.dat[c(1:3,7,8,6,9,13,14,12)]
  
  ### Fix Confidence intervals
  
  final_table.dat$est_CI95_Uni  <- paste0(as.character(final_table.dat$estimate.x)," (",
                                          as.character(final_table.dat$CI_2.5.x)," - ",as.character(final_table.dat$CI_97.5.x),") ")
  
  final_table.dat$est_CI95_Multi  <- paste0(as.character(final_table.dat$estimate.y)," (",
                                            as.character(final_table.dat$CI_2.5.y)," - ",as.character(final_table.dat$CI_97.5.y),") ")
  
  ## Fix NAs
  
  final_table.dat$na_flag <- final_table.dat$estimate.x
  final_table.dat$est_CI95_Uni <- ifelse(is.na(final_table.dat$na_flag), "", final_table.dat$est_CI95_Uni)
  final_table.dat$p.value.x <- ifelse(is.na(final_table.dat$na_flag), "", final_table.dat$p.value.x)
  
  final_table.dat$na_flag <- final_table.dat$estimate.y
  final_table.dat$est_CI95_Multi <- ifelse(is.na(final_table.dat$na_flag), "", final_table.dat$est_CI95_Multi)
  final_table.dat$p.value.y <- ifelse(is.na(final_table.dat$na_flag), "", final_table.dat$p.value.y)
  
  ## Sort by rn
  
  final_table.dat <- final_table.dat %>% arrange(rn)
  
  final_table.dat <- final_table.dat[c(1,3,16,7,17,13)]
  
  names(final_table.dat)[1] <- "Variable"
  names(final_table.dat)[3] <- "Med_IQR"
  names(final_table.dat)[3] <- "Univariate_Estimate_CI"
  names(final_table.dat)[4] <- "Univariate_p_value"
  names(final_table.dat)[5] <- "Multivariate_Estimate_CI"
  names(final_table.dat)[6] <- "Multivariate_p_value"
  
  
  ### To preserve the format
  #final_table.dat <- final_table.dat %>% mutate_all(as.character)
  ## Fix p value estimates
  final_table.dat$Univariate_p_value <- ifelse(final_table.dat$Univariate_p_value=="0.000", "<0.001", final_table.dat$Univariate_p_value)
  final_table.dat$Multivariate_p_value <- ifelse(final_table.dat$Multivariate_p_value=="0.000", "<0.001", final_table.dat$Multivariate_p_value)
  final_table.dat
}

### Interpretation of tobit models
### https://stats.idre.ucla.edu/r/dae/tobit-models/

tobit_tab <- function(mod_inp,precision,precision_p){
  ctable <- coef(summary(mod_inp))
  ### Calculating 95% CI's
  b <- coef(mod_inp)
  se <- sqrt(diag(vcov(mod_inp)))
  ctable2 <- cbind(ctable, LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)
  ### Calculating p values
  pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(mod_inp), lower.tail = FALSE)
  ctable3 <- as.data.frame(cbind(ctable2, pvals))
  ctable3$pvals <- formatC(ctable3$pvals, format = "f", digits = precision_p) 
  ## formatting
  ctable3$Estimate <- formatC(ctable3$Estimate, format = "f", digits = precision)
  ctable3$CI_2.5 <- formatC(ctable3$LL, format = "f", digits = precision)
  ctable3$CI_97.5 <- formatC(ctable3$UL, format = "f", digits = precision)
  # Set row name to 1st column
  ctable3 <- as.data.frame(data.table::setDT(ctable3, keep.rownames = TRUE)[])
  names(ctable3)[1] <- "term"
  ctable3 
}

### Function to loop through multiple ggplots
plot_for_loop2 <- function(df, x_var, y_var) {

  plot_label <-  lm_eqn(rt_mean_covar_dat,rt_mean_covar_dat$mun_Income_pc,rt_mean_covar_dat$Rt_mean)
  yval = 0.8*max(rt_mean_covar_dat$Rt_mean)
  xval = 0.8*max(rt_mean_covar_dat$mun_Income_pc,na.rm=TRUE)

  ggplot(rt_mean_covar_dat, aes(x=mun_Income_pc, y=Rt_mean)) +
    geom_point() +
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    geom_text(x = xval, y = yval , label = plot_label, parse = TRUE)

  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point() +
    geom_line() +
    labs(x = x_var, y = y_var) +
    theme_classic(base_size = 12)
}

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn <- function(df,x,y){
  
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


