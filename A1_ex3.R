library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(gridExtra)

summary(A1_Ex3_signif)
summary(A1_Ex3_not_signif)

file_name <- A1_Ex3_signif    #specifying both csv files as objects(file_name and file_name1)
file_name1 <- A1_Ex3_not_signif

mylm <- function(file_name) { ##Used as a function to return the correct decision and conclusion
  fit <- lm(Y ~ X, data = file_name)
  return(list(fit = fit, data = file_name))
}

mylm1 <- function(file_name1) { ##Used 
  fit0.1 <- lm(Y ~ X, data = file_name1)
  return(list(fit0.1 = fit0.1, data = file_name1))
}

mytest <- function(file_name, file_name1)         ##mytest function calls all functions that need to be displayed in report such as p_values, decisions and conclusions.  
  { 
  beta_hat <- coef(summary(fit1))[2, "Estimate"]  ##All values from simple linear regression test are extracted then stored in an object that is named, such as p-value.
  conf_interval <- confint(fit1)[2, ]
  t_value <- coef(summary(fit1))[2, "t value"]
  df <- summary(fit1)$fstatistic[3]
  p_value <- coef(summary(fit1))[2, "Pr(>|t|)"] 
  beta_hat1 <- coef(summary(fit2))[2, "Estimate"]
  conf_interval1 <- confint(fit2)[2, ]
  t_value1 <- coef(summary(fit2))[2, "t value"]
  p_value1 <- coef(summary(fit2))[2, "Pr(>|t|)"]  
  df1 <- summary(fit2)$fstatistic[3]
  assumptions(file_name)                  ##All functions programmed are called in mytest function in order to display report of both tests including plots or graphs of assumptions
  assumptions1(file_name1)                ##This set of called assumptions and the line above call the scatter plot for x vs y
  FIT(file_name, file_name1)              ##This called function sets up simple linear regression test and displays histogram/scatter plot assumption for the residuals
  print(hypothesis(file_name))            ##Print hypothesis for both tests
  print(summary(fit1))                    ##This calls and displays summary of fit 1 (simple linear regression test of file: A1_Ex3_signif) from within the FIT function, in console
  print(summary(fit2))                                  ##This calls and displays summary of fit 2 (simple linear regression test of file: A1_Ex3_not_signif)  from within the FIT function, in console
  print("Report for A1_Ex3_signif and A1_Ex3_not_signif")##Every object that has been made in mytest gets printed out in a cat command
  cat("\nEstimated Slope (βˆ) for A1_Ex3_signif:", beta_hat, "\n")
  cat("95% Confidence Interval for β, for A1_Ex3_signif:", conf_interval, "\n")
  cat("t-value for A1_Ex3_signif:", t_value, "\n")
  cat("Degrees of Freedom (df) for A1_Ex3_signif:", df, "\n")
  cat("p-value for A1_Ex3_signif:", p_value, "\n")
  cat("Decision for A1_Ex3_signif:", decision.mylm(fit, sig_level = 0.05), "\n")##Decision and conclusion functions are called for both files in cat commands by decision.lm and conclusion.lm
  cat("Conclusion for A1_Ex3_signif:", conclusion.mylm(fit, sig_level = 0.05), "\n")
  cat("\nEstimated Slope (βˆ) for A1_Ex3_not_signif:", beta_hat1, "\n")
  cat("95% Confidence Interval for β, for A1_Ex3_not_signif:", conf_interval1, "\n")
  cat("t-value for A1_Ex3_not_signif:", t_value1, "\n")
  cat("Degrees of Freedom (df) for A1_Ex3_not_signif:", df1, "\n")
  cat("p-value for A1_Ex3_not_signif:", p_value1, "\n")
  cat("Decision for A1_Ex3_not_signif:", decision1.mylm1(fit0.1, sig_level = 0.05), "\n")
  cat("Conclusion for A1_Ex3_not_signif:", conclusion1.mylm1(fit0.1, sig_level = 0.05), "\n")
  
}


  
hypothesis <- function(file_name){ ##Hypothesis of both tests 
  H0 <- print("Null hypothesis = 0")
  H1 <- print("Alternative hypothesis doesn't = 0")
  return(H0)
  return(H1)
}

assumptions <- function(file_name){ ##Assumptions for X vs Y of A1_Ex3_signif
  plot5 <- ggplot(data = file_name, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot: Y vs X for A1_Ex3_signif")
  grid.arrange(plot5, plot6, ncol = 2, nrow = 1)

}
assumptions1 <- function(file_name1){ ##Assumptions for X vs Y of A1_Ex3_not_signif
  plot6 <- ggplot(data = file_name1, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot: Y vs X for A1_Ex3_not_signif")
}

FIT <- function(file_name, file_name1){ ##Linear regression model and histogram/scatter plot residuals assumption
  fit1 <- lm(Y ~ X, data = file_name)
    fit1.res = resid(fit1)
    summary(fit1)
    fit1.res <- resid(fit1)
    fitted_values <- fitted(fit1)
    plot3 <- ggplot(data = file_name, aes(x = fitted_values, y = fit1.res)) +
      geom_point() +
      labs(title = "Residuals vs Fitted Values for A1_Ex3_signif", x = 'Fitted Values', y = 'Residuals')
    plot1 <- ggplot(data = file_name, aes(x = fit1$residuals)) +
      geom_histogram(fill = 'steelblue', color = 'black') + 
      labs(title = 'Histogram of Residuals for A1_Ex3_signif', x = 'Residuals', 'Frequency')


  fit2 <- lm(Y ~ X, data = file_name1)
    fit2.res <- resid(fit2)
    fitted_values <- fitted(fit2)
    plot4 <- ggplot(data = file_name1, aes(x = fitted_values, y = fit2.res)) +
      geom_point() +
      labs(title = "Residuals vs Fitted Values for A1_Ex3_not_signif", x = 'Fitted Values', y = 'Residuals')
    plot2 <- ggplot(data = file_name1, aes(x = fit2$residuals)) +
        geom_histogram(fill = 'steelblue', color = 'black') + 
        labs(title = 'Histogram of Residuals for A1_Ex3_not_signif', x = 'Residuals', 'Frequency')
   
     grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)##Arranging plots to fit on one page
     
} 

   
  decision.mylm <- function(fit, sig_level = 0.05)##Decision for both tests
    if (p_value < sig_level)##On both tests, if p_value is less than sig_level it will reject null hypothesis and print, otherwise it will accept and print a do not reject 
     {
     print("Reject Null Hypothesis")
    }else
    {
     print("Do not reject Null Hypothesis")
    } 
  
  decision1.mylm1 <- function(fit0.1, sig_level = 0.05)##Decision for both tests
    if (p_value1 < sig_level)##On both tests, if p_value is less than sig_level it will reject null hypothesis and print, otherwise it will accept and print a do not reject 
    {
      print("Reject Null Hypothesis")
      
    }else
    {
      print("Do not reject Null Hypothesis")
      
    } 
 
  
  conclusion.mylm <- function(fit, sig_level = 0.05)##Conclusion for both tests
    if (p_value < sig_level) ##On both tests, if p_value is less than sig_level it will declare there's no significant evidence to confirm a relationship and print, otherwise it will say there is enough evidence to confirm a significant relationship and print 
      {
      
      print("There is no significant evidence to suggest a relationship between X and Y.")
      
     }else 
     {
      print("There is evidence to suggest a significant relationship between X and Y.")
     }

  conclusion1.mylm1 <- function(fit0.1, sig_level = 0.05)##Conclusion for both tests
    if (p_value1 < sig_level) ##On both tests, if p_value is less than sig_level it will declare there's no significant evidence to confirm a relationship and print, otherwise it will say there is enough evidence to confirm a significant relationship and print 
    {
      
     print("There is no significant evidence to suggest a relationship between X and Y.")
      
    }else 
    {
      
     print("There is evidence to suggest a significant relationship between X and Y.")
      
    } 
 
mytest(file_name, file_name1)  ##Calls mytest function and displays whole test in formatted report with graphs









