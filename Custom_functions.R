# Custom function to convert the name of an R object to string ====

GetObjectName <- function(v1) {
  deparse(substitute(v1))
}


# Custom function to rename columns in a df based on a named list ====

RenameMatchingCols <- function(dataset, named_list) {
  # ' This function requires that there is at least 1 matching name
  # ' in its current implementation. An error will be thrown if there isn't.
  # Getting the common names
  common_names <- intersect(names(named_list), names(dataset))
  # Getting new names for the matches
  new_names <- unlist(named_list[common_names])
  # Performing the renaming
  data.table::setnames(dataset, common_names, new_names)
}


# Custom function to check data availability by column ====

check_data_avail <- function(df, cols_to_check, type) {
  
  # This function is designed to check the % of available values
  # in a list of columns ("cols_to_check") in a certain dataframe ("df").
  # The function returns a vector with the % of data available or missing
  # ("output" can be either "available" or "missing").
  
  # List to store the results
  n_total <- nrow(df)
  list_pct_available <- c()
  list_pct_missing <- c()
  
  # Checking data availability on a per-column basis
  for (col in cols_to_check) {
    n_missing <- sum(is.na(df[[col]]))
    pct_missing <- round((n_missing/n_total) * 100, 1)
    pct_available <- abs(1 - pct_missing)
    
    # Recording results of the check
    list_pct_available <- append(list_pct_available, pct_available)
    list_pct_missing <- append(list_pct_missing, pct_missing)
  }
  
  # Returning the results as a vector
  if (type == "available") {
    return(list_pct_available)
  } else if (type == "missing") {
    return(list_pct_missing)
  } else {
    print("Incorrect input for the 'type' parameter; please choose between 'available' or 'missing.")
  }
  
}


# Custom function to temporarily deal with irregular col names ====

create_temp_names <- function(df) {
  
  # This function temporarily replaces all column names in a df
  # with "Column1", "Column2" etc. to avoid issues when using
  # packages/functions that do not support irregular column
  # names such as ones containing intervals, special chars, etc.
  # The function renames the columns and saves a list in R's
  # memory called "OriginalColNames" that can then be used to
  # restore the original names by e.g. running the following
  # command:
  # names(df) <- OriginalColNames
  
  # Creating lists for renaming
  OriginalColNames <<- names(df)
  NCols <- length(OriginalColNames)
  NColsRange <- c(1:NCols)
  NewColNames <- c()
  
  for (col in NColsRange) {
    new_col_name <- paste("Column", col, sep = "")
    NewColNames <- append(NewColNames, new_col_name)
  }
  
  # Renaming the cols
  names(df) <- NewColNames
  
  # Returning a new df
  return(df)
}


# Custom function that imputes values on per-country basis ====

impute_data_for_country <- function(df, country_code, m, maxit, method, seed, printFlag) {
  
  # This function takes a df and filters it by country_name, after
  # which point uses the "mice" package to perform imputations for
  # the missing values present in the data. The following mice
  # parameters must be passed on to the the mice() function:
  # m, maxit, method, seed
  
  # Filtering the data for the relevant country
  TempData <- df %>%
    filter(CountryCode == country_code)
  
  # As mice can't deal with irregular spellings of col names,
  # we temporarily rename all columns in the data
  TempData <- create_temp_names(TempData)
  
  # Performing the imputations
  TempData <- mice(TempData, m = m, maxit = maxit, method = method, seed = seed, printFlag = printFlag)
  
  # Restoring the original column names to the df
  names(TempData$data) <- OriginalColNames
  
  # Combining the results in a single df and returning it to the user
  TempData <- mice::complete(TempData, action = "long", include = FALSE)
  return(TempData)
}


# Custom function that imputes values for all countries ====

impute_data_for_all <- function(df, m, maxit, method, seed, printFlag) {
  
  # This function takes a df and without filtering it by country, after
  # which point uses the "mice" package to perform imputations for
  # the missing values present in the data. The following mice
  # parameters must be passed on to the the mice() function:
  # m, maxit, method, seed
  
  # As mice can't deal with irregular spellings of col names,
  # we temporarily rename all columns in the data
  TempData <- create_temp_names(df)
  
  # Performing the imputations
  TempData <- mice(TempData, m = m, maxit = maxit, method = method, seed = seed, printFlag = printFlag)
  
  # Restoring the original column names to the df
  names(TempData$data) <- OriginalColNames
  
  # Combining the results in a single df and returning it to the user
  TempData <- mice::complete(TempData, action = "long", include = FALSE)
  return(TempData)
}


# Custom function to fit a cross-validated ridge regression model ====

FitRidgeModel <- function(dataset, endog_var, exog_var, lambdas) {
  # 'This function takes a dataset, its Y and X variables and fits
  # 'a cross-validated ridge regression model based on the most optimal
  # 'lambda value. It returns the model itself, which can then be used
  # 'to make predictions and/or evaluate the coefficients.
  
  # Printing the X and Y variables to the user
  # Note: if using more than 5 X vars, we only print the first 5 var names
  if (length(exog_var) > 5) {
    difference <- length(exog_var) - 5
    exog_var_str <- paste(exog_var[1:5], collapse = ", ")
    exog_var_str <- paste(exog_var_str, "... and ", difference, " more variable(s).")
  } else {
    exog_var_str <- paste(exog_var, collapse = ", ")
  }
  print(sprintf("Fitting a model for: %s", endog_var))
  print(sprintf("Independent variables: %s", exog_var_str))
  
  # Defining which cols to use as independent variables
  EndogVar <- dataset[[endog_var]]
  ExogVars <- exog_var
  
  # Preparing exogenous vars for use in model
  N_Params <- length(ExogVars)
  ExogVars <- data.matrix(dataset[, ExogVars])
  
  # Fitting model with a series of different lambda values
  Model <-
    cv.glmnet(
      x = ExogVars,
      y = EndogVar,
      alpha = 0,
      standardize = FALSE,
      lambda = lambdas
    )
  
  # Set a seed for reproducibility when fitting the model with glmnet
  set.seed(123)
  
  # Re-fitting model with the best lambda value and generating predictions
  OptimalLambda <- Model$lambda.min
  print(sprintf("The most optimal lambda for this model is: %f", OptimalLambda))
  Model <-
    glmnet(
      x = ExogVars,
      y = EndogVar,
      alpha = 0,
      standardize = FALSE,
      lambda = OptimalLambda
    )
  
  return(Model)
  
}


# Custom function to calculate adjusted R squared ====

CalculateR2 <- function(actual_values, predicted_values, n_obs, n_params) {
  # Calculate the residuals
  residuals <- actual_values - predicted_values
  
  # Calculate the total sum of squares
  TSS <- sum((actual_values - mean(actual_values))^2)
  
  # Calculate the residual sum of squares
  RSS <- sum(residuals^2)
  
  # Calculate the R-squared value
  r_squared <- 1 - (RSS / TSS)
  
  # Calculate the adjusted R-squared value
  adj_r_squared <- 1 - (1 - r_squared) * ((n_obs - 1) / (n_obs - n_params - 1))
  
  return(100 * adj_r_squared)
}


# Custom function to evaluate model fit with a continuous Y ====

FitMetrics_Continuous <-
  function(actual_values,
           predicted_values,
           n_obs,
           n_params,
           round_digits) {
    "
    This function takes two vectors containing the actual and predicted
    values and then calculates model fit metrics based on that. Moreover,
    it also takes user input on the N of observations and parameters included
    in a model. Returns a named list with the relevant fit metrics.
    "
    library(Metrics)
    
    # Calculating summary statistics
    MeanActual <- mean(actual_values)
    MeanPredicted <- mean(predicted_values)
    StDevActual <- sd(actual_values)
    StDevPredicted <- sd(predicted_values)
    
    # Calculating actual fit metrics
    MAE <- mean(abs(actual_values - predicted_values))
    RMSE <- sqrt(mean((actual_values - predicted_values)^2))
    MAPE <- 100 * mean(abs((actual_values - predicted_values) / actual_values))
    MAE_PctOfMean <- 100 * (MAE / MeanActual)
    RMSE_PctOfMean <- 100 * (RMSE / MeanActual)
    AdjustedR2 <- CalculateR2(actual_values, predicted_values, n_obs, n_params)
    
    # Consolidating the metrics
    FitMetrics <- list(
      "MAE" = MAE,
      "RMSE" = RMSE,
      "MAE_PctOfMean" = MAE_PctOfMean,
      "RMSE_PctOfMean" = RMSE_PctOfMean,
      "MAPE" = MAPE,
      "Adj_R2" = AdjustedR2,
      "MeanActual" = MeanActual,
      "MeanPredicted" = MeanPredicted,
      "StDevActual" = StDevActual,
      "StDevPredicted" = StDevPredicted,
      "N_Obs" = n_obs,
      "N_Params" = n_params
    )
    
    # Converting output to data frame
    FitMetrics <- do.call(rbind, FitMetrics)
    FitMetrics <- as.data.frame(FitMetrics)
    colnames(FitMetrics) <- c("Value")
    FitMetrics <- tibble::rownames_to_column(FitMetrics, "Metric") %>%
      mutate(Value = round(Value, round_digits))
    
    return(FitMetrics)
  }


# Custom function to get the range of select model fit metrics ====

GetMetricRange <- function(MetricsDf1, MetricsDf2, MetricName, N_Round) {
  #' Takes the dataframe generated by the FitMetrics_Continuous() function
  #' and generates a string representing the range (min-max) for a specific
  #' user-defined MetricName. Rounds the numbers up to N_Round digits after
  #' the decimal separator. Requires two data frames with metrics as input.
  
  MetricLower <- min(MetricsDf1$Value[MetricsDf1[["Metric"]] == MetricName],
                     MetricsDf2$Value[MetricsDf2[["Metric"]] == MetricName])
  MetricUpper <- max(MetricsDf1$Value[MetricsDf1[["Metric"]] == MetricName],
                     MetricsDf2$Value[MetricsDf2[["Metric"]] == MetricName])
  MetricRange <- paste(round(MetricLower, N_Round), "-", round(MetricUpper, N_Round), sep = "")
  return(MetricRange)
}


# Custom function to plot (ridge) regression coefficients (ggplot2) ====

PlotModelCoef <- function(model, n_plot, plot_title) {
  #' This function takes model variables and coefficients
  #' and creates a plot with the top N coefficients
  #' (absolute size), colored by the direction of their effect.
  #' Works with ridge and OLS regression models.
  
  # Consolidating the model's coefficients
  EffectsSummary <- as.data.frame(as.matrix(coef(Model)))
  EffectsSummary$Variable <- row.names(EffectsSummary)
  row.names(EffectsSummary) <- NULL
  
  names(EffectsSummary) <- c("EffectSize", "Variable")
  EffectsSummary <- EffectsSummary %>%
    filter(Variable != "(Intercept)") %>%
    mutate(
      AbsEffectSize = abs(EffectSize),
      EffectDirection = ifelse(EffectSize < 0,
                               "Negative effect",
                               "Positive effect")
    ) %>%
    arrange(desc(AbsEffectSize)) %>%
    filter(row_number() <= n_plot)
  
  # Plotting the effects by size and direction
  p <-
    ggplot(EffectsSummary,
           aes(
             x = reorder(Variable, AbsEffectSize),
             y = AbsEffectSize,
             fill = EffectDirection
           )) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c(
      "Positive effect" = "#188f76",
      "Negative effect" = "#de425b"
    )) +
    labs(
      title = plot_title,
      x = "Absolute effect size",
      y = "Factor",
      fill = ""
    ) +
    theme(
      legend.position = "top",
      legend.justification = c(1, 0),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  p
}


# Custom function to plot (ridge) regression coefficients (plotly version) ====

PlotModelCoefPlotly <- function(model, n_plot, plot_title, custom_labels = NULL, drop_prefix = NULL) {
  # Consolidating the model's coefficients
  EffectsSummary <- as.data.frame(as.matrix(coef(Model)))
  EffectsSummary$Variable <- row.names(EffectsSummary)
  row.names(EffectsSummary) <- NULL
  
  names(EffectsSummary) <- c("EffectSize", "Variable")
  EffectsSummary <- EffectsSummary %>%
    filter(Variable != "(Intercept)") %>%
    mutate(
      AbsEffectSize = abs(EffectSize),
      EffectDirection = factor(ifelse(EffectSize < 0,
                                      "Negative effect",
                                      "Positive effect"),
                               levels = c("Positive effect", "Negative effect"))
    ) %>%
    arrange(desc(AbsEffectSize)) %>%
    filter(row_number() <= n_plot)
  
  # Replacing variable names with custom labels if so specified by user
  if (!is.null(custom_labels)) {
    # Removing prefix indicating variable category
    if (drop_prefix) {
      EffectsSummary <- EffectsSummary %>%
        mutate(Variable = substr(Variable, 3, nchar(Variable)))
    }
    # Replacing variable names with more readable versions
    EffectsSummary <- EffectsSummary %>%
      left_join(custom_labels, by = "Variable") %>%
      mutate(Variable = Label) %>%
      select(-Label)
  }
  
  
  # Plotting the effects by size and direction
  p <- plot_ly(EffectsSummary, 
               x = ~AbsEffectSize, 
               y = ~reorder(Variable, AbsEffectSize), 
               type = 'bar',
               color = ~EffectDirection, 
               colors = c("#188f76", "#de425b")) %>%
    layout(title = plot_title,
           xaxis = list(title = "Factor"),
           yaxis = list(title = "Absolute effect size",
                        autotypenumbers = "strict"),
           showlegend = TRUE)
  p
}


# Custom function that calculates "real" effect sizes for ridge models ====

GetRealEffectSize <-
  function(Model,
           DataForAnalysis,
           LabelsMapping_Scale,
           LabelsMapping_Var) {
    #' This function takes a ridge regression model's coefficients,
    #' a mapping table of the scaling of the input variables as well as
    #' the data frame containing the original (non-standardized) scaling
    #' of the model's parameters and generates some output that is easier
    #' to interpert, e.g. how much Y change if we had will a 10 p.p. change in X.
    
    # Consolidating the model's coefficients
    EffectsSummary <- as.data.frame(as.matrix(coef(Model)))
    EffectsSummary$Variable <- row.names(EffectsSummary)
    row.names(EffectsSummary) <- NULL
    names(EffectsSummary) <- c("Coefficient", "Variable")
    
    # Getting the standard deviations for the variables
    StDev <- sapply(DataForAnalysis, sd, na.rm = TRUE)
    StDev <- as.data.frame(StDev)
    StDev$Variable <- row.names(StDev)
    row.names(StDev) <- NULL
    
    # Adding the standard deviation and indicating the changes in X and Y
    EffectsSummary <- EffectsSummary %>%
      filter(Variable != "(Intercept)") %>%
      select(Variable, Coefficient) %>%
      left_join(StDev, by = "Variable")
    names(EffectsSummary) <-
      c("Variable", "ChangeIn_Y_Var", "ChangeIn_X_Var")
    
    # Calculating the expected change Y per unit change in X
    EffectsSummary <- EffectsSummary %>%
      mutate(ChangeIn_Y_PerUnitChangeIn_X = ChangeIn_Y_Var / ChangeIn_X_Var)
    #ChangeIn_Y_PerUnitChangeIn_X = round(ChangeIn_Y_PerUnitChangeIn_X, 5)
    
    # Adding info on variable scaling and a more interpretable effect size
    # and also adding a more human-readable name for each variable
    EffectsSummary <- EffectsSummary %>%
      mutate(Variable = substr(Variable, 3, nchar(Variable))) %>%
      left_join(LabelsMapping_Scale, by = "Variable") %>%
      left_join(LabelsMapping_Var, by = "Variable") %>%
      mutate(EffectSize = round(ChangeIn_Y_PerUnitChangeIn_X * ScaleFactor, 4),
             Variable = Label) %>%
      select(-Label)
    
    # Cleaning up and returning
    EffectsSummary <- EffectsSummary %>%
      select(Variable, EffectSize, ScaleDescription)
    return(EffectsSummary)
  }


# Custom function that generates plots based on model output ====

PlotModelOutput <-
  function(ActualVal,
           PredictedVal,
           ActualTest,
           PredictedTest) {
    #' This function takes the actual and predicted values based on
    #' the validation and test sets and generates a 2x2 subplot with
    #' the actual vs. predicted values and the residuals. This can be
    #' helpful for evaluating the accuracy of the model. All inputs
    #' are assumed to be vectors.
    
    # Consolidating actual and predicted values in data frames
    ResultsForVal <- data.frame(ActualVal, PredictedVal)
    names(ResultsForVal) <- c("Actual", "Predicted")
    ResultsForVal <- ResultsForVal %>%
      mutate(Residual = Predicted - Actual)
    ResultsForTest <- data.frame(ActualTest, PredictedTest)
    names(ResultsForTest) <- c("Actual", "Predicted")
    ResultsForTest <- ResultsForTest %>%
      mutate(Residual = Predicted - Actual)
    
    # Actual vs. predicted plot for val set
    ActualVsPredictedVal <-
      ggplot(ResultsForVal, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_smooth(method = lm,
                  se = FALSE,
                  color = "orange") +
      theme_bw() +
      ggtitle("Actual vs. predicted (validation set)")
    
    # Residuals plot for val set
    ResidualsVal <- ggplot(ResultsForVal, aes(x = Actual, y = Residual)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "orange") +
      theme_bw() +
      ggtitle("Residuals plot (validation set)")
    
    # Actual vs. predicted plot for test set
    ActualVsPredictedTest <-
      ggplot(ResultsForTest, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_smooth(method = lm,
                  se = FALSE,
                  color = "orange") +
      theme_bw() +
      ggtitle("Actual vs. predicted (test set)")
    
    # Residuals plot for test set
    ResidualsTest <- ggplot(ResultsForTest, aes(x = Actual, y = Residual)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "orange") +
      theme_bw() +
      ggtitle("Residuals plot (test set)")
    
    # Arrange the plots side by side
    grid.arrange(ActualVsPredictedVal,
                 ResidualsVal,
                 ActualVsPredictedTest,
                 ResidualsTest,
                 ncol = 2)
  }


# Custom function to check whether a vector is constant ====

CheckConstant <- function(VectorValues) {
  #' Checks for whether the values of a selected vecor are constant.
  #' Returns a logical.
  
  if (var(VectorValues) == 0) {
    Constant <- TRUE
  } else {
    Constant <- FALSE
  }
  return(Constant)
}


# Custom function to forward fill data N number of times ====

ForwardFill <- function(DataFrame,
                        SelectedCountry,
                        SelectedVariable,
                        N_Repeats) {
  #' This function can be used to push values forward for N_Repeats times
  #' and is useful for dealing with variables which are constant for a
  #' specific country.
  
  # Filtering the data
  FilteredData <- DataFrame %>%
    filter(Country == SelectedCountry)
  
  # Getting, repeating and returning the value
  Predictions <- rep(min(FilteredData[[SelectedVariable]], na.rm = TRUE),
                     N_Repeats)
  return(Predictions)
}


# Custom function to fit a linear model and get metrics or predictions ====

FitLinear <-
  function(DataFrame,
           SelectedCountry,
           SelectedVariable,
           ReturnType = "RMSE") {
    #' This function filters the input data frame based on "Country"
    #' and focuses on a specific "Variable", for which it fits a
    #' regression where "Variable" is a function of time ("Year").
    #' Assumes a linear relationship between Y and X. Returns either
    #' the RMSE score or the predictions generated.
    
    # Filtering the data
    FilteredData <- DataFrame %>%
      filter(Country == SelectedCountry)
    
    # Fitting a model and generating predictions
    ModelFormula <-
      as.formula(paste(SelectedVariable, "~ Year", sep = ""))
    Model <- lm(ModelFormula, FilteredData)
    Predictions <- predict(Model, FilteredData)
    
    # Calculating RMSE score
    RMSE <-
      100 * sqrt(mean((FilteredData[[SelectedVariable]] - Predictions)^2))
    
    # Returning either RMSE or the generated predictions
    if (ReturnType == "RMSE") {
      return(RMSE)
    } else {
      return(Predictions)
    }
  }


# Custom function to fit a quadratic model and get metrics or predictions ====

FitQuadratic <-
  function(DataFrame,
           SelectedCountry,
           SelectedVariable,
           ReturnType = "RMSE") {
    #' This function filters the input data frame based on "Country"
    #' and focuses on a specific "Variable", for which it fits a
    #' regression where "Variable" is a function of time ("Year").
    #' Assumes a quadratic relationship between Y and X. Returns either
    #' the RMSE score or the predictions generated.
    
    # Filtering the data and adding more variables
    FilteredData <- DataFrame %>%
      filter(Country == SelectedCountry) %>%
      mutate(Year2 = Year ^ 2)
    
    # Fitting a model and generating predictions
    ModelFormula <-
      as.formula(paste(SelectedVariable, "~ Year + Year2", sep = ""))
    Model <- lm(ModelFormula, FilteredData)
    Predictions <- predict(Model, FilteredData)
    
    # Calculating RMSE score
    RMSE <-
      100 * sqrt(mean((FilteredData[[SelectedVariable]] - Predictions)^2))
    
    # Returning either RMSE or the generated predictions
    if (ReturnType == "RMSE") {
      return(RMSE)
    } else {
      return(Predictions)
    }
  }


# Custom function to fit a cubic model and get metrics or predictions ====

FitCubic <-
  function(DataFrame,
           SelectedCountry,
           SelectedVariable,
           ReturnType = "RMSE") {
    #' This function filters the input data frame based on "Country"
    #' and focuses on a specific "Variable", for which it fits a
    #' regression where "Variable" is a function of time ("Year").
    #' Assumes a cubic relationship between Y and X. Returns either
    #' the RMSE score or the predictions generated.
    
    # Filtering the data and adding more variables
    FilteredData <- DataFrame %>%
      filter(Country == SelectedCountry) %>%
      mutate(Year2 = Year ^ 2,
             Year3 = Year ^ 3)
    
    # Fitting a model and generating predictions
    ModelFormula <-
      as.formula(paste(SelectedVariable, "~ Year + Year3", sep = ""))
    Model <- lm(ModelFormula, FilteredData)
    Predictions <- predict(Model, FilteredData)
    
    # Calculating RMSE score
    RMSE <-
      100 * sqrt(mean((FilteredData[[SelectedVariable]] - Predictions)^2))
    
    # Returning either RMSE or the generated predictions
    if (ReturnType == "RMSE") {
      return(RMSE)
    } else {
      return(Predictions)
    }
  }


# Custom function to fit an autoregressive model and get metrics or predictions ====

FitAutoReg <-
  function(DataFrame,
           SelectedCountry,
           SelectedVariable,
           Lags = c(1),
           ReturnType = "RMSE") {
    #' This function filters the input data frame based on "Country"
    #' and focuses on a specific "Variable", for which it fits a
    #' regression where "Variable" is a function of its value in the
    #' preceding time period ("Year"). Allows for testing multiple lags
    #' at the same time but defaults to testing with only 1 lag.
    #' Returns either the RMSE score or the predictions generated.
    
    # Filtering the data
    FilteredData <- DataFrame %>%
      filter(Country == SelectedCountry)
    
    # Creating lag variables
    LagVars <- c()
    for (lag in Lags) {
      Varname <- paste0("Variable_lag_", lag)
      FilteredData[[Varname]] = lag(FilteredData[[SelectedVariable]], lag)
      LagVars <- append(LagVars, Varname)
    }
    
    # Creating a formula containing all relevant lags
    LagVars <- paste(LagVars, collapse = " + ")
    
    # Fitting a model and generating predictions
    ModelFormula <-
      as.formula(paste(SelectedVariable, " ~ ", LagVars, sep = ""))
    Model <- lm(ModelFormula, FilteredData)
    Predictions <- predict(Model, FilteredData)
    
    # Calculating RMSE score
    RMSE <-
      100 * sqrt(mean((FilteredData[[SelectedVariable]] - Predictions)^2))
    
    # Returning either RMSE or the generated predictions
    if (ReturnType == "RMSE") {
      return(RMSE)
    } else {
      return(Predictions)
    }
  }


# Custom function to generate step-wise predictions using an autoregressive model ====

PredictAutoReg <-
  function(DataFrame,
           SelectedCountry,
           SelectedVariable,
           Lags = c(1)) {
    #' This function filters the input data frame based on "Country"
    #' and focuses on a specific "Variable", for which it fits a
    #' regression where "Variable" is a function of its value in the
    #' preceding time period ("Year"). Allows for testing multiple lags
    #' at the same time but defaults to testing with only 1 lag.
    #' Generates predictions one at a time so as to ensure that there
    #' won't be any missing values in the output vector.
    
    # Filtering the data
    FilteredData <- DataFrame %>%
      filter(Country == SelectedCountry)
    
    # Creating lag variables
    LagVars <- c()
    for (lag in Lags) {
      Varname <- paste0("Variable_lag_", lag)
      FilteredData[[Varname]] = lag(FilteredData[[SelectedVariable]], lag)
      LagVars <- append(LagVars, Varname)
    }
    
    # Creating a formula containing all relevant lags
    LagVars <- paste(LagVars, collapse = " + ")
    
    # Fitting a model
    ModelFormula <-
      as.formula(paste(SelectedVariable, " ~ ", LagVars, sep = ""))
    Model <- lm(ModelFormula, FilteredData)
    
    # Generating predictions one step at a time
    # Note: we re-calculate the lags after each prediction
    Iterations <- sum(is.na(FilteredData[[SelectedVariable]]))
    i <- 0
    while (i <= Iterations) {
      # We generate predictions and impute them for missing actual values
      FilteredData[["PredictedValue"]] <- predict(Model, FilteredData)
      FilteredData[[SelectedVariable]] <- ifelse(is.na(FilteredData[[SelectedVariable]]), FilteredData[["PredictedValue"]], FilteredData[[SelectedVariable]])
      # We re-calculate the model's input variables so we can make more predictions
      for (lag in Lags) {
        Varname <- paste0("Variable_lag_", lag)
        FilteredData[[Varname]] = lag(FilteredData[[SelectedVariable]], lag)
        LagVars <- append(LagVars, Varname)
      }
      i <- i + 1
    }
    
    Predictions <- FilteredData[["PredictedValue"]]
    return(Predictions)
  }


# Custom function to automatically fit all model types and record metrics ====

AutoFitModels <- function(DataFrame,
                          SelectedCountry,
                          SelectedVariable) {
  #' Filters the data for a specific country and variable.
  #' Checks whether the variable is constant or not. If not,
  #' fits a series of linear regression models, then calculates
  #' MAPE score and notes down the best performing model. Returns
  #' a list with summarized results based on the MAPE scores.
  
  # Filtering the data
  FilteredData <- DataFrame %>%
    filter(Country == SelectedCountry)
  
  # Checking whether the variable is constant
  VarIsConstant <- CheckConstant(FilteredData[[SelectedVariable]])
  
  # If the variable is not constant, fit LM models and record their MAPE scores
  if (!VarIsConstant) {
    LM_Linear <- FitLinear(DataFrame, SelectedCountry, SelectedVariable, "RMSE")
    LM_Quadratic <- FitQuadratic(DataFrame, SelectedCountry, SelectedVariable, "RMSE")
    LM_Cubic <- FitCubic(DataFrame, SelectedCountry, SelectedVariable, "RMSE")
    LM_Auto1 <- FitAutoReg(DataFrame, SelectedCountry, SelectedVariable, c(1), "RMSE")
    LM_Auto2 <- FitAutoReg(DataFrame, SelectedCountry, SelectedVariable, c(1:2), "RMSE")
    LM_Auto3 <- FitAutoReg(DataFrame, SelectedCountry, SelectedVariable, c(1:3), "RMSE")
    ModelResults <- list("Linear" = LM_Linear, "Quadratic" = LM_Quadratic, "Cubic" = LM_Cubic, "AutoReg1Lags" = LM_Auto1, "AutoReg2Lags" = LM_Auto2, "AutoReg3Lags" = LM_Auto3, "Constant" = NA)
  } else {
    ModelResults <- list("Linear" = NA, "Quadratic" = NA, "Cubic" = NA, "AutoReg1Lags" = NA, "AutoReg2Lags" = NA, "AutoReg3Lags" = NA, "Constant" = 0)
  }
  return(ModelResults)
}