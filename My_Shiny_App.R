library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(janitor)
library(pROC)
library(caret)
library(ggplot2)
library(car)
library(lmtest)

# Main Function
analyze_and_model <- function(df, response_var = NULL, outlier_method = "IQR") {
  
  # 1. Data Cleaning
  
  #Converts all column names into standard format (lowercase + underscores)
  df <- janitor::clean_names(df)
  if (!is.null(response_var)) {   # Fix response_var name to match cleaned column names
    response_var <- janitor::make_clean_names(response_var)}
  
  # Removes duplicate rows
  df <- unique(df)
  
  # Remove Identification Columns
  # Detect columns where EVERY row is unique
  all_unique_cols <- sapply(df, function(col) { 
    length(unique(na.omit(col))) == nrow(df) 
  })
  # Remove columns specifically named 'id' or ending in '_id' 
  named_id_cols <- grepl("^id$|_id$", names(df)) 
  
  cols_to_remove <- all_unique_cols | named_id_cols 
  df <- df[, !cols_to_remove, drop = FALSE]
  
  # Text Cleaning
  # Replace empty strings with NA and trim whitespace
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[x == "" | x == " "] <- NA
      return(trimws(tolower(x)))
    }
    return(x)
  })
  
  # Data Type conversion
  for (col in names(df)) {
    vals <- df[[col]]
    
    if (is.character(vals)) { # Detects text columns
      suppressWarnings(num <- as.numeric(vals)) 
      
      # If ≥80% values convert successfully, treat as numeric
      if (sum(!is.na(num)) / length(vals) > 0.8) {
        df[[col]] <- num
      } else {       # Otherwise treat as categorical variable
        df[[col]] <- as.factor(vals)
      }
    }
    
    # Convert binary numeric (0,1) variables into factor
    if (is.numeric(df[[col]])) {
      unique_vals <- na.omit(unique(df[[col]]))
      if (length(unique_vals) <= 2 && all(unique_vals %in% c(0,1))) {
        df[[col]] <- as.factor(df[[col]])
      }
    }
  }
  
  
  # 2. Variable Classification

  # Extracts numerical variables
  quant_vars <- names(df)[sapply(df, is.numeric)]
  # Extracts categorical variables
  qual_vars  <- names(df)[sapply(df, is.factor)]
  

  # 3. Missing Value Handling

  # Counts missing values per column
  missing_counts <- colSums(is.na(df))
  
  # Mode Function: Finds most frequent category and uses for categorical imputation
  get_mode <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Imputation Strategy: 
  # Numeric- Mean Imputation
  # Categorical- Mode Imputation
  for (var in names(df)) {
    if (any(is.na(df[[var]]))) {
      if (var %in% quant_vars) {
        df[[var]][is.na(df[[var]])] <- mean(df[[var]], na.rm = TRUE)
      } else {
        df[[var]][is.na(df[[var]])] <- get_mode(df[[var]])
      }
    }
  }
  

  # 4. Outlier Detection
 
  outlier_report <- list()
  outlier_plots <- list()
  
  for (var in quant_vars) {
    x <- df[[var]]
    
    #IQR Method
    if (outlier_method == "IQR") {
      Q1 <- quantile(x, 0.25)
      Q3 <- quantile(x, 0.75)
      IQR_val <- Q3 - Q1
      
      # Standard statistical outlier rule
      outliers <- x[x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val)]
      
      # Visualization: Boxplot (IQR)
      p <- ggplot(df, aes(x = .data[[var]], y = 0)) +
        geom_boxplot(alpha = 0.8, fill = "steelblue", outlier.color = "red",  outlier.alpha = 0.6) +
        labs(title = paste("IQR Outlier Detection -", var), x = var, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        theme( plot.title = element_text(face = "bold"), legend.position = "none",
               panel.grid.minor = element_blank())
      
      # Z- Score Method
      } else if (outlier_method == "Z-Score") {
       
      z <- abs(scale(x))  # Standardizes values
      outliers <- x[z > 3]   # Values beyond + or - 3 SD considered outliers
      
      # Visualization: Density plot (Z-score)
      p <- ggplot(df, aes(x = .data[[var]])) +
        geom_density(fill = "steelblue", alpha = 0.4) +
        geom_vline(xintercept = mean(x, na.rm = TRUE), linetype = "dashed", color = "blue") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Z-Score Density Plot -", var), x = var, y = "Density") +
        theme( plot.title = element_text(face = "bold"), legend.position = "none",
               panel.grid.minor = element_blank())
      
    } else {
        stop("Invalid outlier method")
    }
    
    outlier_report[[var]] <- outliers
    outlier_plots[[var]] <- p
    
  }
  outlier_counts <- sapply(outlier_report, length)
  

  # 5. Data Visualization

  plot_list <- list()
  
  # First: Categorical Variables (Vertical Bar Charts)
  for (col in qual_vars) {
    
    freq_table <- as.data.frame(table(df[[col]]))
    names(freq_table) <- c("Category", "Count")
    
    p <- ggplot(freq_table, aes(x = Count, y = reorder(Category, -Count))) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.7) +
      # value labels at end of bars
      geom_text(aes(label = Count), hjust = -0.2, size = 3) +
      labs( title = paste("Bar Chart Distribution of", col), x = "Count", y = col) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(hjust = 1), plot.title = element_text(hjust = 1, face = "bold"), 
            panel.grid.minor = element_blank())
    
    plot_list[[col]] <- p
  }
  
  # Second: Numerical Variables (Histogram Charts)
  for (col in quant_vars) {
    
    range_val <- max(df[[col]], na.rm = TRUE) - min(df[[col]], na.rm = TRUE)
    bin_w <- range_val / 30
    
    p <- ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(fill = "steelblue",  color = "black", binwidth = bin_w, alpha = 0.8 ) +
      labs( title = paste("Histogram Distribution of", col), x = col, y = "Count") +
      theme_minimal(base_size = 14)+
      theme(plot.title = element_text(face = "bold"),panel.grid.minor = element_blank())
                    
    plot_list[[col]] <- p
  }
  

  # 6. Model Training
  
  model_summary <- NULL
  model_obj <- NULL
  model_type <- NULL
  model_metrics <- list()
  model_plots <- list()
  model_diagnostics <- NULL
  
  if (!is.null(response_var) && response_var %in% names(df)) {
    tryCatch({
      
      y <- df[[response_var]]
      
      # Split Data: 80% training / 20% testing
      set.seed(123)
      train_index <- createDataPartition(y, p = 0.8, list = FALSE)
      
      train <- df[train_index, ]
      test <- df[-train_index, ]
      
      
      # Regression Model
      
      if (is.numeric(y)) {
        
        # Applies Linear Regression Model
        m <- lm(as.formula(paste(response_var, "~ .")), data = train)
        
        # Stepwise Selection: Iteratively removes variables to find the most efficient model (lowest AIC)
        m <- step(m, direction = "both", trace = 0)
        
        model_type <- "lm"
        model_summary  <- summary(m)
        model_obj <- m
        
        # Diagnostic Tests: Check if Linear Regression assumptions hold true.
        model_diagnostics <- list(
          # Homoscedasticity (Variance of residuals must be constant)
          homoscedasticity = car::ncvTest(m), 
          # Independence (Observations should not be correlated)
          independence = car::durbinWatsonTest(m),
          # Multicollinearity: Predictors shouldn't overlap too much
          multicollinearity = tryCatch(car::vif(m), error = function(e) "VIF not applicable")
        )
        
        # Regression Performance
        preds <- predict(m, test)
        y_train <- train[[response_var]]
        y_test  <- test[[response_var]]
        
        # Performance Metrics
        model_metrics <- list(
          RMSE = sqrt(mean((y_test - preds)^2)),
          MAE = mean(abs(y_test - preds)),
          MAPE = mean(abs((y_test - preds) / y_test)) * 100
        )
        
        # Model Diagnostic Plots
        # Residual Plots to see if the model missed a pattern.
        
        df_res <- data.frame(fitted = preds, residuals = y_test - preds)
        
        # Residuals vs Fitted
        model_plots$residual_plot <- ggplot(df_res, aes(fitted, residuals)) +
          geom_point(alpha = 0.5, color = "#2C7FB8") +
          geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +
          geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
          labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"), legend.position = "none",
                panel.grid.minor = element_blank())
        
        # Residual histogram
        model_plots$residual_hist <- ggplot(df_res, aes(x = residuals)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "#2C7FB8", color = "black", alpha = 0.7) +
          labs(title = "Histogram of Residuals", x = "Residuals", y = "Density") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"), legend.position = "none",
                panel.grid.minor = element_blank())
        
        # Normality of Residuals (Normal Q-Q)
        model_plots$qq_plot <-ggplot(df_res, aes(sample = residuals)) +
          stat_qq(color = "#2C7FB8", alpha = 0.5) +
          stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +
          labs(title = "Q-Q Plot (Residual Normality)", x = "Theoretical Quantiles", y = "Sample Quantiles"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"), legend.position = "none",
                panel.grid.minor = element_blank())
        
        # Actual vs Predicted
        model_plots$actual_vs_pred <- ggplot(
          data.frame(actual = y_test, predicted = preds),
          aes(x = actual, y = predicted)) +
          geom_point(color = "#2C7FB8") + geom_abline(linetype = "dashed") +
          labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
          theme_minimal(base_size = 14) +
          theme( plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
      }
      
      
      # Classification Model
   
      else if (is.factor(y) && nlevels(y) == 2) {
        
        # Logistic Regression: Predicts the probability of a binary outcome
        m <- glm(as.formula(paste(response_var, "~ .")), data = train, family = binomial)
        # Stepwise Selection
        m <- step(m, direction = "both", trace = 0)
        
        model_type <- "glm"
        model_summary  <- summary(m)
        model_obj <- m
        
        # Classification Performance: Accuracy, Precision, and Recall
        probs <- predict(m, newdata = test, type = "response")
       
        y_train <- train[[response_var]]
        y_test  <- test[[response_var]]
        
        positive_class <- levels(y_train)[2]
        #Default threshold of 0.5 used; can be optimized using ROC curve
        preds <- ifelse(probs > 0.5, positive_class, levels(y_train)[1])
        preds <- factor(preds, levels = levels(y_train))
        y_test <- factor(y_test, levels = levels(y_train))
        
        cm <- confusionMatrix(preds, y_test)
        roc_obj <- roc(y_test, probs)
        
        model_metrics <- list(
          Accuracy = cm$overall["Accuracy"],
          Precision = cm$byClass["Precision"],
          Recall = cm$byClass["Recall"],
          F1 = cm$byClass["F1"],
          AUC = auc(roc_obj)
        )
        
        # ROC Plot
        model_plots$roc <- ggplot(data.frame(tpr = roc_obj$sensitivities, fpr = 1 - roc_obj$specificities),
                                  aes(fpr, tpr)) +
          geom_line(color = "blue") +
          geom_abline(linetype = "dashed") +
          labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"), legend.position = "none",
                panel.grid.minor = element_blank())
        
        # Confusion Matrix Plot
        cm_df <- as.data.frame(cm$table)
        model_plots$confusion_matrix <- ggplot(cm_df, aes(Prediction, Reference, fill = Freq)) +
          geom_tile() + geom_text(aes(label = Freq)) +
          scale_fill_gradient(low = "white", high = "red") +
          labs(title = "Confusion Matrix") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"), legend.position = "none",
                panel.grid.minor = element_blank())
        
        # Feature Importance
        coef_df <- data.frame(Feature = names(coef(m))[-1], Coefficient = coef(m)[-1])
        coef_df$Importance <- abs(coef_df$Coefficient)
        
        model_plots$feature_importance <- ggplot(coef_df, aes(x = reorder(Feature, Coefficient),
                                                              y = Coefficient)) +
          geom_bar(stat = "identity", fill = "steelblue") + coord_flip() +
          labs(title = "Feature Importance", x = "Features", y = "Coefficient")+
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"), legend.position = "none",
                panel.grid.minor = element_blank())
        }
      
   }, error = function(e) {
      model_summary <- NULL
      model_obj <- NULL })
  }
  
return(list(
      clean_data = df,
      quant = quant_vars,
      qual = qual_vars,
      missing = missing_counts,
      outliers = outlier_report,
      outlier_counts = outlier_counts,
      outlier_plots = outlier_plots,
      plots = plot_list,
      model_summary = model_summary,
      model_metrics = model_metrics,
      model_type = model_type,
      model_plots = model_plots,
      model_diagnostics = model_diagnostics,
      model_obj = model_obj
  ))
}



# User Interface (UI)

ui <- bs4DashPage(
  title = "Data Analysis Dashboard", 
  header = bs4DashNavbar( title = "AutoAnalytic Pro" ),
  
  # Side Bar (Navigation Menu)
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Upload", tabName = "upload"),
      bs4SidebarMenuItem("Variable Overview", tabName = "vars"),
      bs4SidebarMenuItem("Missing Values", tabName = "missing"),
      bs4SidebarMenuItem("Outliers", tabName = "outliers"),
      bs4SidebarMenuItem("Visualization", tabName = "viz"),
      bs4SidebarMenuItem("Model Summary", tabName = "model_summary"),
      bs4SidebarMenuItem("Model Diagnostics", tabName = "diagnostics")
    )
  ),
  
  # Main Body (All Dashboard Pages and Tabs )
  body = bs4DashBody(
  
  tags$style(HTML("
   .progress { display: none !important; }
   .card { border-radius: 10px; }
   .card-header { font-weight: 600; }
    ")),

    bs4TabItems(
      # Tab 1: Uploading files and displaying the cleaning logic.
      bs4TabItem(tabName = "upload",
                 bs4Card(width = 12,
                         title = tags$span(
                           style = "color:#1f77b4; font-weight:700; font-size:22px;",
                           "Upload Dataset"), fileInput("file", "Choose CSV File"),
          tags$hr(),
          tags$div(
            style = "padding:10px; background:#f8f9fa; border-radius:6px;",
            textOutput("upload_status")),
          
          tags$h4(
            style = "font-size:18px; color:#1f77b4; font-weight:700; margin-top:20px;",
            "Data Overview"), verbatimTextOutput("data_overview"),
          
          tags$h4(
            style = "font-size:18px; color:#1f77b4; font-weight:700; margin-top:15px;",
            "Data Cleaning Pipeline"),
          
          tags$div(
            tags$ul(
              tags$li("Column names standardized (lowercase + underscores)"),
              tags$li("Duplicate rows removed"),
              tags$li("ID-like columns removed (unique identifiers)"),
              tags$li("Empty strings converted to NA"),
              tags$li("Text cleaned (trim + lowercase)"),
              tags$li("Auto type conversion applied"),
              
              tags$ul(
                tags$li("Character → numeric (if >80% valid)"),
                tags$li("Character → factor (otherwise)"),
                tags$li("Binary numeric (0/1) → factor")
              )
            )
          ),
          tags$hr(), # Horizontal line to separate content from footer
          tags$div(
            style = "text-align: right; color: #6c757d; font-style: italic; font-size: 14px;",
            paste("Created by:", "Chooladeva Piyasiri")
          )
        )
      ),
      
      # # Tab 2: Categorizing variables (Numeric vs Factor).
      bs4TabItem(tabName = "vars",
                 bs4Card(width = 12, title = tags$span(
                   style = "color:#1f77b4; font-weight:700; font-size:22px;",
                   "Overview of Variables"), verbatimTextOutput("vars"))
                 ),
      
      # Tab 3: Identifying Missing Values
      bs4TabItem(tabName = "missing",
                 bs4Card(width = 12,
                         title = tags$span(style = "color:#1f77b4; font-weight:700; font-size:22px;",
                                           "Missing Values & Imputation"),
                 tags$div(
                   style = "padding:10px; background:#f8f9fa; border-radius:6px; margin-bottom:15px;",
                   "Missing values were successfully imputed using type-specific imputation (Mean for numeric; Mode for qualitative variables)."
                   ), DTOutput("missing"))
                 ),
      
      # Tab 4: Interactive Outlier detection
      bs4TabItem(tabName = "outliers",
                 bs4Card(width = 12,
                         title = tags$span(
                           style = "color:#1f77b4; font-weight:700; font-size:22px;", "Outlier Detection"),
                
                selectInput("outlier_method",
                            "Select Method",
                            choices = c("IQR", "Z-Score"),
                            selected = "IQR"),
                tags$hr(),
                verbatimTextOutput("outlier_text"),
                uiOutput("outlier_ui"))
                ),
      
      # Tab 5: Grid of visualization plots for every variable in the data.
      bs4TabItem(tabName = "viz",
                 bs4Card(width = 12,
                         title = tags$span(
                           style = "color:#1f77b4; font-weight:700; font-size:22px;",
                           "Data Visualization"), 
                            uiOutput("plots_ui"))
                         ),
      
      # Tab 6: Model coefficients and performance metrics.
      bs4TabItem(tabName = "model_summary",
                 bs4Card(width = 12,
                         title = tags$span(
                           style = "color:#1f77b4; font-weight:700; font-size:22px;",
                           "Model Summary"),
              tags$div(style = "padding:8px; background:#eef5ff; border-radius:6px; margin-bottom:10px;",
              "Response variables are filtered to include only continuous variables and binary categorical variables in the dropdown below."
          ),
          
          selectInput("response_var",
                      "Select Response Variable",
                      choices = NULL),
          verbatimTextOutput("model_summary"),
          
          tags$h4("Model Formula",style = "color:#1f77b4; font-size:18px; font-weight:700; margin-top:15px;"),
          verbatimTextOutput("model_call"),
          
          tags$h4("Model Coefficients",
                  style = "color:#1f77b4; font-size:18px; font-weight:700; margin-top:15px;"),
          DTOutput("coeff_table"),
          
          tags$h4("Model Statistics",
                  style = "color:#1f77b4; font-size:18px; font-weight:700; margin-top:15px;"),
          verbatimTextOutput("model_stats"),
          
          tags$h4("Performance Metrics",
                  style = "color:#1f77b4; font-size:18px; font-weight:700; margin-top:15px; margin-bottom:10px;"),
          DTOutput("metrics"),
          
          tags$div(style = "margin-top:20px;",
                   uiOutput("model_notes"))
          )
          ),
      
      # Tab 7: Model diagnostic plots.
      bs4TabItem(tabName = "diagnostics",
                 bs4Card(width = 12,
                         title = tags$span(
                           style = "color:#1f77b4; font-weight:700; font-size:22px;",
                           "Model Diagnostic Plots"), uiOutput("diagnostic_plots"),
          tags$hr(),
          uiOutput("regression_tests_ui"))
        )
    )
  )
)


# Server(Backend)

server <- function(input, output, session) {
  
  # Reactive Data Loading: Only runs when a file is uploaded.
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Initial Analysis: Runs automatically to clean the data and prep the variable list.
  clean_res <- reactive({
    req(data())
    
    analyze_and_model(
      df = data(),
      response_var = NULL,
      outlier_method = input$outlier_method
    )
  })
  
  # Model Analysis: Triggered only when the user chooses a Response variable.
  model_res <- reactive({
    req(data())
    req(input$response_var != "")
    
    analyze_and_model(
      df = data(),
      response_var = input$response_var,
      outlier_method = input$outlier_method
    )
  })
  
  # Response Variable Dropdown
  observe({
    req(clean_res())
    
    df <- clean_res()$clean_data
    
    valid_vars <- names(df)[sapply(df, function(y) {
      is.numeric(y) || (is.factor(y) && nlevels(y) == 2)
      })]
    
    updateSelectInput(
      session, "response_var",
      choices = c("Select a variable" = "", valid_vars),
      selected = ""
    )
  })
  
  # Data File Upload Status 
  output$upload_status <- renderText({
    req(input$file)
    paste("File upload status: Uploaded successfully (", input$file$name, ")")
  })
  
  output$data_overview <- renderPrint({
    req(clean_res())
    df <- clean_res()$clean_data
  
    cat("Total Rows:", nrow(df), "\n")
    cat("Total Columns:", ncol(df), "\n")
  })
  

  # Variables Tab: Rendering Outputs
  output$vars <- renderPrint({
    req(clean_res())
    
    cat("Quantitative Variables:\n")
    for (v in clean_res()$quant) cat(" • ", v, "\n")
    cat("\nQualitative Variables:\n")
    for (v in clean_res()$qual) cat(" • ", v, "\n")
  })
  
  # Missing Values Tab: Table
  output$missing <- renderDT({
    req(clean_res())
    datatable(data.frame(
      Variable = names(clean_res()$missing),
      Count = as.numeric(clean_res()$missing)
    ))
  })
  
  # Outliers Tab: Summary Text
  output$outlier_text <- renderPrint({
    req(clean_res())
    counts <- clean_res()$outlier_counts
    for (var in names(counts)) {
      cat(sprintf("Variable '%s': Found %d outliers\n", var, counts[var]))
    }
  })
  
  # Outliers Tab: Dynamic Plots
  output$outlier_ui <- renderUI({
    req(clean_res())
    lapply(names(clean_res()$outlier_plots), function(n)
      plotOutput(paste0("o_", n)))
  })
  observe({
    req(clean_res())
    lapply(names(clean_res()$outlier_plots), function(n) {
      local({
        nm <- n
        output[[paste0("o_", nm)]] <- renderPlot(clean_res()$outlier_plots[[nm]])
      })
    })
  })
  
  # Visualization Tab: Rendering Plots
  output$plots_ui <- renderUI({
    req(clean_res())

    fluidRow(
      lapply(names(clean_res()$plots), function(n) {
        column(
          width = 6,
          div(
            style = "margin-bottom:20px;",
            plotOutput(paste0("p_", n), height = "350px")
          )
        )
      })
    )
  })
  observe({
    req(clean_res())
    lapply(names(clean_res()$plots), function(n) {
      local({
        nm <- n
        output[[paste0("p_", nm)]] <- renderPlot(clean_res()$plots[[nm]])
      })
    })
  })
  
  # Model Stats & Metrics
  output$model_call <- renderPrint({
    req(model_res())
    model_res()$model_summary$call
  })
  
  output$coeff_table <- renderDT({
    req(model_res()$model_summary)
    
    coef_df <- as.data.frame(model_res()$model_summary$coefficients)
    datatable(coef_df)
  })
  
  output$model_stats <- renderPrint({
    req(model_res())
    
    s <- model_res()$model_summary
    model_type <- model_res()$model_type
    
    if (model_type == "lm") {
      
      cat("Model Type: Regression\n\n")
      cat("R-squared:", round(s$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(s$adj.r.squared, 4), "\n")
      cat("Residual Std. Error:", round(s$sigma, 2), "\n")
      
      if (!is.null(s$fstatistic)) {
        cat("F-statistic:", round(s$fstatistic[1], 2), "\n")
      }
      
    } else if (model_type == "glm") {
      
      cat("Model Type: Binary Classification\n\n")
      cat("AIC:", AIC(model_res()$model_obj), "\n")
      cat("Null Deviance:", s$null.deviance, "\n")
      cat("Residual Deviance:", s$deviance, "\n")
    }
  })
  
  output$model_notes <- renderUI({
    req(model_res())
    type <- model_res()$model_type
    if (is.null(type)) return(NULL)
    if (type == "lm") {
      
      tags$div(
        style = "padding:10px; background:#e8f5e9; border-radius:6px;",
        tags$b("Model Notes:"),
        tags$ul(
          tags$li("Linear Regression model applied"),
          tags$li("Stepwise AIC used for feature selection"),
          tags$li("R-squared indicates model explanatory power"),
          tags$li("Some variables removed to reduce multicollinearity")
        )
      )
    } else if (type == "glm") {
      tags$div(
        style = "padding:10px; background:#e8f5e9; border-radius:6px;",
        tags$b("Model Notes:"),
        tags$ul(
          tags$li("Logistic Regression model applied"),
          tags$li("Stepwise AIC used for feature selection"),
          tags$li("Model predicts probability of class membership"),
          tags$li("Some variables removed to reduce multicollinearity")
        )
      )
    }
  })
  
  output$metrics <- renderDT({
    req(model_res())
    metrics <- model_res()$model_metrics
    req(length(metrics) > 0)
    metrics_df <- data.frame(
      Metric = names(metrics),
      Value = as.numeric(metrics),
      row.names = NULL
    )
    datatable(metrics_df, options = list(dom = 't'))
  })
  
  # Diagnostics Tab: Rendering Plots
  output$diagnostic_plots <- renderUI({
    req(input$response_var != "")
    req(model_res())
    plots <- model_res()$model_plots
  
    lapply(names(plots), function(name) {
      column(width = 8, offset = 2,
             div(style = "margin-bottom:20px;", plotOutput(paste0("diag_", name), height = "400px"))
             )
      })
  })
  observe({
    req(model_res())
    req(input$response_var != "")
    plots <- model_res()$model_plots
    lapply(names(plots), function(name) {
      local({nm <- name 
      output[[paste0("diag_", nm)]] <- renderPlot(plots[[nm]])
      })
    })
  })
  
  # Diagnostics Tab: Test Results
  output$regression_tests_ui <- renderUI({
    req(model_res())
    # Only generate the cards if it is a Linear Model
    if (model_res()$model_type == "lm") {
      fluidRow(
        column(width = 12, bs4Card(title = "Homoscedasticity Test", verbatimTextOutput("ncv_result"))),
        column(width = 12, bs4Card(title = "Independence Test", verbatimTextOutput("dw_result"))),
        column(width = 12, bs4Card(title = "Multicollinearity (VIF)", verbatimTextOutput("vif_result")))
      )
    } 
  })

  # Diagnostics Tab: Test Cards
  output$regression_tests_ui <- renderUI({
    req(model_res())
    # Only generate the cards if it is a Linear Model
    if (model_res()$model_type == "lm") {
      tagList(
        bs4Card(
          title = "1. Homoscedasticity Test", 
          status = "primary", width = 12,
          verbatimTextOutput("ncv_result")
        ),
        bs4Card(
          title = "2. Independence Test (Durbin-Watson)", 
          status = "primary", width = 12,
          verbatimTextOutput("dw_result")
        ),
        bs4Card(
          title = "3. Multicollinearity (VIF)", 
          status = "primary", width = 12,
          verbatimTextOutput("vif_result")
        )
      )
    } else {
      return(NULL)
    }
  })
  # Homoscedasticity Test Result
  output$ncv_result <- renderPrint({
    req(model_res())
    req(model_res()$model_type == "lm") 
    model_res()$model_diagnostics$homoscedasticity
  })
  # Independence Test Result
  output$dw_result <- renderPrint({
    req(model_res())
    req(model_res()$model_type == "lm")
    model_res()$model_diagnostics$independence
  })
  # Multicollinearity Result
  output$vif_result <- renderPrint({
    req(model_res())
    req(model_res()$model_type == "lm")
    model_res()$model_diagnostics$multicollinearity
  })
}

shinyApp(ui, server)