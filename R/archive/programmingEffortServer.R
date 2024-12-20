programmingEffortServer <- function(id, tracker_data) {
  # cat("programmingEffortServer initialized with id:", id, "\n")  # Add this line
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # observe({
    #   cat("tracker_data contents:", str(tracker_data()), "\n")  # Add this line
    # })
    # Production Programmer vs Report Type
    output$progressPlotProd <- renderPlot({
      # req(tracker_data())
      df <- tracker_data()
      
      tryCatch({
        # Ensure we have both production_programmer and report_type
        if (!("production_programmer" %in% names(df)) ||
            !("report_type" %in% names(df))) {
          plot.new()
          title("Missing required columns for this plot")
          return()
        }
        
        # Create a contingency table of production_programmer vs report_type
        prod_type_counts <- table(df$report_type, df$production_programmer)
        
        # Convert this table to a data frame suitable for ggplot
        prod_type_df <- as.data.frame(prod_type_counts)
        colnames(prod_type_df) <- c("report_type", "production_programmer", "count")
        
        # If you want colors based on programmers, you can define them dynamically or statically.
        # For example, if you have a known set of programmers:
        # For simplicity, we'll just use a palette from RColorBrewer or a fixed set of colors.
        unique_prods <- unique(prod_type_df$production_programmer)
        prod_colors <- setNames(RColorBrewer::brewer.pal(min(length(
          unique_prods
        ), 8), "Set3")[1:length(unique_prods)],
        unique_prods)
        
        ggplot(prod_type_df,
               aes(
                 x = report_type,
                 y = count,
                 fill = production_programmer
               )) +
          geom_col(position = position_dodge()) +
          scale_fill_manual(values = prod_colors) +
          labs(title = "Reports by Type and Production Programmer",
               x = "Report Type",
               y = "Count of Reports") +
          theme_minimal()
        
      }, error = function(e) {
        message("Error in production_programmer/type plot: ", e$message)
        plot.new()
        title("Error generating production_programmer/type plot")
      })
    }, res = 96)
    
    output$progressPieType <- renderPlot({
      # req(tracker_data())
      df <- tracker_data()
      
      tryCatch({
        # Check that we have both 'status' and 'report_type'
        if (!("status" %in% names(df)) ||
            !("report_type" %in% names(df))) {
          plot.new()
          title("Missing required columns (status or report_type)")
          return()
        }
        
        # Create a data frame summarizing counts by report_type and status
        type_status_counts <- as.data.frame(table(df$report_type, df$status))
        colnames(type_status_counts) <- c("report_type", "status", "count")
        
        # Define colors for each status
        status_colors <- c(
          "Not Started" = "#E0E0E0",
          "Production Started" = "#BBE1FA",
          "Production Ready" = "#C8E6C9",
          "Under QC" = "#FFF9C4",
          "QC Failed" = "#FFCDD2",
          "QC Pass" = "#A5D6A7"
        )
        
        # Create a faceted pie chart: one pie per report_type
        # We use geom_col + coord_polar("y") as before, but now facet by report_type
        ggplot(type_status_counts, aes(
          x = "",
          y = count,
          fill = status
        )) +
          geom_col(width = 1, color = "white") +
          coord_polar("y") +
          facet_wrap(~ report_type) +
          scale_fill_manual(values = status_colors) +
          labs(title = "Status Distribution by Report Type", fill = "Status") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5),
                strip.text = element_text(size = 14))
        
      }, error = function(e) {
        message("Error in pie chart by type: ", e$message)
        plot.new()
        title("Error generating pie chart by type")
      })
    }, res = 96)
    
    
    output$progressPie <- renderPlot({
      # req(tracker_data())
      df <- tracker_data()
      
      tryCatch({
        # Create a data frame summarizing counts by status
        status_counts <- as.data.frame(table(df$status))
        colnames(status_counts) <- c("status", "count")
        
        # Define colors for each status
        status_colors <- c(
          "Not Started" = "#E0E0E0",
          "Production Started" = "#BBE1FA",
          "Production Ready" = "#C8E6C9",
          "Under QC" = "#FFF9C4",
          "QC Failed" = "#FFCDD2",
          "QC Pass" = "#A5D6A7"
        )
        
        # Ensure all statuses from the colors vector are factors in data
        # This step is optional but helpful if some statuses might not appear
        status_counts$status <- factor(status_counts$status, levels = names(status_colors))
        
        # Create a pie chart using ggplot2
        ggplot(status_counts, aes(
          x = "",
          y = count,
          fill = status
        )) +
          geom_col(width = 1, color = "white") +
          coord_polar("y") +
          scale_fill_manual(values = status_colors) +
          labs(title = "Status Distribution", fill = "Status") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        
      }, error = function(e) {
        message("Error in pie chart: ", e$message)
        plot.new()
        title("Error generating pie chart")
      })
    }, res = 96)
    
    # QC Programmer vs Report Type
    output$progressPlotQC <- renderPlot({
      # req(tracker_data())
      df <- tracker_data()
      
      tryCatch({
        # Ensure we have both qc_programmer and report_type
        if (!("qc_programmer" %in% names(df)) ||
            !("report_type" %in% names(df))) {
          plot.new()
          title("Missing required columns for this plot")
          return()
        }
        
        # Create a contingency table of qc_programmer vs report_type
        qc_type_counts <- table(df$report_type, df$qc_programmer)
        
        # Convert this table to a data frame suitable for ggplot
        qc_type_df <- as.data.frame(qc_type_counts)
        colnames(qc_type_df) <- c("report_type", "qc_programmer", "count")
        
        # Similarly define colors for QC programmers
        unique_qcs <- unique(qc_type_df$qc_programmer)
        qc_colors <- setNames(RColorBrewer::brewer.pal(min(length(unique_qcs), 8), "Set3")[1:length(unique_qcs)], unique_qcs)
        
        ggplot(qc_type_df,
               aes(
                 x = report_type,
                 y = count,
                 fill = qc_programmer
               )) +
          geom_col(position = position_dodge()) +
          scale_fill_manual(values = qc_colors) +
          labs(title = "Reports by Type and QC Programmer",
               x = "Report Type",
               y = "Count of Reports") +
          theme_minimal()
        
      }, error = function(e) {
        message("Error in qc_programmer/type plot: ", e$message)
        plot.new()
        title("Error generating qc_programmer/type plot")
      })
    }, res = 96)
    
    
    output$progressPlot <- renderPlot({
      # req(tracker_data())
      df <- tracker_data()
      
      tryCatch({
        # status_counts is a named integer vector from table(df$status)
        status_counts <- table(df$status)
        status_colors <- c(
          "Not Started" = "#E0E0E0",
          "Production Started" = "#BBE1FA",
          "Production Ready" = "#C8E6C9",
          "Under QC" = "#FFF9C4",
          "QC Failed" = "#FFCDD2",
          "QC Pass" = "#A5D6A7"
        )
        
        # Convert status_counts to a data frame for ggplot
        df_status <- as.data.frame(status_counts)
        colnames(df_status) <- c("status", "count")
        
        ggplot(df_status, aes(
          x = status,
          y = count,
          fill = status
        )) +
          geom_col() +
          scale_fill_manual(values = status_colors) +
          labs(title = "Current Status Distribution",
               x = "Status",
               y = "Count of Reports") +
          scale_y_continuous(breaks = seq(0, max(df_status$count), 1)) +
          theme_minimal()
        
      }, error = function(e) {
        message("Error in plot: ", e$message)
        plot.new()
        title("Error generating plot")
      })
    }, res = 96)
    
    output$progressPlotType <- renderPlot({
      # req(tracker_data())
      df <- tracker_data()
      
      tryCatch({
        # Ensure we have both status and report_type
        if (!("status" %in% names(df)) ||
            !("report_type" %in% names(df))) {
          plot.new()
          title("Missing required columns for this plot")
          return()
        }
        
        # Create a contingency table of status vs report_type
        type_status_counts <- table(df$report_type, df$status)
        
        # Convert this table to a data frame suitable for ggplot
        type_status_df <- as.data.frame(type_status_counts)
        colnames(type_status_df) <- c("report_type", "status", "count")
        
        # Define colors for each status
        status_colors <- c(
          "Not Started" = "#E0E0E0",
          "Production Started" = "#BBE1FA",
          "Production Ready" = "#C8E6C9",
          "Under QC" = "#FFF9C4",
          "QC Failed" = "#FFCDD2",
          "QC Pass" = "#A5D6A7"
        )
        
        # Create a grouped bar plot using ggplot
        ggplot(type_status_df,
               aes(
                 x = report_type,
                 y = count,
                 fill = status
               )) +
          geom_col(position = position_dodge()) +
          scale_fill_manual(values = status_colors) +
          labs(title = "Reports by Type and Status",
               x = "Report Type",
               y = "Count of Reports") +
          theme_minimal()
        
      }, error = function(e) {
        message("Error in type/status plot: ", e$message)
        plot.new()
        title("Error generating report type/status plot")
      })
    }, res = 96)
  })
}