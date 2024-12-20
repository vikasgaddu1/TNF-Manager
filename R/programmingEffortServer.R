programmingEffortServer <- function(id, tracker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status_colors <- c(
      "Not Started" = "#E0E0E0",
      "Production Started" = "#BBE1FA",
      "Production Ready" = "#C8E6C9",
      "Under QC" = "#FFF9C4",
      "QC Failed" = "#FFCDD2",
      "QC Pass" = "#A5D6A7"
    )
    
    # Reactive dataset
    data <- reactive({
      tracker_data()
    })
    
    # Calculate percentage done for each report_type
    percentage_done <- reactive({
      data() %>%
        mutate(status = case_when(
          status == "QC Pass" ~ "Done",
          TRUE ~ "In Progress"
        )) %>%
        group_by(priority,report_type) %>%
        summarise(
          total = n(),
          done = sum(status == "Done"),
          percentage_done = round((done / total) * 100, 2),
          .groups = "drop"
        )
    })
    

    # Render dynamic valueBoxes
    output$valueBoxes <- renderUI({
      percentage_done_df <- percentage_done()  # Store the reactive data frame

      # Use pmap with correct arguments
      value_boxes <- purrr::pmap(
        list(
          report_type = percentage_done_df$report_type,
          priority = percentage_done_df$priority,
          percentage_done = percentage_done_df$percentage_done,
          color = case_when(
            percentage_done_df$percentage_done >= 75 ~ "green",
            percentage_done_df$percentage_done >= 50 ~ "yellow",
            TRUE ~ "red"
          )
        ),
        function(report_type, priority,percentage_done, color) {
          valueBox(
            paste0(percentage_done, "%"), 
            subtitle = paste0("Priority ", priority,": ",report_type), 
            color = color, 
            icon = icon("chart-bar"),
            width = 2
          )
        }
      )
      
      # Combine all value boxes into a single UI element
      tagList(value_boxes)
    })
    
    
    
    
    # Filter data for tasks due in 3 days (excluding QC Pass)
    output$dueSoon <- renderDT({
      due_soon <- data() %>%
        mutate(due_date = as.Date(due_date)) %>%
        filter(due_date <= Sys.Date() + 3 & due_date >= Sys.Date() & status != "QC Pass") %>%
        select(
          Report_Key = report_key, 
          Production_Programmer = production_programmer, 
          QC_Programmer = qc_programmer, 
          Assign_Date = assign_date, 
          Due_Date = due_date, 
          Priority = priority,
          Status = status
        )
      br()
      datatable(due_soon) %>% 
        DT::formatStyle("Status",
                        target = "row",
                        backgroundColor = DT::styleEqual(
                          names(status_colors),
                          as.vector(status_colors)
                        ))
    })
    
    # Filter data for past due tasks (excluding QC Pass)
    output$pastDue <- renderDT({
      past_due <- data() %>%
        mutate(due_date = as.Date(due_date)) %>%
        filter(due_date < Sys.Date() & status != "QC Pass") %>%
        select(
          Report_Key = report_key, 
          Production_Programmer = production_programmer, 
          QC_Programmer = qc_programmer, 
          Assign_Date = assign_date, 
          Due_Date = due_date, 
          Priority = priority,
          Status = status
        )
      datatable(past_due) %>% 
        DT::formatStyle("Status",
                        target = "row",
                        backgroundColor = DT::styleEqual(
                          names(status_colors),
                          as.vector(status_colors)
                        ))
    })
    
    workload_table <- reactive({
      data() %>%
        select(production_programmer, qc_programmer) %>%
        mutate(
          production_programmer = ifelse(is.na(production_programmer), "Unassigned", production_programmer),
          qc_programmer = ifelse(is.na(qc_programmer), "Unassigned", qc_programmer)
        ) %>%
        pivot_longer(
          cols = c(production_programmer, qc_programmer),
          names_to = "role",
          values_to = "Programmer"
        ) %>%
        mutate(
          role = case_when(
            role == "production_programmer" ~ "Production",
            role == "qc_programmer" ~ "QC"
          ),
          Programmer = factor(Programmer, levels = c(setdiff(unique(Programmer), "Unassigned"), "Unassigned"))
        ) %>%
        select(role, Programmer) 
    })
    
    output$statusPriorityChart <- renderPlotly({
      # Prepare summary data
      chart_data <- data() %>%
        group_by(report_type, status, priority) %>%
        summarise(count = n(), .groups = "drop")
      
      # Create a stacked bar chart with borders
      plot_ly(
        data = chart_data,
        x = ~report_type,
        y = ~count,
        color = ~status,
        colors = status_colors,
        text = ~paste("Priority:", priority, "<br>Count:", count),
        type = 'bar',
        hoverinfo = 'text',
        marker = list(
          line = list(
            color = 'black',  # Border color
            width = 1         # Border width
          )
        )
      ) %>%
        layout(
          title = "Report Type vs Status (Grouped by Priority)",
          xaxis = list(title = "Report Type"),
          yaxis = list(
            title = "Count",
            tickmode = "linear",  # Linear tick mode
            dtick = 1,            # Force ticks to be at integer intervals
            rangemode = "tozero"  # Ensure the y-axis starts at 0
          ),
          barmode = 'stack'  # Stacked bars
        )
    })
    
    
    # Programmer workload table with a single programmer column and roles
    output$summaryTable1 <- render_gt({
      workload_table() %>%
        gtsummary::tbl_summary(
          by = role,                           # Grouping by role
          statistic = all_categorical() ~ "{n} ({p}%)",  # Count and percentages
          missing = "no"                       # Exclude missing values
        ) %>%
        add_overall() %>%                      # Adds a Total Column
        modify_header(label = "") %>%
        as_gt() %>% 
        tab_header(md("**Table 1. Work Distribution of Programmers by Role**"))
    })
    
    output$summaryTable2 <- render_gt({
      workload_table() %>%
        gtsummary::tbl_summary(
          by = Programmer,                           # Grouping by role
          statistic = all_categorical() ~ "{n} ({p}%)",  # Count and percentages
          missing = "no"                       # Exclude missing values
        ) %>%
        add_overall() %>%                      # Adds a Total Column
        modify_header(label = "") %>%
        as_gt() %>% 
        tab_header(md("**Table 2. Work Distribution of Role by Programmer**"))
    })
    
    output$statusPieChart <- renderPlotly({
      # Prepare task summary
      task_summary <- data() %>%
        group_by(status) %>%
        summarise(TaskCount = n(), .groups = "drop")
      
      # Create a pie chart with custom colors
      plot_ly(
        data = task_summary,
        labels = ~status,
        values = ~TaskCount,
        type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial',
        marker = list(colors = status_colors[task_summary$status]) # Map colors dynamically
      ) %>%
        layout(
          title = "Status Summary",
          showlegend = TRUE
        )
    })
    
    
    # Interactive plot for tasks grouped by priority and status with custom colors
    output$taskPlot <- renderPlotly({
      task_summary <- data() %>%
        group_by(priority, status) %>%
        summarise(TaskCount = n(), .groups = "drop")
      
      plot_ly(
        task_summary,
        x = ~priority,
        y = ~TaskCount,
        type = 'bar',
        color = ~status,
        colors = status_colors,
        text = ~paste("Priority:", priority, "<br>Status:", status, "<br>Tasks:", TaskCount),
        hoverinfo = 'text',
        marker = list(
          line = list(
            color = 'black',  # Border color
            width = 1         # Border width
          )
        )
      ) %>%
        layout(
          title = "Task Summary by Priority and Status",
          xaxis = list(
            title = "Priority",
            tickangle = 0,          # Ensures ticks are horizontal
            tickmode = "array",     # Specify tick positions and labels manually
            tickvals = unique(task_summary$priority), # Align ticks with the bars
            ticktext = unique(task_summary$priority)  # Display priority labels at tick positions
          ),
          yaxis = list(
            title = "Task Count",
            tickmode = "linear",    # Linear tick mode
            dtick = 1,             # Force ticks to be at integer intervals
            rangemode = "tozero"    # Ensure the y-axis starts at 0
          )
        )
    })
    
  })
}