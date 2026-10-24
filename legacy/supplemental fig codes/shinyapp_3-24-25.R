library(shiny)
library(readxl)
library(DT)
library(writexl)
library(dplyr)
library(tidyr)
library(officer)
library(flextable)

ui <- fluidPage(
  titlePanel("📊 Summary Stats Formatter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      downloadButton("download_dw", "Download Dry Weight Table"),
      downloadButton("download_af", "Download As Fed Table"),
      downloadButton("download_kcal", "Download kcal Table")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  process_data <- reactive({
    req(input$file)
    tryCatch({
      df <- read_excel(input$file$datapath, sheet = "Combined data") %>%
        filter(!grepl("Fresh", Type, ignore.case = TRUE)) %>%
        filter(Type %in% c("Canned", "Kibble"))
      
      col_map <- list(
        `Dry Weight (ug/g)` = c("CML_ug_per_g_food", "MG_ug_per_g_food", "SF_ug_per_g_food", "LF_AGE_ug_per_g_food"),
        `As Fed (ug/g)` = c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist"),
        `As Fed (ug/kcal)` = c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "SF_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist")
      )
      
      result <- list()
      metric_order <- c("CML", "MG", "CML + MG", "Hydrophilic Fl", "Hydrophobic Fl", "Hydrophilic Fl + hydrophobic Fl", "Total AGE")
      
      for (norm_label in names(col_map)) {
        cols <- col_map[[norm_label]]
        df_sub <- df %>% select(Type, all_of(cols))
        df_sub[cols] <- lapply(df_sub[cols], function(x) as.numeric(as.character(x)))
        
        df_sub <- df_sub %>%
          mutate(
            `CML` = get(cols[1]),
            `MG` = get(cols[2]),
            `CML + MG` = get(cols[1]) + get(cols[2]),
            `Hydrophilic Fl` = get(cols[3]),
            `Hydrophobic Fl` = get(cols[4]),
            `Hydrophilic Fl + hydrophobic Fl` = `Hydrophilic Fl` + `Hydrophobic Fl`,
            `Total AGE` = `CML` + `MG` + `Hydrophilic Fl` + `Hydrophobic Fl`
          ) %>%
          select(Type, all_of(metric_order))
        
        df_long <- df_sub %>% pivot_longer(-Type, names_to = "MeasureLabel", values_to = "Value")
        
        summary <- df_long %>%
          group_by(MeasureLabel, Type) %>%
          summarise(
            Mean = round(mean(Value, na.rm = TRUE), 2),
            Median = round(median(Value, na.rm = TRUE), 2),
            `Q1-Q3` = paste0(round(quantile(Value, 0.25, na.rm = TRUE), 2), "–", round(quantile(Value, 0.75, na.rm = TRUE), 2)),
            `Min-Max` = paste0(round(min(Value, na.rm = TRUE), 2), "–", round(max(Value, na.rm = TRUE), 2)),
            .groups = 'drop'
          ) %>%
          mutate(MeasureLabel = factor(MeasureLabel, levels = metric_order)) %>%
          arrange(MeasureLabel)
        
        summary_with_spacers <- summary %>%
          group_by(MeasureLabel) %>%
          do(bind_rows(., tibble(MeasureLabel = "", Type = "", Mean = NA, Median = NA, `Q1-Q3` = NA, `Min-Max` = NA))) %>%
          ungroup()
        
        result[[norm_label]] <- summary_with_spacers
      }
      
      result
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$table <- renderDT({
    req(process_data())
    datatable(bind_rows(process_data()), options = list(pageLength = 50, scrollX = TRUE))
  })
  
  render_ft_doc <- function(data, title, file) {
    data <- data %>% filter(!(is.na(Mean) & is.na(Median) & is.na(`Q1-Q3`) & is.na(`Min-Max`)))
    doc <- read_docx()
    doc <- body_add_par(doc, title, style = "heading 1")
    ft <- flextable(as.data.frame(data))
    ft <- autofit(ft)
    ft <- bold(ft, i = ~Type == "Canned", bold = TRUE)
    ft <- bg(ft, i = ~Type == "Canned", bg = "#000000")
    ft <- color(ft, i = ~Type == "Canned", color = "white")
    ft <- bg(ft, i = ~Type == "Kibble", bg = "#DDDDDD")
    ft <- set_header_labels(ft, Mean = "Mean", Median = "Median", `Q1-Q3` = "Q1–Q3", `Min-Max` = "Min–Max")
    doc <- body_add_flextable(doc, ft)
    print(doc, target = file)
  }
  
  output$download_dw <- downloadHandler(
    filename = function() paste0("Dry_Weight_Table_", Sys.Date(), ".docx"),
    content = function(file) render_ft_doc(process_data()[["Dry Weight (ug/g)"]], "1. Dry Weight (ug/g)", file)
  )
  
  output$download_af <- downloadHandler(
    filename = function() paste0("As_Fed_Table_", Sys.Date(), ".docx"),
    content = function(file) render_ft_doc(process_data()[["As Fed (ug/g)"]], "2. As Fed (ug/g)", file)
  )
  
  output$download_kcal <- downloadHandler(
    filename = function() paste0("Kcal_Table_", Sys.Date(), ".docx"),
    content = function(file) render_ft_doc(process_data()[["As Fed (ug/kcal)"]], "3. As Fed (ug/kcal)", file)
  )
}

shinyApp(ui, server)

