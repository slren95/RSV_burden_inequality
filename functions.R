#
# ------------------------------------------------------------------------------
# Script: functions.R
# Purpose:
#   Project-wide helper functions and shared theming utilities used by analysis
#   scripts (table exports, plotting helpers, and styling defaults).
#
# Inputs:
#   - None directly. Called via `source("functions.R")` from other scripts.
#
# Outputs:
#   - Defines functions in the current R session (no files are written unless a
#     caller invokes an export helper such as `createAppendixTable()`).
#
# Usage:
#   source("functions.R")
#
# Notes:
#   Keep this file side-effect light: prefer function definitions over running
#   analysis code at import time.
# ------------------------------------------------------------------------------

library(officer)
library(flextable)
library(openxlsx)
library(showtext)
library(cowplot)

export_to_three_line_table <- function(df, file_path, header_labels = NULL) {
  
  # Create a flextable object
  ft <- flextable(df)
  ft <- autofit(ft, part = "all")
  
  # Set table width to 100% (full page width)
  ft <- set_table_properties(ft, width = 1, layout = "autofit")
  # Optional: add "three-line table" horizontal rules
  # ft <- flextable::hline(ft, i = 1, j = 1:ncol(df), border = fp_border(width = 1.5))  # header top rule
  # ft <- flextable::hline(ft, i = 2, j = 1:ncol(df), border = fp_border(width = 1.5))  # header bottom rule
  # ft <- flextable::hline(ft, i = nrow(df), j = 1:ncol(df), border = fp_border(width = 1.5))  # table bottom rule
  
  
  # Create a Word document and insert the table
  doc <- read_docx()
  doc <- doc %>% body_add_flextable(ft)
  
  # Save the Word document
  print(doc, target = file_path)
  
  cat("三线表已成功导出到文件：", file_path, "\n")
}


# Simplified helper to create an SCI-style appendix table
createAppendixTable <- function(df, file_path,rowheights=25) {
  # Create workbook
  wb <- createWorkbook()
  
  # Add worksheet and write data
  addWorksheet(wb, "Appendix Table")
  
  writeData(wb, "Appendix Table", df)
  
  # Header style: small font, Times New Roman, white text, blue fill, centered, bold
  header_style <- createStyle(
    fontSize = 10, 
    fontColour = "#FFFFFF", 
    fontName = "Times New Roman",
    fgFill = "#4F81BD", 
    halign = "CENTER", 
    valign = "CENTER", 
    textDecoration = "bold", 
    border = "TopBottomLeftRight"  # add borders for header row
  )
  
  # Body styles: wrap text, borders, zebra striping, Times New Roman
  content_style_odd <- createStyle(
    wrapText = TRUE, 
    border = "TopBottomLeftRight", 
    fgFill = "#F2F2F2",  # zebra stripe (light)
    fontSize = 8, 
    fontName = "Times New Roman",
    valign = "CENTER"  # vertical align center
  )
  
  content_style_even <- createStyle(
    wrapText = TRUE, 
    border = "TopBottomLeftRight", 
    fgFill = "#FFFFFF",  # zebra stripe (dark)
    fontSize = 8, 
    fontName = "Times New Roman",
    valign = "CENTER"  # vertical align center
  )
  
  # Apply header style (first row)
  addStyle(wb, "Appendix Table", style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  
  # Apply zebra striping styles
  for (i in 2:(nrow(df) + 1)) {
    if (i %% 2 == 0) {
      # Even rows
      addStyle(wb, "Appendix Table", style = content_style_even, rows = i, cols = 1:ncol(df), gridExpand = TRUE)
    } else {
      # Odd rows
      addStyle(wb, "Appendix Table", style = content_style_odd, rows = i, cols = 1:ncol(df), gridExpand = TRUE)
    }
  }
  
  # Set column widths (uniform)
  cols_width <- rep(12, ncol(df))  # width = 12 for each column
  setColWidths(wb, "Appendix Table", cols = 1:ncol(df), widths = cols_width)
  
  # Set row heights for compact layout
  setRowHeights(wb, "Appendix Table", rows = 1:(nrow(df) + 1), heights = rowheights)
  
  # Save workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}



get_legend_35 <- function(plot) {
  # return all legend candidates
  legends <- get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  nonzero <- vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE)
  idx <- which(nonzero)
  # return first non-zero legend if exists, and otherwise first element (which will be a zeroGrob) 
  if (length(idx) > 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}

theme_set(
  theme_gray() +  
    theme(
      text = element_text(family = "Arial") 
    )
)
