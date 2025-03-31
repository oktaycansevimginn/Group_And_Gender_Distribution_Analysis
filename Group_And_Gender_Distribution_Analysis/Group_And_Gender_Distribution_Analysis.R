
# OKTAY CAN SEVIMGIN

setwd_manual <- function(directory) {
  cat("WORKING INDEX: ", directory, "\n")
}

read_delim_manual <- function(file_path, sep = " ", header = TRUE, fileEncoding = "UTF-8") {
  file_connection <- file(file_path, open = "r", encoding = fileEncoding)
  data <- list()
  column_names <- NULL
  line_number <- 0
  
  while (length(line <- readLines(file_connection, n = 1, warn = FALSE)) > 0) {
    line_number <- line_number + 1
    split_line <- unlist(strsplit(line, sep))
    
    if (line_number == 1 && header) {
      column_names <- split_line
    } else {
      data[[line_number - 1]] <- split_line
    }
  }
  
  close(file_connection)
  
  data_frame <- data.frame(matrix(unlist(data), ncol = length(column_names), byrow = TRUE))
  colnames(data_frame) <- column_names
  
  return(data_frame)
}

table_manual <- function(variable) {
  freq_table <- list()
  for (value in unique(variable)) {
    freq_table[[value]] <- sum(variable == value)
  }
  return(freq_table)
}

draw_bar_chart_manual <- function(counts, main_title = "Bar Chart", x_label = "Categories", y_label = "Frequency", 
                                  x_lim = NULL, y_lim = NULL, bar_colors = "blue", horizontal = FALSE) {
  
  categories <- names(counts)
  frequencies <- as.numeric(counts)
  num_bars <- length(categories)
  
  if (is.null(x_lim)) x_lim <- c(0, num_bars + 1)
  if (is.null(y_lim)) y_lim <- c(0, max(frequencies) * 1.2)
  
  plot(0, type = "n", xlim = x_lim, ylim = y_lim, xaxt = "n", yaxt = "n", 
       xlab = x_label, ylab = y_label, main = main_title)
  
  bar_width <- 0.5
  for (i in seq_along(categories)) {
    rect(i - bar_width, 0, i + bar_width, frequencies[i], col = bar_colors, border = "black")
    text(i, frequencies[i] + max(frequencies) * 0.05, labels = frequencies[i], cex = 0.8)
  }
  
  axis(1, at = 1:length(categories), labels = categories)
  axis(2)
}

draw_multiple_bar_charts_manual <- function(counts1, counts2, 
                                            main_title1 = "Chart 1", main_title2 = "Chart 2",
                                            x_label1 = "Categories", x_label2 = "Categories",
                                            y_label1 = "Frequency", y_label2 = "Frequency",
                                            bar_colors1 = "yellow", bar_colors2 = "red",
                                            layout = "horizontal") {
  
  par(mar = c(4, 4, 2, 1))
  
  if (layout == "horizontal") {
    par(mfrow = c(1, 2))  
  } else {
    par(mfrow = c(2, 1))  
  }
  
  draw_bar_chart_manual(counts1, main_title = main_title1, x_label = x_label1, y_label = y_label1, bar_colors = bar_colors1)
  draw_bar_chart_manual(counts2, main_title = main_title2, x_label = x_label2, y_label = y_label2, bar_colors = bar_colors2)
  
  par(mfrow = c(1, 1))  
}

my_data <- read_delim_manual("C:\\Users\\Oktay Can\\Desktop\\Dataset\\DatasetNA.txt", sep = " ", header = TRUE, fileEncoding = "UTF-8")

group_freq <- table_manual(my_data$Group)
gender_freq <- table_manual(my_data$Gender)

draw_multiple_bar_charts_manual(group_freq, gender_freq, 
                                main_title1 = "Group Distribution", 
                                main_title2 = "Gender Distribution",
                                x_label1 = "Group", 
                                x_label2 = "Gender",
                                y_label1 = "Frequency", 
                                y_label2 = "Frequency",
                                bar_colors1 = "yellow", 
                                bar_colors2 = "red",
                                layout = "horizontal")

draw_multiple_bar_charts_manual(group_freq, gender_freq, 
                                main_title1 = "Group Distribution", 
                                main_title2 = "Gender Distribution",
                                x_label1 = "Group", 
                                x_label2 = "Gender",
                                y_label1 = "Frequency", 
                                y_label2 = "Frequency",
                                bar_colors1 = "yellow", 
                                bar_colors2 = "red",
                                layout = "vertical")
