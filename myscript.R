CARSReport <- function(df) {
  class(df) <- c("CARSReport", "data.frame")
  return(df)
}

print.CARSReport <- function(x, ...) {
  usd_to_rub <- function(usd) {
    usd_numeric <- as.numeric(gsub("[$,]", "", usd))
    return(usd_numeric * 80)
  }
  
  makes <- unique(x$Make)
  types <- unique(x$Type)
  output <- character()
  
  for (make in makes) {
    make_data <- x[x$Make == make, ]
    origin <- unique(make_data$Origin)

    output <- c(output, paste0(make, " [", origin, "]"))
    type_counts <- sapply(types, function(type) {
      sum(make_data$Type == type)
    })
    count_str <- paste0(types, "=", type_counts, collapse = " ")
    output <- c(output, count_str)
    max_invoice <- sapply(types, function(type) {
      type_data <- make_data[make_data$Type == type, ]
      if (nrow(type_data) > 0) {
        max_invoice_usd <- max(usd_to_rub(type_data$Invoice))
        sprintf("%.2f RUB", max_invoice_usd)
      } else {
        "0.00 RUB"
      }
    })
    invoice_str <- paste0(types, "=", max_invoice, collapse = " ")
    output <- c(output, invoice_str)
  }
  cat(output, sep = "\n")
}

CARSStats <- function(df) {
  lines <- readLines('manufacturer_summary_sink.txt')
  num <- 1
  stats <- list()
  types <- list()
  regions <- list()
  
  while (num <= length(lines)-2){
      first_line <- lines[num]
      second_line <- lines[num+1]
      third_line <- lines[num+2]
    
      cleaned <- sub("\\]", "", first_line)
      parts <- strsplit(cleaned, "\\[")[[1]]
      make <- parts[1]
      region <- parts[2]
      num <- num + 3
      if (!(region %in% names(stats))){
          stats[[region]] <- list()
      }
      
      parts <- strsplit(third_line, " ")[[1]]
      for (part in parts) {
          if (part != "RUB" && part != "") { 
              key_value <- strsplit(part, "=")[[1]]
              type <- key_value[1]
              price <- as.numeric(key_value[2])
              types <- c(types, type)
              regions <- c(regions, region)
              if (!(type %in% names(stats[[region]]))){
                  stats[[region]][[type]]  <- price
              }
              else{
                  stats[[region]][[type]] <- max(stats[[region]][[type]], price)
              }
          }
      }
  }
  
  all_types <- unique(types)
  all_origins <- unique(regions)
  
  out <- matrix(nrow=length(all_types), ncol=length(all_origins))
  rownames(out) <- all_types
  colnames(out) <- all_origins
  
  for (i in all_types) {
      for (j in all_origins){
          if (j %in% names(stats) && i %in% names(stats[[j]])) {
              out[i, j] <- stats[[j]][[i]]
          }
      }
  }

  class(out) <- c("CARSStats", "matrix")
  return(out)
}

print.CARSStats <- function(x, ...) {
  formatted_out <- apply(x, c(1,2), function(val) {
    if(is.na(val) || is.null(val) || val == 0) {
      "0.00 RUB"
    } else {
      paste0(sprintf("%.2f", val), " RUB")
    }
  })
  print(formatted_out, quote = FALSE)
}

write.csv <- function(x, file, ...) {
  UseMethod("write.csv")
}

write.csv.default <- function(x, file, ...) {
  utils::write.csv(x, file, ...)
}

write.csv.CARSStats <- function(x, file, ...) {
  formatted_out <- apply(x, c(1,2), function(val) {
    if (is.na(val) || is.null(val) || val == 0) {
      "0.00 RUB"
    } else {
      paste0(sprintf("%.2f", val), " RUB")
    }
  })
  utils::write.csv(formatted_out, file, ...)
}

plot.CARSStats <- function(x, ...) {
  barplot(
      t(x),            
      xlab = "Тип кузова",             
      names.arg = rownames(x), 
      legend.text = colnames(x),
  )
}

cars_df <- read.csv("CARS.csv", stringsAsFactors = FALSE)

cars_report <- CARSReport(cars_df)
sink("manufacturer_summary_sink.txt")
print(cars_report)
sink()

cars_stats <- CARSStats(cars_df)

print(cars_stats)
write.csv(cars_stats, "summary.csv")
plot(cars_stats)

