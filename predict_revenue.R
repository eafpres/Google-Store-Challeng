#
# Google Store prediction competition
#
  rm(list = ls())
#  
  library(jsonlite)
#
  insert_decoded_cols <- function(data, position, no_cols, new_names) {
    for (i in 1:no_cols) {
      new_col <- data.frame(matrix(ncol = 1, 
                                   nrow = nrow(data)))
      if (no_cols > 1) {
        colnames(new_col) <- new_names[i]
      } else {
        colnames(new_col) <- new_names
      }

      if (position < ncol(data)) {
        data <- cbind(decoded_cols[, position - 1],
                      new_col,
                      decoded_cols[position:ncol(data)])
      } else {
        data <- cbind(decoded_cols,
                              new_col)
      }
      position <- position + 1
    }
    return(data)
  }
#    
  temp_classes <- c("character", "numeric", 
                    rep("character", 7), 
                    "numeric", "numeric")
  temp <- read.csv("train.csv", stringsAsFactors = FALSE, 
                   colClasses = temp_classes, nrows = 1000)
  temp_device <- as.data.frame(fromJSON(temp[1, "device"]),
                               stringsAsFactors = FALSE)
  device_cols <- length(temp_device)
  temp_network <- as.data.frame(fromJSON(temp[1, "geoNetwork"]),
                                stringsAsFactors = FALSE)
  network_cols <- length(temp_network)
  temp_totals <- as.data.frame(fromJSON(temp[1, "totals"]),
                               stringsAsFactors = FALSE)
  totals_cols <- length(temp_totals)
  temp_traffic <- as.data.frame(fromJSON(temp[1, "trafficSource"]),
                                       stringsAsFactors = FALSE)
  traffic_cols <- length(temp_traffic)
  decode_cols <- cbind(temp_device, temp_network, 
                       temp_totals, temp_traffic)
  decoded_cols <- decode_cols
#  assign("decoded_cols", decode_cols, envir = .GlobalEnv)
#
  for (i in 2:nrow(temp)) {
    temp_device <- as.data.frame(fromJSON(temp[i, "device"]),
                                 stringsAsFactors = FALSE)
    new_cols <- which(!(names(temp_device) %in% colnames(decoded_cols)))
    device_cols <- device_cols + length(new_cols)
    if (length(new_cols) > 0) {
      decoded_cols <- insert_decoded_cols(decoded_cols, 
                          device_cols, 
                          length(new_cols), 
                          names(temp_device)[new_cols])
    }
    temp_network <- as.data.frame(fromJSON(temp[i, "geoNetwork"]),
                                  stringsAsFactors = FALSE)
    new_cols <- which(!(names(temp_network) %in% colnames(decoded_cols)))
    network_cols <- network_cols + length(new_cols)
    if (length(new_cols) > 0) {
      decoded_cols <- 
        insert_decoded_cols(decoded_cols, 
                            device_cols + network_cols, 
                            length(new_cols), 
                            names(temp_network)[new_cols])
    }
    temp_totals <- as.data.frame(fromJSON(temp[i, "totals"]),
                                 stringsAsFactors = FALSE)
    new_cols <- which(!(names(temp_totals) %in% colnames(decoded_cols)))
    totals_cols <- totals_cols + length(new_cols)
    if (length(new_cols) > 0) {
      decoded_cols <- 
        insert_decoded_cols(decoded_cols,
                            device_cols + network_cols + totals_cols, 
                            length(new_cols), 
                            names(temp_totals)[new_cols])
    }
    temp_traffic <- as.data.frame(fromJSON(temp[i, "trafficSource"]),
                                  stringsAsFactors = FALSE)
    new_cols <- which(!(names(temp_traffic) %in% colnames(decoded_cols)))
    traffic_cols <- traffic_cols + length(new_cols)
    if (length(new_cols) > 0) {
      decoded_cols <- 
        insert_decoded_cols(decoded_cols,
                            device_cols + network_cols + 
                              totals_cols + traffic_cols, 
                            length(new_cols), 
                            names(temp_traffic)[new_cols])
    }
    decode_cols <- cbind(temp_device, temp_network, 
                         temp_totals, temp_traffic)
    matched_cols <- which(names(decoded_cols) %in% colnames(decode_cols))
    decoded_cols[i, ]  <- 
      data.frame(matrix(nrow = 1, rep(NA, ncol(decoded_cols))))
    for (k in matched_cols) {
      decoded_cols[i, k] <- decode_cols[1, which(colnames(decode_cols) ==
                                                   colnames(decoded_cols)[k])]
    }
  }
  