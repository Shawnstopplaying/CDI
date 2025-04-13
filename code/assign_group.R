
# assign_group(): Classify each sample name into groups like Donor, SI0, SI7, RI0, RI7...

assign_group <- function(sample_name) {
  # Patient sample starts with A
  if (grepl("^A", sample_name)) {
    id <- as.numeric(gsub("A(\\d+)\\..*", "\\1", sample_name))   # extract patient ID
    day <- as.numeric(gsub(".*\\.(\\d+)_16S", "\\1", sample_name))  # extract day info
    
    # Single Intervention/SI group patients
    if (id %in% c(6,11,12,13,19,21,23,26,29,31,32,35)) {
      return(ifelse(day == 0, "SI0", paste0("SI", day)))
    }
    
    # Repeat Intervention/RI group patients
    else if (id %in% c(1,2,3,4,5,7,8,9,10,14,17,18,20,24,27,28,33)) {
      return(ifelse(day == 0, "RI0", paste0("RI", day)))
    }
    
    # Unclassified patient
    else {
      return("Unknown")
    }
  }
  
  # Donor samples
  else {
    return("Donor")
  }
}
