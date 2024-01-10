# ------- Checking common elements
# Define the function
check_common_elements <- function(list1, list2, list3) {
  # Combine the second and third lists
  combined_list <- c(list2, list3)
  
  # Check if any elements of the first list are in the combined list of the other two
  common_elements <- list1[list1 %in% combined_list]
  
  # If common elements are found, return them, otherwise return NULL
  if (length(common_elements) > 0) {
    common_values <- common_elements
    #return(common_elements)
  } else {
    common_values <- NULL
#   return(common_values)
  }
  
  # Print the result
  if (!is.null(common_values)) {
    print(paste("Common elements:", paste(common_values, collapse = ", ")))
  } else {
    print("No common elements found in the given lists.")
  }
}

# -----------------------------------------------------------------------------
