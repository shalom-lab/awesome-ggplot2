# Required packages
library(jsonlite)
library(stringr)

convert_md_to_json <- function(md_file) {
  # Read the markdown file
  lines <- readLines(md_file, warn = FALSE)
  
  # Initialize variables
  current_structure <- list()
  current_level <- NULL
  current_section <- NULL
  current_subsection <- NULL
  items <- list()
  
  # Helper function to extract package info
  extract_package_info <- function(line) {
    # Match {[name](url)}: description pattern
    pkg_match <- str_match(line, "\\{\\[([^\\]]+)\\]\\(([^\\)]+)\\)\\}:?\\s*(.*)$")
    if (!is.na(pkg_match[1])) {
      return(list(
        name = pkg_match[2],
        url = pkg_match[3],
        description = if (!is.na(pkg_match[4])) trimws(pkg_match[4]) else ""
      ))
    }
    
    # Match [name](url) pattern
    link_match <- str_match(line, "\\[([^\\]]+)\\]\\(([^\\)]+)\\)")
    if (!is.na(link_match[1])) {
      return(list(
        name = link_match[2],
        url = link_match[3],
        description = trimws(str_replace(line, "\\[([^\\]]+)\\]\\(([^\\)]+)\\)", ""))
      ))
    }
    
    # Plain text
    return(list(
      name = trimws(line),
      url = "",
      description = ""
    ))
  }
  
  # Process each line
  for (line in lines) {
    # Skip empty lines
    if (trimws(line) == "") next
    
    # Check for headers
    if (startsWith(line, "#")) {
      level <- str_count(line, "^#+")
      title <- trimws(str_replace(line, "^#+\\s*", ""))
      
      if (level == 1) {
        current_level <- title
        current_structure[[current_level]] <- list()
      } else if (level == 2) {
        current_section <- title
        if (is.null(current_structure[[current_level]][[current_section]])) {
          current_structure[[current_level]][[current_section]] <- list()
        }
      }
      next
    }
    
    # Process list items
    if (startsWith(trimws(line), "*")) {
      item <- trimws(str_replace(line, "^\\*\\s*", ""))
      info <- extract_package_info(item)
      
      if (!is.null(current_section)) {
        # Add to subsection
        current_structure[[current_level]][[current_section]] <- 
          c(current_structure[[current_level]][[current_section]], 
            list(info))
      } else {
        # Add to main section
        current_structure[[current_level]] <- 
          c(current_structure[[current_level]], 
            list(info))
      }
    }
  }
  
  # Convert to JSON
  json_data <- toJSON(current_structure, pretty = TRUE, auto_unbox = TRUE)
  return(json_data)
}

# Convert README.md to JSON
json_data <- convert_md_to_json("README.md")
write(json_data, "readme_data.json") 