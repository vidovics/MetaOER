library(yaml)
library(fs)

# ---------------------------
# Helpers
# ---------------------------

read_meta <- function(file) {
  
  if (!file.exists(file)) return(NULL)
  
  lines <- readLines(file, warn = FALSE)
  
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]
  
  if (is.na(yaml_start) || is.na(yaml_end)) return(NULL)
  
  yaml_text <- paste(lines[(yaml_start + 1):(yaml_end - 1)], collapse = "\n")
  
  tryCatch(yaml::yaml.load(yaml_text), error = function(e) NULL)
}

to_html <- function(path) {
  path <- gsub("\\.qmd$", ".html", path)
  gsub(" ", "%20", path)
}

clean_path <- function(x) {
  gsub("%20", " ", x)
}

# ---------------------------
# Read Quarto structure
# ---------------------------

quarto <- yaml::read_yaml("_quarto.yml")

menu_items <- quarto$website$navbar$left

# Separate top-level items and Units
units_index <- which(sapply(menu_items, function(x) !is.null(x$text) && x$text == "Units"))
units_menu <- menu_items[[units_index]]$menu

section_names <- sapply(units_menu, function(x) x$text)

entry_qmd <- sapply(units_menu, function(x) {
  gsub("%20", " ", x$href)
})

entry_html <- vapply(entry_qmd, to_html, character(1))

# ---------------------------
# Build sitemap header
# ---------------------------

output <- c(
  "---",
  'title: "Site map"',
  "format: html",
  "---",
  "",
  "## Site Map",
  ""
)

# ---------------------------
# Top-Level Pages
# ---------------------------

top_items <- menu_items[!sapply(menu_items, function(x) !is.null(x$menu))]

for (item in top_items) {
  
  if (is.null(item$href)) next
  
  qmd_file <- gsub("%20", " ", item$href)
  
  meta <- read_meta(qmd_file)
  
  if (!is.null(meta) && !is.null(meta$title)) {
    
    html <- to_html(qmd_file)
    
    output <- c(output,
                paste0("- [", meta$title, "](../", html, ")")
    )
  }
}

# Blank line before units
output <- c(output, "")

# ---------------------------
# Unit sections (as nested list)
# ---------------------------

for (i in seq_along(entry_qmd)) {
  
  section_name <- section_names[i]
  current_file <- entry_qmd[i]
  
  # Unit as top-level list item
  output <- c(output,
              paste0("- **", section_name, "**")
  )
  
  visited <- c()
  
  # controlled recursion
  for (step in 1:100) {
    
    if (!file.exists(current_file)) break
    
    meta <- read_meta(current_file)
    if (is.null(meta)) break
    
    title <- meta$title
    html_path <- to_html(current_file)
    
    if (!(html_path %in% visited)) {
      output <- c(output,
                  paste0("  - [", title, "](../", html_path, ")")
      )
      visited <- c(visited, html_path)
    }
    
    # Stop if no next
    if (is.null(meta[["next"]]) || is.null(meta[["next"]][["href"]])) break
    
    # Resolve next path
    next_href <- meta[["next"]][["href"]]
    next_href <- gsub("\\.html$", ".qmd", next_href)
    next_href <- clean_path(next_href)
    
    next_file <- fs::path_norm(
      fs::path(dirname(current_file), next_href)
    )
    
    next_html <- to_html(next_file)
    
    # STOP: if next is another section entry
    if (next_html %in% entry_html && next_html != to_html(entry_qmd[i])) break
    
    # STOP: if leaving folder
    if (dirname(next_file) != dirname(current_file)) break
    
    # STOP: avoid loops
    if (next_html %in% visited) break
    
    current_file <- next_file
  }
  
  # spacing between sections
  output <- c(output, "")
}

# ---------------------------
# Write output
# ---------------------------

writeLines(output, "Site map.qmd")
