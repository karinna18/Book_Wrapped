suppressPackageStartupMessages({
library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(janitor)
library(cli)
})

# --- CREDENTIALS ---

NOTION_TOKEN <<- Sys.getenv("NOTION_TOKEN_BOOKS")
BOOKS_DATABASE_ID <<- "2d55a4c40d5c81939cdfc1a27fc628e4"
AUTHORS_DATABASE_ID <<- "2d85a4c40d5c80d6ba65ca36a4c2671a"
THEMES_DATABASE_ID <<- "2d75a4c40d5c80aabb6cea8e4608d641"
SERIES_DATABASE_ID <<- "33d5a4c40d5c80f78dc1ffcbfe1cd15c"

if (!dir.exists("www/notion_cache")) {
  dir.create("www/notion_cache", recursive = TRUE)
}

# --- THE API WORKHORSE ---
fetch_notion_db <- function(db_id, last_sync_time = NULL, strdb = "") {
  #
  #
  #cat("\n--- Fetching database:", strdb, "---\n")
  status_msg <- cli_status("Fetching database: {.strong {strdb}}...")
  #
  #
  url <- paste0("https://api.notion.com/v1/databases/", db_id, "/query")
  
  filter_payload <- list()
  if (!is.null(last_sync_time)) {
    filter_payload <- list(
      filter = list(
        timestamp = "last_edited_time",
        last_edited_time = list(on_or_after = last_sync_time)
      )
    )
  }
  
  pages <- list()
  has_more <- TRUE
  cursor <- NULL
  
  page_count <- 0

  while (has_more) {
    page_count <- page_count + 1
    #
    #
    #cat("\r  > Total pages retrieved:", page_count)
    cli_status_update(id = status_msg, "Fetching {.strong {strdb}}: Page {page_count} retrieved...")
    #
    #
    
    # Start with the filter (might be empty list)
    body <- filter_payload
    
    # Add cursor if it exists
    if (!is.null(cursor)) body$start_cursor <- cursor
    
    # Create the request
    req <- request(url) %>%
      req_method("POST") %>%
      req_headers(
        `Authorization` = paste("Bearer", NOTION_TOKEN),
        `Notion-Version` = "2022-06-28",
        `Content-Type` = "application/json"
      )
    
    # ONLY add the body if it has actual content
    # This prevents the [ ] vs { } conflict
    if (length(body) > 0) {
      req <- req %>% req_body_json(body)
    }
    
    # Perform
    resp <- req %>% 
      req_perform() %>% 
      resp_body_json()
    
    pages <- c(pages, resp$results)
    has_more <- resp$has_more
    cursor <- resp$next_cursor
  }
  #
  #
  #cat("  ✓ Total items retrieved:", length(pages), "\n")
  cli_status_clear(id = status_msg)
  cli_alert_success("Retrieved {length(pages)} items from {.strong {strdb}}.")
  #
  #
  return(pages)
}

# --- THE DATA PARSER ---
parse_notion_to_df <- function(pages, label = "Pages") {
  #
  #
  cat("  > Parsing raw JSON to data frame...\n")
  #
  #
  map_df(pages, function(p) {
    props <- p$properties
    page_id <- p$id
    
    get_val <- function(x, nm) {
      type <- x$type
      clean_nm <- tolower(nm)
      
      val <- if (type %in% c("title", "rich_text")) {
        if (length(x[[type]]) == 0) NA else x[[type]][[1]]$plain_text
      } else if (type == "number") {
        x$number # Stays as numeric
      } else if (type == "select" || type == "status") {
        x[[type]]$name
      } else if (type == "multi_select") {
        if (length(x$multi_select) == 0) NA else paste(map_chr(x$multi_select, "name"), collapse = ", ")
      } else if (type == "date") {
          list(start = as.character(x$date$start %||% NA), end = as.character(x$date$end %||% NA))
      } else if (type == "formula") {
        # Formulas are the usual culprits. We force them to character to be safe.
        as.character(x$formula[[x$formula$type]] %||% NA)
      } else if (type == "relation") {
        if (length(x$relation) == 0) NA else paste(map_chr(x$relation, "id"), collapse = ",")
      } else if (type == "checkbox") {
        x$checkbox # Returns TRUE/FALSE
      } else if (type == "people") {
        if (length(x$people) == 0) NA else paste(map_chr(x$people, "name"), collapse = ", ")
        
      } else if (type %in% c("url", "email", "phone_number")) {
        x[[type]] %||% NA
        
      } else if (type == "formula") {
        # Formulas can be strings, numbers, or dates; forcing character for bind_rows safety
        as.character(x$formula[[x$formula$type]] %||% NA)
        
      } else if (type == "rollup") {
        # Rollups are lists of values. Like Python, we convert the whole array to a string.
        as.character(jsonlite::toJSON(x$rollup$array))
        
      } else if (type == "files") {
          if (length(x$files) == 0) {
            NA 
          } else {
            # If it's a cover property, we usually want the URL, not just the name
            # We check for 'file$url' (hosted) or 'external$url' (linked)
            urls <- map_chr(x$files, function(f) {
              if (!is.null(f$file$url)) {
                ext <- tools::file_ext(f$name)
                if(ext == "") ext <- "png"
                safe_name <- paste0(page_id, ".", ext)
                local_path <- file.path("www/notion_cache", safe_name)
                request(f$file$url) %>% req_perform(path = local_path)
                if (!file.exists(local_path)) {
                  # cat("\r  > Downloading new cover:", safe_name)
                  request(f$file$url) %>% req_perform(path = local_path)
                }
                return(file.path("notion_cache", safe_name))
              }
              if (!is.null(f$external$url)) return(f$external$url)
              return(f$name) # Fallback to name if it's just a file list
            })
            paste(urls, collapse = ", ")
          }
        
      } else {
        NA
      }
      
      if (clean_nm %in% c("rating")) {
        return(as.character(val %||% NA))
      } else if (clean_nm %in% c("rating_personal", "rating_technical", "pages", "page_number")) {
        return(as.numeric(val %||% NA))
      }
      return(val %||% NA)
    }
    
    # Use imap (indexed map) to pass the property name into get_val
    res <- imap(props, get_val) %>% 
      set_names(tolower(names(props)))
    
    # Manually expand the dates for 'date' and 're-reads'
    if (!is.null(res$date)) {
      res$date_start <- res$date$start
      res$date_end   <- res$date$end
      res$date <- NULL
    }
    if (!is.null(res$`re-reads`)) {
      res$reread_start <- res$`re-reads`$start
      res$reread_end   <- res$`re-reads`$end
      res$`re-reads` <- NULL
    }
    if (!is.null(res$`purchased`)) {
      res$purchased <- res$`purchased`$start
    }
    
    as_tibble_row(res) %>%
      mutate(id = p$id, last_edited_time = p$last_edited_time)
  }, .progress = paste("Parsing", label))
}

# --- MAIN SYNC EXECUTION ---
sync_data <- function() {
  cache_file <- "notion_cache.rds"
  
  #
  #
  #cat("--- Initializing Sync ---\n")
  cli_h1("Initializing Sync")
  #
  #
  
  if (file.exists(cache_file)) {
    cache <- readRDS(cache_file)
    # Get the latest timestamp across all three dbs to be safe
    all_times <- c(cache$books$last_edited_time, cache$authors$last_edited_time, cache$themes$last_edited_time, cache$series$last_edited_time)
    last_sync <- if(length(all_times) > 0) max(all_times, na.rm = TRUE) else NULL
    #
    #
    #cat("  ✓ Cache found. Last sync point:", as.character(last_sync), "\n")
    cli_alert_success("Cache found. Last sync: {.val {as.character(last_sync)}}")
    #
    #
  } else {
    cache <- list(books = tibble(), authors = tibble(), themes = tibble(), series = tibble())
    last_sync <- NULL
    #
    #
    #cat("  ! No cache found. Performing full sync.\n")
    cli_alert_warning("No cache found. Performing full sync.")
    #
    #
  }
  
  # Fetch updates using the correct IDs
  new_b_raw <- fetch_notion_db(BOOKS_DATABASE_ID, last_sync, "books")
  new_a_raw <- fetch_notion_db(AUTHORS_DATABASE_ID, last_sync, "authors")
  new_t_raw <- fetch_notion_db(THEMES_DATABASE_ID, last_sync, "themes")
  new_s_raw <- fetch_notion_db(SERIES_DATABASE_ID, last_sync, "series")
  
  
  #
  #
  #cat("\n--- Updating Cache Tables ---\n")
  cli_h2("Updating Cache Tables")
  #
  #
  # Update Books
  if (length(new_b_raw) > 0) {
    new_b <- parse_notion_to_df(new_b_raw, "Books")
    
    # Check if cache actually has rows before trying to filter
    if (nrow(cache$books) > 0) {
      cache$books <- bind_rows(
        cache$books %>% filter(!id %in% new_b$id), 
        new_b
      )
    } else {
      cache$books <- new_b
    }
  }
  # Update Authors
  if (length(new_a_raw) > 0) {
    new_a <- parse_notion_to_df(new_a_raw, "Authors")
    if (nrow(cache$authors) > 0) {
      cache$authors <- bind_rows(cache$authors %>% filter(!id %in% new_a$id), new_a)
    } else {
      cache$authors <- new_a
    }
  }
  
  # Update Themes
  if (length(new_t_raw) > 0) {
    new_t <- parse_notion_to_df(new_t_raw, "Themes")
    if (nrow(cache$themes) > 0) {
      cache$themes <- bind_rows(cache$themes %>% filter(!id %in% new_t$id), new_t)
    } else {
      cache$themes <- new_t
    }
  }
  
  # Update Themes
  if (length(new_s_raw) > 0) {
    new_s <- parse_notion_to_df(new_s_raw, "Series")
    if (nrow(cache$series) > 0) {
      cache$series <- bind_rows(cache$series %>% filter(!id %in% new_s$id), new_s)
    } else {
      cache$series <- new_s
    }
  }
  #
  #
  #cat("  ✓ All tables updated locally.\n")
  cli_alert_success("All tables updated locally.")
  #
  #
  saveRDS(cache, cache_file)
  #
  #
  cli_alert_info("Cache saved to {.file {cache_file}}")
  #cat("  ✓ Cache saved to:", cache_file, "\n")
  #
  #
  return(cache)
}

# --- RUN SYNC AND CLEAN ---
all_data <- sync_data()


#
#
cat("\n--- Running Final Data Processing ---\n")
#
#

# Lookups
author_lookup <- all_data$authors %>% select(id, name) %>% deframe()
theme_lookup  <- all_data$themes  %>% select(id, theme) %>% deframe()
series_lookup  <- all_data$series  %>% select(id, name) %>% deframe()


# Final Dataframes for app.R
authors <<- all_data$authors %>%
  select(name, diverse, ethnicity, gender, lgbt) %>%
  rename(author = name) %>%
  clean_names()

themes <<- all_data$themes %>% 
  select(theme) %>% 
  drop_na(theme) %>%
  clean_names()

series <<- all_data$series %>%
  select(name, `total books`) %>%
  rename(series = name, total_books = `total books`) %>%
  clean_names()

#
#
cat("  > Processing book relations and metrics...\n")
#
#

books <<- all_data$books %>% 
  clean_names() %>% 
  mutate(
    author = map_chr(author, ~ {
      ids <- unlist(strsplit(.x, ","))
      paste(author_lookup[ids], collapse = ", ")
    }),
    
    themes = map_chr(themes, ~ {
      ids <- unlist(strsplit(.x, ","))
      paste(theme_lookup[ids], collapse = ", ")
    }),
    
    series = map_chr(series, ~ {
      ids <- unlist(strsplit(.x, ","))
      paste(series_lookup[ids], collapse = ", ")
    }),
  ) %>%
  select(name, pages, goodreads_rating, author, date_start, date_end, format, genre, 
         load_pages_day, mood, origin, owned, page_number, 
         publication_date, rating, reason, rec, series, series_number,
         star_rating, state, type, themes, reread, resonance, 
         enjoyment, rating_personal, rating_technical, expectation, expectation_difference, cover) %>% left_join(series %>% select(series, total_books), by = join_by(series == series)) %>%
  mutate(
    series         = na_if(series, "NA"), # Replaces string "NA" with real NA
    rating_rounded = sub(".*\\((.*)\\).*", "\\1", rating) %>% as.numeric() %>% na_if(0),
    rating = sub("\\(.*", "", rating) %>% as.numeric() %>% na_if(0), 
    theme          = themes,
    themes         = NULL,
    date_start = as.Date(date_start),
    date_end   = as.Date(date_end),
    days = as.numeric(date_end-date_start)+1,
    page_number = as.numeric(page_number),
    pages = as.numeric(pages),
    pages = coalesce(pages, page_number),
    expectation_difference = as.numeric(expectation_difference),
    reading_experience = rating_rounded + expectation_difference,
    series_base = if_else(!is.na(series), series, name),
    series = if_else(is.na(series), NA_character_, paste(series, series_number)),
    series_last = series_number == total_books,
    expectation = as.numeric(sub(" .*", "", expectation))
  ) %>% filter(pages>30) 

#
#
cat("✓ Sync and processing complete!\n\n")
#
#

