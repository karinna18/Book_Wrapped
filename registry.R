suppressMessages({
library(readxl)
library(shiny)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(patchwork)
library(webshot2)
library(scales)
library(plotly)
library(gridExtra)
library(rstatix)
library(ggpubr)
library(ggpattern)
library(purrr)
library(gt)
library(ggnewscale)
library(wordcloud2)
})
'

library(shiny)
library(bslib)
library(tidyverse)  # For map, list_modify, dplyr, tidyr, etc.
library(plotly)     # For plot_ly, layout, renderPlotly
library(readxl)     # To load plots.xlsx
library(shades)     # For color brightness adjustments
library(lubridate)  # For floor_date, year(), format()
library(janitor)
'
source("source.R")
source("global.R")


# Reading in data ---------------------------------------------------------
'
books <- read_csv("notion_export/books.csv") %>% janitor::clean_names() %>% select(name, pages, goodreads_rating, author, date_start, date_end, days, format, genre, load_pages_day, mood, origin, owned, page_number, publication_date, rating, reason, rec, series, star_rating, state, type, themes, reread, resonance, enjoyment,rating_personal,rating_technical) %>% 
  mutate(
    rating_rounded = sub(".*\\((.*)\\).*", "\\1", rating) %>% as.numeric() %>% na_if(0),
    rating = sub("\\(.*", "", rating) %>% as.numeric() %>% na_if(0), 
    theme = themes,
    themes = NULL
  )

authors <- read_csv("notion_export/authors.csv") %>% janitor::clean_names() %>% select(name, diverse, ethnicity, gender, lgbt) %>% rename(author=name)
themes <- read_csv("notion_export/themes.csv") %>% janitor::clean_names() %>% select(theme)
'

colors <- read_excel("plots.xlsx", sheet = "colors")

evals <- read_excel("plots.xlsx", sheet = "plots") %>% split(.$id) %>% map(~ as.list(select(.x, -id)))

# Helpers -----------------------------------------------------------------

build_cell_components <- function(id, cfg, input, output, dataframe){
  output[[id]] <- renderPlotly({
    cfg$plot(dataframe(), input)
  })
  id_card <- paste0(id, "_card")
  output[[id_card]] <- renderPlotly({
    cfg$plot(dataframe(), input, is_card = TRUE)
  })
}



apply_filters <- function(df, input, is_card = FALSE){
  if(input$year == "dated"){
    df <- df %>% filter(!is.na(date_end))
  } else if(input$year != "all") {
    df <- df %>% filter(year(date_end) == as.numeric(input$year))
  }
  df %>% filter(state == "read")
}

# Registry ----------------------------------------------------------------

evals$ratings_by_mood <- evals$ratings_by_mood %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{fullData.name}"
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    
    df <- df %>% filter(state == "read")
    
    f <- df %>% mutate(rating_rounded = factor(rating_rounded)) %>% drop_na(mood) %>% 
      group_by(mood, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n()) %>% plot_ly(
      x = ~mood,
      y = ~count,
      color = ~rating_rounded,
      colors = vals,
      type = "bar",
      text = ~I(books), 
      textposition = "none",
      hovertemplate = hover_text
    ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x") 
    f %>% apply_sleektheme()
  }
)
evals$ratings_by_mood_category <- evals$ratings_by_mood %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{fullData.name}"
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    df <- df %>% left_join(colors %>% filter(category == "mood") %>% select(name, summary_name), by = c("mood" = "name")) %>% mutate(mood = summary_name) %>% filter(state == "read")
    
    light_moods <- c("funny", "easy", "soft", "meaningful")
    dark_moods  <- c("creepy", "epic", "strong", "difficult", "informative")
    
    mood_summary <- df %>%
      filter(state == "read", !is.na(mood)) %>%
      summarise(
        light_books = sum(mood %in% light_moods),
        dark_books = sum(mood %in% dark_moods)
      )
    
    f <- df %>% mutate(rating_rounded = factor(rating_rounded)) %>% drop_na(mood) %>% 
      group_by(mood, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n()) %>% plot_ly(
        x = ~mood,
        y = ~count,
        color = ~rating_rounded,
        colors = vals,
        type = "bar",
        text = ~I(books), 
        textposition = "none",
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x",annotations = list(
        list(
          x = 1,
          y = 1,
          xref = "paper",
          yref = "paper",
          text = paste0(
            "light moods: ", mood_summary$light_books, " books<br>",
            "dark moods: ", mood_summary$dark_books, " books"
          ),
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "top"
        )
      )) 
    f %>% apply_sleektheme()
  }
)
evals$ratings_by_genre <- evals$ratings_by_genre %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{fullData.name}"
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    df <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre))
    f <- df %>% filter(state == "read") %>% mutate(rating_rounded = factor(rating_rounded)) %>% drop_na(genre) %>% 
      group_by(genre, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n()) %>% 
      plot_ly(
        x = ~genre,
        y = ~count,
        color = ~rating_rounded,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x") 
    f %>% apply_sleektheme()
  }
)

evals$ratings_by_genre_category <- evals$ratings_by_genre %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{fullData.name}"
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    df <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre))
    df <- df %>% left_join(colors %>% filter(category == "genre") %>% select(name, summary_name), by = c("genre" = "name")) %>% mutate(genre = summary_name)
    f <- df %>% filter(state == "read") %>% mutate(rating_rounded = factor(rating_rounded)) %>% drop_na(genre) %>% 
      group_by(genre, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n()) %>% 
      plot_ly(
        x = ~genre,
        y = ~count,
        color = ~rating_rounded,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x") 
    f %>% apply_sleektheme()
  }
)
evals$distribution_ratings <- evals$distribution_ratings %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{text}"
    
    f <- df %>% filter(state == "read") %>% mutate(rating_rounded = factor(rating_rounded)) %>% 
      group_by(goodreads_rating, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n(), .groups = "drop_last") %>% 
      plot_ly(
        x = ~goodreads_rating,
        y = ~count,
        color = ~rating_rounded,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)

evals$ratings_by_rec <- evals$ratings_by_rec %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{fullData.name}"
    
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    f <- df %>% filter(state == "read") %>% mutate(rating_rounded = factor(rating_rounded)) %>% drop_na(reason) %>% separate_rows(rec, sep = ", ")%>% 
      group_by(rec, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n()) %>% plot_ly(
        x = ~rec,
        y = ~count,
        color = ~rating_rounded,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)
evals$ratings_by_reason <- evals$ratings_by_reason %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{fullData.name}</extra>" else "%{fullData.name}"
    vals <- if (!is_card) get_colormap("rating") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$rating_rounded))), 0.8))
    f <- df %>% filter(state == "read") %>% mutate(rating_rounded = factor(rating_rounded)) %>% drop_na(reason) %>% 
      group_by(reason, rating_rounded) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = n()) %>% plot_ly(
      x = ~reason,
      y = ~count,
      color = ~rating_rounded,
      colors = vals,
      type = "bar",
      text = ~I(books),  
      textposition = "none",
      hovertemplate = hover_text
    ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)

evals$monthly_by_reason <- evals$monthly_by_reason %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    vals <- if (!is_card) get_colormap("reason") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$reason))), 0.8))
    
    df <- df %>% filter(!is.na(date_end))
    df <- df %>% mutate(days = as.numeric(date_end-date_start)+1, pages_per_day = page_number / days
    ) %>% mutate(reason = factor(reason, levels = colors %>% filter(category == "reason") %>% .$name)) 
    df_months <- df %>% rowwise() %>%  mutate(month_seq = list(seq(
      floor_date(date_start, "month"),
      floor_date(date_end, "month"),
      by = "month"))) %>% unnest(month_seq) %>% ungroup() %>% mutate(month = format(month_seq, "%Y-%m"), month_str = format(month_seq, "%y-%b"), month_str = factor(month_str, levels = unique(month_str[order(month)])))
    f <- df_months %>% drop_na(reason) %>% arrange(reason) %>% mutate(month_amt = n(), weight = 1/month_amt, .by=name) %>%
      group_by(month_str, reason) %>%  summarize(books = str_flatten(name, collapse = "<br>"), count = sum(weight), rating = round(mean(rating),2), .groups = "drop_last") %>% plot_ly(
        x = ~month_str,
        y = ~count,
        color = ~reason,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        customdata = ~reason,
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
    
  }
)

evals$monthly_by_mood <- evals$monthly_by_mood %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    vals <- if (!is_card) get_colormap("mood") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$mood))), 0.8))
    
    
    df <- df %>% filter(!is.na(date_end))
    df <- df %>% mutate(days = as.numeric(date_end-date_start)+1, pages_per_day = page_number / days
    ) %>% mutate(mood = factor(mood, levels = colors %>% filter(category == "mood") %>% .$name)) 
    df_months <- df %>% rowwise() %>%  mutate(month_seq = list(seq(
      floor_date(date_start, "month"),
      floor_date(date_end, "month"),
      by = "month"))) %>% unnest(month_seq) %>% ungroup() %>%  group_by(name) %>% mutate(month_weight = 1 / n()) %>% 
      ungroup() %>% mutate(month = format(month_seq, "%Y-%m"), month_str = format(month_seq, "%y-%b"), w_pages = page_number * month_weight, w_days  = days * month_weight, month_str = factor(month_str, levels = unique(month_str[order(month)])))
    f <- df_months %>% drop_na(mood) %>% group_by(month_str) %>% mutate(total_w_days = sum(w_days, na.rm = TRUE)) %>% ungroup() %>% arrange(mood) %>%
      group_by(month_str, mood) %>%  summarize(books = str_flatten(name, collapse = "<br>"), avg_speed = sum(w_pages, na.rm = TRUE) / mean(total_w_days), rating = round(mean(rating),2)) %>% plot_ly(
        x = ~month_str,
        y = ~avg_speed,
        color = ~mood,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        customdata = ~mood,
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)


evals$monthly_by_genre <- evals$monthly_by_genre %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    vals <- if (!is_card) get_colormap("genre") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$genre))), 0.8))
    df <- df %>% mutate(genre = trimws(sub(",.*", "", genre))) 
    df <- df %>% filter(!is.na(date_end))
    df <- df %>%  mutate(days = as.numeric(date_end-date_start)+1, pages_per_day = page_number / days
    ) %>% mutate(genre = fct_rev(factor(genre, levels = colors %>% filter(category == "genre") %>% .$name)))
    df_months <- df %>% rowwise() %>%  mutate(month_seq = list(seq(
      floor_date(date_start, "month"),
      floor_date(date_end, "month"),
      by = "month"))) %>% unnest(month_seq) %>% ungroup() %>% mutate(month = format(month_seq, "%Y-%m"), month_str = format(month_seq, "%y-%b"), month_str = factor(month_str, levels = unique(month_str[order(month)])))
    f <- df_months %>% drop_na(genre) %>% arrange(genre) %>% mutate(month_count = n(), pages_per_month = page_number/month_count, .by=name) %>%
      group_by(month_str, genre) %>%  summarize(books = str_flatten(name, collapse = "<br>"), pages = round(sum(pages_per_month)), rating = round(mean(rating),2)) %>% plot_ly(
        x = ~month_str,
        y = ~pages,
        color = ~genre,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        customdata = ~genre,
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)
evals$monthly_by_format <- evals$monthly_by_format %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    vals <- if (!is_card) get_colormap("format") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$format))), 0.8))
    df <- df %>%  mutate(days = as.numeric(date_end-date_start)+1, pages_per_day = page_number / days
    ) %>% separate_rows(format, sep = ", ")  %>% filter(format != "audiobook") %>% mutate(format = factor(format, levels = colors %>% filter(category == "format") %>% .$name))
    df_months <- df %>% rowwise() %>%  mutate(month_seq = list(seq(
      floor_date(date_start, "month"),
      floor_date(date_end, "month"),
      by = "month"))) %>% unnest(month_seq) %>% group_by(name) %>% mutate(month_count = n(), pages_per_month = page_number/month_count) %>% ungroup() %>% mutate(month = format(month_seq, "%Y-%m"),month_str = format(month_seq, "%y-%b"), month_str = factor(month_str, levels = unique(month_str[order(month)])))
    df_months <- df_months %>% drop_na(format) %>% arrange(format) %>% 
      group_by(month_str, format) %>%  summarize(books = str_flatten(name, collapse = "<br>"), pages = round(sum(pages_per_month)), rating = round(mean(rating),2)) 
    f <- df_months %>% plot_ly(
        x = ~month_str,
        y = ~pages,
        color = ~format,
        colors = vals,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        customdata = ~format,
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)

evals$origin_by_state <- evals$origin_by_state %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("state") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$state))), 0.8))
    
    f <- df %>% mutate(origin = factor(origin, levels = colors %>% filter(category == "origin") %>% .$name),state = fct_rev(factor(state, levels = colors %>% filter(category == "state") %>% .$name))) %>% drop_na(origin) %>% 
      group_by(origin, state) %>% summarize(count = n()) %>% plot_ly(
        x = ~origin,
        y = ~count,
        color = ~state,
        colors = vals,
        type = "bar"
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)

evals$owned_origin <- evals$owned_origin %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("origin") else shades::brightness(colorRampPalette( color_themes[[input$theme]]), 0.8)(4 + length(unique(df$origin))) 
    
    f <- df %>% mutate(origin = factor(origin, levels = colors %>% filter(category == "origin") %>% .$name)) %>% drop_na(origin) %>% 
      group_by(origin) %>%  summarize(count = n()) %>% arrange(origin) %>% plot_ly(
        labels = ~origin,
        values = ~count,
        marker = list(colors = vals),
        type = "pie",
        sort = FALSE,
        direction = "clockwise"
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = if (!is_card) TRUE else FALSE)
    f %>% apply_sleektheme()
  }
)

evals$owned_state <- evals$owned_state %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("state") else shades::brightness(colorRampPalette( color_themes[[input$theme]]), 0.8)(4 + length(unique(df$state))) 
    
    f <- df %>% mutate(state = factor(state, levels = colors %>% filter(category == "state") %>% .$name)) %>% drop_na(reason) %>% 
      group_by(state) %>%  summarize(count = n()) %>% arrange(state) %>% plot_ly(
        labels = ~state,
        values = ~count,
        marker = list(colors = vals),
        type = "pie",
        sort = FALSE,
        direction = "clockwise"
      )%>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = if (!is_card) TRUE else FALSE)
    f %>% apply_sleektheme()
  }
)


evals$read_format <- evals$read_format %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("format",TRUE) else shades::brightness(colorRampPalette( color_themes[[input$theme]]), 0.8)(4 + length(unique(df$format)))
    
    slice_text <- if (!is_card) "<br>%{percent} (%{value})" else "%{value}"
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    
    df <- df %>% separate_rows(format, sep = ", ") %>% filter(format != "audiobook") %>% 
      left_join(colors %>% filter(category == "format") %>% select(name, summary_name), by = c("format" = "name")) %>% 
      mutate(format = factor(summary_name, levels = colors %>% filter(category == "format") %>% .$summary_name %>% unique())) %>% 
      group_by(format) %>% summarize(count = n(),books = str_flatten(name, collapse = "<br>"))
    f <- df %>% 
       plot_ly(
        labels = ~format,
        values = ~count,
        marker = list(colors = vals),
        type = "pie",
        sort = FALSE,
        direction = "clockwise",
        text = ~I(books),  
        customdata = ~format,
        texttemplate = slice_text,
        hovertemplate = hover_text)%>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = if (!is_card) TRUE else FALSE)
    f %>% apply_sleektheme()
  }
)

evals$read_length <- evals$read_length %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("page_number") else shades::brightness(colorRampPalette( color_themes[[input$theme]]), 0.8)(4 + length(unique(df$page_number))) 
    slice_text <- if (!is_card) "<br>%{percent} (%{value})" else "%{value}"
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    df <- df %>%
      mutate(page_number = factor(page_number, levels = colors %>% filter(category == "page_number") %>% .$name)) %>% 
      group_by(page_number) %>% summarize(count = n(),books = str_flatten(name, collapse = "<br>"))
    f <- df %>% 
      plot_ly(
        labels = ~page_number,
        values = ~count,
        marker = list(colors = vals),
        type = "pie",
        sort = FALSE,
        direction = "clockwise",
        text = ~I(books),  
        customdata = ~page_number,
        texttemplate = slice_text,
        hovertemplate = hover_text,
        hole = 0.5
      )%>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = if (!is_card) TRUE else FALSE)
    f %>% apply_sleektheme()
  }
)

evals$read_origin <- evals$read_origin %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("origin") else shades::brightness(colorRampPalette( color_themes[[input$theme]]), 0.8)(4 + length(unique(df$origin)))
    slice_text <- if (!is_card) "<br>%{percent} (%{value})" else "%{value}"
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    df <- df %>% filter(owned == TRUE) %>% 
      mutate(origin = factor(origin)) %>% 
      group_by(origin) %>% summarize(count = n(),books = str_flatten(name, collapse = "<br>"))
    f <- df %>% 
      plot_ly(
        labels = ~origin,
        values = ~count,
        marker = list(colors = vals),
        type = "pie",
        sort = FALSE,
        direction = "clockwise",
        text = ~I(books),  
        customdata = ~origin,
        texttemplate = slice_text,
        hovertemplate = hover_text, hoverinfo = if (is_card) "label" else "text+label",
        hole = 0.5
      )%>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = if (!is_card) TRUE else FALSE)
    f %>% apply_sleektheme()
  }
)

evals$read_type <- evals$read_type %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    vals <- if (!is_card) get_colormap("type") else shades::brightness(colorRampPalette( color_themes[[input$theme]]), 0.8)(4 + length(unique(df$type)))
    slice_text <- if (!is_card) "<br>%{percent} (%{value})" else "%{value}"
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{customdata}"
    df <- df %>%
      mutate(type = factor(type, levels = colors %>% filter(category == "type") %>% .$name)) %>% 
      group_by(type) %>% summarize(count = n(),books = str_flatten(name, collapse = "<br>"))
    f <- df %>% 
      plot_ly(
        labels = ~type,
        values = ~count,
        marker = list(colors = vals),
        type = "pie",
        sort = FALSE,
        direction = "clockwise",
        text = ~I(books),  
        customdata = ~type,
        texttemplate = slice_text,
        hovertemplate = hover_text
      )%>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = if (!is_card) TRUE else FALSE)
    f %>% apply_sleektheme()
  }
)


evals$mood_map <- evals$mood_map %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{text}"
    vals <- if (!is_card) get_colormap("mood") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$mood))), 0.8))
  
    
    df <- df %>% filter(!is.na(date_end)) %>% mutate(tone = case_when(
      mood == "unsetteling" ~ 0,
      mood == "suspense"    ~ 0.3,
      mood == "playful"     ~ 1,
      mood == "easy"        ~ 0.7,
      mood == "sharp"       ~ 0.4,
      mood == "gentle"      ~ 0.8,
      mood == "intimate"    ~ 0.6,
      mood == "epic"        ~ 0.4,
      mood == "human"       ~ 0.5,
      mood == "liminal"     ~ 0.5,
      mood == "strong"      ~ 0.1,
      mood == "demanding"   ~ 0.4,
      mood == "curiosity"   ~ 0.5,
      TRUE ~ NA_real_),
      month = floor_date(date_end, "month"),
      mood = factor(mood, levels = colors %>% filter(category == "mood") %>% .$name)
      )
    
    most_common <- df %>%
      count(mood) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(mood)
    
    line_color <- if (is_card) {
      ramp <- vals
      ramp[ceiling(length(ramp) / 5)] %>% scales::alpha(0.8)
    } else {
      scales::alpha(
        vals[which(levels(df$mood) == most_common)],
        0.8
      )
    }
    
    df <- df %>%
      arrange(date_end, date_start, series) %>%
      mutate(
        month_id = dense_rank(month)
      ) %>%
      group_by(month) %>%
      mutate(
        n = n(),
        pos = (row_number() - 0.5) / n,
        book_map = month_id - 1 + pos
      ) %>%
      ungroup()
     
    f <- df %>% 
      plot_ly() %>%
      add_markers(
        data = df,
        x = ~book_map,
        y = ~tone,
        mode = "markers",
        
        color = ~mood,
        colors = vals,
        
        text = ~I(name),
        customdata = ~mood,
        hovertemplate = hover_text
      ) %>%
      add_lines(
        data = df,

        x = ~book_map,
        y = ~tone,
        type = "scatter",
        mode = "lines",
        
        line = list(shape = "spline",smoothing = 1,color = line_color)
        
      ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        hovermode = "closest",
        
        xaxis = list(
          title = NULL,
          tickmode = "array",
          tickvals = unique(df$month_id - 0.5),
          ticktext = format(unique(df$month), "%b"),
          zeroline = FALSE
        ),
        
        yaxis = list(
          showticklabels = FALSE,
          showline = FALSE,
          ticks = ""
          
        )
      )
    f %>% apply_sleektheme()%>% layout(xaxis = list(title = NULL))
  }
)

evals$page_map <- evals$page_map %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{text}"
    vals <- if (!is_card) get_colormap("page_number") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$page_number))), 0.8))
    
    df <- df %>% filter(!is.na(date_end)) %>% mutate(
      month = floor_date(date_end, "month"),
      page_number = factor(page_number, levels = colors %>% filter(category == "page_number") %>% .$name)
    )
    most_common <- df %>%
      count(page_number) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(page_number)
    
    line_color <- if (is_card) {
      ramp <- vals
      ramp[ceiling(length(ramp) / 5)] %>% scales::alpha(0.8)
    } else {
      scales::alpha(
        vals[which(levels(df$page_number) == most_common)],
        0.8
      )
    }
    
    df <- df %>%
      arrange(date_end, date_start, series) %>%
      mutate(
        month_id = dense_rank(month)
      ) %>%
      group_by(month) %>%
      mutate(
        n = n(),
        pos = (row_number() - 0.5) / n,
        book_map = month_id - 1 + pos
      ) %>%
      ungroup()
    
    f <- df %>% 
      plot_ly() %>%
      add_markers(
        data = df,
        x = ~book_map,
        y = ~pages,
        mode = "markers",
        
        color = ~page_number,
        colors = vals,
        
        text = ~I(name),
        customdata = ~pages,
        hovertemplate = hover_text
      ) %>%
      add_lines(
        data = df,
        
        x = ~book_map,
        y = ~pages,
        type = "scatter",
        mode = "lines",
        
        line = list(shape = "spline",smoothing = 1,color = line_color),
        
      ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        hovermode = "closest",
        
        xaxis = list(
          title = NULL,
          tickmode = "array",
          tickvals = unique(df$month_id - 0.5),
          ticktext = format(unique(df$month), "%b"),
          zeroline = FALSE
        ),
        
        yaxis = list(
          title = NULL,
          showticklabels = FALSE,
          showline = FALSE,
          ticks = ""
          
        )
      )
    f %>% apply_sleektheme()%>% layout(xaxis = list(title = NULL))
  }
)

evals$fiction_map <- evals$fiction_map %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- if (!is_card) "%{text}<extra>%{customdata}</extra>" else "%{text}"
    vals <- if (!is_card) get_colormap("genre") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$genre))), 0.8))
    
    genre_lookup <- colors %>%
      filter(category == "genre") %>%
      mutate(
        fiction = case_when(
          summary_name == "ya" ~ 0.7,
          summary_name == "fantasy" ~ 1,
          summary_name == "romance" ~ 0.5,
          summary_name == "other literature" ~ 0.4,
          summary_name == "other language" ~ 0.5,
          summary_name == "literature" ~ 0.4,
          summary_name == "suspense" ~ 0.3,
          summary_name == "non-fiction" ~ 0,
          summary_name == "sci-fi" ~ 0.9,
          TRUE ~ NA_real_
        )
      )
    
    df <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre))
    df <- df %>% filter(!is.na(date_end)) %>% left_join(genre_lookup, by = c("genre" = "name")) %>%
      mutate(
      month = floor_date(date_end, "month"),
      genre = factor(genre, levels = colors %>% filter(category == "genre") %>% .$name)
    )
    df <- df %>%
      group_by(name) %>%
      mutate(fiction = mean(fiction, na.rm = TRUE)) %>%
      slice(1) %>%
      ungroup()
    
    most_common <- df %>%
      count(genre) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(genre)
    
    line_color <- if (is_card) {
      ramp <- vals
      ramp[ceiling(length(ramp) / 5)] %>% scales::alpha(0.8)
    } else {
      scales::alpha(
        vals[which(levels(df$genre) == most_common)],
        0.8
      )
    }
    
    
    df <- df %>%
      arrange(date_end, date_start, series) %>%
      mutate(
        month_id = dense_rank(month)
      ) %>%
      group_by(month) %>%
      mutate(
        n = n(),
        pos = (row_number() - 0.5) / n,
        book_map = month_id - 1 + pos
      ) %>%
      ungroup()
    
    f <- df %>% 
      plot_ly() %>%
      add_markers(
        data = df,
        x = ~book_map,
        y = ~fiction,
        mode = "markers",
        
        color = ~genre,
        colors = vals,
        
        text = ~I(name),
        customdata = ~genre,
        hovertemplate = hover_text
      ) %>%
      add_lines(
        data = df,
        
        x = ~book_map,
        y = ~fiction,
        type = "scatter",
        mode = "lines",
        
        line = list(shape = "spline",smoothing = 1,color = line_color),
        
      ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        hovermode = "closest",
        
        xaxis = list(
          title = NULL,
          tickmode = "array",
          tickvals = unique(df$month_id - 0.5),
          ticktext = format(unique(df$month), "%b"),
          zeroline = FALSE
        ),
        
        yaxis = list(
          showticklabels = FALSE,
          showline = FALSE,
          ticks = ""
          
        )
      )
    f %>% apply_sleektheme() %>% layout(xaxis = list(title = NULL))
    
    
  }
)

evals$pages_by_genre <- evals$pages_by_genre %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- "%{text}<extra>%{customdata}</extra>" 
    vals <- if (!is_card) get_colormap("genre") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$genre))), 0.8))
    df <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre))
    df <- df %>% filter(!is.na(date_end))
    f <- df %>% drop_na(genre) %>% arrange(date_end) %>%
      group_by(genre) %>% summarize(books = str_flatten(name, collapse = "<br>"),pages = sum(pages, na.rm = TRUE)) %>% plot_ly(
        x = ~genre,
        y = ~pages,
        color = ~genre,
        colors = vals,
        customdata = ~pages,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        hovertemplate = hover_text
      ) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE, barmode = "stack",hovermode = "x")
    f %>% apply_sleektheme()
  }
)

evals$pages_by_mood <- evals$pages_by_mood %>% list_modify(
  plot = function(df, input, is_card = FALSE) {
    hover_text <- "%{text}<extra>%{customdata}</extra>" 
    vals <- if (!is_card) get_colormap("mood") else as.character(shades::brightness(colorRampPalette(color_themes[[input$theme]])(1 + length(unique(df$mood))), 0.8))
    df <- df %>% filter(!is.na(date_end))

    f <- df %>% drop_na(mood) %>% arrange(date_end) %>%
      group_by(mood) %>% summarize(books = str_flatten(name, collapse = "<br>"),pages = sum(pages, na.rm = TRUE)) %>% plot_ly(
        x = ~mood,
        y = ~pages,
        color = ~mood,
        colors = vals,
        customdata = ~pages,
        type = "bar",
        text = ~I(books),  
        textposition = "none",
        hovertemplate = hover_text
        
      ) %>% layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        barmode = "stack",
        hovermode = "x"
      )
    f %>% apply_sleektheme()
  }
)