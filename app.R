suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(magrittr)
  library(dplyr)
  library(rlang)
  library(car)
  library(xtable)
  library(performance)
  library(see)
  library(shiny)
  library(shinyWidgets)
})

source("registry.R")

options(
  shiny.sanitize.errors = FALSE,
  shiny.reactlog = TRUE,
  error = traceback,
  shiny.fullstacktrace = TRUE
)


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  tags$head(
    tags$style(HTML(
      "
      p{font-size: 18px;}
      .nav-tabs {
        position: sticky;
        top: 50px;
        background: white;
        z-index: 999;
        padding-bottom: 10px;
        border-bottom: none;
        tags$style(HTML(
      .js-plotly-plot .plotly .main-svg .xtick text, 
      .js-plotly-plot .plotly .main-svg .ytick text,
      .js-plotly-plot .plotly .main-svg .gtitle,
      .js-plotly-plot .plotly .main-svg .xtitle,
      .js-plotly-plot .plotly .main-svg .ytitle,
      .js-plotly-plot .plotly .main-svg .nums {
        fill: currentColor !important;
      }
      
      .js-plotly-plot .plotly .main-svg .gridlayer path {
        stroke: currentColor !important;
        opacity: 0.1 !important;
      }
      }
      
      .staggered-shelf img { 
  transition: all 0.3s ease; 
}
.staggered-shelf img:hover { 
  transform: scale(1.1) translateY(-10px); 
  z-index: 100; 
}
      
      "
    ))
  ),
  br(),
  tags$div(
    style = "
      position: sticky;
      padding-top: 10px;
      top: 0;
      gap: 20px;
      background: white;
      z-index: 1000;
      display: flex;
    ",
    selectInput("year", "Year", choices = c("2026", "2025", "dated", "all")),
    uiOutput("get_theme")
    
  ),
  tabsetPanel(
    id = "main_navigation",
    tabPanel("All Books",
             fluidPage(
               div(style = "display:flex; gap: 20px; align-items:flex-end; width:100%;",
                   uiOutput("current_reads", style = "flex: 1;"),
                   div(style = "text-align:center; flex: 0 0 200px;", 
                       actionButton("pick_btn", "Pick Next Book", class = "btn-primary"),br(),br(),
                       uiOutput("spinner"))),br(),br(),
               
               div(h4("Rating Distribution", style = "text-align:center; color:#777;"),
                   selectInput("pick_rating", "By:", 
                               choices = c("General" = "distribution_ratings", "Mood" = "ratings_by_mood", "Mood Category" = "ratings_by_mood_category", "Genre" = "ratings_by_genre", "Genre Category" = "ratings_by_genre_category", 
                                           "Reason" = "ratings_by_reason", "Recommendation" = "ratings_by_rec")),
                   plotlyOutput("view_rating")),
               br(),br(),
               div(h4("Maps", style = "text-align:center; color:#777;"),
                   selectInput("pick_map", "By:", 
                               choices = c("Mood" = "mood_map", "Pages" = "page_map", "Fiction" = "fiction_map")),
                   plotlyOutput("view_map")),
               br(),br(),
               div(h4("Monthly Distribution", style = "text-align:center; color:#777;"),
                   selectInput("pick_monthly", "By:", 
                               choices = c("Count / Reason" = "monthly_by_reason", "Pages / Format" = "monthly_by_format",
                                            "Pages / Genre" = "monthly_by_genre", "Speed / Mood" = "monthly_by_mood")),
                   plotlyOutput("view_monthly")),
               br(),br(),
               
               div(h4("Read Books Distribution", style = "text-align:center; color:#777;"),
                   selectInput("pick_structure", "By:", 
                               choices = c("Format" = "read_format", "Length" = "read_length", 
                                           "Origin" = "read_origin", "Type" = "read_type", "Genre" = "pages_by_genre", "Mood" = "pages_by_mood")),
                   plotlyOutput("view_structure")),
               br(),br(),
               
               div(h4("Owned Books Distribution", style = "text-align:center; color:#777;"),
                   selectInput("pick_owned", "By:", 
                               choices = c("State" = "owned_state", "Origin" = "owned_origin", "Origin / Status" = "origin_by_state")),
                   plotlyOutput("view_owned")),
               
               br(), br(),
               uiOutput("highly_anticipated"),
               br(),br(),
             )
    ),
    tabPanel("Yearly Wrapup",
             br(),
             #wordcloud2Output("genre_cloud"),
             
             br(),
             div(
               style="text-align:center; margin-bottom:30px;",
               uiOutput("tab_title")
             ),
             hr(),
             uiOutput("new_wrapped"),
             br()
             

    )
))

'
             div(
               style="display:flex; gap:20px;",
               div(style="flex:1;", plotlyOutput("read_format")),
               div(style="flex:1;", plotlyOutput("read_length"))
             ),
             div(
               style="display:flex; gap:20px;",
               div(style="flex:1;", plotlyOutput("read_reason")),
               div(style="flex:1;", plotlyOutput("read_type"))
             )
             '
#df <- books %>% filter(state=="read")

server <- function(input, output, session){
  df_books <- reactive(apply_filters(books, input))
  df_owned <- reactive(books %>% filter(owned == TRUE))
  
  df_authors <- reactive({
    authors %>% right_join(df_books() %>% select(name, rating, theme, author, state)) %>% group_by(author) %>% 
      mutate(themes = str_flatten(unique(theme), collapse = ", "))
  })
  
  df_themes <- reactive({
    themes %>% right_join(df_books() %>% select(name, rating, genre, theme, state)) %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>% separate_rows(theme, sep = ", ") %>% group_by(theme) %>% 
      mutate(genres = str_flatten(unique(genre), collapse = ", ") )
  })
  
  list_df <- list(
    df_books = df_books,
    df_authors = df_authors,
    df_themes = df_themes,
    df_owned = df_owned
  )
  
  nextbooklist <- books %>% filter(!(state %in% c("read", "currently reading"))) %>% filter(expectation >= 4.25) %>% group_by(series_base) %>% arrange(series) %>% slice(1) %>% ungroup() %>% arrange(expectation, genre) 
  random_choice <- eventReactive(input$pick_btn, {
    if (!isTRUE(input$pick_btn > 0)) {
      return(list(
        name = "next title", 
        img = "https://png.pngtree.com/png-vector/20240131/ourmid/pngtree-blank-book-cover-png-image_11522681.png" # Path to a generic cover or icon
      ))
    }
    chosen <- nextbooklist %>% slice_sample(n = 1)
    # Return both the name and the image path
    list(name = chosen$name, img = chosen$cover)
  }, ignoreNULL = FALSE)
    
  # loop
  
  for (id in names(evals)) {
    local({
      cfg <- evals[[id]]
      dataframe <- list_df[[cfg$df]]
      build_cell_components(id, cfg, input, output, dataframe)
    })
  }
  output$spinner <- renderUI({
    current_book <- random_choice()
    card(
      title = paste(random_choice()$name),
      body_text = div(style = "text-align:center;",
        cov_f(random_choice()$img)
      ),
      layout = list(width = "auto", theme = "none")
    )
  })
  output$current_reads <- renderUI({
    active <- books %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>% left_join(colors %>% filter(category=="genre"), by = c("genre" = "name")) %>% filter(state == "currently reading") %>% distinct(name, .keep_all = TRUE) %>% arrange(date_start)
    slots <- lapply(unique(active$type), function(g) {
      bks <- active %>% filter(type == g)
      is_warn <- nrow(bks) > 1
      is_bad <- nrow(bks) > 2
      
      div(style = "margin: 10px; text-align: center;",
          div(style = "font-size: 0.7rem; color: gray;", bks$type[1]),
          div(style = sprintf("border: 2px solid %s; border-radius: 15px; padding: 3px; display: flex; align-items: center; gap: 3px;", 
                              if(is_bad) "#d50000" else if (is_warn) "#ffab00" else "#ccc"),
              lapply(1:nrow(bks), function(i) {
                div(style = "display: flex; flex-direction: column; align-items: center;",
                    if(is_warn) span(style = "font-size: 0.6rem; font-weight: bold; color: #666; margin-bottom:0px;", 
                         bks$summary_name[i]),
                    bks[i, ] %>% pull(cover) %>% cov_f()
                )
              })

      ))
    })

    card(
      title = "Current Reads",
      body_text = div(style = "display: flex; flex-wrap: wrap; justify-content: center;align-items:flex-end;", slots),
        
      layout = list(
        width = "auto", theme = "none"
      )
    )
  })
  
  
  output$tab_title <- renderUI({
    h1(tagList(input$year, " Reading Wrap-Up"))
  })
  
  output$get_theme <- renderUI({
    df <- df_books() 
    #df<-books %>% filter(state == "read") %>% filter(year(date_end) == as.numeric("2026"))
    genre_both<- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>% left_join(colors %>% filter(category == "genre") %>% select(name, summary_name), by = c("genre" = "name")) %>% mutate(genre = summary_name) %>% group_by(genre) 
    genre_both <- genre_both %>% 
      summarize(n = n(), rating = (n / (n + 2)) * mean(rating) + (2 / (n + 2)) * genre_both %>% ungroup() %>% pull(rating) %>% mean()) 
    mood_both <- df %>% left_join(colors %>% filter(category == "mood") %>% select(name, summary_name), by = c("mood" = "name")) %>% mutate(mood = summary_name) %>% group_by(mood) 
    mood_both <- mood_both%>% 
      summarize(n = n(), rating = (n / (n + 2)) * mean(rating) + (2 / (n + 2)) * mood_both %>% ungroup() %>% pull(rating) %>% mean()) 
    
    genre_cat_amt <- genre_both %>%  arrange(desc(n),desc(rating)) %>% slice(1)%>% pull(genre)
    #genre_cat_amt
    mood_cat_amt <- mood_both %>%  arrange(desc(n),desc(rating)) %>% slice(1)%>% pull(mood)
    
    selectInput(
      inputId = "theme",
      label = "Choose a theme",
      choices = names(color_themes),           # all available theme names
      selected = get_theme_from_archetype(genre_cat_amt, mood_cat_amt)     # default selection
    )
  })
  
  output$new_wrapped <- renderUI({
    df <- df_books()
    top_genre <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>% count(genre, sort = TRUE) %>% slice(1) %>% pull(genre)
    top_mood  <- df %>% count(mood,  sort = TRUE) %>% slice(1) %>% pull(mood)
    top_read_author <- df %>% group_by(author) %>% summarise(n = n(), total_pages = sum(pages, na.rm = TRUE)) %>% arrange(desc(n), desc(total_pages)) %>% slice(1)
    top_read_genre  <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>% group_by(genre) %>% summarise(n = n(), total_pages = sum(pages, na.rm = TRUE)) %>% arrange(desc(n), desc(total_pages)) %>% slice(1)
    top_read_mood   <- df %>% group_by(mood) %>% summarise(n = n(), total_pages = sum(pages, na.rm = TRUE)) %>% arrange(desc(n), desc(total_pages)) %>% slice(1)
    
    top_rated_author <- top_bayes(df, "author")
    top_rated_genre  <- top_bayes(df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)), "genre")
    top_rated_mood   <- top_bayes(df %>% separate_rows(author, sep = ", "), "mood")
    
    pct <- mean(df$rating_rounded == 5, na.rm = TRUE) * 100
    pct_4 <- mean(df$rating > 4, na.rm = TRUE) * 100
    
    most_generous_genre <- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>%
      group_by(genre) %>%
      summarise(avg_rating = mean(rating, na.rm = TRUE), n = n()) %>%
      filter(n >= 3) %>%
      arrange(desc(avg_rating)) %>%
      slice(1)
    
    most_disappointing_author <- df %>%
      filter(!is.na(author)) %>%
      group_by(author) %>%
      summarise(avg = mean(rating, na.rm = TRUE), n = n()) %>%
      filter(n >= 2) %>%
      arrange(avg) %>%
      slice(1)

    longest <- df %>% slice_max(pages, n = 1) %>% slice(1)
    shortest <- df %>% slice_min(pages, n = 1) %>% slice(1)
    fastest <- df %>% arrange(desc(pages)) %>% slice_min(.data$days, n = 1, with_ties = FALSE) %>% slice(1)
    slowest <- df %>% arrange(pages) %>% slice_max(.data$days, n = 1, with_ties = FALSE) %>% slice(1)    
    
    total_pages <- sum(df$pages, na.rm = TRUE)
    avg_pages <- round(mean(df$pages, na.rm = TRUE))
    
    owned_pct <- mean(df$owned == TRUE, na.rm = TRUE) * 100
    
    top3 <- df %>%
      arrange(desc(rating_rounded), desc(rating), desc(reread), desc(enjoyment)) %>%
      slice(1:3)
    
    series_s <- nrow(df %>% filter(grepl("1$", series)))
    series_c <- nrow(df %>% filter(!is.na(series) & !grepl("1$", series) & series_last != TRUE))
    series_t <- nrow(df %>% filter(!is.na(series)))
    series_f <- nrow(df %>% filter(series_last == TRUE))
    series_amt <- n_distinct(df$series, na.rm = TRUE)
    
    series_pct_s <- mean(grepl("1$", df$series)) * 100
    series_pct_c <- mean(!is.na(df$series) & !grepl("1$", df$series) & !grepl("1$", df$series)) * 100
    series_pct_f <- mean(grepl("1-", df$series)) * 100
    series_pct_t<- mean(!is.na(df$series)) * 100
    
    rec_pct <- mean(!is.na(df$rec)) * 100
    
    type_pct <- df %>% count(type, sort=TRUE) %>% mutate(pct = n/sum(n)*100)
    themes_top <- df %>% separate_rows(theme, sep = ", ") 
    glob <- themes_top %>%
      distinct(name, .keep_all = TRUE) %>%
      pull(rating) %>% mean()
    themes_top <- themes_top %>% group_by(theme) %>% summarise(
        book_count = n(),
        avg_rating = mean(rating, na.rm = TRUE),
        bay_avg = (book_count / (book_count + 2)) * avg_rating +
          (2 / (book_count + 2)) * glob
      ) %>% filter(book_count >= 2) %>% arrange(desc(bay_avg), desc(book_count)) %>%
      slice(1:3)
    
    formats <- df %>% count(format, sort=TRUE) %>% mutate(pct = n/sum(n)*100)
    reasons_top <- df %>% count(reason, sort=TRUE)
    origins_top <- df %>% filter(owned) %>%  count(origin, sort=TRUE)
    
    top_format <- formats %>% slice(1)
    top_origin <- origins_top %>% slice(1)
    
    top_reason <- reasons_top %>% slice(1)%>% pull(reason)%>% .[1]
    main_type <- type_pct %>% slice(1)
    worst <- df %>% arrange(rating) %>% slice(1)
    avg_days <- mean(df$days, na.rm=TRUE, trim = 0.1)
    
    rec_sources <- df %>% left_join(colors %>% filter(category == "rec") %>% select(name, summary_name), by = c("rec" = "name")) %>% mutate(rec_source = summary_name) %>% group_by(rec_source) %>% 
      summarize(rating = mean(rating, na.rm=TRUE), n = n()) %>%  arrange(desc(rating)) 
    top_rec <- rec_sources %>% slice(1)%>% pull(rec_source)%>% .[1]
    top_rec_amt <- rec_sources %>% filter(!is.na(rec_source)) %>% arrange(desc(n)) %>% slice(1) %>% pull(rec_source)%>% .[1]
    
    reasons_rt <- df %>% group_by(reason) %>% 
      summarize(rating = mean(rating, na.rm=TRUE)) %>%  arrange(desc(rating)) 
    top_reason_rt <- reasons_rt %>% slice(1) %>% pull(reason)%>% .[1]
    
    genre_both<- df %>% separate_longer_delim(genre, delim = ", ") %>% mutate(genre = trimws(genre)) %>% left_join(colors %>% filter(category == "genre") %>% select(name, summary_name), by = c("genre" = "name")) %>% mutate(genre = summary_name) %>% group_by(genre) 
    genre_both <- genre_both %>% 
      summarize(n = n(), rating = (n / (n + 2)) * mean(rating) + (2 / (n + 2)) * genre_both %>% ungroup() %>% pull(rating) %>% mean()) 
    mood_both <- df %>% left_join(colors %>% filter(category == "mood") %>% select(name, summary_name), by = c("mood" = "name")) %>% mutate(mood = summary_name) %>% group_by(mood) 
    mood_both <- mood_both%>% 
      summarize(n = n(), rating = (n / (n + 2)) * mean(rating) + (2 / (n + 2)) * mood_both %>% ungroup() %>% pull(rating) %>% mean()) 
    
    genre_cat_amt <- genre_both %>%  arrange(desc(n),desc(rating)) %>% slice(1)%>% pull(genre)
    genre_cat_rat <- genre_both %>%  arrange(desc(rating),desc(n)) %>% slice(1)%>% pull(genre)
    mood_cat_amt <- mood_both %>%  arrange(desc(n),desc(rating)) %>% slice(1)%>% pull(mood)
    mood_cat_rat <- mood_both %>%  arrange(desc(rating),desc(n)) %>% slice(1)%>% pull(mood)
    
    year_bias <- df %>%
      mutate(pub_year_clean = case_when(
        grepl("^\\d{4}$", publication_date) ~ as.numeric(publication_date),
        grepl("^\\d{4}s$", publication_date) ~ as.numeric(substr(publication_date, 1, 4)) + 5,
        grepl("^\\d{4}-\\d{4}$", publication_date) ~ 
          (as.numeric(substr(publication_date, 1, 4)) + as.numeric(substr(publication_date, 6, 9))) / 2,
        grepl("^\\d{4}s-\\d{4}s$", publication_date) ~ 
          (as.numeric(substr(publication_date, 1, 4)) + as.numeric(substr(publication_date, 6, 9))) / 2 + 10,
        TRUE ~ NA_real_)) %>% pull(pub_year_clean) %>%  median(na.rm = TRUE)
    
    author_stats <- df %>% separate_rows(author, sep = ", (?=[A-Za-zÀ-ÖØ-öø-ÿ]+,)") %>% distinct(author, .keep_all = TRUE) %>% left_join(authors) %>%  summarise(
      pct_non_male = mean(gender != "male", na.rm = TRUE),
      pct_male = mean(gender == "male", na.rm = TRUE),
      pct_lgbt = mean(lgbt == TRUE, na.rm = TRUE),
      n_ethnicities = n_distinct(ethnicity),
      n = n(),
      ethnicities = str_flatten(unique(na.omit(ethnicity)), collapse = ", ")
    )
    
    covers <- df %>% arrange(date_end, date_start) %>% merge_series_covers(s = 0.8) %>% pull(cover)
    star_covers <- df %>% filter(rating>=4.875) %>% arrange(date_end, date_start) %>% merge_series_covers(s = 0.8) %>% pull(cover)

    expect <- df %>% mutate(abs_diff = abs(expectation_difference))
    surprises <- expect %>% arrange(desc(expectation_difference), desc(rating)) %>% slice(c(1:3, (n()-2):n()))
    exp_books <- df %>% arrange(desc(reading_experience)) %>% slice(c(1:3, (n()-2):n()))
    cutoff <- 0.25
    exceeded <- expect %>% filter(expectation_difference > cutoff) %>% merge_series_covers(s = 0.8) %>% arrange(desc(reading_experience))
    missed <- expect %>% filter(expectation_difference < -cutoff) %>% merge_series_covers(s = 0.8) %>% arrange(reading_experience)
    met <- expect %>% filter(expectation_difference >= -cutoff & expectation_difference <= cutoff) %>% merge_series_covers(s = 0.8) %>% arrange(desc(reading_experience))

    personal_5 <- df %>% filter(rating_personal>=4.875 & rating_technical < 4.875) %>% merge_series_covers(s = 0.8) %>% arrange(desc(rating))
    technical_5 <- df %>% filter(rating_technical>=4.875 & rating_personal < 4.875) %>% merge_series_covers(s = 0.8) %>% arrange(desc(rating))
    hall_of_fame <- df %>% filter(rating_technical>=4.875 & rating_personal >= 4.875) %>% merge_series_covers(s = 0.8) %>% arrange(desc(rating))
    
    streak <- df %>% arrange(date_end) %>% with(rle(.$rating_rounded >= 4.5), max(lengths[values]))
    low_books <- nrow(df %>% filter(rating <= 2))
    
    theme_choice <- input$theme

    streaks <- df %>% arrange(date_end) %>%
      mutate(good = rating_rounded >= 4.5,  streak_id = cumsum(!good)) %>%
      filter(good) %>%
      group_by(streak_id) %>%
      summarise(
        length = n(),
        start = min(date_end),
        end = max(date_end),
        .groups = "drop"
      ) %>%
      arrange(desc(length))
    
    best_streak <- streaks %>% slice(1)
    
    streak_months <- paste0(
      format(best_streak$start, "%B"),
      " - ",
      format(best_streak$end, "%B"))
    
    dark_theme <- grepl("^2\\s", theme_choice)
    glass_bg <- if(dark_theme) "rgba(0, 0, 0, 0.15)" else "rgba(255,255,255,0.15)"
    
    div(
      style="
    display:flex;
    flex-wrap:wrap;
    gap:20px;
    width:100%;
    align-items: stretch;
    ",

      card(
        identity = list(
          label = "Your Reader Persona is",
          value = get_book_identity(genre_cat_amt, mood_cat_amt, type = "archetype")
        ),
        
        body_text = 
          div(
            style = "display:flex; flex-direction: column; gap:3px; align-items:center; margin-top:5px;",
            div(
              style = "font-weight:500; font-size:0.9em; opacity: 0.9;",
              "Your Narrative Lens"
            ),
            div(
              style = paste0("display:inline-block; background:", glass_bg,"; font-weight:600; font-size:1em; padding:4px 10px; border-radius:12px; margin-top:5px;"),
              get_book_identity(genre_cat_rat, mood_cat_rat, type = "niche")
            )
        
        ),
        layout = list(
          width = "auto", theme = theme_choice,
          identity_pre = TRUE,
          index = 1
        )
      ),
      
      card(
        title = "Your Highlights",
        
        body_text = div(
            style="display:flex; flex-direction:column; gap:10px; width:100%;",
            div(
              style = "display:flex; justify-content:space-between; width:100%; opacity:0.7; font-size:12px; font-weight:400;",
              div("most read"),
              div("top rated")
            ),
            div(
              style = "display:flex; gap:8px; flex-wrap: wrap;justify-content: space-between;",
              badge(tolower(top_read_author$author), glass_bg),
              badge(tolower(top_rated_author$author), glass_bg)
            ),
            div(
              style = "display:flex; gap:8px; justify-content: space-between;flex-wrap: wrap;",
              badge(paste0(top_read_genre$genre), glass_bg),
              badge(paste0(top_rated_genre$genre), glass_bg)
            ),
            div(
              style = "display:flex; gap:8px;justify-content: space-between; flex-wrap: wrap;",
              badge(paste0(top_read_mood$mood), glass_bg),
              badge(paste0(top_rated_mood$mood), glass_bg)
            )
          ),
        layout = list(
          width = "small", theme = theme_choice,
          index = 2
        )),
      
      card(
        title = NULL,
        stats = list(
          list(label = "books read", value = nrow(df) ),
          list(label = "pages read", value = total_pages ),
          list(label = "avg book length", value = avg_pages ),
          list(label = "avg rating", value = round(mean(df$rating),2) )
        ),
        
        layout = list(
          width = "mini", theme = theme_choice,
          index = 3
        )
      ),
      card(
        plot = "distribution_ratings_card",
        
        layout = list(
          width = "plot", theme = theme_choice,
          index = 4
        )
      ),
      card(
        
        insights = list(
          intro = tagList(round(pct,1), "% of your books earned the 5★ treatment."),
          insight = case_when(
            pct > 80 ~ "Every book gets a gold star and a hug.",
            pct > 50 ~ "You either really know how to pick your books or you have a people pleaser problem.",
            pct < 5 ~ "Perfection, to you, is almost theoretical.",
            pct < 10 ~ "An approval stamp from you has real meaning.",
            TRUE ~ "Selective, but fair. A critic with restraint."
          )
        ),
        
        identity = list(
          label = "Rating Personality",
          value = case_when(
            pct_4 > 80 ~ 
              "Golden Retriever",
            pct_4 > 65 ~ 
              "Joyful Explorer", 
            pct_4 < 10 ~ 
              "Ice-Cold Critic",
            pct_4 < 40 ~ 
              "Stern Judge", 
            TRUE ~ 
              "Nuanced Arbitrator" 
          )
        ),
        layout = list(
          width = "auto", theme = theme_choice,
          identity_pre = TRUE,
          index = 5
        )
      ),
      card(
        title = "Your Shelf",
        body_text = div(class = "staggered-shelf",
                        style = "display: flex; flex-wrap: wrap; justify-content: space-evenly;",
                        covers),
        layout = list(
          width = "total", theme = theme_choice,
          index = 6
        )
      ),

      card(
        title = "Your Reading Rhythm",
        body_text = tagList("You read ", nrow(df), " books this year. ",
          tagList("That’s about ", round(nrow(df)/12,1), " books per month."),
          case_when(
            nrow(df) > 150 ~ "Are you even human?",
            nrow(df) > 100 ~ "Menacing.",
            nrow(df) > 60 ~ "Concerning.",
            nrow(df) > 30 ~ "Impressive!",
            nrow(df) > 15 ~ "Solid.",
            TRUE ~ "Selective. Intentional. Every book had to earn its place."
          )
        ),
        
        
        insights = list(
          insight = tagList(case_when(
            avg_days < 6 ~ 
              "You don't read books. You consume them",
            avg_days < 22 & avg_days > 5 ~ 
              "Steady. Sustainable. Therapist-approved",
            TRUE ~ 
              "You marinate. You steep. You *experience* literature"
          ), " (Average time: ", round(avg_days,0), " days)")
        ),
        
        identity = list(
          label = "Reader Identity",
          value = case_when(
            nrow(df)*avg_days > 360 ~ 
              "Marathon Reader",
            nrow(df)*avg_days > 180  ~ 
              "Commited Reader",
            nrow(df)*avg_days > 60  ~ 
              "Steady Regular",
            nrow(df)*avg_days > 30  ~ 
              "Casual Hobbyist", 
            TRUE ~
              "Page Flipper" #i like waiting room but nothing like waitingroom regular, the point isnt being in waitingrooms often, its only reading there, i also like lobby, but again its not about lingering in lobbys its just that you only read when you have nothing else to do aka in a lobby
          )
        ),
        
        layout = list(
          width = "auto", theme = theme_choice,
          identity_pre = FALSE,
          index = 7
        )
      ),
      
      card(
        plot = "monthly_by_reason_card",
        
        layout = list(
          width = "plot", theme = theme_choice,
          index = 8
        )
      ),
    

      card( 
        title = "Range, Baby",
        
        body_text = div(
          style="
      display:grid;
      grid-template-columns:1fr 1fr;
      gap:12px;
    ",
          
          extreme_tile("Longest", longest$name, paste0(longest$pages, " pages")),
          extreme_tile("Shortest", shortest$name, paste0(shortest$pages, " pages")),
          extreme_tile("Fastest", fastest$name, paste0(fastest$days, " days")),
          extreme_tile("Slowest", slowest$name, paste0(slowest$days, " days"))
        ),
        
        
        insights = list(
          insight = case_when(
            longest$pages > 800 & shortest$pages < 120 ~ "From epics to espresso shots. Your attention span contains multitudes.",
            longest$pages > 800 ~ "You had something to prove.",
            shortest$pages < 100 ~ "You like a hit-and-run reading experience.",
            TRUE ~ "You didn't take big risks with size."
          )
        ),
        
        layout = list(
          width = "half",
          index = 9,
          theme = theme_choice
        )
      ),
      
      card( 
        title = "Who Tells Your Stories",
        
        intro = tagList("You read books by ",author_stats$n, " different authors this year." ),
        
        insights = list(
          insight = tagList(
            case_when(
              author_stats$pct_non_male > 0.6 ~ 
                "Your reading leans strongly toward non-male voices.",
              author_stats$pct_non_male > 0.4 ~ 
                "A relatively balanced mix of perspectives.",
              TRUE ~ 
                "Your reading is dominated by male authors."
            ), progress_bar("Non-male authors", author_stats$pct_non_male),
            
            br(),
            case_when(
              author_stats$pct_lgbt > 0.3 ~ 
                "Queer voices are a meaningful part of your reading landscape.",
              author_stats$pct_lgbt > 0.1 ~ 
                "Some LGBTQ+ representation, with room to grow.",
              TRUE ~ 
                "Limited LGBTQ+ representation in in this lineup."
            ),
            progress_bar("LGBTQ+", author_stats$pct_lgbt),
            br(),
            case_when(
              author_stats$n_ethnicities > 7 ~ 
                "A globally diverse reading list.",
              author_stats$n_ethnicities > 5 ~ 
                "A pretty diverse reading list, with room to grow.",
              author_stats$n_ethnicities > 3 ~ 
                "Some diversity, but still clustered.",
              TRUE ~ 
                "A fairly narrow cultural range."
            ), #tagList("(",author_stats$ethnicities, ")"),
            progress_bar("Ethnic diversity", author_stats$n_ethnicities / 10)
            
          )
        ),
        
        layout = list(
          width = "half", theme = theme_choice,
          index = 1
        )
      ),
      
      card(
        title = "Your Bookshelf in Time",
        insights = list(
          intro = tagList("Median Year: ", year_bias),
          insight = case_when(
            year_bias > 2020 ~ 
              "Chronically current. You read in real time with the world.",
            year_bias > 2000 ~ 
              "Mostly contemporary. You like your stories fresh, but not brand new.",
            year_bias > 1980 ~ 
              "Modern with range. You respect the recent past.",
            year_bias > 1900 ~ 
              "You like your books tested by time.",
            TRUE ~ 
              "You read like you own a study and write letters by candlelight."
          )
        ),
        identity = list(
          label = "Your Reading Era Style",
          value = get_reading_tag(year_bias, avg_pages)            ),
        
        layout = list(
          width = "auto",
          index = 2, theme = theme_choice, identity_pre = FALSE
        )
      ),
      card(
        stats = list(
          list(value = plotlyOutput("mood_map_card", height = "150px") ),
          list(value = plotlyOutput("page_map_card", height = "150px") ),
          list(value = plotlyOutput("fiction_map_card", height = "150px"))
        ),
        
        layout = list(
          width = "mini", theme = theme_choice,
          index = 3
        )
      ),
      


      card(
        title = "5-star Reads",
        body_text = div(class = "staggered-shelf",
                        style = "display: flex; flex-wrap: wrap; justify-content:center; gap:5px;",
                        star_covers),
        layout = list(width = "medium", theme = theme_choice, index = 2, inactive = TRUE)
        ),
      card(
        title = "Your Hot Streak",
        
        body_text = div(style = "align: center;",
          tagList("From", strong(streak_months), ","), 
          br(),
          tagList("you read ",
          strong(best_streak$length),
          " great books in a row.")
        ),
        insights = list(
          insight = case_when(
            best_streak$length >= 8 ~ 
              "Everything you touched turned to gold.",
            best_streak$length >= 5 ~ 
              "You found a groove and stayed in it.",
            TRUE ~ 
              "Short, but undeniable momentum."
          )
        ),
        
        layout = list(width = "small", theme = theme_choice, index = 2, inactive = TRUE)
      ),
    
      card( 
        title = "Reality Check", 
        
        
        intro = "But not every book understood the assignment.",
        
        body_text = tagList(
          strong(most_disappointing_author$author),
          " didn’t live up to expectations (", round(most_disappointing_author$avg,2), "★ avg).",
          br(),
          tagList("\"", worst$name, "\" hit a low of ", worst$rating, "★.")
        ),
        
        footer = "Some books build character. Others just test it.",
        
        layout = list(
          width = "auto", theme = theme_choice,
          index = 3, inactive = TRUE
        )
      ),
      card(
        body_text = div(style = "display:flex; flex-direction: column; gap:20px;",
                        div(style = "display:flex; gap:20px;",
                            card(
                              title = "Exceeded Expectations",
                              background_img = if(dark_theme) "https://wallpaperaccess.com/full/2748948.jpg" else "https://burst.shopifycdn.com/photos/yellow-flowers-reach-to-blue-sky.jpg?width=1000&format=pjpg&exif=0&iptc=0",
                              body_text = div(style="display:flex; flex-wrap:wrap; justify-content:center; gap:15px;",
                                              exceeded$cover
                              ),
                              layout = list(width = "auto", theme = theme_choice, color = FALSE, index = 4)
                            ),
                            card(
                              title = "Missed the Mark",
                              background_img = if(dark_theme) "https://images.unsplash.com/photo-1431440869543-efaf3388c585?fm=jpg&q=60&w=3000&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxzZWFyY2h8MXx8YWVzdGhldGljJTIwd2FsbHBhcGVyfGVufDB8fDB8fHww" else "https://burst.shopifycdn.com/photos/fog-on-dark-waters-edge.jpg?width=1000&format=pjpg&exif=0&iptc=0",
                              body_text = div(style="display:flex; flex-wrap:wrap; justify-content:center; gap:15px;",
                                              missed$cover
                              ),
                              layout = list(width = "auto", theme = theme_choice, color = FALSE, index = 4)
                            )),
          card(
        title = "Met Expectations",
        background_img = if(!dark_theme) "https://plus.unsplash.com/premium_photo-1676070095335-751e5ad358ff?fm=jpg&q=60&w=3000&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxzZWFyY2h8N3x8aGQlMjBiYWNrZ3JvdW5kfGVufDB8fDB8fHww" else "https://burst.shopifycdn.com/photos/mountain-magic-hour.jpg?width=1000&format=pjpg&exif=0&iptc=0",
        body_text = div(style="display:flex; flex-wrap:wrap; justify-content:center; gap:10px;",
                        met$cover
        ),
        layout = list(width = "auto", theme = theme_choice, color = FALSE, index = 4)
      )
      ),
      layout = list(width = "auto", theme = theme_choice, index = 4)
      ),


      
      card( 
        identity = list(
          label = "Decision Style",
          value = case_when(
            top_reason_rt == "classic" & top_reason == "classic" ~ 
              "True Traditionalist", #timless t_ , t_ traditionalist, give me some options, stop suggesting library! i asked for traditionalist
            top_reason_rt == "curiosity" & top_reason == "curiosity" ~
              "Wildcard Winner",
            top_reason_rt == "recommendation" & top_reason == "recommendation" ~
              "Peer Proven",
            top_reason_rt == "familiarity" & top_reason == "familiarity" ~
              "Habitual Hero",
            top_reason_rt == "research" & top_reason == "research" ~
              "Analytic Architect",
            top_reason_rt %in% c("hype","superficial") & top_reason %in% c("hype","superficial") ~
              "Trend Titan",
            top_reason == "classic" ~ 
              "Prestige Puppet",
            top_reason == "curiosity" ~ 
              "Risky Roller",
            top_reason == "recommendation" ~ 
              "Social Sacrifice",
            top_reason == "familiarity" ~ 
              "Comfort Captive",
            top_reason == "research" ~ 
              "Clueless Curator",
            top_reason %in% c("hype","superficial") ~ 
              "Viral Victim"
          )
        ),
        
        body_text = tagList(
          tagList("You most often read books because of ", top_reason),
          case_when(
            top_reason == top_reason_rt~
              " and that worked out great.",
            TRUE~
              "... but you really shouldn't have."
          )),
        
        insights = list(
          insight = tagList(
            tagList("You do best when "),
            case_when(
              top_reason_rt == "curiosity" ~ 
                "exploring and trying new things",
              top_reason_rt == "recommendation" ~ 
                "trusting others. Delegation is a skill.",
              top_reason_rt == "familiarity" ~ 
                "you return to what you know you like.",
              top_reason_rt == "research" ~ 
                "you investigate, compare, and commit.", 
              top_reason_rt == "hype" | top_reason_rt == "superficial"~ 
                "you read whatever strikes your eye.",
              top_reason_rt == "classic"~ 
                "you stick by with has proven itself over time."
            )
          )
        ),
        
        layout = list(
          width = "half", theme = theme_choice,
          index = 5
        )
      ),
      
      card( 
        identity = list(
          label = "Recommendation Style",
          value = case_when(
            top_rec == top_rec_amt & top_rec_amt == "friend" ~ 
              "Bookclub Boss",
            top_rec == top_rec_amt & top_rec_amt == "internet" ~ 
              "Scroll Savvy",
            top_rec == top_rec_amt & top_rec_amt == "famous person" ~ 
              "Public Pupil",
            top_rec == top_rec_amt & top_rec_amt == "family" ~ 
              "Ancestral Ally",
            top_rec == top_rec_amt & top_rec_amt == "university" ~ 
              "Academic Ace",
            is.na(top_rec) & is.na(top_rec_amt) ~
              "Solo Scout", #dont love sovereign but want to keep alliteration
            top_rec_amt == "friend" ~ 
              "People Pleaser",
            top_rec_amt == "internet" ~ 
              "Server Slave",
            top_rec_amt == "famous person" ~ 
              "Parasocial Puppet",
            top_rec_amt == "family" ~ 
              "Bloodline Bound", 
            top_rec_amt == "university" ~ 
              "Ivory Inmate",
            TRUE ~
              "Stubborn Straggler"
          )
        ),
        
        insights = list(
          intro = if(!is.na(top_rec)) {tagList("Your top recommendations came from: ", top_rec)
          } else {tagList("Your top recommendations came from: yourself. Trust no one.")},
          insight = case_when(
            top_rec == "friend" ~ 
              "Your friends got you. Keep them forever",
            top_rec == "internet" ~ 
              "The algorithm knows your soul. Slightly concerning",
            top_rec == "famous person" ~ 
              "Outsourcing taste to professionals. Efficient.",
            top_rec == "family" ~ 
              "Trusted sources. High emotional credibility.", 
            top_rec == "university" ~ 
              "You paid for that advice. Might as well use it.",
            TRUE ~ 
              "You are the moment. The authority. The oracle."
          )
        ),
        
        layout = list(
          width = "half", theme = theme_choice,
          index = 6
        )
      ),
      card(
        title = NULL,
        stats = list(
          list(label = "owned books", value = tagList(round(owned_pct,0), "%") ),
          list(label = "recs read", value = tagList(round(rec_pct,0), "%") ),
          list(label = "series read", value = tagList(round(series_pct_t,0), "%" ))
        ),
        
        layout = list(
          width = "mini", theme = theme_choice,
          index = 7
        )
      ),
    card (     
      identity = list(
        label = "Format Persona",
        value = case_when(
          top_format$format == "library book"~
            "Library Loyalist",
          top_format$format %in% c("library book","borrowed book") & owned_pct < 50 ~ 
            "Resourceful Citizen",  #does citizen or steward fit the rest of the vibe better? (i think steward would need a different first word?)
          top_format$format == "physical book" & owned_pct > 60  ~
            "Shelf Conqueror",
          top_format$format == "ebook" & owned_pct < 20 ~ 
            "Digital Purist",
          top_format$format == "ebook" ~ 
            "Kindle Specialist",

          TRUE ~
            "Hybrid Reader"
        )
      ),
      title = "Format Personality",
      
      insights = list(
        insight = case_when(
          top_format$format == "ebook" ~ 
            "Efficient. Portable. Slightly detached from reality",
          top_format$format == "library book" | top_format$format == "borrowed book" ~ 
            "Community-minded. Budget-concious. Morally superior",
          top_format$format == "physical book" ~ 
            "You like commitment. And shelves. So many shelves"
        )
      ),
      layout = list(
        width = "total",
        index = 8,
        theme = theme_choice
      )),
      card( 
            title = "Book sourcing",
            
            body_text = tagList(
              "You mainly get your books from", strong(top_origin$origin)
            ),
            
            insights = list(
              insight = case_when(
                owned_pct > 70 ~ 
                  "You actually read what you own. Revolutionary behavior.",
                owned_pct < 30 ~ 
                  "You love acquiring books. Reading them is... optional.",
                TRUE ~ 
                  "A fine mix of impulse and intention." 
              )
            ),
            layout = list(
              width = "auto",
              index = 1,
              theme = theme_choice, inactive = TRUE
            )
          ),
    card(
      body_text = div(
        style="text-align:center;",
        
        div(style="font-size:36px; font-weight:800;", low_books),
        div("books under 2★")
      ),
      
      footer = case_when(low_books>0 ~ "You could have stopped. You didn’t.", low_books >5~ "I think you have a problem", TRUE ~ "You've either got a good eye or a tendency to dnf."),
      
      layout = list(width="small", theme = theme_choice,
                    index = 9, inactive = TRUE)
    ),
        
    card(
      title = "Series Overview",
      body_text = div(
        style="display:flex; justify-content:space-between;",
        
        div("Started", strong(series_s)),
        div("Finished", strong(series_f))
      ),
      
      insights = list(
        insight = case_when(
          series_s/series_t > 0.5  ~ 
            "...always chasing the next beginning.",
          series_c/series_t > 0.5 ~ 
            "...you live inside ongoing stories.",
          series_f >= series_s ~ 
            "...you actually finish what you start.",
          series_f/series_t > 0.3 ~ 
            "...you actually finish what you start.",
          TRUE ~
            "...beginnings, middles, and endings all get love."
        )
      ),
      layout = list(width = "small", index = 9, theme = theme_choice)
      
    ),
        card( 
          intro = tagList("You read", series_t, "books that were part of a series"),
          
          body_text = case_when(
            series_pct_t < 15 ~ 
              "You mostly stay with standalones. You like your stories self-contained.",
            series_pct_t < 40 ~ 
              "A balanced reader — you dip into series, but don’t get stuck.",
            series_pct_t < 70 ~ 
              "Series are your comfort zone. You like staying in a world.",
            TRUE ~ 
              "You don’t read books. You read sagas."
          ), 
          
          
          
          identity = list(
            label = "Reader Identity",
            value = case_when(
              series_f/series_amt >= 0.5 ~ 
                "Closure Champion",
              series_f>=series_s ~ 
                "Closure Champion",
              series_s/series_t > 0.5 ~ 
                "Serial Starter",
              series_pct_t > 50 ~
                "World Dweller",
              series_pct_t < 15 ~
                "Commitment Phobe", 
              TRUE ~
                "Mixed Explorer"
            )
          ),
          
          layout = list(
            width = "auto", theme = theme_choice,
            identity_pre = FALSE,
            index = 1
          )
        ),
        
        
        card( 
          title = "Your Podium",
          
          list_items = list(
              items = top3,
              renderer = function(b,i){
                heights <- c("190px","160px","140px")  # 2nd is tallest
                order <- c(2,1,3) # reorder visually
                
                idx <- order[i]
                b <- b[idx,]
                
                
                div(
                  style=paste0("
      flex:1;
      display:flex;
      flex-direction:column;
      align-items:center;
      height:", heights[idx], ";
      background: rgba(255,255,255,0.08);
      border-radius:12px;
      padding:10px;
    "),
                  #if(!is.null(b$cover)) cov_f(b$cover, size = 0.3, rank = paste0("#", idx), bg_g = glass_bg),
                  div(style="font-size:12px; opacity:0.7;", paste0("#", idx)),
                  div(style="font-weight:600; text-align:center;", b$name),
                  div(style="font-size:11px; opacity:0.7;", b$author),
                  div(style="margin-top:5px;", paste0("★ ", b$rating))
                )
              }
              
          ),
          
          layout = list(
            width = "auto", theme = theme_choice,
            index = 2
          )
        ),
    card( 
      title = "Your Roman Empire",
      
      intro = "You kept coming back to:",
      
      list_items = list(
        items = data.frame(name = themes_top$theme[1:3]),
        renderer = function(b,i) {
          b <- b[i,]
          badge(b)
        }
      ),
      
      footer = "Different books. Same conversation.",
      
      layout = list(
        width = "small", theme = theme_choice,
        index = 3
      )
    ),
    
    card(
      background_img = if(dark_theme) "https://images.squarespace-cdn.com/content/v1/6096fe964208c322b59e9941/238476f4-dc98-4cc9-946b-e32132e92094/Hibiscus+small+repeat.jpg" else "https://patternobserver.com/wp-content/uploads/2024/11/Pattern-Observer_BANNER_03.jpg",
      
      body_text = div(style = "display:flex; flex-direction: column; gap:20px;",
                      card(
                        title = "Hall of Fame",
                        body_text = div(style="display:flex; flex-wrap:wrap; justify-content:center; gap:10px;",
                                        hall_of_fame$cover
                        ),
                        layout = list(width = "auto", theme = theme_choice, index = 4)
                      ),
                      div(style = "display:flex; gap:20px;",
                          card(
                            title = "Personal 5 Stars",
                            body_text = div(style="display:flex; flex-wrap:wrap; justify-content:center; gap:5px;",
                                            personal_5$cover
                            ),
                            layout = list(width = "auto", theme = theme_choice, index = 5)
                          ),
                          card(
                            title = "Technical 5 Stars",
                            body_text = div(style="display:flex; flex-wrap:wrap; justify-content:center; gap:5px;",
                                            technical_5$cover
                            ),
                            layout = list(width = "auto", theme = theme_choice, index = 6)
                          ))
                      
      ),
      layout = list(width = "auto", theme = theme_choice,color=TRUE, index = 5)
    )
    
    
    )
  })
  # Proxy for Quadrant 1
  output$view_rating <- renderPlotly({
    req(input$pick_rating)
    # We pull the logic directly from the output we already built in the loop
    # This avoids re-calculating the plot from scratch
    evals[[input$pick_rating]]$plot(df_books(), input)
  })
  
  output$view_map <- renderPlotly({
    req(input$pick_map)
    # We pull the logic directly from the output we already built in the loop
    # This avoids re-calculating the plot from scratch
    evals[[input$pick_map]]$plot(df_books(), input)
  })
  
  # Proxy for Quadrant 2
  output$view_monthly <- renderPlotly({
    req(input$pick_monthly)
    evals[[input$pick_monthly]]$plot(df_books(), input)
  })
  
  # Proxy for Quadrant 3
  output$view_owned <- renderPlotly({
    req(input$pick_owned)
    evals[[input$pick_owned]]$plot(df_owned(), input)
  })
  
  # Proxy for Quadrant 4
  output$view_structure <- renderPlotly({
    req(input$pick_structure)
    evals[[input$pick_structure]]$plot(df_books(), input)
  })
  
  output$highly_anticipated <- renderUI({
    booklist <- books %>% filter(state != "read") %>% filter(expectation >= 4.5) %>% arrange(expectation, genre) 
    card(
      title = "Highly Anticipated",
      background_img = "https://images.wallpapersden.com/image/download/small-town-hd-aesthetic-cool-art_bWdqa22UmZqaraWkpJRmbmdlrWZmZWU.jpg",
      body_text = div(
        if(nrow(booklist %>% filter(expectation == 5))!=0){div(style="display:flex; flex-wrap:wrap; justify-content:center;padding:7px;",
                      booklist %>% filter(expectation == 5) %>% merge_series_covers(s = 1) %>% pull(cover))},
        if(nrow(booklist %>% filter(expectation == 4.7))!=0){div(style="display:flex; flex-wrap:wrap; justify-content:center;padding:7px;",
                                                                   booklist %>% filter(expectation == 4.7) %>% merge_series_covers(s = 0.9) %>% pull(cover))},
        if(nrow(booklist %>% filter(expectation < 4.7))!=0){div(style="display:flex; flex-wrap:wrap; justify-content:center;padding:7px;",
                                                                   booklist %>% filter(expectation < 4.7) %>% merge_series_covers(s = 0.8) %>% pull(cover))}
      ),
      layout = list(width = "auto", theme = "none", color = FALSE)
    )
  })
}
shinyApp(ui, server)

