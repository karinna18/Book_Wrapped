suppressPackageStartupMessages({
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
})
'

library(plotly)      # For plotly_build and plotlyOutput
library(grDevices)   # For col2rgb, rgb, and colorRampPalette
library(scales)      # Useful for color mapping
library(ggplot2)     # For the base plot_simple_bar logic
library(rlang)
'
apply_sleektheme <- function(p, corner_radius = 12, gap = 0.5, line_opacity = 0.6) {
  p <- plotly::plotly_build(p)
  
  # 1. Identify if it's a Pie chart
  is_pie <- any(sapply(p$x$data, function(x) x$type == "pie"))
  line_rgba <- sprintf("rgba(255, 255, 255, %s)", line_opacity)
  
  if (is_pie) {
    # --- PIE: Force White Dividers ---
    for (i in seq_along(p$x$data)) {
      if (p$x$data[[i]]$type == "pie") {
        # We must replace the whole marker list to ensure the line is recognized
        p$x$data[[i]]$marker$line <- list(color = line_rgba, width = 1)
      }
    }
  } else {
    # --- BAR: Skinny, Rounded, and White Dividers ---
    p$x$layout$bargap <- gap

    # Y-Axis: Muted grid lines
    p$x$layout$yaxis$showgrid <- TRUE
    p$x$layout$yaxis$gridcolor <-  # Subtle white grid
    # Force clean axes
    p$x$layout$xaxis$showgrid <- FALSE
    p$x$layout$yaxis$gridcolor <- "rgba(120, 120, 120, 0.2)"
    p$x$layout$yaxis$zeroline <- FALSE
    
    universal_font <- list(family = "inherit", color = "currentColor", size = 12)
    
    # 2. Force the global font
    p$x$layout$font <- universal_font
    
    # 3. Universal Axis Fix (Works for X, Y, and any others like Y2)
    # We loop through all layout items to find anything named 'axis'
    axis_names <- names(p$x$layout)[grep("axis", names(p$x$layout))]
    
    for (ax in axis_names) {
      # Force the title to be a list so we can set the color (prevents 'atomic' error)
      if (is.character(p$x$layout[[ax]]$title)) {
        p$x$layout[[ax]]$title <- list(text = p$x$layout[[ax]]$title)
      }
      
      # Apply theme colors
    p$x$layout[[ax]]$title$font <- universal_font
    p$x$layout[[ax]]$tickfont <- universal_font
    }
    p$x$layout$plot_bgcolor <- "rgba(0,0,0,0)"
    p$x$layout$paper_bgcolor <- "rgba(0,0,0,0)"
    
    
    # Loop through traces to force white borders
    #for (i in seq_along(p$x$data)) {
    #  if (p$x$data[[i]]$type == "bar") {
    #    # Ensure we don't wipe out existing colors while adding the line
    #    if (is.null(p$x$data[[i]]$marker)) p$x$data[[i]]$marker <- list()
    #    p$x$data[[i]]$marker$line <- list(color = line_rgba, width = 1)
    #  }
    #}
    
  }
  
  return(p)
}

set_theme(theme_minimal())
theme_update(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  text = element_text(color = "gray50",size = 16),
  axis.text = element_text(color = "gray50"),
  plot.title = element_text(hjust = 0.5, color = "gray40", size = 18)
)

get_colormap <- function(colormap, summary = FALSE){
  if (!summary){return(setNames(colors %>% filter(category == colormap) %>% distinct(name, .keep_all = TRUE) %>% .$hex, colors %>% filter(category == colormap) %>% distinct(name, .keep_all = TRUE) %>% .$name))}
  vals <- setNames(colors %>% filter(category == colormap) %>% .$hex, colors %>% filter(category == colormap) %>% .$summary_name)
  clean_vals <- vals[!duplicated(names(vals))]
  return(clean_vals)
}
    
plot_simple_bar <- function(df,aesthetics,colormap){
  vals <- get_colormap(colormap)
  df %>% ggplot(aesthetics) + geom_col() +
    scale_fill_manual(values = vals, drop = FALSE) + 
    guides(fill = "none")
}


badge <- function(text, glass_bg = "rgba(255,255,255,0.15)"){
  
  div(
    style=paste0("
        padding:6px 10px;
        border-radius:999px;
        background: ",glass_bg,";
        font-size:12px;
      "),
    text
  )
}


top_bayes <- function(df, group_col, rating_col = "rating", m = 2) {
  C <- mean(df[[rating_col]], na.rm = TRUE)  # global average
  df %>%
    group_by(across(all_of(group_col))) %>%
    summarize(
      R = mean(.data[[rating_col]], na.rm = TRUE),
      v = n(),
      .groups = "drop"
    ) %>%
    mutate(bayes_avg = (v * R + m * C) / (v + m)) %>%
    arrange(desc(bayes_avg)) %>%
    slice(1)
}


# --- Table 1: Character Archetypes ---
archetypes <- tribble(
  ~genre,             ~creepy,                  ~funny,                   ~easy,                      ~soft,                                     ~epic,                                          ~meaningful,                ~strong,                        ~difficult,                      ~informative,
  "ya",               "the social outcast",     "manic pixie dream girl", "the isekai heroine",       "a loyal best friend",                     "the chosen one",                               "a dead mom",               "the symbol of a revolution",   "an unreliable narrator",        "an overachieving valedictorian",
  "romance",          "a tortured hot vampire", "a gay best friend",      "the girl next door",       "the sunshine to someone else's grumpy",   "the cold crown prince with a soft interior",   "a long lost love",         "a free-spirited spinster",     "the morally gray love interest", "an ambitious modiste",
  "other literature", "the vengeful phantom",   "the court jester",       "the lovestruck dreamer",   "the hopeless romantic",                   "a playwright / candlelight hermit",            "the tragic ghost",         "the tragic martyr",            "a nihilistic philosopher",      "a shakespearean scholar",
  "other languages",  "the local banshee",      "the comedic relief",     "an enthusiastic tourist",  "the village storyteller",                 "a weathered traveler",                         "the exiled poet",          "the national hero",            "a translator",                  "the wandering scribe",
  "literature",       "a victorian ghost",      "a bored aristocrat",     "the relatable protagonist", "a kind stranger",                         "a studied scribe",                             "a loving friend",          "a soldier",                    "a scruffy professor",           "a librarian",
  "suspense",         "deranged detective",     "an amnesiac covered in blood", "the nosy neighbor",  "an unsuspecting witness",                 "an interdimensional spy",                      "a whistleblower",          "a corrupt cop",                "the final girl",                 "a forensic specialist",
  "non-fiction",      "mad scientist",          "an eccentric hobbyist",  "a curious amateur",        "a nature documentarian",                  "an intrepid explorer",                         "a voice for the voiceless", "an investigative journalist",  "a visionary before his time",   "a dedicated academic",
  "fantasy",          "a necromancer",          "a trickster gnome",      "an elf ranger",            "a magical pet",                           "a heroic adventurer",                          "a wise old mentor",        "a corrupt royal advisor",      "a fallen deity",                "the master alchemist",
  "kids",             "a creepy doll",          "talking animal sidekick", "the new kid in town",      "a gentle giant",                          "a dragonrider",                                "a small hero, big heart",  "a young revolutionary",        "a burnt out gifted kid",        "the curious apprentice",
  "sci-fi",           "a hive-mind entity",     "an intergalactic repairman", "a space trucker",          "a lonely terraformer",                    "a space-traveling knight",                     "the solitary astronaut",   "the leader of the rebellion",  "a 4th dimensional being",       "a spaceship engineer"
)

# --- Table 2: Niche Story Types ---
niches <- tribble(
  ~genre,             ~creepy,                        ~funny,                     ~easy,                                     ~soft,                              ~epic,                                     ~meaningful,                         ~strong,                         ~difficult,                           ~informative,
  "ya",               "small town gothic horror",     "a high-school rom-com",    "a summer romance",                        "a coming of age roadtrip",         "a magical boarding school saga",          "a found family tale",               "a dystopian fantasy trilogy",   "a dark academia book",               "a how-to guide",
  "romance",          "a vampire love story",         "a fake dating story",      "a summer romance",                        "a coffee shop AU",                 "a quest to rescue the princess",          "a tale of generational love",       "a tragic love story",           "a forbidden love story",             "historical romance",
  "other literature", "a morbid victorian verse",     "a shakespeare comedy",     "modern instagram poetry",                 "a book of haikus",                 "a greek tragedy",                         "an existentialist stage drama",     "a one woman show",              "post-modern absurdism",              "the bible",
  "other languages",  "ancient folklore horror",      "a slice-of-life comedy",   "domestic slice-of-life",                  "a pastoral slice-of-life",         "a traditional folklore tale",             "a post-colonial memoir",            "a sociopolitical manifesto",    "an existentialist russian doorstopper", "a dictionary",
  "literature",       "sensationalist gothic horrors", "a womans journey to find herself",   "domestic slice-of-life",                  "a found family tale",              "a multi generational family saga",        "a found family tale",               "a feminist classic",            "an existentialist russian doorstopper", "a biographical fiction",
  "suspense",         "a psychological thriller",     "a slapstick crime caper",  "a whodunit",                              "a low stakes culinary mystery",    "a global espionage thriller",             "a grief-driven crime drama",        "a legal corruption thriller",    "a nonlinear mystery",                "true crime stories",
  "non-fiction",      "a dark medical history",       "a satirical essay",        "a for dummies book",                      "a hopeful memoir",                 "a global history",                        "a book of essays",                  "an anti-capitalist manifesto",  "a comprehensive world history",      "a book on quantum physics",
  "fantasy",          "a grimdark tale",              "a d&d adventure",          "a heros journey",                         "a gentle fairy tale",              "a high fantasy quest to save the kingdom", "mythic retellings",                 "political court intrigue",      "the silmarillion",                   "a magical bestiary",
  "kids",             "a distorted fairy-tale",       "a d&d adventure",          "a hidden portal adventure to magic world", "a bedtime story",                  "dragon-bonding chronicles",               "a moralistic fable",                "a moralistic fable",            "a choose your own adventure book",   "a for dummies book",
  "sci-fi",           "a body horror tale",           "intergalactic road trip",           "a first contact story",                   "an interplanetary friendship story", "an epic space opera",                     "a tale of interplanetary cooperation", "a cyberpunk class warfare novel", "a book on quantum physics",           "the schematics for a rocket"
)

archetypes_long <- tribble(
  ~genre,             ~mood,          ~tag,
  "ya",               "creepy",       "the final girl", #the final girl (want to keep both somewhere)
  "ya",               "funny",        "the disaster bisexual", #manic pixie dream girl
  "ya",               "easy",         "the manic pixie dream girl", #the isekai heroine
  "ya",               "soft",         "the himbo",
  "ya",               "epic",         "the chosen one",
  "ya",               "meaningful",   "a dead mom", #yeah this isnt great
  "ya",               "strong",       "the face of the resistance",
  "ya",               "difficult",    "the unreliable narrator",
  "ya",               "informative",  "the overachieving valedictorian",
  
  "romance",          "creepy",       "the hot tortured vampire",
  "romance",          "funny",        "the gay best friend",
  "romance",          "easy",         "the girl next door",
  "romance",          "soft",         "the golden retriever", 
  "romance",          "epic",         "the 'touch her and you die' prince",
  "romance",          "meaningful",   "the soulmate",
  "romance",          "strong",       "the free-spirited spinster", 
  "romance",          "difficult",    "the morally gray love interest",
  "romance",          "informative",  "the strong independent woman",
  
  "other literature", "creepy",       "the vengeful phantom",
  "other literature", "funny",        "the court jester",
  "other literature", "easy",         "the cozy cottagecore dreamer",
  "other literature", "soft",         "the hopeless romantic",
  "other literature", "epic",         "the starving artist",
  "other literature", "meaningful",   "the victorian ghost",
  "other literature", "strong",       "the revolutionary poet", #the tragic martyr
  "other literature", "difficult",    "the nihilistic philosopher",
  "other literature", "informative",  "the shakespearean scholar",
  
  "other languages",  "creepy",       "the local banshee",
  "other languages",  "funny",        "the comedic relief",
  "other languages",  "easy",         "the enthusiastic tourist",
  "other languages",  "soft",         "the village storyteller",
  "other languages",  "epic",         "the weathered traveler",
  "other languages",  "meaningful",   "the exiled poet",
  "other languages",  "strong",       "the national hero",
  "other languages",  "difficult",    "the scruffy professor",
  "other languages",  "informative",  "the wandering scribe",
  
  "literature",       "creepy",       "the gaslit wife",
  "literature",       "funny",        "the witty socialite",
  "literature",       "easy",         "the relatable protagonist",
  "literature",       "soft",         "the kind stranger",
  "literature",       "epic",         "the studied scribe",
  "literature",       "meaningful",   "the loving friend",
  "literature",       "strong",       "the nameless soldier",
  "literature",       "difficult",    "the scruffy professor",
  "literature",       "informative",  "the helpful librarian",
  
  "suspense",         "creepy",       "the deranged detective",
  "suspense",         "funny",        "an amnesiac covered in blood",
  "suspense",         "easy",         "the nosy neighbor",
  "suspense",         "soft",         "the unsuspecting witness",
  "suspense",         "epic",         "the deep-cover operative",
  "suspense",         "meaningful",   "the grieving investigator",
  "suspense",         "strong",       "a corrupt cop",
  "suspense",         "difficult",    "the manipulative mastermind",
  "suspense",         "informative",  "the forensic specialist",
  
  "non-fiction",      "creepy",       "a mad scientist",
  "non-fiction",      "funny",        "an eccentric hobbyist",
  "non-fiction",      "easy",         "a curious amateur",
  "non-fiction",      "soft",         "a nature documentarian",
  "non-fiction",      "epic",         "an intrepid explorer",
  "non-fiction",      "meaningful",   "the whistleblower",
  "non-fiction",      "strong",       "an investigative journalist",
  "non-fiction",      "difficult",    "a visionary before their time",
  "non-fiction",      "informative",  "a dedicated academic",
  
  "fantasy",          "creepy",       "the necromancer",
  "fantasy",          "funny",        "the sarcastic familiar",
  "fantasy",          "easy",         "the elf ranger",
  "fantasy",          "soft",         "the cozy tavern-keeper",
  "fantasy",          "epic",         "the heroic adventurer",
  "fantasy",          "meaningful",   "the wise old mentor",
  "fantasy",          "strong",       "the corrupt royal advisor",
  "fantasy",          "difficult",    "the anti-hero with a god complex",
  "fantasy",          "informative",  "the master alchemist",
  
  "kids",             "creepy",       "the creepy doll",
  "kids",             "funny",        "the talking animal sidekick",
  "kids",             "easy",         "the new kid in town",
  "kids",             "soft",         "the misunderstood beast",
  "kids",             "epic",         "a dragonrider",
  "kids",             "meaningful",   "a small hero with a big heart",
  "kids",             "strong",       "the symbol of the revolution",
  "kids",             "difficult",    "the burnt out gifted kid",
  "kids",             "informative",  "the curious apprentice",
  
  "sci-fi",           "creepy",       "a hive-mind entity",
  "sci-fi",           "funny",        "an intergalactic repairman",
  "sci-fi",           "easy",         "a space trucker",
  "sci-fi",           "soft",         "a lonely terraformer",
  "sci-fi",           "epic",         "a space-traveling knight",
  "sci-fi",           "meaningful",   "the solitary astronaut",
  "sci-fi",           "strong",       "the leader of the rebellion",
  "sci-fi",           "difficult",    "a 4th dimensional being",
  "sci-fi",           "informative",  "the spaceship engineer"
)

niches_long <- tribble(
  ~genre,             ~mood,          ~tag,
  "ya",               "creepy",       "small town gothic",
  "ya",               "funny",        "a high-school rom-com", #dislike
  "ya",               "easy",         "academic rivals-to-lovers", #10/10 not sure about placement so still want suggestions
  "ya",               "soft",         "a coffee shop AU", #10/10
  "ya",               "epic",         "a magical boarding school saga",
  "ya",               "meaningful",   "a found family tale",
  "ya",               "strong",       "a dystopian revolution arc", #rephrase
  "ya",               "difficult",    "dark academia",
  "ya",               "informative",  "a how-to guide",
  
  "romance",          "creepy",       "a vampire love story",
  "romance",          "funny",        "fake dating antics",
  "romance",          "easy",         "cozy small town romance",
  "romance",          "soft",         "slow burn mutual pining", #10/10
  "romance",          "epic",         "a quest to rescue the princess", #dont like high fantasy romantasy, too basic, but redo
  "romance",          "meaningful",   "a tale of generational love",
  "romance",          "strong",       "a tragic love story",
  "romance",          "difficult",    "forbidden love angst",
  "romance",          "informative",  "historical romance",
  
  "other literature", "creepy",       "a morbid victorian verse",
  "other literature", "funny",        "a shakespeare comedy",
  "other literature", "easy",         "modern instagram poetry",
  "other literature", "soft",         "a book of haikus",
  "other literature", "epic",         "a greek tragedy",
  "other literature", "meaningful",   "an existentialist stage drama",
  "other literature", "strong",       "a one woman show",
  "other literature", "difficult",    "post-modern absurdism",
  "other literature", "informative",  "the bible",
  
  "other languages",  "creepy",       "ancient folklore horror",
  "other languages",  "funny",        "a slice-of-life comedy",
  "other languages",  "easy",         "domestic slice-of-life",
  "other languages",  "soft",         "a pastoral slice-of-life",
  "other languages",  "epic",         "a traditional folklore tale",
  "other languages",  "meaningful",   "a post-colonial memoir",
  "other languages",  "strong",       "a sociopolitical manifesto",
  "other languages",  "difficult",    "an existentialist russian doorstopper",
  "other languages",  "informative",  "a dictionary",
  
  "literature",       "creepy",       "sensationalist gothic horrors",
  "literature",       "funny",        "a womans journey to find herself",
  "literature",       "easy",         "an airport novel",
  "literature",       "soft",         "a found family tale",
  "literature",       "epic",         "a multi generational family saga",
  "literature",       "meaningful",   "a found family tale",
  "literature",       "strong",       "a feminist classic",
  "literature",       "difficult",    "an existentialist russian doorstopper",
  "literature",       "informative",  "a biographical fiction",
  
  "suspense",         "creepy",       "a psychological thriller",
  "suspense",         "funny",        "a slapstick crime caper",
  "suspense",         "easy",         "beach read thriller",
  "suspense",         "soft",         "a low stakes culinary mystery",
  "suspense",         "epic",         "international espionage",
  "suspense",         "meaningful",   "a grief-driven crime drama",
  "suspense",         "strong",       "a legal corruption thriller",
  "suspense",         "difficult",    "a nonlinear mystery",
  "suspense",         "informative",  "true crime stories",
  
  "non-fiction",      "creepy",       "a dark medical history",
  "non-fiction",      "funny",        "a satirical essay",
  "non-fiction",      "easy",         "a for dummies book",
  "non-fiction",      "soft",         "a hopeful memoir",
  "non-fiction",      "epic",         "a global history",
  "non-fiction",      "meaningful",   "an anti-capitalist manifesto",
  "non-fiction",      "strong",       "dense socio-political theory",
  "non-fiction",      "difficult",    "a comprehensive world history",
  "non-fiction",      "informative",  "a niche deep-dive",
  
  "fantasy",          "creepy",       "grimdark fantasy",
  "fantasy",          "funny",        "a chaotic d&d adventure",
  "fantasy",          "easy",         "a heros journey",
  "fantasy",          "soft",         "a whimsical fairy tale",
  "fantasy",          "epic",         "high fantasy political intrigue",
  "fantasy",          "meaningful",   "mythic retelling",
  "fantasy",          "strong",       "assassin-training arc",
  "fantasy",          "difficult",    "the silmarillion",
  "fantasy",          "informative",  "a magical bestiary",
  
  "kids",             "creepy",       "a distorted fairy-tale",
  "kids",             "funny",        "a d&d adventure",
  "kids",             "easy",         "magic portal adventure",
  "kids",             "soft",         "gentle bedtime story",
  "kids",             "epic",         "dragon-bonding chronicles",
  "kids",             "meaningful",   "a moralistic fable",
  "kids",             "strong",       "a moralistic fable",
  "kids",             "difficult",    "a choose your own adventure book",
  "kids",             "informative",  "a for dummies book",
  
  "sci-fi",           "creepy",       "body horror",
  "sci-fi",           "funny",        "intergalactic road trip",
  "sci-fi",           "easy",         "a first contact story",
  "sci-fi",           "soft",         "an interplanetary friendship story",
  "sci-fi",           "epic",         "an epic space opera",
  "sci-fi",           "meaningful",   "a tale of interplanetary cooperation",
  "sci-fi",           "strong",       "cyberpunk class warfare",
  "sci-fi",           "difficult",    "hard sci-fi physics lecture", #redo better
  "sci-fi",           "informative",  "speculative tech blueprints"
)

get_book_identity <- function(user_genre, user_mood, type = "archetype") {
  
  # Choose the correct table
  target_data <- if(type == "archetype") archetypes_long else if (type == "theme") themes_long else niches_long
  
  # The case_when logic
  result <- target_data %>%
    filter(genre == user_genre & mood == user_mood) %>%
    pull(tag)
  
  return(result)
}


reading_matrix <- tribble(
  ~time_period,  ~`>550`,                ~`>400`,                ~`>250`,                  ~under,
  ">2015",       "Modern Titan",         "Primary Consumer",     "Trend Chaser",           "Micro-Dose Reader",
  ">2000",       "Modern Titan",         "Blog Specialist",     "Pocket Curator",        "Micro-Dose Reader",
  ">1960",       "Vintage Hoarder",      "Vinyl Scholar",        "Thrift-Store Scavenger", "Magazine Junkie",
  ">1900",       "Industrial Masochist", "Drawing-Room Scholar", "Literary Tourist",       "Telegram Addict",
  "under",       "Ancient Historian",    "Museum Chronicler",    "Homeric Tourist",        "Oracle Visiter"
)
time_breaks <- c(2015, 2000, 1960, 1900, -Inf)
time_labels <- c(">2015", ">2000", ">1960", ">1900", "under")

page_breaks <- c(550, 400, 250, -Inf)
page_labels <- c(">550", ">400", ">250", "under")

# --- Convert to Long Format for Searching ---
reading_long <- reading_matrix %>%
  pivot_longer(
    cols = -time_period, 
    names_to = "page_length", 
    values_to = "identity_tag"
  )
get_reading_tag <- function(user_year, user_pages) {
  
  t_grp <- cut(user_year, 
               breaks = c(-Inf, 1900, 1960, 2000, 2015, Inf), 
               labels = c("under", ">1900", ">1960", ">2000", ">2015"),
               right = FALSE) %>% as.character()
  
  # 2. Determine Page Bucket
  p_grp <- cut(user_pages, 
               breaks = c(-Inf, 250, 400, 550, Inf), 
               labels = c("under", ">250", ">400", ">550"),
               right = FALSE) %>% as.character()
  
  # 3. Lookup the tag
  tag <- reading_long %>%
    filter(time_period == t_grp, page_length == p_grp) %>%
    pull(identity_tag)
  
  # Logic handling if no match is found
  if (length(tag) == 0) return("Undefines Mystery")
  
  return(tag)
}

color_themes <- list( #general you dont need to give me palettes where you didnt change anything
  none = c("#eeeeee", "#eeeeee"),
  
  `ocean` = c("#B3ECF9", "#7FD6F5",  "#1DA7D9","#55C1E5" ,"#ccefff"),

  `rosewood glade` = c("#A8E6CF", "#DBE1DD", "#F2B5D4","#CCD1E1", "#C7F2F2", "#A8E6CF"),

  `coral reef` = c("#FF6F61", "#FFB6B9", "#FFDAB9", "#FFB07C", "#FF6F61"),

  `lesbian` = c("#EA642B", "#FF9A56", "#FFFFFF", "#EB9AC9", "#CD569C"), 
  
  `pastel` = c("#FAD9C1", "#E6B8B8", "#C1D3FE", "#D6EBC1", "#FAD9C1"),
  
  `phantom mist` = c("#D3D3D3", "#A9CCE3", "#C5B4E3", "#A9CCE3", "#D3D3D3"),
  `forest` = c("#70A494", "#8BCFB0", "#D3E5B8", "#8BCFB0", "#70A494"),
  `aurora glass` = c("#A8E6CF", "#DCE3F0", "#C1A3E8", "#DCE3F0", "#A8E6CF"),
  
  
  
  `dusty sunset` = c("#D98C53", "#C183B5", "#FFD1B3", "#C183B5", "#D98C53"),
  

  `1 ya` = c("#FFDAB9","#FFFDD0" ,"#EB9FEF","#FF6F61","#FF7F50","#FFDAB9"), 
  `2 ya` = c("#FFB3FF", "#B085E2", "#66CCFF", "#B085E2", "#FFB3FF"),
  `1 romance` = c("#EC4A78", "#F7879A",  "#F7E7CE","#E6B8B8" ,"#D8BCAB","#EC4A78" ),
  `2 romance` = c("#800020", "#BA6974",  "#F3D1C8","#FFFFF0" ,"#D46E7A", "#800020"), 
  `1 other lit` = c("#F7F0CD", "#DBB857",  "#C27C3F","#4434A2" ,"#F0E1C9", "#F7F0CD"), 
  `2 other lit` = c("#435853","#679193","#E9E3CB","#CB7251" ,"#83A96F", "#435853"), 
  `1 other lang` = c("#3DD9C9", "#E4952B","#AB1978","#8A4381","#3DD9C9"), #check graph col
  `2 other lang` = c("#F5A224",  "#A784A4", "#54058D","#A33955", "#E8B908"), 
  `1 fantasy` = c("#8374C1", "#B4E3B1",  "#EFDFC6","#DB9797" ,"#5DA8DE","#7A6AB9" ),
  `2 fantasy` = c("#873F8D", "#675BAB","#1AAEB8" ,"#86B97D","#E0C95B","#873F8D" ), 
  `1 kids` = c("#6FD1E2", "#A7E4AD", "#F7F5C9", "#FFCE4B", "#EBB5CC", "#6FD1E2"),
  `2 kids` = c("#FF84D0", "#FFA482", "#FFD93D", "#ADCD94", "#5BC0EB", "#AD97D9", "#FF84D0"), 
  `1 nonfic` = c("#FAD9C1", "#E6B8B8", "#9B766B", "#93A0D6", "#FAD9C1"), 
  `2 nonfic` = c("#6C2F05","#7B5544", "#C6A46A", "#4C5932", "#EBC593", "#6C2F05"),
  `1 suspense` = c("#D3D3D3", "#A9CCE3", "#71797e", "#A9CCE3", "#D3D3D3"),
  `2 suspense` = c( "#262654","#6D757A","#B8BDC0","#597994","#262654"),
  `1 lit` = c("#CCD1E1", "#D39670", "#A8DCE6", "#DBE1DD", "#F0E4C8","#CCD1E1"), 
  `2 lit` = c("#F0E4C8", "#EBC593","#C8D5B4","#6FA38A","#F0E4C8"),
  `1 scifi` = c("#C9664E", "#D89D63", "#F5E6CF","#9299C4", "#C9664E"), 
  `2 scifi` = c("#081367", "#049CA0", "#06E9D0", "#CD068A","#081367")

  

)

extreme_tile <- function(label, title, subtitle){
  div(
    style="
      padding:12px;
      border-radius:12px;
      background: rgba(255,255,255,0.08);
      display:flex;
      flex-direction:column;
      gap:4px;
    ",
    
    div(style="font-size:14px; opacity:0.9;", label),
    div(style="font-weight:600;", title),
    div(style="font-size:12px; opacity:0.8;", subtitle)
  )
}
progress_bar <- function(label, value, text_color = "#ffffff"){
  
  pct <- round(value * 100, 0)
  
  div(
    style="display:flex; flex-direction:column; gap:4px;",
    
    div(
      style="display:flex; justify-content:space-between; font-size:12px; opacity:0.8;",
      span(label),
      if( label == "Ethnic diversity") span(paste0(pct/10)) else span(paste0(pct, "%"))
    ),
    
    div(
      style=paste0("
        width:100%;
        height:8px;
        background: rgba(255,255,255,0.15);
        border-radius:10px;
        overflow:hidden;
      "),
      
      div(
        style=paste0("
          width:", pct, "%;
          height:100%;
          border-radius:10px;
          background: currentColor;
          transition: width 0.6s ease;
        ")
      )
    )
  )
}

card <- function(
    title = NULL,
    stats = NULL,
    intro = NULL,
    insights = NULL,
    body_text = NULL,
    plot = NULL,
    background_img = NULL,
    identity = NULL,
    list_items = NULL,
    comparisons = NULL,
    footer = NULL,
    themes = color_themes,
    layout = list(width = "auto", identity_pre = TRUE, theme = "forest (l)", identity_style = "badge", index = 1, color = TRUE)
){
  
  `%||%` <- function(a,b) if(!is.null(a)) a else b
  
  width <- layout$width %||% "auto"
  identity_pre <- layout$identity_pre %||% TRUE
  theme <- layout$theme %||% "forest (l)"
  identity_style <- layout$identity_style %||% "badge"
  index <- layout$index %||% 1
  color <- layout$color %||% TRUE
  w <- width
  # width styles
  width_style <- switch(width,
                        "mini" = "flex:1 1 100%;",
                        "half" = "flex: 1 1 calc(50% - 10px);",
                        "auto" = "flex: 1 1 auto;",
                        "total" = "flex:1 1 100%;",
                        "medium" = "max-width: 400px; align-items: center;",
                        "small" = "max-width: 300px; 
                                    border-radius: 8px;
                                    padding:15px;
                                    box-shadow: 3px 3px 0px currentColor;
                                    transform: rotate(-1deg);  font-weight:600;",
                        "plot" = "border-radius: 8px;
                                    padding:15px; max-width: 320px;"
                        
  )


  
  gradient_colors <- themes[[theme]]
  palette_fn <- colorRampPalette(gradient_colors)
  steps <- palette_fn(10)
  
  col1 <- steps[index]
  col2 <- steps[index+1]
  
  
  bg <- paste0(
    "linear-gradient(135deg, ",
    col1, ", ",
    col2, ")"
  )
  
  rgb_vals <- col2rgb(col1)/255
  lum <- (299*rgb_vals[1]*255 + 587*rgb_vals[2]*255 + 114*rgb_vals[3]*255) / 1000
  
  text_color_from_gradient <- function(hue_shift = 0.15){
  
  base_color <- if(lum > 128) c(0,0,0) else c(1,1,1)
  
  
  # Slightly shift text color toward theme hue for cohesion
  rgb_vals <- if(lum > 128) {  # bright background → dark text
    base_color * (1 - (hue_shift+0.2)) + rgb_vals * (hue_shift+0.2)
  } else {                        # dark background → light text
    base_color * (1 - (hue_shift)) + rgb_vals * (hue_shift)
  }
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
  }
  
  glass_bg <- if(lum < 128) "rgba(0, 0, 0, 0.15)" else "rgba(255, 255, 255, 0.2)"
  
  text_color <- text_color_from_gradient()
  
  img_layer <- NULL
  if (!is.null(background_img) && background_img != "") {
    # Logic for the image styling
    img_style <- paste0(
      "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 0; ",
      "background-image: url('", background_img, "') ; ",
      if(theme!="none") "mix-blend-mode: luminosity;",
      #if(theme!="none") paste0("mix-blend-mode:",if(lum < 128) "overlay" else "multiply","; ") else "",
      "background-size: cover; ",
      "background-position: center;"
    )
    # Create the actual layer
    img_layer <- div(style = img_style)
  }
  
  bg_style <- if (isFALSE(color)) {
    paste0("background:transparent;
   color:",text_color,";
   border-radius:20px;
   padding:20px;")
  } else { paste(
    "background:", bg, ";
   color:",text_color,";
   border-radius:20px;
   padding:20px;"
  )}
  
  
  # identity styles
  identity_block <- function(identity){
    if(is.null(identity)) return(NULL)
    
    label <- identity$label
    value <- identity$value
    
    if(identity_style == "badge"){
      div(
        style=paste0("
      margin-top:10px;
      padding:15px;
      border-radius:15px;
      text-align:center;
      backdrop-filter: blur(10px);
      background: ", glass_bg, ";" 
        ),
        div(style="font-size:12px; opacity:0.8;", label),
        div(style="font-size:22px; font-weight:700;", value)
      )
      
    } else if(identity_style == "center"){
      div(
        style="text-align:center; margin:15px 0;",
        div(style="font-size:13px; opacity:0.7;", label),
        div(style="font-size:26px; font-weight:800;", value)
      )
    } else if(identity_style == "inline"){
      tagList(
        strong(tagList(label, ": ")),
        span(value)
      )
    }
  }
  
  # mini card
  if(width == "mini" && !is.null(stats)){
    return(
      div(
        style = paste0("display:flex; gap: 15px;", width_style, "width: 100%;",bg_style),
          lapply(stats, function(s){
            div(
              style = paste0("flex:1; text-align:center; background:",glass_bg, "; justify-content:space-around; border-radius:15px; padding: 15px;"),
              div(style="font-size:22px; font-weight:700;", s$value),
              div(style="font-size:12px; opacity:0.7;", s$label)
            )
          })
        ))
  }
    
  # list renderer
  list_block <- NULL

  if(!is.null(list_items) && isTRUE(list_items$comparison)){
    list_block <- div(
      style = "display: flex; gap: 15px; width: 100%;",
      div(style = "flex: 1;",
          if(!is.null(list_items$title)) tagList(strong(list_items$title),br()),
          div(style="display:flex; flex-direction:column; align-items:flex-start; gap:10px;", 
              lapply(seq_len(nrow(list_items$items)), function(i){
                list_items$renderer(list_items$items, i)
              }))
      ),
      div(style = "flex: 1;",
          if(!is.null(list_items$title2)) tagList(strong(list_items$title2),br()),
          div(style="display:flex; flex-direction:column; align-items:flex-start; gap:10px;", 
              lapply(seq_len(nrow(list_items$items2)), function(i){
                list_items$renderer(list_items$items2, i)
              }))
      )
    )
  } else if(!is.null(list_items)){
    list_block <- {div(
      if(!is.null(list_items$title)) tagList(strong(list_items$title),br()),
      div(style="display:flex; flex-wrap:wrap; align-items:flex-end; gap:10px;", 
          lapply(seq_len(nrow(list_items$items)), function(i){
            list_items$renderer(list_items$items, i)
      }))
    )}
  }
  
  # insights block
  insights_block <- NULL
  if(!is.null(insights)){
    insights_block <- tagList(
      if(!is.null(insights$intro)) tagList(insights$intro, br()),
      if(!is.null(insights$insight)) tagList(insights$insight),
      if(!is.null(insights$outro)) tagList(br(), insights$outro)
    )
  }
  
  # assemble card
  blocks <- list()
  
  if(!is.null(title)){
    blocks <- append(blocks, list(
      div(style=paste0("font-weight:700; font-size:20px; text-align:center; width:fit-content; margin:0 auto;border-radius: 10px; padding: 3px 10px;",if(!is.null(background_img)) "background:",glass_bg,"; backdrop-filter:blur(4px);"), title)
    ))
  }
  
  
  if(!is.null(intro)){
    blocks <- append(blocks, list(div(intro)))
  }
  
  if(identity_pre && !is.null(identity)){
    blocks <- append(blocks, list(div(identity_block(identity))))
  }
  
  if(!is.null(body_text)){
    blocks <- append(blocks, list(div(body_text)))
  }
  
  if(!is.null(list_block)){
    blocks <- append(blocks, list(div(list_block)))
  }
  
  if(!is.null(insights_block)){
    blocks <- append(blocks, list(div(insights_block)))
  }
  
  if(!is.null(plot)){
    blocks <- append(blocks, list(
      div(
        style = "display: flex; justify-content: center; align-items: center; width: 100%;",
        # We add 'color: inherit' to the container
        div(style = paste0("color:", text_color, "; width: 100%;"), 
            plotlyOutput(plot, height = "250px")
        )
      )
    ))
  }
  
  if(!identity_pre && !is.null(identity)){
    blocks <- append(blocks, list(div(identity_block(identity))))
  }
  
  if(!is.null(footer)){
    blocks <- append(blocks, list(
      div(style="opacity:0.8;", footer)
    ))
  }
  
  if(!is.null(img_layer)){
    final_card <- div(style = paste0(bg_style, "position: relative; overflow: hidden; display: flex; flex-direction: column; ", width_style),
      img_layer,
      div(style = paste0("position: relative; z-index: 1; width: 100%; display:flex; flex-direction: column; min-width: 0; align-items: stretch; gap: 16px;", width_style),
      tagList(blocks) 
    )
    )
  } else{
    final_card <- div(
      style = paste0(bg_style, "display:flex; flex-direction: column; min-width: 0; align-items: stretch; gap: 16px;", width_style),
      # Use tagList to "unpack" the blocks safely
      tagList(blocks) 
    )
  } 
  
  
  if(is.null(layout$inactive) | isFALSE(layout$inactive)) return(final_card) else return()
  
  div(
    style = paste0(bg_style, "display:flex; flex-direction: column; min-width: 0; align-items: stretch; gap: 16px;", width_style), 
    blocks
    )
}

get_theme_from_archetype <- function(genre, mood) {
  # Map genres to base theme sets
  genre_theme <- list(
    "ya"            = "ya",
    "romance"       = "romance",
    "other lit"     = "other lit",
    "other lang"    = "other lang",
    "literature"    = "lit",
    "suspense"      = "suspense",
    "nonfic"        = "nonfic",
    "fantasy"       = "fantasy",
    "kids"          = "kids",
    "sci-fi"        = "scifi"
  )
  
  # Define which moods are light vs dark
  light_moods <- c("funny", "easy", "soft", "meaningful")
  dark_moods  <- c("creepy", "epic", "strong", "difficult", "informative")
  
  # Determine light/dark suffix
  mood_prefix <- ifelse(mood %in% light_moods, "1", 
                        ifelse(mood %in% dark_moods, "2", ""))
  
  # Combine
  theme_name <- paste0(mood_prefix, " ", genre_theme[[genre]])
  
  return(theme_name)
}


cov_f <- function(img_url, size = 1, rank = NULL, bg_g = "rgba(255,255,255,0.15)") {
  if (is.na(img_url) || img_url == "") return(NULL)
  
  h <- 150 * size
  p <- 10 * size
  bg_g <- sub(",[^,]+\\)$", ", 0.2)", bg_g)
  
  div(
    style = paste0("position: relative; display: inline-block; padding: ", p, "px;"),
    tags$img(
      src = img_url,
      style = paste0(
        "height: ", h, "px; width: auto; border-radius: 2px; object-fit: contain; ",
        if(!is.null(rank)) "opacity:0.8;" else ""
      )
    ),
    if (!is.null(rank)) {
      div(
        style = paste0("position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); 
                 color: currentColor; font-size: 14px; opacity: 0.9;
                 width:fit-content; margin:0 auto;border-radius: 4px; background:",bg_g,"; backdrop-filter:blur(10px);"),
        rank
      )
    }
  )
}


render_stack <- function(cover_list, size = 1) {
  cover_list <- cover_list[!is.na(cover_list) & cover_list != ""]
  if (length(cover_list) == 0) return(NULL)
  
  h <- 150 * size
  p <- 10 * size
  tab_w <- 15 * size # The exact width of the 'tab' visible on the right

  div(
    style = paste0(
      "display: inline-grid; ",
      "grid-template-columns: min-content; ", # Shrink-wrap to fit the fan
      "padding: ", p, "px; "
    ),
    lapply(seq_along(cover_list), function(i) {
      shadow_style <- if(i < length(cover_list)) "box-shadow: 4px 0 4px -2px rgba(0,0,0,0.3);" else ""
      tags$img(
        src = cover_list[[i]],
        style = paste0(
          "height: ", h, "px; width: auto; border-radius: 2px; object-fit: contain; ",
          "grid-area: 1 / 1; ",          # Place all books in the exact same spot
          "z-index: ", 100 - i, "; ",    # Book 1 is z-index 99 (top), Book 2 is 98...
          "margin-left: ", (i - 1) * tab_w, "px; ",
          "background-color: white; ", # Prevents the shadow behind from leaking through
          shadow_style
        )
      )
    })
  )
}

merge_series_covers <- function(df, sized = FALSE, s = 1){
  if (nrow(df) == 0) return(NULL)
  df %>% mutate(.orig_idx = row_number()) %>% group_by(series_base) %>% mutate(siz =  if(!sized) s else first(abs_diff), reading_experience = mean(reading_experience)) %>% arrange(series) %>% 
    mutate(cover = if (n() > 1) list(render_stack(cover, size = siz)) else list(cov_f(cover, size = siz))) %>% slice(1) %>% ungroup() %>% arrange(.orig_idx) %>% select(-.orig_idx)
  }



