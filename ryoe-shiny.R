#install.packages("shiny")
#install.packages("shinythemes")
library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)
library(bayesboot)
library(rvest)
library(shiny)
library(shinythemes)
library(Cairo)
library(ggbeeswarm)

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    )}

pbp_all_un_1 <- read_csv(url("https://raw.githubusercontent.com/tejseth/RYOE/main/pbp_all_un_1.csv"))
pbp_all_un_2 <- read_csv(url("https://raw.githubusercontent.com/tejseth/RYOE/main/pbp_all_un_2.csv"))
pbp_all_un <- rbind(pbp_all_un_1, pbp_all_un_2)

teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

# team abbreviations for dropdown menus
ids <- teams %>%
  pull(team_abbr)

pbp_all_un <- pbp_all_un %>%
  group_by(rusher_player_name) %>%
  mutate(count = n()) %>%
  mutate(over_five = case_when(
    count >= 10 ~ "Yes",
    count < 10 ~ "No"
  )) %>%
  ungroup()

pbp_all_un2 <- pbp_all_un %>%
  filter(over_five == "Yes")
  
rushers <- unique(pbp_all_un2$rusher_player_name)

options(shiny.usecairo=T)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  titlePanel("Rushing Yards Over Expected"),
  
  mainPanel(
    navbarPage("@mfbanalytics",
      tabPanel("By Season",
               fluidRow(
                 column(4, align = "center",
                        
                        tags$h3("Parameters"),
                        
                        selectInput(
                          inputId =  "season",
                          label = "Season:",
                          choices = 1999:2020,
                          selected = 2020
                        ),
                        
                        sliderInput(
                          inputId =  "min_rushes",
                          label = "Minimum Rushes:",
                          min = 1, max = 300,
                          value = 80
                        ),
                        
                 )
               ),
               
               mainPanel(
                 plotOutput(outputId = "rusher_graph",
                            width = "100%",
                            height = "50%"),
                 tableOutput("rusher_table")
               )        
               
  ),
  tabPanel("By Team",
           fluidRow(
             column(4, align = "center",
                    
                    tags$h3("Parameters"),
             selectInput("team",
                         "Offense:",
                         c(sort(unique(as.character(ids)))), selected = "DET"),
             sliderInput(
               inputId =  "team_min_rushes",
               label = "Minimum Rushes:",
               min = 1, max = 300,
               value = 50
             ),
             selectInput(
               inputId =  "team_season",
               label = "Season:",
               choices = 1999:2020,
               selected = 2020
             ),
            )
           ),
          mainPanel(
            plotOutput(outputId = "team_graph",
                       width = "100%",
                       height = "50%"),
            tableOutput(outputId = "team_table_1"),
            tableOutput(outputId = "team_table_2")
          ),
      ),
  tabPanel('Rusher Comparison',
           fluidRow(
             column(4, align = "center",
                    tags$h3('Parameters'),
              selectInput("player_1",
                          "Player 1", 
                          c(sort(unique(as.character(rushers)))), selected = "A.Kamara"),
              selectInput("player_2",
                          "Player 2", 
                          c(sort(unique(as.character(rushers)))), selected = "N.Chubb"),
              selectInput("player_3",
                          "Player 3", 
                          c(sort(unique(as.character(rushers)))), selected = "D.Henry"),
             ),
             sliderInput("year_range", "Year Range", value = c(2018, 2020), min = 1999, max = 2020, sep = ""),
             sliderInput("week_range", "Weeks Range", value = c(1, 17), min = 1, max = 17),
           ),
           mainPanel(
             plotOutput(outputId = "csum_graph",
                        width = "750px", height = "500px"),
             tableOutput(outputId = "rusher_comp_tab"), 
             plotOutput(outputId = "perc_stacked",
                        width = "750px", height = "500px")
           ),
           column(6, plotOutput(outputId = "rusher_graph_1", width = "750px", height = "500px")),
           column(9, plotOutput(outputId = "rusher_graph_2", width = "750px", height = "500px")),
           column(12, plotOutput(outputId = "rusher_graph_3", width = "750px", height = "500px")),
           #plotOutput(outputId = "rusher_graph_1"),
           #plotOutput(outputId = "rusher_graph_2"),
           #plotOutput(outputId = "rusher_graph_3"),
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rusher_graph <-  renderPlot({
    
    rushes_season <- pbp_all_un %>%
      filter(season == input$season)
    
    rushers_season <- rushes_season %>%
      filter(!is.na(rusher_player_name)) %>%
      group_by(rusher_player_name, posteam) %>%
      summarize(rushes = n(),
                mean_epa = mean(epa),
                avg_ryoe = mean(ryoe))
    
    rushers_season <- rushers_season %>%
      filter(rushes >= as.numeric(input$min_rushes))
    
    rushers_season <- rushers_season %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    rushers_season %>% 
      ggplot() +
      ggrepel::geom_text_repel(
        aes(x = mean_epa, y = avg_ryoe, label = rusher_player_name),
        box.padding = 0.3, size = 5
      ) + 
      geom_point(
        aes(x = mean_epa, y = avg_ryoe, size = rushes, fill = team_color, color = team_color2), 
        shape = 21
      ) +
      geom_hline(yintercept = 0, color = "blue", linetype = "dashed", alpha=0.7) +
      geom_vline(xintercept =  0, color = "blue", linetype = "dashed", alpha=0.7) +
      scale_color_identity(aesthetics =  c("fill", "color")) +
      scale_size(name = "Designed Rushes") +
      theme_minimal() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
      labs(x = "EPA Per Rush",
           y = "Rushing Yards Over Expected Per Rush",
           title = paste0("Rushing Yards Over Expected and EPA in ", input$season),
           subtitle = paste0("RYOE is a xgboost model, min of ", input$min_rushes," designed rushes"),
           caption = "By Tej Seth | @mfbanalytics") +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
      ) 
  },  height = 600, width = 850)
  
  output$rusher_table <- render_gt({
  
  rushes_season <- pbp_all_un %>%
    filter(season == input$season)
  
  rushers_season <- rushes_season %>%
    filter(!is.na(rusher_player_name)) %>%
    group_by(rusher_player_name, posteam) %>%
    summarize(rushes = n(),
              epa_per_rush = mean(epa),
              avg_ryoe = mean(ryoe))
  
  rushers_season <- rushers_season %>%
    filter(rushes >= as.numeric(input$min_rushes))
  
  rushers_season <- rushers_season %>%
    left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
  
  rushers_season <- rushers_season %>%
    arrange(desc(avg_ryoe)) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    select(rank, rusher_player_name, team_logo_espn, rushes, epa_per_rush, avg_ryoe)
  
  rushers_season <- rushers_season %>%
    mutate_if(is.numeric, ~round(., 2))
  
  rushers_season %>% gt() %>%
    text_transform(
      locations = cells_body(c(team_logo_espn)),
      fn = function(x){
        web_image(
          url = x,
          height = px(35)
        )
      }
    ) %>% 
    cols_label(
      rank = "Rank",
      rusher_player_name = "Rusher",
      team_logo_espn = "",
      rushes = "Rushes",
      epa_per_rush = "EPA/rush",
      avg_ryoe = "RYOE/rush") %>%
    data_color(
      columns = c(avg_ryoe),
      colors = scales::col_numeric(
        palette = c("white", "#3fc1c9"),
        domain = NULL
      )
    ) %>% 
    tab_source_note(
      source_note = md("By Tej Seth | @mfbanalytics <br>Inspiration: @thomas_mock")
    ) %>% 
    opt_align_table_header(align = "center") %>%
    tab_header(
      title = md(paste0("Rushing Yards Over Expected, ", input$season)),
      subtitle = md(paste0("RYOE is an xgboost model built on @nflfastR, min. of ", input$min_rushes, " designed rushes"))
    ) %>% 
    opt_row_striping() %>%
    gt_theme_538()
  }, width = 600)
  
  output$team_graph <-  renderPlot({
    
    team_rushes <- pbp_all_un %>%
      filter(season == input$team_season) %>%
      filter(posteam == input$team)
    
    team_rushes <- team_rushes %>%
      filter(!is.na(rusher_player_name)) %>%
      group_by(rusher_player_name) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      filter(count >= as.numeric(input$team_min_rushes))
    
    team_rushes <- team_rushes %>%
      mutate(Label = case_when(
        ryoe < 0 ~ "Less than Zero RYOE",
        ryoe >= 0 & ryoe < 5 ~ "0 to 5 RYOE",
        ryoe >= 5 & ryoe < 15 ~ "5 to 15 RYOE",
        ryoe >= 15 ~ "15+ RYOE"
      ))
    
    team_rushes %>%
      filter(ryoe <= 25) %>%
      filter(ryoe > -7.5) %>%
      filter(week <= 17) %>%
      ggplot(aes(x = rusher_player_name, y = ryoe, fill = Label)) + 
      geom_quasirandom(pch = 21, size = 4.5) + 
      scale_fill_brewer(palette = "Spectral") +
      theme_classic(15) +  
      geom_hline(yintercept = 0, color = "black", alpha=1.0) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(
        x = "Rusher",
        y = "RYOE",
        title = paste0("The RYOE of All ", input$team, "'s Rushes in ", input$team_season),
        subtitle = "Based off of a Rushing Yards over Expected (RYOE) model trained on @nflfastR data",
        caption = "By Tej Seth | @mfbanalytics"
      ) +
      theme(
        legend.position = "None",
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      ) 
  }, height = 400, width = 600)
  
  output$team_table_1 <- render_gt({
    
    team_rushes <- pbp_all_un %>%
      filter(season == input$team_season) %>%
      filter(posteam == input$team)
    
    team_rushes <- team_rushes %>%
      filter(!is.na(rusher_player_name)) %>%
      filter(!is.na(epa)) %>%
      filter(!is.na(ryoe)) %>%
      group_by(rusher_player_name) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      filter(count >= as.numeric(input$team_min_rushes))
    
    team_rushers <- team_rushes %>%
      group_by(rusher_player_name, posteam) %>%
      summarize(rushes = n(),
                epa_per_rush = mean(epa),
                avg_ryoe = mean(ryoe))
    
    team_rushers <- team_rushers %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    team_rushers <- team_rushers %>%
      arrange(desc(avg_ryoe)) %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      select(rank, rusher_player_name, team_logo_espn, rushes, epa_per_rush, avg_ryoe)
    
    team_rushers <- team_rushers %>%
      mutate_if(is.numeric, ~round(., 2))
    
    team_rushers %>% gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>% 
      cols_label(
        rank = "Rank",
        rusher_player_name = "Rusher",
        team_logo_espn = "",
        rushes = "Rushes",
        epa_per_rush = "EPA/rush",
        avg_ryoe = "RYOE/rush") %>%
      data_color(
        columns = c(avg_ryoe),
        colors = scales::col_numeric(
          palette = c("white", "#3fc1c9"),
          domain = NULL
        )
      ) %>% 
      tab_source_note(
        source_note = md("By Tej Seth | @mfbanalytics <br>Inspiration: @thomas_mock")
      ) %>% 
      opt_align_table_header(align = "center") %>%
      tab_header(
        title = md(paste0("The RYOE of ", input$team, "'s Rushers in ", input$team_season)),
        subtitle = md(paste0("RYOE is an xgboost model built on @nflfastR, min. of ", input$team_min_rushes, " designed rushes"))
      ) %>% 
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 600)
  
  output$team_table_2 <- render_gt({
    
    team_rushes <- pbp_all_un %>%
      filter(!is.na(epa)) %>%
      filter(posteam == input$team)
    
    season_rushers <- team_rushes %>%
      filter(!is.na(ryoe)) %>%
      group_by(posteam, season) %>%
      summarize(rushes = n(),
                epa_per_rush = mean(epa),
                avg_ryoe = mean(ryoe)) %>%
      arrange(season)
    
    season_rushers <- season_rushers %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    season_rushers <- season_rushers %>%
      select(posteam, season, team_logo_espn, rushes, epa_per_rush, avg_ryoe)
    
    season_rushers <- season_rushers %>%
      mutate_if(is.numeric, ~round(., 2))
    
    season_rushers %>% gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>% 
      cols_label(
        posteam = "Team",
        season = "Season",
        team_logo_espn = "",
        rushes = "Rushes",
        epa_per_rush = "EPA/rush",
        avg_ryoe = "RYOE/rush") %>%
      data_color(
        columns = c(avg_ryoe),
        colors = scales::col_numeric(
          palette = c("white", "#3fc1c9"),
          domain = NULL
        )
      ) %>% 
      tab_source_note(
        source_note = md("By Tej Seth | @mfbanalytics <br>Inspiration: @thomas_mock")
      ) %>% 
      opt_align_table_header(align = "center") %>%
      tab_header(
        title = md(paste0("The Historical RYOE of ", input$team)),
        subtitle = md(paste0("RYOE is an xgboost model built on @nflfastR, designed rushes only"))
      ) %>% 
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 600)
  
  output$csum_graph <- renderPlot({
    
    rushers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(rusher_player_name %in% rushers_needed) %>%
      filter(season >= input$year_range[1] & season <= input$year_range[2]) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    filtered_pbp$csum <- ave(filtered_pbp$ryoe, filtered_pbp$rusher_player_name, FUN=cumsum)
    
    filtered_pbp$rush_att <- ave(filtered_pbp$ryoe, filtered_pbp$rusher_player_name, FUN = seq_along)
    
    filtered_pbp$Rusher <- filtered_pbp$rusher_player_name
    
    filtered_pbp %>%
      ggplot( aes(x=rush_att, y=csum, group=Rusher, color=Rusher)) +
      geom_line(size = 2) +
      theme_bw() + 
      scale_color_brewer(palette = "Dark2") +
      labs(x = "Rushing Attempts",
           y = "Cumulative RYOE",
           title = "Cumulative Rushing Yards Over Expected (RYOE)",
           subtitle = paste0(input$year_range[1], "-", input$year_range[2], ", weeks ", input$week_range[1], "-", input$week_range[2]),
           caption = "By Tej Seth | @mfbanalytics") +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text=element_text(size=14),
        legend.title = element_text(face = "bold", size=16)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  }, height = 500, width = 750)
  
  output$perc_stacked <- renderPlot({
    
    rushers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(rusher_player_name %in% rushers_needed) %>%
      filter(season >= input$year_range[1] & season <= input$year_range[2]) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    filtered_pbp$csum <- ave(filtered_pbp$ryoe, filtered_pbp$rusher_player_name, FUN=cumsum)
    
    filtered_pbp$rush_att <- ave(filtered_pbp$ryoe, filtered_pbp$rusher_player_name, FUN = seq_along)
    
    filtered_pbp$Rusher <- filtered_pbp$rusher_player_name
    
    filtered_pbp <- filtered_pbp %>%
      mutate(Label = case_when(
        ryoe < 0 ~ "4: Less than Zero RYOE",
        ryoe >= 0 & ryoe < 5 ~ "3: 0 to 5 RYOE",
        ryoe >= 5 & ryoe < 15 ~ "2: 5 to 15 RYOE",
        ryoe >= 15 ~ "1: 15+ RYOE"
      ))
    
    grouped <- filtered_pbp %>%
      group_by(Rusher, Label) %>%
      summarize(count = n())
    
    ggplot(filtered_pbp, aes(fill=Label, y=count, x=Rusher)) + 
      geom_bar(position="fill", stat="identity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = "Rusher",
           y = "RYOE Percentage",
           title = "Each Rusher's Rushing Yards Over Expected Make-Up",
           caption = "By Tej Seth | @mfbanalytics",
           subtitle = paste0(input$year_range[1], "-", input$year_range[2], ", weeks ", input$week_range[1], "-", input$week_range[2]))+
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 16),
        legend.text=element_text(size=14),
        legend.title = element_text(face = "bold", size=16),
        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  }, height = 500, width = 750)
  
  output$rusher_comp_tab <- render_gt({
    
    rushers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(rusher_player_name %in% rushers_needed) %>%
      filter(season >= input$year_range[1] & season <= input$year_range[2]) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    rushers_logos <- filtered_pbp %>%
      group_by(rusher_player_name, posteam) %>%
      summarize(count = n()) %>%
      arrange(rusher_player_name, count) %>%
      top_n(1) %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    rusher_tab <- filtered_pbp %>%
      group_by(rusher_player_name) %>%
      summarize(rushes = n(),
                epa_per_rush = mean(epa),
                avg_ryoe = mean(ryoe))
    
    rusher_tab <- rusher_tab %>%
      left_join(rushers_logos, by = "rusher_player_name")
    
    rusher_tab <- rusher_tab %>%
      arrange(desc(avg_ryoe)) %>%
      mutate(rank = row_number()) %>%
      select(rank, rusher_player_name, team_logo_espn, rushes, epa_per_rush, avg_ryoe)
    
    rusher_tab <- rusher_tab %>%
      arrange(rank) %>%
      mutate_if(is.numeric, ~round(., 2))
    
    rusher_tab %>% gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>% 
      cols_label(
        rank = "Rank",
        rusher_player_name = "Rusher",
        team_logo_espn = "",
        rushes = "Rushes",
        epa_per_rush = "EPA/rush",
        avg_ryoe = "RYOE/rush") %>%
      data_color(
        columns = c(avg_ryoe),
        colors = scales::col_numeric(
          palette = c("white", "#3fc1c9"),
          domain = NULL
        )
      ) %>% 
      tab_source_note(
        source_note = md("By Tej Seth | @mfbanalytics <br>Inspiration: @thomas_mock")
      ) %>% 
      opt_align_table_header(align = "center") %>%
      tab_header(
        title = md(paste0("Rushing Yards Over Expected Comparison")),
        subtitle = md(paste0(input$year_range[1], "-", input$year_range[2], ", weeks ", input$week_range[1], "-", input$week_range[2]))
      ) %>% 
      opt_row_striping() %>%
      gt_theme_538()
    
    
  }, width = 600)
  
  
  output$rusher_graph_1 <- renderPlot({
    
    the_rusher_grouped <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(rusher_player_name == input$player_1) %>%
      group_by(rusher_player_name, season, posteam) %>%
      summarize(avg_ryoe = mean(ryoe)) %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    min_year = min(the_rusher_grouped$season)
    max_year = max(the_rusher_grouped$season)
    
    rushers_grouped <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(season >= min_year & season <= max_year) %>%
      group_by(rusher_player_name, season) %>%
      summarize(rushes = n(),
                avg_ryoe = mean(ryoe)) %>%
      filter(rushes >= 75) %>%
      filter(avg_ryoe <= 2)
    
    ggplot() +
      geom_jitter(data = rushers_grouped, aes(x = season, y = avg_ryoe), color = "black", 
                  width = 0.025, alpha = 0.35, size = 3) + 
      geom_line(data = the_rusher_grouped, aes(x = season, y = avg_ryoe, color = team_color)) +
      geom_image(data = the_rusher_grouped, aes(x = season, y = avg_ryoe, image = team_logo_espn), 
                 size = 0.07, asp = 16 / 9) +
      theme_bw() +
      scale_color_identity(aesthetics =  c("fill", "color")) +
      labs(x = "Season", 
           y = "Average RYOE",
           title = paste0(input$player_1, "'s Rushing Yards Over Expected by Season"),
           subtitle = "RYOE is a xgboost model, min. of 75 designed rushes for each dot",
           caption = "By Tej Seth | @mfbanalytics") +
      theme(
        legend.position = "None",
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept =  0, color = "blue", linetype = "dashed", alpha=0.9) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(breaks = unique(the_rusher_grouped$season))
      

  }, width = 750, height= 500)
  
  output$rusher_graph_2 <- renderPlot({
    
    the_rusher_grouped <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(rusher_player_name == input$player_2) %>%
      group_by(rusher_player_name, season, posteam) %>%
      summarize(avg_ryoe = mean(ryoe)) %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    min_year = min(the_rusher_grouped$season)
    max_year = max(the_rusher_grouped$season)
    
    rushers_grouped <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(season >= min_year & season <= max_year) %>%
      group_by(rusher_player_name, season) %>%
      summarize(rushes = n(),
                avg_ryoe = mean(ryoe)) %>%
      filter(rushes >= 75) %>%
      filter(avg_ryoe <= 2)
    
    ggplot() +
      geom_jitter(data = rushers_grouped, aes(x = season, y = avg_ryoe), color = "black", 
                  width = 0.025, alpha = 0.35, size = 3) + 
      geom_line(data = the_rusher_grouped, aes(x = season, y = avg_ryoe, color = team_color)) +
      geom_image(data = the_rusher_grouped, aes(x = season, y = avg_ryoe, image = team_logo_espn), 
                 size = 0.07, asp = 16 / 9) +
      theme_bw() +
      scale_color_identity(aesthetics =  c("fill", "color")) +
      labs(x = "Season", 
           y = "Average RYOE",
           title = paste0(input$player_2, "'s Rushing Yards Over Expected by Season"),
           subtitle = "RYOE is a xgboost model, min. of 75 designed rushes for each dot",
           caption = "By Tej Seth | @mfbanalytics") +
      theme(
        legend.position = "None",
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept =  0, color = "blue", linetype = "dashed", alpha=0.9) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(breaks = unique(the_rusher_grouped$season))
  }, width = 750, height= 500)
  
  output$rusher_graph_3 <- renderPlot({
    
    the_rusher_grouped <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(rusher_player_name == input$player_3) %>%
      group_by(rusher_player_name, season, posteam) %>%
      summarize(avg_ryoe = mean(ryoe)) %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
    min_year = min(the_rusher_grouped$season)
    max_year = max(the_rusher_grouped$season)
    
    rushers_grouped <- pbp_all_un %>%
      filter(!is.na(ryoe)) %>%
      filter(season >= min_year & season <= max_year) %>%
      group_by(rusher_player_name, season) %>%
      summarize(rushes = n(),
                avg_ryoe = mean(ryoe)) %>%
      filter(rushes >= 75) %>%
      filter(avg_ryoe <= 2)
    
    
    ggplot() +
      geom_jitter(data = rushers_grouped, aes(x = season, y = avg_ryoe), color = "black", 
                  width = 0.025, alpha = 0.35, size = 3) + 
      geom_line(data = the_rusher_grouped, aes(x = season, y = avg_ryoe, color = team_color)) +
      geom_image(data = the_rusher_grouped, aes(x = season, y = avg_ryoe, image = team_logo_espn), 
                 size = 0.07, asp = 16 / 9) +
      theme_bw() +
      scale_color_identity(aesthetics =  c("fill", "color")) +
      labs(x = "Season", 
           y = "Average RYOE",
           title = paste0(input$player_3, "'s Rushing Yards Over Expected by Season"),
           subtitle = "RYOE is a xgboost model, min. of 75 designed rushes for each dot",
           caption = "By Tej Seth | @mfbanalytics") +
      theme(
        legend.position = "None",
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept =  0, color = "blue", linetype = "dashed", alpha=0.9) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(breaks = unique(the_rusher_grouped$season))
  }, width = 750, height= 500)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

