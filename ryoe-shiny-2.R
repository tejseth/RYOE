library(tidyverse)
library(ggthemes)
library(nflfastR)
library(ggimage)
library(shiny)
library(shinythemes)
library(gt)
library(ggrepel)
library(ggbeeswarm)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 16),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

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
        sides = "bottom", color = "white", weight = px(2)
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
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "white",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    )}

ryoe_github <- read_csv(url("https://raw.githubusercontent.com/tejseth/RYOE/main/ryoe_github.csv"))

ryoe_github <- ryoe_github %>%
  dplyr::select(-starts_with(".."))

teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

ids <- teams %>%
  pull(team_abbr)

ryoe_github <- ryoe_github %>%
  group_by(player) %>%
  mutate(count = n()) %>%
  filter(count >= 5) %>%
  ungroup()

rushers <- unique(ryoe_github$player)
seasons <- unique(ryoe_github$season)

options(shiny.usecairo=T)

ui <- fluidPage(
  
  titlePanel("Rushing Yards Over Expected"),
  
  mainPanel(
    navbarPage("By Tej Seth at PFF",
        tabPanel("By Season",
           fluidRow(
             column(4, align = "center",
                    sliderInput("year_range", "Year Range", value = c(2019, 2021), min = 2006, max = 2021, sep = ""),
             ),
             column(7, align = "center",
                    sliderInput("min_rushes", "Minimum Rushes", value = 100, min = 5, max = 1000),
             ),
           mainPanel(
             plotOutput(outputId = "rusher_graph",
                        width = "100%",
                        height = "50%"),
             br(),
             tableOutput("rusher_table"),
             br()
           )
        )   
      ),
      tabPanel("By Team",
               fluidRow(
                 tags$h3("Parameters"),
                 column(4, align = "center",
                        selectInput("team",
                                    "Offense:",
                                    c(sort(unique(as.character(ids)))), selected = "DET"),
                        ),
                 column(7, align = "center",
                        selectInput(
                          inputId =  "team_season",
                          label = "Season:",
                          choices = 2006:2021,
                          selected = 2021
                        ),
                        sliderInput(
                          inputId =  "team_min_rushes",
                          label = "Minimum Rushes:",
                          min = 1, max = 300,
                          value = 5
                        ),
                    ),
               ),
               mainPanel(
                 plotOutput(outputId = "team_graph",
                            width = "100%",
                            height = "50%"),
                 br(),
                 tableOutput(outputId = "team_table_1"),
                 br()
               ),
      ),
      tabPanel('Rusher Comparison',
               fluidRow(
                 column(7, align = "center",
                        selectInput("player_1",
                                    "Player 1", 
                                    c(sort(unique(as.character(rushers)))), selected = "Alvin Kamara"),
                        selectInput("player_2",
                                    "Player 2", 
                                    c(sort(unique(as.character(rushers)))), selected = "Nick Chubb"),
                        selectInput("player_3",
                                    "Player 3", 
                                    c(sort(unique(as.character(rushers)))), selected = "Derrick Henry"),
                 ),
                 column(4, align = "center", 
                 sliderInput("range_years", "Year Range", value = c(2018, 2021), min = 2006, max = 2021, sep = ""),
                 sliderInput("week_range", "Weeks Range", value = c(1, 17), min = 1, max = 17),
               ),
               mainPanel(
                 plotOutput(outputId = "csum_graph",
                            width = "750px", height = "500px"),
                 br(),
                 br(),
                 br(),
                 tableOutput(outputId = "rusher_comp_tab"), 
                 br(),
               ),
        )
      )
    )
  )
)

server <- function(input, output) { 
  output$rusher_graph <-  renderPlot({
    
    rushes_season <- ryoe_github %>%
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    rb_colors <- rushes_season %>%
      group_by(player, offense) %>%
      summarize(plays = n()) %>%
      arrange(-plays) %>%
      group_by(player) %>%
      top_n(n = 1) %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
      
    rushers_season <- rushes_season %>%
      filter(!is.na(player)) %>%
      group_by(player) %>%
      summarize(rushes = n(),
                mean_epa = mean(EPA, na.rm = T),
                actual_yards = mean(yards, na.rm = T),
                exp_yards = mean(exp_yards, na.rm = T),
                avg_ryoe = mean(ryoe, na.rm = T))
    
    rushers_season <- rushers_season %>%
      filter(rushes > as.numeric(input$min_rushes))
    
    rushers_season <- rushers_season %>%
      left_join(rb_colors, by = c("player"))
    
    rushers_season %>% 
      ggplot() +
      ggrepel::geom_text_repel(
        aes(x = mean_epa, y = avg_ryoe, label = player),
        box.padding = 0.3, size = 5
      ) + 
      geom_point(
        aes(x = mean_epa, y = avg_ryoe, size = rushes, fill = team_color, color = team_color2), 
        shape = 21
      ) +
      geom_hline(yintercept = mean(rushers_season$avg_ryoe), color = "black", linetype = "dashed", alpha=0.7) +
      geom_vline(xintercept =  mean(rushers_season$mean_epa), color = "black", linetype = "dashed", alpha=0.7) +
      scale_color_identity(aesthetics =  c("fill", "color")) +
      scale_size(name = "Designed Rushes") +
      theme_minimal() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
      labs(x = "EPA Per Rush",
           y = "Rushing Yards Over Expected Per Rush",
           title = paste0("Rushing Yards Over Expected and EPA, ", input$year_range[1], "-", input$year_range[2]),
           subtitle = paste0("Minimum of ", input$min_rushes," designed rushes in the time period"),
           caption = "By Tej Seth | @tejfbanalytics | PFF") +
      theme_reach()
  },  height = 600, width = 850)
  
  output$rusher_table <- render_gt({
    
    rushes_season <- ryoe_github %>%
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    rb_colors <- rushes_season %>%
      group_by(player, offense) %>%
      summarize(plays = n()) %>%
      arrange(-plays) %>%
      group_by(player) %>%
      top_n(n = 1) %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
    
    rushers_season <- rushes_season %>%
      filter(!is.na(player)) %>%
      group_by(player) %>%
      summarize(rushes = n(),
                mean_epa = mean(EPA, na.rm = T),
                actual_yards = mean(yards, na.rm = T),
                exp_yards = mean(exp_yards, na.rm = T),
                avg_ryoe = mean(ryoe, na.rm = T))
    
    rushers_season <- rushers_season %>%
      filter(rushes > as.numeric(input$min_rushes))
    
    rushers_season <- rushers_season %>%
      left_join(rb_colors, by = c("player"))
    
    rusher_gt <- rushers_season %>%
      dplyr::select(player, team_logo_espn, rushes, mean_epa, actual_yards, exp_yards, avg_ryoe) %>%
      mutate_if(is.numeric, ~round(., 2)) %>%
      arrange(-avg_ryoe) %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      dplyr::select(rank, everything())
    
    rusher_gt %>% gt() %>%
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
        player = "Player",
        team_logo_espn = "Team",
        rushes = "Rushes",
        mean_epa = "EPA/Rush",
        actual_yards = "Yards Per Carry",
        exp_yards = "Expected Yards",
        avg_ryoe = "Rushing Yards Over Expected") %>%
      data_color(
        columns = c(avg_ryoe),
        colors = scales::col_numeric(
          palette = c("#D70915", "#FD8C24", "#085D29"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 850)
  
  output$team_graph <-  renderPlot({
    
    team_rushes <- ryoe_github %>%
      filter(season == input$team_season) %>%
      filter(offense == input$team)
    
    team_rushes <- team_rushes %>%
      filter(!is.na(player)) %>%
      group_by(player) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      filter(count >= as.numeric(input$team_min_rushes))
    
    team_rushes %>%
      filter(ryoe <= 25) %>%
      filter(ryoe > -7.5) %>%
      filter(week <= 17) %>%
      ggplot(aes(x = player, y = ryoe, fill = ryoe)) + 
      geom_quasirandom(pch = 21, size = 5) + 
      scale_fill_viridis_c() +
      theme_reach() +
      geom_hline(yintercept = 0, color = "black", alpha=1.0) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(
        x = "Rusher",
        y = "RYOE",
        title = paste0("The Rushing Yards Over Expected of All ", input$team, "'s Rushes in ", input$team_season),
        subtitle = paste0("Minimum of ", input$team_min_rushes, " rushes in the specified season"),
        caption = "By Tej Seth | @tejfbanalytics | PFF"
      ) 
  }, height = 550, width = 800)
  
  output$team_table_1 <- render_gt({
    
    team_rushes <- ryoe_github %>%
      filter(season == input$team_season) %>%
      filter(offense == input$team)
    
    team_rushes <- team_rushes %>%
      filter(!is.na(player)) %>%
      filter(!is.na(EPA)) %>%
      filter(!is.na(ryoe)) %>%
      group_by(player) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      filter(count >= as.numeric(input$team_min_rushes))
    
    team_rushers <- team_rushes %>%
      group_by(player, offense) %>%
      summarize(rushes = n(),
                mean_epa = mean(EPA, na.rm = T),
                actual_yards = mean(yards, na.rm = T),
                exp_yards = mean(exp_yards, na.rm = T),
                avg_ryoe = mean(ryoe, na.rm = T))
    
    team_rushers <- team_rushers %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
    
    team_rushers <- team_rushers %>%
      arrange(desc(avg_ryoe)) %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      select(rank, player, team_logo_espn, rushes, mean_epa, actual_yards, exp_yards, avg_ryoe)
    
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
        player = "Player",
        team_logo_espn = "Team",
        rushes = "Rushes",
        mean_epa = "EPA/Rush",
        actual_yards = "Yards Per Carry",
        exp_yards = "Expected Yards",
        avg_ryoe = "Rushing Yards Over Expected") %>%
      data_color(
        columns = c(avg_ryoe),
        colors = scales::col_numeric(
          palette = c("#D70915", "#FD8C24", "#085D29"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
    
  }, width = 800)
  
  output$csum_graph <- renderPlot({
    
    rushers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp <- ryoe_github %>%
      filter(!is.na(ryoe)) %>%
      filter(player %in% rushers_needed) %>%
      filter(season >= input$range_years[1] & season <= input$range_years[2]) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    filtered_pbp$csum <- ave(filtered_pbp$ryoe, filtered_pbp$player, FUN=cumsum)
    
    filtered_pbp$rush_att <- ave(filtered_pbp$ryoe, filtered_pbp$player, FUN = seq_along)
    
    filtered_pbp$Rusher <- filtered_pbp$player
    
    filtered_pbp %>%
      ggplot( aes(x=rush_att, y=csum, group=Rusher, color=Rusher)) +
      geom_line(size = 2) +
      theme_bw() + 
      scale_color_viridis_d() +
      labs(x = "Rushing Attempts",
           y = "Cumulative RYOE",
           title = "Cumulative Rushing Yards Over Expected (RYOE)",
           subtitle = paste0(input$range_years[1], "-", input$range_years[2], ", weeks ", input$week_range[1], "-", input$week_range[2]),
           caption = "By Tej Seth | @tejfbanalytics | PFF") +
      theme_reach() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 16)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
  }, height = 550, width = 800)
  
  output$rusher_comp_tab <- render_gt({
    
    rushers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp <- ryoe_github %>%
      filter(!is.na(ryoe)) %>%
      filter(player %in% rushers_needed) %>%
      filter(season >= input$range_years[1] & season <= input$range_years[2]) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    rushers_logos <- filtered_pbp %>%
      group_by(player, offense) %>%
      summarize(count = n()) %>%
      arrange(player, count) %>%
      top_n(1) %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
    
    rusher_tab <- filtered_pbp %>%
      group_by(player) %>%
      summarize(rushes = n(),
                mean_epa = mean(EPA, na.rm = T),
                actual_yards = mean(yards, na.rm = T),
                exp_yards = mean(exp_yards, na.rm = T),
                avg_ryoe = mean(ryoe, na.rm = T))
    
    rusher_tab <- rusher_tab %>%
      left_join(rushers_logos, by = "player")
    
    rusher_tab <- rusher_tab %>%
      arrange(desc(avg_ryoe)) %>%
      mutate(rank = row_number()) %>%
      select(rank, player, team_logo_espn, rushes, mean_epa, actual_yards, exp_yards, avg_ryoe)
    
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
        player = "Player",
        team_logo_espn = "Team",
        rushes = "Rushes",
        mean_epa = "EPA/Rush",
        actual_yards = "Yards Per Carry",
        exp_yards = "Expected Yards",
        avg_ryoe = "Rushing Yards Over Expected") %>%
      data_color(
        columns = c(avg_ryoe),
        colors = scales::col_numeric(
          palette = c("#D70915", "#FD8C24", "#085D29"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
    
  }, width = 800)
  
}

shinyApp(ui = ui, server = server)







