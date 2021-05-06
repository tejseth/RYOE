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

pbp_all_un <- read_csv(url("https://raw.githubusercontent.com/tejseth/RYOE/main/pbp_all_un.csv"))

#pbp_all_un <- read.csv(file = 'pbp_all_un.csv')

ids <- teams_colors_logos %>%
  pull(team_abbr)

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
                          choices = 2010:2020,
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
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("update", "Update", width = '50%')
                 )),
               
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
               choices = 2010:2020,
               selected = 2020
             ),
            )
           ),
           fluidRow(
             column(12, align = "center",
                    actionButton("update", "Update", width = '50%')
             
             )
          ),
          mainPanel(
            plotOutput(outputId = "team_graph",
                       width = "100%"),
            tableOutput(outputId = "team_table_1"),
            tableOutput(outputId = "team_table_2")
          ),
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
  })
  
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
  })
  
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
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

