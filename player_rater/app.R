# "RADARS"
# SHADOW CATS
# ADD FILTERS FOR GP AND MIN

suppressMessages(library(shiny))
suppressMessages(library(tidyverse))
suppressMessages(library(rvest))
suppressMessages(library(shinyWidgets))
suppressMessages(library(rlang))
library(httr)
library(funtasy)

sketch <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'rk'),
      th(rowspan = 2, 'player_name'),
      th(rowspan = 2, 'nba'),
      th(rowspan = 2, 'fantasy'),
      th(rowspan = 2, 'position'),
      th(rowspan = 2, 'gp'),
      th(rowspan = 2, 'min'),
      th(colspan = 9, 'per game stats'),
      th(colspan = 9, 'adjusted stats'),
      th(rowspan = 2, 'total')
    ),
    tr(
      lapply(rep(c('fg', 'ft', 'fg3m', 'reb', 'ast', 'stl', 'blk', 'tov', 'pts'), 2), th)
    )
  )
))

fantrax_api <- POST("https://www.fantrax.com/fxpa/req?leagueId=phgkz2f4m1rsp3pr",
                    config = add_headers("Host"= "www.fantrax.com",
                                         "User-Agent"= "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:129.0) Gecko/20100101 Firefox/129.0",
                                         "Accept"= "application/json",
                                         "Accept-Language"= "en-US,en;q=0.5",
                                         "Accept-Encoding"= "gzip, deflate, br, zstd",
                                         "Referer"= "https://www.fantrax.com/fantasy/league/phgkz2f4m1rsp3pr/players;statusOrTeamFilter=ALL;pageNumber=1;view=OVERVIEW;sortType=OVERVIEW_PERCENT_OWNED_2;maxResultsPerPage=500",
                                         "Content-Type"= "text/plain",
                                         "Content-Length" = "423",
                                         "DNT"= "1",
                                         "Connection"= "keep-alive",
                                         "Origin" = "https://www.fantrax.com",
                                         "Sec-GPC" = "1",
                                         "Cookie" = "uig=7hf2rzyzlpfvrjfe; ui=4u5zeeslj8n95pi1; _ga=GA1.2.2045361299.1701807679; _ga_DM2Q31JXYV=GS1.2.1712004348.2.1.1712004348.60.0.0; FX_RM=_qpxzUx1RHA4JGRsPSgsNB0NaVwcACwNcEh8LBg1dQFRcB1E=; SOCS=CAISNQgQEitib3FfaWRlbnRpdHlmcm9udGVuZHVpc2VydmVyXzIwMjQwNTE0LjA2X3AwGgJmaSADGgYIgOu0sgY; d_prefs=MjoxLGNvbnNlbnRfdmVyc2lvbjoyLHRleHRfdmVyc2lvbjoxMDAw; twtr_pixel_opt_in=N; dtCookie=v_4_srv_2_sn_E4D8AAFA39DA5D885FD854DD4D0895A3_perc_100000_ol_0_mul_1_app-3Aea7c4b59f27d43eb_1; JSESSIONID=node0zo7m4v9myac71ds18g53xobgw46038.node0"),
                    body = '{"msgs":[{"method":"getPlayerStats","data":{"statusOrTeamFilter":"ALL","pageNumber":"1","view":"OVERVIEW","sortType":"OVERVIEW_PERCENT_OWNED_2","maxResultsPerPage":"500"}}],"uiv":3,"refUrl":"https://www.fantrax.com/fantasy/league/phgkz2f4m1rsp3pr/players;statusOrTeamFilter=ALL;pageNumber=1;view=OVERVIEW;sortType=OVERVIEW_PERCENT_OWNED_2;maxResultsPerPage=500","dt":0,"at":0,"av":"3.0","tz":"Europe/Tallinn","v":"163.0.0"}') %>%
  content()

headers <- fantrax_api$responses[[1]]$data$tableHeader$cells %>% map_dfr(flatten_dfc)

one_player <- function(list){
  
  scorer <- list$scorer %>% map(~.[[1]]) %>% unlist() %>% as_tibble_row()
  stats <- list$cells %>% map(~.[[1]]) %>% unlist() %>% setNames(headers$shortName %>% janitor::make_clean_names()) %>% as_tibble_row()
  
  scorer %>% bind_cols(stats)
  
}

api_data <- map_dfr(fantrax_api$responses[[1]]$data$statsTable, one_player) %>% 
  type_convert(col_types = cols())

yr <- "2024-25"

if(file.exists(str_c("data_",yr,".csv"))){
  
  data <- read_csv(str_c("data_",yr,".csv"), col_types = cols())
  if(max(data$game_date) != lubridate::today(tzone = "EST") - 1){
    
    cat("Updating data...")
    data <- funtasy::leaguegamelog(season = "2024-25", type = "Regular+Season", league = "00")
    
    # data %>% readr::write_csv(str_c("data_",yr,".csv"))
    
  } else{
    cat("Data up to date...")
  }
  
} else {
  cat("Updating data...")
  data <- funtasy::leaguegamelog(season = yr, type = "Regular+Season")
  
  data %>% readr::write_csv(str_c("data_",yr,".csv"))
  
}

fantasy_teams <- api_data %>% 
  transmute(Player = name, Position = posShortNames, Status = sta) %>% 
  mutate(Position = str_remove_all(Position, ",Flx") %>% str_split(","),
         Status = if_else(Status == "FA", NA_character_, Status)) %>%
  mutate(Player = case_when(Player == "Luc Richard Mbah a Moute" ~ "Luc Mbah a Moute",
                            Player == "Moe Harkless" ~ "Maurice Harkless",
                            Player == "Nazareth Mitrou-Long"  ~ "Naz Mitrou-Long",
                            Player == "DeAndre Bembry"  ~ "DeAndre' Bembry",
                            Player == "Devonte Graham"  ~ "Devonte' Graham",
                            Player == "Nicolas Claxton"  ~ "Nic Claxton",
                            Player == "Cameron Thomas" ~ "Cam Thomas",
                            Player == "Nah'Shon Hyland" ~ "Bones Hyland",
                            Player == "BJ Boston" ~ "Brandon Boston",
                            Player == "Jakob Poeltl" ~ "Jakob Poltl",
                            Player == "Alexandre Sarr" ~ "Alex Sarr",
                            TRUE ~ Player),
         Player = Player %>% fix_names() %>% stringi::stri_trans_general(id = "Latin-ASCII")) %>%
  rename(player_name = Player, team = Status)

data <-
  data %>%
  # filter(game_date %>% month() == 12) %>% 
  mutate(player_name = player_name %>% fix_names() %>% stringi::stri_trans_general(id = "Latin-ASCII")) %>% 
  left_join(fantasy_teams, by = "player_name") %>%
  replace_na(list(team = "(Free Agent)"))

ui <- fluidPage(
  # tags$head(includeHTML("google-analytics.html")),
  tags$style(HTML('.table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
  background-color: #a91b0c;
    color: white;}')),
  
  theme = shinythemes::shinytheme("simplex"),
  tags$head(tags$style(HTML('* {font-family: "IBM Plex Mono"};'))),
  titlePanel("A Better* Player Rater"),
  wellPanel(
    fluidRow(
      
      column(2, textInput("player_search",
                          "Player Search:",
                          value = "")),
      column(
        2,
        pickerInput(
          "man",
          "NBA Team:",
          c("All", sort(unique(
            as.character(data$team_name)
          ))),
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = "All"
        )
      ),
      column(
        2,
        pickerInput(
          "cyl",
          "Fantasy Team:",
          c("All", sort(unique(
            as.character(data$team)
          ))),
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = "All"
        )
      ),
      column(
        1,
        pickerInput(
          "pos",
          "Position:",
          c("All", c("G", "F", "C")),
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = "All"
        )
      ),
      column(
        1,
        numericInput(
          inputId = "num",
          "Days Back:",
          value = 7,
          min = 1,
          max = as.numeric(max(data$game_date) - min(data$game_date) + 1)
        )
      ),
      column(
        3,
        radioGroupButtons(
          inputId = "totals",
          label = "Rank By:",
          choices = c("Totals",
                      "Per Game", 
                      "Per Minute"),
          selected = "Per Game",
          status = "primary",
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon"),
            no = icon("remove",
                      lib = "glyphicon")
          )
        )
      ),
      column(
        1,numericInput("filter_gp",
                       "Filter GP:", min = 0,
                       value = NA), offset = 4
      ),
      column(
        1,numericInput("filter_min",
                       "Filter Minutes:", min = 0,
                       value = NA)
      ),
      column(
        6,
        checkboxGroupButtons(
          inputId = "Id059",
          label = "Include in Total:",
          choices = c("fg", "ft", "fg3m", "reb", "ast",
                      "stl", "blk", "tov", "pts"),
          status = "primary",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")
          ),
          selected = c("fg", "ft", "fg3m", "reb", "ast",
                       "stl", "blk", "tov", "pts"),
          justified = F,
          individual = F)))),
  fluidRow(div(style = "font-family: 'IBM Plex Mono', monospace; color: black", 
               DT::dataTableOutput("table"))),
  fluidRow(htmlOutput("txt")))

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  output$txt <- renderUI({
    HTML("*Not yet better")
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    req(input$man)    
    req(input$cyl)
    req(input$pos)

    data <- data %>% filter(game_date > (max(game_date) - input$num))
    
    per_game_stats <- data %>% 
      group_by(player_name) %>% 
      summarize(across(fgm:pts, mean)) %>%
      mutate(fg = str_c((fgm/fga * 100) %>% round(1), "% (", fgm %>% round(1),"/", fga %>% round(1),")"),
             ft = str_c((ftm/fta * 100) %>% round(1), "% (", ftm %>% round(1),"/", fta %>% round(1),")")) %>% 
      select(player_name, fg, ft, fg3m, reb, ast, stl, blk, tov, pts) %>% 
      mutate(across(fg3m:pts, ~round(., 1)))
    
    if(input$totals == "Per Game"){
      data <- data %>%
        group_by(player_name) %>%
        mutate(gp = n(),
               team_name = toString(unique(team_name)),
               position = map_chr(Position, toString)) %>%
        group_by(player_name, team_name, team, gp, position) %>%
        summarise_at(
          .vars = c(
            "min",
            "fg3m",
            "reb",
            "ast",
            "stl",
            "blk",
            "tov",
            "pts",
            "fgm",
            "ftm",
            "fga",
            "fta"),
          .funs = mean
        ) %>%
        rename(
          "nba" = "team_name",
          "fantasy" = "team") %>%
        ungroup() %>% 
        zscore() %>% 
        rowwise() %>% 
        mutate(total = sum(!!!syms(input$Id059)))
  
    }
    
    if(input$totals == "Per Minute"){
      data <- data %>%
        mutate(min = if_else(min == 0, 1, min)) %>% 
        group_by(player_name) %>%
        mutate(gp = n(),
               team_name = toString(unique(team_name)),
               position = map_chr(Position, toString)) %>%
        group_by(player_name, team_name, team, gp, position) %>%
        summarise(
          min = sum(min),
          fg3m = sum(fg3m) / min,
          reb = sum(reb) / min,
          ast = sum(ast) / min,
          stl = sum(stl) / min,
          blk = sum(blk) / min,
          tov = sum(tov) / min,
          pts = sum(pts) / min,
          fga = sum(fga) / min,
          fgm = sum(fgm) / min,
          fta = sum(fta) / min,
          ftm = sum(ftm) / min
        ) %>%
        rename(
          "nba" = "team_name",
          "fantasy" = "team") %>%
        ungroup() %>% 
        zscore() %>% 
        rowwise() %>% 
        mutate(total = sum(!!!syms(input$Id059)))
      
    }
    if(input$totals == "Totals"){
      data <- data %>%
        group_by(player_name) %>%
        mutate(gp = n(),
               position = map_chr(Position, toString),
               team_name = toString(unique(team_name))) %>%
        group_by(player_name, team_name, team, gp, position) %>%
        summarise(
          min = sum(min),
          fg3m = sum(fg3m),
          reb = sum(reb),
          ast = sum(ast),
          stl = sum(stl),
          blk = sum(blk),
          tov = sum(tov),
          pts = sum(pts),
          fga = sum(fga),
          fgm = sum(fgm),
          fta = sum(fta),
          ftm = sum(ftm)
        ) %>%
        rename(
          "nba" = "team_name",
          "fantasy" = "team") %>%
        ungroup() %>% 
        zscore() %>%  
        rowwise() %>% 
        mutate(total = sum(!!!syms(input$Id059)))
    }
    
    if (!is.na(input$filter_min)) {
      data <- data %>% filter(min >= input$filter_min %>% as.numeric())
    }
    
    if (!is.na(input$filter_gp)) {
      data <- data %>% filter(gp >= input$filter_gp %>% as.numeric())
    }
    
    data_pres <- data %>%
      ungroup() %>% 
      mutate_if(is.numeric, list(~round(., 2))) %>%
      mutate(min = round(min, 1),
             rk = rank(-total, ties.method = "min")) %>% 
      select(rk, player_name, nba, fantasy, position, gp, min, fg, ft, everything())
    
    
    if (input$pos != "All" && length(input$pos) == 1) {
      data_pres <- data_pres %>% filter(str_detect(position, input$pos[[1]]))
    }
    if (input$pos != "All" && length(input$pos) == 2) {
      data_pres <-
        data_pres %>% filter(str_detect(position, input$pos[[1]]) | str_detect(position, input$pos[[2]]))
    }
    if (input$pos != "All" && length(input$pos) >= 3) {
      data_pres <- data_pres
    }
    if (input$player_search != "") {
      data_pres <- data_pres[str_detect(data_pres$player_name, regex(input$player_search, ignore_case = T)),]
    }
    if (input$man[1] != "All" & length(input$man) > 0) {
      data_pres <- data_pres[data_pres$nba %in% c(input$man),]
    }
    if (input$cyl[1] != "All" & length(input$cyl) > 0) {
      data_pres <- data_pres[data_pres$fantasy %in% c(input$cyl),]
    }
    
    data_pres %>% left_join(per_game_stats, by = "player_name", suffix = c("", "_per")) %>% 
      relocate(contains("_per"), .after = min)
    
  },
  rownames = F, 
  filter = "none", 
  style = "bootstrap",
  extensions = "FixedHeader",
  container = sketch,
  options = list(
    fixedHeader = T,
    pageLength = 100,
    bLengthChange = 0,
    bFilter = 0,
    order = list(list(25, 'desc')),
    columnDefs = list(list(
      orderSequence = c('desc', 'asc'),
      targets = "_all"
    ))
  )) %>% 
    DT::formatStyle(columns = c("fg", "ft", "reb", "ast", "stl", "blk", "tov", "pts", "fg3m"), 
                    backgroundColor = DT::styleInterval(c(-3.5:3.5), 
                                                        colorRampPalette(c("#a91b0c", "white", "forestgreen"))(9))) %>% 
    DT::formatStyle(c("player_name", "total"), fontWeight = "bold"))
}

shinyApp(ui, server)
