## R package for scraping live data from the MLB Stats API
Shoutout @TJStats on twitter for the inspiration and structure
Use his Python version here https://github.com/tnestico/mlb_scraper 

## Packages
- tidyverse
- R6
- httr
- httr2
- jsonlite
- data.table
- progress
- janitor
- magrittr
- rvest
- lubridate

## Functions

#### `get_sport_id()`
- **Returns**: Dataframe with information about different levels of baseball

#### `get_game_types()`
- **Returns**: Dataframe with information about different types of games and their corresponding code

#### `get_schedule(season = c(2025), sport_id = c(1), game_type = c('R'))`
- **Inputs**:
  - `season` (int): A list of seasons to get the schedule from. Default: 2025
  - `sport_id` (int): A list of sport ids to get the schedule from. Default: 1
  - `game_type` (chr): A list of game types to get the schedule from. Default: "R"
- **Returns**: A dataframe with schedule information

#### `get_data_json(ids_list)`
- **Inputs**:
  - `ids_list` (chr): A list of game ids.
- **Returns**: A list of JSON files with live data for the given game id(s)

#### `get_pbp_data(data_list)`
- **Inputs**:
  - `data_list` (list): A list of JSON objects from the get_data_json() function
- **Returns**: A dataframe with pbp data from the MLB API for the given game id(s)

#### `get_players(sport_id = c(1), season = c(2025))`
- **Inputs**:
  - `sport_id` (int): A list of sport ids to get the schedule from. Default: 1
  - `season` (int): A list of seasons to get the schedule from. Default: 2025
- **Returns**: Dataframe of player information for the given sport id(s) and season(s)

#### `get_player_games_list(player_id, season = c(2025), start_date = NULL, end_date = NULL, game_type = c('R'), sport_id = c(1))`
- **Inputs**:
  - `player_id` (int): 6 digit player id
  - `season` (int): A list of seasons to get the schedule from. Default: 2025
  - `start_date` (chr): First date to get list from. yyyy-mm-dd format. Default: NULL
  - `end_date` (chr): Last date to get list from. yyyy-mm-dd format. Default: NULL
  - `game_type` (chr): A list of game types to get the schedule from. Default: "R"
  - `sport_id` (int): A list of sport ids to get the schedule from. Default: 1
- **Returns**: List of game ids for the given player

#### `get_pbp_season(season = c(2025), start_date = NULL, end_date = NULL, sport_id = c(1), game_type = c('R'))`
- **Inputs**:
  - `season` (int): A list of seasons to pull play-by-play from. Default: 2025
  - `start_date` (chr): First date to include. yyyy-mm-dd format. Default: NULL
  - `end_date` (chr): Last date to include. yyyy-mm-dd format. Default: NULL
  - `sport_id` (int): A list of sport ids to include. Default: 1
  - `game_type` (chr): A list of game types to include. Default: "R"
- **Returns**: Dataframe with play-by-play for all matching games (filters schedule to states "F", "D", "I")

#### `get_pbp_player(player_id, season = c(2025), start_date = NULL, end_date = NULL, game_type = c('R'), sport_id = c(1))`
- **Inputs**:
  - `player_id` (int): 6 digit MLBAM player id
  - `season` (int): A list of seasons to search for the player's games. Default: 2025
  - `start_date` (chr): First date to include. yyyy-mm-dd format. Default: NULL
  - `end_date` (chr): Last date to include. yyyy-mm-dd format. Default: NULL
  - `game_type` (chr): A list of game types to include. Default: "R"
  - `sport_id` (int): A list of sport ids to include. Default: 1
- **Returns**: Dataframe with play-by-play from games involving the specified player

#### `get_teams()`
- **Returns**: A dataframe with team information

#### `get_run_expectancy(df, matrix_type = c(24, 288))`
- **Inputs**:
  - `df` (dataframe): Play-by-play dataframe from get_pbp_data()
  - `matrix_type` (int): Either 24 (base-out states only) or 288 (base-out-count states). Default: 24
- **Returns**: Dataframe with run expectancy values for each state

#### `get_guts(seasons = NULL)`
- **Inputs**:
  - `seasons` (int): A list of seasons to retrieve Guts constants for. Default: NULL (returns all available seasons)
- **Returns**: Dataframe scraped from FanGraphs Guts page containing wOBA weights and other constants

#### `apply_wOBA(df)`
- **Inputs**:
  - `df` (dataframe): Play-by-play dataframe from get_pbp_data()
- **Returns**: Original dataframe with added `wOBA` column containing weighted on-base average value for each play (0 for outs, NA for non-AB pitches)

#### `apply_re288(df)`
- **Inputs**:
  - `df` (dataframe): Play-by-play dataframe from get_pbp_data()
- **Returns**: Original dataframe with added `run_expectancy` and `delta_run_exp` columns. `delta_run_exp` represents the change in run expectancy from one pitch to the next, accounting for runs scored

## Usage Examples
```r
devtools::install_github("benresnic/MLBScrapeR", force = TRUE) #Run once

library(MLBScrapeR) #Load in the library

#PBP data for Mookie Betts in the World Seried for 2024 & 2025
Mookie_data <- MLBScrapeR::get_pbp_player(player_id = 605141, #Mookie Betts
                                   season = 2024:2025, #2024 & 2025
                                   start_date = NULL, 
                                   end_date = NULL,
                                   game_type = "W", #World Series
                                   sport_id = 1) #Sport ID 1: MLB
          
#PBP data for the 2025 Regular Season
data <- MLBScrapeR::get_pbp_season(season = 2025, #2025 season
                                   start_date = NULL,
                                   end_date = NULL,
                                   sport_id = c(1),# Sport ID 1: MLB
                                   game_type = c('R')) #Regular Season

#Get RE24 Matrix
re24 <- MLBScrapeR::get_run_expectancy(data, 24)

#Get RE288 Matrix
re288 <- MLBScrapeR::get_run_expectancy(data, 288)

#Get GUTS table from Fangraphs for 2020 through 2025
guts <- MLBScrapeR::get_guts(2020:2025)

#Apply run values to the data
data <- MLBScrapeR::apply_re288(data, full_season_data)

#Apply wOBA values to the data
data <- MLBScrapeR::apply_wOBA(data)


```
