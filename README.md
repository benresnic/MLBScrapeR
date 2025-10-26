

## R package for scraping live data from the MLB Stats API

Shoutout @TJStats on twitter for the inspiration and structure

Use his Python version here https://github.com/tnestico/mlb_scraper 

## Packages
- tidyverse
- R6
- httr
- jsonlite
- data.table
- progress
- janitor
- magrittr


## Functions

#### `get_sport_id()`
- **Returns**: Dataframe with information about different levels of baseball

#### `get_game_types()`
- **Returns**: Dataframe with information about different types of games and their corresponding code

#### `get_schedule(season = c(2025), sport_id = c(1), game_type = c('R'))`
- **Inputs**:
  - `season` (int): A list of seasons to get the schedule from. Default: 2025
  - `sport_id` (int):  A list of sport ids to get the schedule from. Default: 1
  - `game_type` (chr):  A list of game types to get the schedule from. Default: "R"
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
  - `sport_id` (int):  A list of sport ids to get the schedule from. Default: 1
  - `season` (int):  A list of seasons to get the schedule from. Default: 2025
- **Returns**: Dataframe of player information for the given sport id(s) and season(s)

#### `get_player_games_list(player_id, season = c(2025), start_date = NULL, end_date = NULL, game_type = c('R'), sport_id = c(1))`
- **Inputs**:
  - `player_id` (int): 6 digit player id
  - `season` (int):  A list of seasons to get the schedule from. Default: 2025
  - `start_date` (chr):  First date to get list from. yyyy-mm-dd format. Default: NULL
  - `end_date` (chr):  Last date to get list from. yyyy-mm-dd format. Default: NULL
  - `game_type` (chr):  A list of game types to get the schedule from. Default: "R"
  - `sport_id` (int):  A list of sport ids to get the schedule from. Default: 1
- **Returns**: List of game ids for the given player

#### `get_pbp_season(season = c(2025), start_date = NULL, end_date = NULL, sport_id = c(1), game_type = c('R'))`
- **Inputs**:
  - `season` (int): A list of seasons to pull play-by-play from. Default: 2025
  - `start_date` (chr): First date to include. yyyy-mm-dd format. Default: NULL
  - `end_date` (chr): Last date to include. yyyy-mm-dd format. Default: NULL
  - `sport_id` (int): A list of sport ids to include. Default: 1
  - `game_type` (chr): A list of game types to include. Default: "R"
- **Returns**: Dataframe with play-by-play for all matching games (filters schedule to states "F", "D", "I"; returns empty dataframe if no games)

#### `get_pbp_player(player_id, season = c(2025), start_date = NULL, end_date = NULL, game_type = c('R'), sport_id = c(1), pitching = FALSE)`
- **Inputs**:
  - `player_id` (int): 6 digit MLBAM player id
  - `season` (int): A list of seasons to search for the player's games. Default: 2025
  - `start_date` (chr): First date to include. yyyy-mm-dd format. Default: NULL
  - `end_date` (chr): Last date to include. yyyy-mm-dd format. Default: NULL
  - `game_type` (chr): A list of game types to include. Default: "R"
  - `sport_id` (int): A list of sport ids to include. Default: 1
  - `pitching` (lgl): If TRUE, returns games the player pitched in; otherwise games the player appeared in. Default: FALSE
- **Returns**: Dataframe with play-by-play from games involving the specified player that match the filters


#### `get_teams()`
- **Returns**: A dataframe with team information
