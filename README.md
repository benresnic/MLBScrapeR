

R package for scraping live data from the MLB Stats API

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

#### `get_teams()`
- **Returns**: A dataframe with team information
