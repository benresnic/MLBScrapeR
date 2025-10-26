# R/client-singleton.R
#' @keywords internal
#' @noRd
.mlbscrape_env <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.get_client <- function() {
  cli <- .mlbscrape_env$client
  if (is.null(cli)) {
    cli <- MLB_Scrape$new()
    .mlbscrape_env$client <- cli
  }
  cli
}


# R/api.R

#' Get MLB teams
#'
#' @return Tibble with team metadata.
#' @examples
#' \dontrun{ get_teams() }
#' @export
get_teams <- function() {
  .get_client()$get_teams()
}

#' Get game types
#'
#' @return Data frame of MLB game types.
#' @export
get_game_types <- function() {
  .get_client()$get_game_types()
}

#' Get sport IDs
#'
#' @return Data frame of sport levels.
#' @export
get_sport_id <- function() {
  .get_client()$get_sport_id()
}

#' Get schedule
#'
#' @param season Integer vector of years.
#' @param sport_id Integer vector of sport IDs.
#' @param game_type Character vector of game type codes (e.g., "R").
#' @return Data frame of schedule rows.
#' @export
get_schedule <- function(season = 2025, sport_id = 1, game_type = "R") {
  .get_client()$get_schedule(season = season, sport_id = sport_id, game_type = game_type)
}

#' Fetch live game JSON
#'
#' @param ids_list Integer vector of gamePk IDs.
#' @return List of parsed JSON objects.
#' @export
get_data_json <- function(ids_list) {
  .get_client()$get_data_json(ids_list)
}

#' Parse play-by-play
#'
#' @param data_list List returned by `get_data_json()`.
#' @return Tibble of pitch-by-pitch rows.
#' @export
get_pbp_data <- function(data_list) {
  .get_client()$get_pbp_data(data_list)
}

#' Get players
#'
#' @param sport_id Integer vector.
#' @param season Integer vector.
#' @return Tibble of player info.
#' @export
get_players <- function(sport_id = 1, season = 2025) {
  .get_client()$get_players(sport_id = sport_id, season = season)
}

#' Get a player's game IDs
#'
#' @param player_id Integer MLBAM ID.
#' @param season Integer vector of years.
#' @param start_date,end_date Optional YYYY-MM-DD strings.
#' @param game_type Character vector (e.g., "R").
#' @param sport_id Integer.
#' @return Integer vector of gamePk IDs.
#' @export
get_player_games_list <- function(player_id,
                                  season = 2025,
                                  start_date = NULL,
                                  end_date = NULL,
                                  game_type = "R",
                                  sport_id = 1) {
  .get_client()$get_player_games_list(
    player_id = player_id,
    season = season,
    start_date = start_date,
    end_date = end_date,
    game_type = game_type,
    sport_id = sport_id
  )
}

#' Get pbp data for a season
#' @export
get_pbp_season <- function(season = c(2025),
                           start_date = NULL,
                           end_date = NULL,
                           sport_id = c(1),
                           game_type = c("R")) {
  .get_client()$get_pbp_season(
    season = season, start_date = start_date, end_date = end_date,
    sport_id = sport_id, game_type = game_type
  )
}

#' Get pbp data for a player
#' @export
get_pbp_player <- function(player_id,
                           season = c(2025),
                           start_date = NULL,
                           end_date = NULL,
                           game_type = c("R"),
                           sport_id = c(1)) {
  .get_client()$get_pbp_player(
    player_id = player_id, season = season,
    start_date = start_date, end_date = end_date,
    game_type = game_type, sport_id = sport_id
  )
}

