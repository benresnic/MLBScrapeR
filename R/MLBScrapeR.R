library(R6)
library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(data.table)
library(progress)
library(janitor)

MLB_Scrape <- R6::R6Class(
  "MLB_Scrape",

  public = list(

    initialize = function(self) {
    },

    get_sport_id = function() {
      response <- GET("https://statsapi.mlb.com/api/v1/sports")
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))

      as.data.frame(data$sports)
    },

    get_game_types = function() {
      response <- GET("https://statsapi.mlb.com/api/v1/gameTypes")
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))

      as.data.frame(data)
    },

    get_schedule = function(season = c(2025), sport_id = c(1), game_type = c('R')) {
      if (!is.vector(season) || !all(sapply(season, is.numeric))) {
        stop("season must be a vector of integers.")
      }
      if (!is.vector(sport_id) || !all(sapply(sport_id, is.numeric))) {
        stop("sport_id must be a vector of integers.")
      }
      if (!is.vector(game_type) || !all(sapply(game_type, is.character))) {
        stop("game_type must be a vector of strings.")
      }

      year_input_str <- paste(season, collapse = ",")
      sport_id_str <- paste(sport_id, collapse = ",")
      game_type_str <- paste(game_type, collapse = ",")

      game_call <- GET(paste0("https://statsapi.mlb.com/api/v1/schedule/?sportId=", sport_id_str,
                              "&gameTypes=", game_type_str, "&season=", year_input_str,
                              "&hydrate=lineup,players"))

      game_call_data <- content(game_call, "text", encoding = "UTF-8")

      game_call_data_parsed <- fromJSON(game_call_data)


      game_list <-  unlist(list(bind_rows(game_call_data_parsed$dates$games)$gamePk))
      time_list <- unlist(list(bind_rows(game_call_data_parsed$dates$games)$gameDate))
      date_list <-  unlist(list(bind_rows(game_call_data_parsed$dates$games)$officialDate))
      away_team_list <- unlist(list(bind_rows(game_call_data_parsed$dates$games)$teams$away$team$name))
      home_team_list <-  unlist(list(bind_rows(game_call_data_parsed$dates$games)$teams$home$team$name))
      state_list <- unlist(list(bind_rows(game_call_data_parsed$dates$games)$status$codedGameState))
      venue_id <-  unlist(list(bind_rows(game_call_data_parsed$dates$games)$venue$id))
      venue_name <- unlist(list(bind_rows(game_call_data_parsed$dates$games)$venue$name))


      game_df <- data.frame(game_id = game_list,
                            time = time_list,
                            date = date_list,
                            away = away_team_list,
                            home = home_team_list,
                            state = state_list,
                            venue_id = venue_id,
                            venue_name = venue_name)

      if (nrow(game_df) == 0) {
        return("Schedule Length of 0, please select different parameters.")
      }

      game_df$date <- as.Date(game_df$date)
      game_df$time <- format(ymd_hms(game_df$time), "%I:%M %p")

      game_df %>% distinct(game_id, .keep_all = TRUE) %>% arrange(date)


    },

    get_data_json = function(ids_list) {
      data_total <- vector("list", length(ids_list))

      pb <- progress_bar$new(
        format = "Retrieving Data [:bar] :percent (:current/:total)",
        total = length(ids_list),
        clear = FALSE,
        width = 60
      )

      for (i in seq_along(ids_list)) {
        url <- paste0("https://statsapi.mlb.com/api/v1.1/game/", ids_list[i], "/feed/live")
        response <- GET(url)

        if (status_code(response) == 200) {
          data_total[[i]] <- fromJSON(content(response, "text"), flatten = TRUE)
        } else {
          data_total[[i]] <- list(error = paste("Failed to fetch data for game", ids_list[i]))
        }

        pb$tick()
      }

      data_total
    },
    get_pbp_data = function(data_list) {

      pb <- progress_bar$new(
        format = "Binding Data [:bar] :percent (:current/:total)",
        total = length(data_list),
        clear = FALSE,
        width = 60
      )

      `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


      .gv <- function(row, flat, nested) {
        if (length(flat) == 1 && flat %in% names(row)) return(row[[flat]])
        v <- if (flat %in% names(row)) row[[flat]] else NA
        if (!all(is.na(v))) return(v)
        if (!length(nested)) return(NA)
        x <- row
        for (nm in nested) {
          if (!nm %in% names(x)) return(NA)
          x <- x[[nm]]
        }
        x %||% NA
      }


      swing_codes <- c("X","F","S","D","E","T","W")
      whiff_codes <- c("S","T","W")
      csw_codes <- c("S","T","W", "C")

      out <- vector("list", 0L)

      for (g in data_list) {
        if (!is.list(g) || !is.null(g$error)) next

        at_bats <- g$liveData$plays$allPlays
        if (is.null(at_bats) || NROW(at_bats) == 0L) next

        home <- g$gameData$teams$home
        away <- g$gameData$teams$away

        home_team         <- home$name %||% home$abbreviation %||% NA
        home_level_id     <- (home$sport$id %||% home$league$id) %||% NA_integer_
        home_level_name   <- (home$sport$name %||% home$league$name) %||% NA_character_
        home_parentorg_id <- (home$parentOrgId %||% home$parentOrg$id) %||% NA_integer_
        home_parentorg_nm <- (home$parentOrgName %||% home$parentOrg$name) %||% NA_character_
        home_league_id    <- home$league$id %||% NA_integer_
        home_league_name  <- home$league$name %||% NA_character_

        away_team         <- away$name %||% away$abbreviation %||% NA
        away_level_id     <- (away$sport$id %||% away$league$id) %||% NA_integer_
        away_level_name   <- (away$sport$name %||% away$league$name) %||% NA_character_
        away_parentorg_id <- (away$parentOrgId %||% away$parentOrg$id) %||% NA_integer_
        away_parentorg_nm <- (away$parentOrgName %||% away$parentOrg$name) %||% NA_character_
        away_league_id    <- away$league$id %||% NA_integer_
        away_league_name  <- away$league$name %||% NA_character_

        for (i in seq_len(NROW(at_bats))) {
          ab <- at_bats[i, , drop = FALSE]

          pidx0 <- ab$pitchIndex[[1]]
          if (is.null(pidx0) || !length(pidx0)) next

          evs <- ab$playEvents[[1]]
          if (is.null(evs) || NROW(evs) == 0L) next

          pidx <- as.integer(pidx0) + 1L
          pidx <- pidx[pidx >= 1L & pidx <= NROW(evs)]
          if (!length(pidx)) next

          is_top <- if ("about.isTopInning" %in% names(ab)) ab$`about.isTopInning` else ab$about[[1]]$isTopInning
          batting_team  <- if (isTRUE(is_top)) away$abbreviation %||% NA else home$abbreviation %||% NA
          fielding_team <- if (isTRUE(is_top)) home$abbreviation %||% NA else away$abbreviation %||% NA

          for (j in seq_along(pidx)) {
            k <- pidx[j]
            row_e <- evs[k, , drop = FALSE]
            last_pitch <- j == length(pidx)

            sA <- .gv(row_e, "count.strikes", c("count","strikes"))
            bA <- .gv(row_e, "count.balls",   c("count","balls"))
            oA <- .gv(row_e, "count.outs",    c("count","outs"))

            if (k > 1L) {
              prev <- evs[k-1L, , drop = FALSE]
              sB <- .gv(prev, "count.strikes", c("count","strikes"))
              bB <- .gv(prev, "count.balls",   c("count","balls"))
              oB <- .gv(prev, "count.outs",    c("count","outs"))
            } else {
              sB <- 0L; bB <- 0L; oB <- oA
            }

            code <- .gv(row_e, "details.code", c("details","code"))

            is_pitch_val <- isTRUE(.gv(row_e, "isPitch", "isPitch"))
            code_val <- if (is.null(code) || length(code) == 0) NA_character_ else as.character(code)

            is_swing <- if (is_pitch_val) {
              if (!is.na(code_val)) as.integer(code_val %in% swing_codes) else NA_integer_
            } else {
              NA_integer_
            }

            is_csw <- if (is_pitch_val) {
              if (!is.na(code_val)) as.integer(code_val %in% csw_codes) else NA_integer_
            } else {
              NA_integer_
            }

            is_whiff <- if (is_pitch_val && !is.na(code_val) && (code_val %in% swing_codes)) {
              as.integer(code_val %in% whiff_codes)
            } else {
              NA_integer_
            }

            pitch_zone <- suppressWarnings(
              as.integer(.gv(row_e, "pitchData.zone", c("pitchData","zone")))
            )

            in_zone <- if (is_pitch_val && !is.na(pitch_zone)) {
              if (pitch_zone < 10L) 1L else 0L
            } else {
              NA_integer_
            }

            is_chase <- if (is_pitch_val && is_swing == 1L) {
              if (is.na(in_zone)) {
                NA_integer_
              } else if (in_zone == 0L) {
                1L
              } else {
                0L
              }
            } else {
              NA_integer_
            }

            in_zone_whiff <- if (is_pitch_val && is_swing == 1L && !is.na(in_zone) && in_zone == 1L) {
              if (isTRUE(is_whiff)) 1L else 0L
            } else {
              NA_integer_
            }
            in_play_flag <- isTRUE(.gv(row_e, "details.isInPlay", c("details","isInPlay")))
            ev  <- suppressWarnings(as.numeric(.gv(row_e, "hitData.launchSpeed", c("hitData","launchSpeed"))))
            ang <- suppressWarnings(as.numeric(.gv(row_e, "hitData.launchAngle",  c("hitData","launchAngle"))))

            in_play <- if (is_pitch_val){
              as.integer(in_play_flag)
            } else {
              NA_integer_
            }

            is_barrel <- if (in_play_flag) {
              if (!is.na(ev) && !is.na(ang)) {
                as.integer((ev * 1.5 - ang) >= 117 &&
                             (ev + ang) >= 124 &&
                             ang <= 50 &&
                             ev >= 98)
              } else {
                NA_integer_
              }
            } else {
              NA_integer_
            }

            is_hard_hit <- if (in_play_flag){
              if (!is.na(ev) && !is.na(ang)) {
                as.integer(ev >= 95)
              } else {
                NA_integer_
              }
            } else {
              NA_integer_
            }

            if (i > 1L) {
              prev_ab <- at_bats[i - 1L, , drop = FALSE]

              pre_away_score_val <- suppressWarnings(as.integer(
                if ("result.awayScore" %in% names(prev_ab)) {
                  prev_ab$`result.awayScore`
                } else if (!is.null(prev_ab$result) && length(prev_ab$result) >= 1 &&
                           "awayScore" %in% names(prev_ab$result[[1]])) {
                  prev_ab$result[[1]]$awayScore
                } else NA
              ))

              pre_home_score_val <- suppressWarnings(as.integer(
                if ("result.homeScore" %in% names(prev_ab)) {
                  prev_ab$`result.homeScore`
                } else if (!is.null(prev_ab$result) && length(prev_ab$result) >= 1 &&
                           "homeScore" %in% names(prev_ab$result[[1]])) {
                  prev_ab$result[[1]]$homeScore
                } else NA
              ))
            } else {
              pre_away_score_val <- 0L
              pre_home_score_val <- 0L
            }

            pre1_id <- pre1_nm <- pre2_id <- pre2_nm <- pre3_id <- pre3_nm <- NA
            rf <- if ("runners" %in% names(ab)) ab$runners[[1]] else NULL

            if (is.data.frame(rf) && nrow(rf) > 0) {
              origin <- if ("movement.originBase" %in% names(rf)) rf$`movement.originBase`
              else if ("movement.start" %in% names(rf)) rf$`movement.start`
              else rep(NA_character_, nrow(rf))

              ids <- if ("details.runner.id" %in% names(rf)) rf$`details.runner.id` else NA_integer_
              nms <- if ("details.runner.fullName" %in% names(rf)) rf$`details.runner.fullName` else NA_character_

              pre_df <- tibble(origin = origin, id = ids, name = nms) %>%
                filter(!is.na(origin) & origin %in% c("1B","2B","3B")) %>%
                distinct(origin, id, .keep_all = TRUE) %>%
                dplyr::group_by(origin) %>% dplyr::slice_head(n = 1) %>% dplyr::ungroup()

              if ("1B" %in% pre_df$origin) {
                pre1_id <- pre_df$id  [pre_df$origin == "1B"][1]; pre1_nm <- pre_df$name[pre_df$origin == "1B"][1]
              }
              if ("2B" %in% pre_df$origin) {
                pre2_id <- pre_df$id  [pre_df$origin == "2B"][1]; pre2_nm <- pre_df$name[pre_df$origin == "2B"][1]
              }
              if ("3B" %in% pre_df$origin) {
                pre3_id <- pre_df$id  [pre_df$origin == "3B"][1]; pre3_nm <- pre_df$name[pre_df$origin == "3B"][1]
              }
            }


            r <- list(
              game_id    = g$gamePk %||% NA,
              game_date  = g$gameData$datetime$officialDate %||% NA,
              play_id    = .gv(row_e, "playId", "playId"),
              is_pitch    = .gv(row_e, "isPitch", "isPitch"),
              away_score = pre_away_score_val,
              home_score = pre_home_score_val,
              top_bottom = if ("about.halfInning" %in% names(ab)) ab$`about.halfInning` else ab$about[[1]]$halfInning,
              inning    = if ("about.inning" %in% names(ab)) ab$`about.inning` else ab$about[[1]]$inning,
              ab_number    = ab$atBatIndex %||% NA_integer_,
              pitch_number = j,
              batter_id  = if ("matchup.batter.id" %in% names(ab)) ab$`matchup.batter.id` else ab$matchup[[1]]$batter$id,
              batter_name = if ("matchup.batter.fullName" %in% names(ab)) ab$`matchup.batter.fullName` else ab$matchup[[1]]$batter$fullName,
              batter_side    = if ("matchup.batSide.code" %in% names(ab)) ab$`matchup.batSide.code` else ab$matchup[[1]]$batSide$code,
              pitcher_id       = if ("matchup.pitcher.id" %in% names(ab)) ab$`matchup.pitcher.id` else ab$matchup[[1]]$pitcher$id,
              pitcher_name = if ("matchup.pitcher.fullName" %in% names(ab)) ab$`matchup.pitcher.fullName` else ab$matchup[[1]]$pitcher$fullName,
              pitcher_side   = if ("matchup.pitchHand.code" %in% names(ab)) ab$`matchup.pitchHand.code` else ab$matchup[[1]]$pitchHand$code,
              batter_split    = if ("matchup.splits.batter" %in% names(ab)) ab$`matchup.splits.batter` else ab$matchup[[1]]$splits$batter %||% NA,
              pitcher_split  = if ("matchup.splits.pitcher" %in% names(ab)) ab$`matchup.splits.pitcher` else ab$matchup[[1]]$splits$pitcher %||% NA,
              runners_on_base = if ("matchup.splits.menOnBase" %in% names(ab)) ab$`matchup.splits.menOnBase` else ab$matchup[[1]]$splits$menOnBase %||% NA,
              balls   = as.integer(bB),
              strikes = as.integer(sB),
              outs  = as.integer(oB),
              is_strike      = .gv(row_e, "details.isStrike",    c("details","isStrike")),
              is_ball     = .gv(row_e, "details.isBall",      c("details","isBall")),
              last_pitch_of_ab = if (last_pitch) 1L else 0L,
              play_type       = .gv(row_e, "type", "type"),
              description    = .gv(row_e, "details.description", c("details","description")),
              pitch_call_code           = code,
              pitch_call = .gv(row_e, "details.call.description", c("details","call","description")),
              pitch_type   = .gv(row_e, "details.type.code",        c("details","type","code")),
              pitch_name= .gv(row_e, "details.type.description", c("details","type","description")),
              is_swing = is_swing,
              is_whiff = is_whiff,
              in_zone = in_zone,
              is_chase = is_chase,
              in_zone_whiff = in_zone_whiff,
              is_barrel = is_barrel,
              is_hard_hit = is_hard_hit,
              in_play = in_play,
              sz_top    = .gv(row_e, "pitchData.strikeZoneTop",    c("pitchData","strikeZoneTop")) * 12,
              sz_bot = .gv(row_e, "pitchData.strikeZoneBottom", c("pitchData","strikeZoneBottom")) * 12,
              velocity      = .gv(row_e, "pitchData.startSpeed",       c("pitchData","startSpeed")),
              end_velocity     = .gv(row_e, "pitchData.endSpeed",         c("pitchData","endSpeed")),
              plate_time      = .gv(row_e, "pitchData.plateTime",        c("pitchData","plateTime")),
              vb  = .gv(row_e, "pitchData.breaks.breakVertical",           c("pitchData","breaks","breakVertical")),
              ivb = .gv(row_e, "pitchData.breaks.breakVerticalInduced",    c("pitchData","breaks","breakVerticalInduced")),
              hb  = .gv(row_e, "pitchData.breaks.breakHorizontal",         c("pitchData","breaks","breakHorizontal")),
              break_angle    = .gv(row_e, "pitchData.breaks.breakAngle",    c("pitchData","breaks","breakAngle")),
              break_length   = .gv(row_e, "pitchData.breaks.breakLength",   c("pitchData","breaks","breakLength")),
              spin_rate     = .gv(row_e, "pitchData.breaks.spinRate",      c("pitchData","breaks","spinRate")),
              spin_direction = .gv(row_e, "pitchData.breaks.spinDirection", c("pitchData","breaks","spinDirection")),
              zone          = .gv(row_e, "pitchData.zone",             c("pitchData","zone")),
              extension    = .gv(row_e, "pitchData.extension",        c("pitchData","extension")),
              aX = .gv(row_e, "pitchData.coordinates.aX", c("pitchData","coordinates","aX")),
              aY = .gv(row_e, "pitchData.coordinates.aY", c("pitchData","coordinates","aY")),
              aZ = .gv(row_e, "pitchData.coordinates.aZ", c("pitchData","coordinates","aZ")),
              plate_x= .gv(row_e, "pitchData.coordinates.pX",  c("pitchData","coordinates","pX")) * 12,
              plate_z = .gv(row_e, "pitchData.coordinates.pZ",  c("pitchData","coordinates","pZ")) * 12,
              vx0= .gv(row_e, "pitchData.coordinates.vX0", c("pitchData","coordinates","vX0")),
              vy0 = .gv(row_e, "pitchData.coordinates.vY0", c("pitchData","coordinates","vY0")),
              vz0 = .gv(row_e, "pitchData.coordinates.vZ0", c("pitchData","coordinates","vZ0")),
              release_x = .gv(row_e, "pitchData.coordinates.x0",  c("pitchData","coordinates","x0")),
              release_y = .gv(row_e, "pitchData.coordinates.y0",  c("pitchData","coordinates","y0")),
              release_z = .gv(row_e, "pitchData.coordinates.z0",  c("pitchData","coordinates","z0")),
              exit_velocity    = .gv(row_e, "hitData.launchSpeed",   c("hitData","launchSpeed")),
              launch_angle    = .gv(row_e, "hitData.launchAngle",   c("hitData","launchAngle")),
              hit_distance  = .gv(row_e, "hitData.totalDistance", c("hitData","totalDistance")),
              hit_trakectory    = .gv(row_e, "hitData.trajectory",    c("hitData","trajectory")),
              hit_hardness      = .gv(row_e, "hitData.hardness",      c("hitData","hardness")),
              hit_location      = .gv(row_e, "hitData.location",      c("hitData","location")),
              hit_coordinate_x = .gv(row_e, "hitData.coordinates.coordX", c("hitData","coordinates","coordX")),
              hit_coordinate_z = .gv(row_e, "hitData.coordinates.coordY", c("hitData","coordinates","coordY")),
              event = if (last_pitch) (if ("result.event" %in% names(ab)) ab$`result.event`
                                       else ab$result[[1]]$event) else NA,
              event_type = if (last_pitch) (if ("result.eventType" %in% names(ab)) ab$`result.eventType`
                                            else ab$result[[1]]$eventType) else NA,
              play_description = if (last_pitch) (if ("result.description" %in% names(ab)) ab$`result.description`
                                                  else ab$result[[1]]$description) else NA,
              rbi = if (last_pitch) (if ("result.rbi" %in% names(ab)) ab$`result.rbi` else ab$result[[1]]$rbi) else NA,
              scoring_play = if ("about.isScoringPlay" %in% names(ab)) ab$`about.isScoringPlay`else
                ab$about[[1]]$isScoringPlay,
              pre_runner_1b_id   = pre1_id,
              pre_runner_1b_name = pre1_nm,
              pre_runner_2b_id   = pre2_id,
              pre_runner_2b_name = pre2_nm,
              pre_runner_3b_id   = pre3_id,
              pre_runner_3b_name = pre3_nm,
              postrunner_1b_id  = if ("matchup.postOnFirst.id" %in% names(ab)) ab$`matchup.postOnFirst.id`
              else ab$matchup[[1]]$postOnFirst$id %||% NA,
              post_runner_1b_name = if ("matchup.postOnFirst.fullName" %in%
                                        names(ab)) ab$`matchup.postOnFirst.fullName` else
                                          ab$matchup[[1]]$postOnFirst$fullName %||% NA,
              post_runner_2b_id  = if ("matchup.postOnSecond.id" %in%
                                       names(ab)) ab$`matchup.postOnSecond.id` else
                                         ab$matchup[[1]]$postOnSecond$id %||% NA,
              post_runner_2b_name = if ("matchup.postOnSecond.fullName" %in%
                                        names(ab)) ab$`matchup.postOnSecond.fullName` else
                                          ab$matchup[[1]]$postOnSecond$fullName %||% NA,
              post_runner_3b_id   = if ("matchup.postOnThird.id" %in%
                                        names(ab)) ab$`matchup.postOnThird.id` else
                                          ab$matchup[[1]]$postOnThird$id %||% NA,
              post_runner_3b_name = if ("matchup.postOnThird.fullName" %in%
                                        names(ab)) ab$`matchup.postOnThird.fullName` else
                                          ab$matchup[[1]]$postOnThird$fullName %||% NA,
              home_team          = home_team,
              home_level_id      = home_level_id,
              home_level_name    = home_level_name,
              home_parentOrg_id  = home_parentorg_id,
              home_parentOrg_name= home_parentorg_nm,
              home_league_id     = home_league_id,
              home_league_name   = home_league_name,
              away_team          = away_team,
              away_level_id      = away_level_id,
              away_level_name    = away_level_name,
              away_parentOrg_id  = away_parentorg_id,
              away_parentOrg_name= away_parentorg_nm,
              away_league_id     = away_league_id,
              away_league_name   = away_league_name,
              batting_team  = batting_team,
              fielding_team = fielding_team
            )

            out[[length(out) + 1L]] <- r
          }
        }

        pb$tick()
      }

      if (!length(out)) tibble()

      data.table::rbindlist(out, use.names = TRUE, fill = TRUE) %>% as_tibble()

    },
    get_players = function(sport_id = c(1), season = c(2025)) {

      `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

      sport_ids <- as.integer(unlist(sport_id, use.names = FALSE))
      seasons   <- as.integer(unlist(season,   use.names = FALSE))
      combos <- expand.grid(sport_id = sport_ids, Season = seasons, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

      rows <- vector("list", nrow(combos))

      for (i in seq_len(nrow(combos))) {
        sp <- combos$sport_id[i]
        se <- combos$Season[i]

        url <- sprintf("https://statsapi.mlb.com/api/v1/sports/%s/players?season=%s", sp, se)
        resp <- tryCatch(httr::GET(url), error = function(e) NULL)
        parsed <- tryCatch(httr::content(resp, "parsed", encoding = "UTF-8"), error = function(e) NULL)

        people <- if (!is.null(parsed) && !is.null(parsed$people)) parsed$people else list()
        if (length(people) == 0) {
          rows[[i]] <- tibble::tibble(
            sport_id = integer(0), Season = integer(0), player_id = integer(0),
            first_name = character(0), last_name = character(0), name = character(0),
            position = character(0), team = integer(0), weight = integer(0),
            height = character(0), age = integer(0), birthDate = character(0)
          )
          next
        }

        n <- length(people)
        rows[[i]] <- tibble::tibble(
          sport_id   = rep(sp, n),
          Season     = rep(se, n),
          player_id  = vapply(people, function(p) p$id %||% NA_integer_, integer(1)),
          first_name = vapply(people, function(p) p$firstName %||% NA_character_, character(1)),
          last_name  = vapply(people, function(p) p$lastName %||% NA_character_, character(1)),
          name       = vapply(people, function(p) p$fullName %||% NA_character_, character(1)),
          position   = vapply(people, function(p) if (!is.null(p$primaryPosition)) p$primaryPosition$abbreviation %||% NA_character_ else NA_character_, character(1)),
          team       = vapply(people, function(p) if (!is.null(p$currentTeam))     p$currentTeam$id %||% NA_integer_            else NA_integer_, integer(1)),
          weight     = vapply(people, function(p) p$weight %||% NA_integer_, integer(1)),
          height     = vapply(people, function(p) p$height %||% NA_character_, character(1)),
          age        = vapply(people, function(p) p$currentAge %||% NA_integer_, integer(1)),
          birthDate  = vapply(people, function(p) p$birthDate %||% NA_character_, character(1))
        )
      }

      bind_rows(rows)  %>%
        distinct(sport_id, Season, player_id, .keep_all = TRUE)
    },
    get_player_games_list = function(player_id,
                                     season = c(2025),
                                     start_date = NULL,
                                     end_date   = NULL,
                                     game_type  = c("R"),
                                     sport_id   = 1) {

      `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
      .is_date <- function(x) grepl("^\\d{4}-\\d{2}-\\d{2}$", x)

      seasons <- as.character(unlist(season, use.names = FALSE))
      if (!length(seasons)) stop("`season` must be a year or a vector/list of years.")

      if (!is.null(start_date) && !.is_date(start_date)) {
        stop(paste("start_date", start_date, "is not in YYYY-MM-DD format"))
      }
      if (!is.null(end_date) && !.is_date(end_date)) {
        stop(paste("end_date", end_date, "is not in YYYY-MM-DD format"))
      }

      game_type_str <- paste(game_type, collapse = ",")

      fetch_one <- function(yr) {
        sd <- start_date %||% paste0(yr, "-01-01")
        ed <- end_date   %||% paste0(yr, "-12-31")

        url <- paste0(
          "https://statsapi.mlb.com/api/v1/people/", player_id,
          "?hydrate=stats(type=gameLog,season=", yr,
          ",startDate=", sd,
          ",endDate=", ed,
          ",sportId=", sport_id,
          ",gameType=[", game_type_str, "]),hydrations"
        )

        resp <- tryCatch(httr::content(httr::GET(url), "parsed"), error = function(e) NULL)
        if (is.null(resp)) return(integer(0))

        splits <- tryCatch(resp$people[[1]]$stats[[1]]$splits, error = function(e) NULL)
        if (is.null(splits) || length(splits) == 0) return(integer(0))

        as.integer(sapply(splits, function(x) x$game$gamePk))
      }
      pks <- unlist(lapply(seasons, fetch_one), use.names = FALSE)
      unique(pks[!is.na(pks)])
    },
    get_teams = function() {
      response <- GET("https://statsapi.mlb.com/api/v1/teams/")
      teams <- content(response, "text") %>% fromJSON(flatten = TRUE)

      teams$teams %>%
        tibble::as_tibble() %>%
        transmute(
          team_id = id,
          city = franchiseName,
          name = teamName,
          franchise = name,
          abbreviation = abbreviation
        ) %>%
        distinct() %>%
        filter(!is.na(team_id)) %>%
        arrange(team_id)

    }

  )
)
