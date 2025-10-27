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

      .get_ab_index <- function(ed) {
        if ("atBatIndex" %in% names(ed)) {
          as.integer(ed$atBatIndex)
        } else if ("about.atBatIndex" %in% names(ed)) {
          as.integer(ed$`about.atBatIndex`)
        } else {
          vapply(ed$about, function(a) as.integer(a$atBatIndex %||% NA_integer_), integer(1))
        }
      }

      .safe_post_on_id <- function(ed, base_tag = c("First","Second","Third")) {
        base_tag <- match.arg(base_tag)
        flat_col <- paste0("matchup.postOn", base_tag, ".id")
        if (flat_col %in% names(ed)) {
          return(suppressWarnings(as.integer(ed[[flat_col]])))
        }
        n <- NROW(ed)
        if (is.null(n) || n == 0) return(integer())
        vapply(seq_len(n), function(i) {
          mu <- ed$matchup[[i]]
          if (is.null(mu)) return(NA_integer_)
          slot <- switch(base_tag,
                         First  = "postOnFirst",
                         Second = "postOnSecond",
                         Third  = "postOnThird")
          val <- tryCatch(mu[[slot]]$id, error = function(e) NA_integer_)
          if (is.null(val) || length(val) == 0) NA_integer_ else as.integer(val[1])
        }, integer(1))
      }

      .safe_post_outs <- function(ed) {
        if ("count.outs" %in% names(ed)) {
          return(suppressWarnings(as.integer(ed$`count.outs`)))
        }
        n <- NROW(ed)
        if (is.null(n) || n == 0) return(integer())
        vapply(seq_len(n), function(i) {
          cnt <- ed$count[[i]]
          if (!is.null(cnt) && !is.null(cnt$outs)) as.integer(cnt$outs) else NA_integer_
        }, integer(1))
      }

      .ghost_flags <- function(ed) {
        n <- NROW(ed)
        if (is.null(n) || n == 0) return(logical())
        vapply(seq_len(n), function(i) {
          pe <- ed$playEvents[[i]]
          if (is.null(pe) || !is.data.frame(pe) || nrow(pe) == 0) return(FALSE)
          if ("details.event" %in% names(pe)) {
            any(pe$`details.event` == "Runner Placed On Base", na.rm = TRUE)
          } else {
            any(vapply(seq_len(nrow(pe)), function(j) {
              det <- tryCatch(pe$details[[j]], error = function(e) NULL)
              !is.null(det) && !is.null(det$event) && identical(det$event, "Runner Placed On Base")
            }, logical(1)))
          }
        }, logical(1))
      }

      .ghost_id_for_each_ab <- function(ed) {
        n <- NROW(ed)
        if (is.null(n) || n == 0) return(integer())
        vapply(seq_len(n), function(i) {
          pe <- ed$playEvents[[i]]
          if (is.null(pe) || !is.data.frame(pe) || nrow(pe) == 0) return(NA_integer_)
          idx <- integer(0)
          if ("details.event" %in% names(pe)) {
            idx <- which(pe$`details.event` == "Runner Placed On Base")
          } else {
            idx <- which(vapply(seq_len(nrow(pe)), function(j) {
              det <- tryCatch(pe$details[[j]], error = function(e) NULL)
              !is.null(det) && !is.null(det$event) && identical(det$event, "Runner Placed On Base")
            }, logical(1)))
          }
          if (!length(idx)) return(NA_integer_)
          j <- idx[1]
          if ("player.id" %in% names(pe)) {
            zid <- suppressWarnings(as.integer(pe$`player.id`[j]))
            if (!is.na(zid)) return(zid)
          }
          if ("player" %in% names(pe)) {
            p <- pe$player[[j]]
            if (!is.null(p) && !is.null(p$id)) return(as.integer(p$id))
          }
          NA_integer_
        }, integer(1))
      }

      track_base_out_by_event <- function(event_data) {
        n <- NROW(event_data)
        if (is.null(n) || n == 0) {
          return(tibble::tibble(
            event_index = integer(),
            pre_runner_1b_id = integer(), pre_runner_2b_id = integer(), pre_runner_3b_id = integer(), pre_outs = integer(),
            post_runner_1b_id = integer(), post_runner_2b_id = integer(), post_runner_3b_id = integer(), post_outs = integer()
          ))
        }

        ei <- .get_ab_index(event_data)

        post_first  <- .safe_post_on_id(event_data, "First")
        post_second <- .safe_post_on_id(event_data, "Second")
        post_third  <- .safe_post_on_id(event_data, "Third")
        post_outs   <- .safe_post_outs(event_data)

        post_state <- tibble::tibble(
          event_index = ei,
          post_runner_1b_id = post_first,
          post_runner_2b_id = post_second,
          post_runner_3b_id = post_third,
          post_outs         = post_outs
        )

        pre_state <- post_state %>%
          dplyr::transmute(
            event_index,
            pre_runner_1b_id = dplyr::lag(post_runner_1b_id, 1),
            pre_runner_2b_id = dplyr::lag(post_runner_2b_id, 1),
            pre_runner_3b_id = dplyr::lag(post_runner_3b_id, 1),
            pre_outs         = dplyr::lag(post_outs, 1, default = 0) %% 3
          )

        z_flag <- .ghost_flags(event_data)
        if (any(z_flag, na.rm = TRUE)) {
          z_ids <- .ghost_id_for_each_ab(event_data)
          pre_state$pre_runner_2b_id[z_flag] <- z_ids[z_flag]
        }

        dplyr::left_join(pre_state, post_state, by = "event_index") %>%
          dplyr::select(event_index, dplyr::starts_with("pre_"), dplyr::starts_with("post_"))
      }

      swing_codes <- c("X","F","S","D","E","T","W")
      whiff_codes <- c("S","T","W")
      csw_codes   <- c("S","T","W","C")

      out_rows <- vector("list", 0L)

      for (g in data_list) {
        if (!is.list(g) || !is.null(g$error)) next

        at_bats <- g$liveData$plays$allPlays
        if (is.null(at_bats) || NROW(at_bats) == 0L) { pb$tick(); next }

        bos_evt <- track_base_out_by_event(at_bats)
        bos_idx <- if (nrow(bos_evt)) setNames(seq_len(nrow(bos_evt)), bos_evt$event_index) else integer()

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

        players_list <- g$gameData$players %||% list()
        id_to_name <- function(id) {
          if (is.na(id)) return(NA_character_)
          key <- paste0("ID", as.character(id))
          pl  <- players_list[[key]]
          if (is.null(pl)) return(NA_character_)
          nm <- pl$fullName %||% NA_character_
          if (is.null(nm)) NA_character_ else nm
        }


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

          ei <- if ("atBatIndex" %in% names(ab)) ab$atBatIndex else ab$about[[1]]$atBatIndex
          boe <- if (length(bos_idx)) bos_evt[ bos_idx[[as.character(ei)]] , ] else NULL

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
            } else NA_integer_

            is_csw <- if (is_pitch_val) {
              if (!is.na(code_val)) as.integer(code_val %in% csw_codes) else NA_integer_
            } else NA_integer_

            is_whiff <- if (is_pitch_val && !is.na(code_val) && (code_val %in% swing_codes)) {
              as.integer(code_val %in% whiff_codes)
            } else NA_integer_

            pitch_zone <- suppressWarnings(
              as.integer(.gv(row_e, "pitchData.zone", c("pitchData","zone")))
            )

            in_zone <- if (is_pitch_val && !is.na(pitch_zone)) {
              if (pitch_zone < 10L) 1L else 0L
            } else NA_integer_

            is_chase <- if (is_pitch_val && is_swing == 1L) {
              if (is.na(in_zone)) NA_integer_ else if (in_zone == 0L) 1L else 0L
            } else NA_integer_

            in_zone_whiff <- if (is_pitch_val && is_swing == 1L && !is.na(in_zone) && in_zone == 1L) {
              if (is_whiff == 1L) 1L else 0L
            } else NA_integer_

            in_play_flag <- isTRUE(.gv(row_e, "details.isInPlay", c("details","isInPlay")))
            ev  <- suppressWarnings(as.numeric(.gv(row_e, "hitData.launchSpeed", c("hitData","launchSpeed"))))
            ang <- suppressWarnings(as.numeric(.gv(row_e, "hitData.launchAngle",  c("hitData","launchAngle"))))
            in_play <- if (is_pitch_val) as.integer(in_play_flag) else NA_integer_

            is_barrel <- if (in_play_flag) {
              if (!is.na(ev) && !is.na(ang)) {
                as.integer((ev * 1.5 - ang) >= 117 && (ev + ang) >= 124 && ang <= 50 && ev >= 98)
              } else NA_integer_
            } else NA_integer_

            is_hard_hit <- if (in_play_flag) {
              if (!is.na(ev) && !is.na(ang)) as.integer(ev >= 95) else NA_integer_
            } else NA_integer_

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

            if (!is.null(boe) && nrow(boe) == 1) {
              pre1_id  <- boe$pre_runner_1b_id
              pre2_id  <- boe$pre_runner_2b_id
              pre3_id  <- boe$pre_runner_3b_id
              post1_id <- boe$post_runner_1b_id
              post2_id <- boe$post_runner_2b_id
              post3_id <- boe$post_runner_3b_id
              pre_outs_val  <- boe$pre_outs
              post_outs_val <- boe$post_outs
            } else {
              pre1_id <- pre2_id <- pre3_id <- post1_id <- post2_id <- post3_id <- NA_integer_
              pre_outs_val <- post_outs_val <- NA_integer_
            }

            r <- list(
              game_id    = g$gamePk %||% NA,
              game_date  = g$gameData$datetime$officialDate %||% NA,
              play_id    = .gv(row_e, "playId", "playId"),
              is_pitch   = .gv(row_e, "isPitch", "isPitch"),
              away_score = pre_away_score_val,
              home_score = pre_home_score_val,
              top_bottom = if ("about.halfInning" %in% names(ab)) ab$`about.halfInning` else ab$about[[1]]$halfInning,
              inning     = if ("about.inning" %in% names(ab)) ab$`about.inning` else ab$about[[1]]$inning,
              ab_number  = ab$atBatIndex %||% NA_integer_,
              pitch_number = j,
              batter_id    = if ("matchup.batter.id" %in% names(ab)) ab$`matchup.batter.id` else ab$matchup[[1]]$batter$id,
              batter_name  = if ("matchup.batter.fullName" %in% names(ab)) ab$`matchup.batter.fullName` else ab$matchup[[1]]$batter$fullName,
              batter_side  = if ("matchup.batSide.code" %in% names(ab)) ab$`matchup.batSide.code` else ab$matchup[[1]]$batSide$code,
              pitcher_id   = if ("matchup.pitcher.id" %in% names(ab)) ab$`matchup.pitcher.id` else ab$matchup[[1]]$pitcher$id,
              pitcher_name = if ("matchup.pitcher.fullName" %in% names(ab)) ab$`matchup.pitcher.fullName` else ab$matchup[[1]]$pitcher$fullName,
              pitcher_side = if ("matchup.pitchHand.code" %in% names(ab)) ab$`matchup.pitchHand.code` else ab$matchup[[1]]$pitchHand$code,
              batter_split   = if ("matchup.splits.batter" %in% names(ab)) ab$`matchup.splits.batter` else ab$matchup[[1]]$splits$batter %||% NA,
              pitcher_split  = if ("matchup.splits.pitcher" %in% names(ab)) ab$`matchup.splits.pitcher`
              else ab$matchup[[1]]$splits$pitcher %||% NA,
              runners_on_base= if ("matchup.splits.menOnBase" %in% names(ab)) ab$`matchup.splits.menOnBase`
              else ab$matchup[[1]]$splits$menOnBase %||% NA,
              balls = as.integer(bB),
              strikes = as.integer(sB),
              outs = as.integer(oB),
              balls_post = as.integer(bA),
              strikes_post = as.integer(sA),
              outs_post = as.integer(oA),
              is_strike   = .gv(row_e, "details.isStrike",    c("details","isStrike")),
              is_ball     = .gv(row_e, "details.isBall",      c("details","isBall")),
              last_pitch_of_ab = if (last_pitch) 1L else 0L,
              play_type     = .gv(row_e, "type", "type"),
              pitch_code    = code,
              pitch_call    = .gv(row_e, "details.call.description",   c("details","call","description")),
              pitch_type    = .gv(row_e, "details.type.code",          c("details","type","code")),
              pitch_name    = .gv(row_e, "details.type.description",   c("details","type","description")),
              is_swing      = is_swing,
              is_csw        = is_csw,
              is_whiff      = is_whiff,
              in_zone       = in_zone,
              is_chase      = is_chase,
              in_zone_whiff = in_zone_whiff,
              is_barrel     = is_barrel,
              is_hard_hit   = is_hard_hit,
              in_play       = in_play,
              sz_top     = 12 * .gv(row_e, "pitchData.strikeZoneTop",    c("pitchData","strikeZoneTop")),
              sz_bot     = 12 * .gv(row_e, "pitchData.strikeZoneBottom", c("pitchData","strikeZoneBottom")),
              velocity   = .gv(row_e, "pitchData.startSpeed",            c("pitchData","startSpeed")),
              end_velocity = .gv(row_e, "pitchData.endSpeed",            c("pitchData","endSpeed")),
              vb        = .gv(row_e, "pitchData.breaks.breakVertical",           c("pitchData","breaks","breakVertical")),
              ivb       = .gv(row_e, "pitchData.breaks.breakVerticalInduced",    c("pitchData","breaks","breakVerticalInduced")),
              hb        = .gv(row_e, "pitchData.breaks.breakHorizontal",         c("pitchData","breaks","breakHorizontal")),
              zone      = .gv(row_e, "pitchData.zone",                   c("pitchData","zone")),
              plate_time= .gv(row_e, "pitchData.plateTime",              c("pitchData","plateTime")),
              extension = .gv(row_e, "pitchData.extension",              c("pitchData","extension")),
              aX = .gv(row_e, "pitchData.coordinates.aX", c("pitchData","coordinates","aX")),
              aY = .gv(row_e, "pitchData.coordinates.aY", c("pitchData","coordinates","aY")),
              aZ = .gv(row_e, "pitchData.coordinates.aZ", c("pitchData","coordinates","aZ")),
              plate_x = 12 * .gv(row_e, "pitchData.coordinates.pX",  c("pitchData","coordinates","pX")),
              plate_z = 12 * .gv(row_e, "pitchData.coordinates.pZ",  c("pitchData","coordinates","pZ")),
              vx0    = .gv(row_e, "pitchData.coordinates.vX0", c("pitchData","coordinates","vX0")),
              vy0    = .gv(row_e, "pitchData.coordinates.vY0", c("pitchData","coordinates","vY0")),
              vz0    = .gv(row_e, "pitchData.coordinates.vZ0", c("pitchData","coordinates","vZ0")),
              release_x = .gv(row_e, "pitchData.coordinates.x0",  c("pitchData","coordinates","x0")),
              release_y = .gv(row_e, "pitchData.coordinates.y0",  c("pitchData","coordinates","y0")),
              release_z = .gv(row_e, "pitchData.coordinates.z0",  c("pitchData","coordinates","z0")),
              break_angle  = .gv(row_e, "pitchData.breaks.breakAngle",    c("pitchData","breaks","breakAngle")),
              spin_rate    = .gv(row_e, "pitchData.breaks.spinRate",      c("pitchData","breaks","spinRate")),
              spin_direction = .gv(row_e, "pitchData.breaks.spinDirection", c("pitchData","breaks","spinDirection")),
              exit_velocity = .gv(row_e, "hitData.launchSpeed",   c("hitData","launchSpeed")),
              launch_angle  = .gv(row_e, "hitData.launchAngle",   c("hitData","launchAngle")),
              hit_distance  = .gv(row_e, "hitData.totalDistance", c("hitData","totalDistance")),
              hit_trajectory= .gv(row_e, "hitData.trajectory",    c("hitData","trajectory")),
              hit_hardness  = .gv(row_e, "hitData.hardness",      c("hitData","hardness")),
              hit_location  = .gv(row_e, "hitData.location",      c("hitData","location")),
              hit_coordinate_x = .gv(row_e, "hitData.coordinates.coordX", c("hitData","coordinates","coordX")),
              hit_coordinate_z = .gv(row_e, "hitData.coordinates.coordY", c("hitData","coordinates","coordY")),
              event            = if (last_pitch) (if ("result.event" %in% names(ab)) ab$`result.event` else ab$result[[1]]$event) else NA,
              event_type       = if (last_pitch) (if ("result.eventType" %in% names(ab)) ab$`result.eventType` else ab$result[[1]]$eventType) else NA,
              play_description = if (last_pitch) (if ("result.description" %in% names(ab)) ab$`result.description` else ab$result[[1]]$description) else NA,
              rbi              = if (last_pitch) (if ("result.rbi" %in% names(ab)) ab$`result.rbi` else ab$result[[1]]$rbi) else NA,
              scoring_play     = if ("about.isScoringPlay" %in% names(ab)) ab$`about.isScoringPlay` else ab$about[[1]]$isScoringPlay,
              pre_runner_1b_id   = pre1_id,
              pre_runner_2b_id   = pre2_id,
              pre_runner_3b_id   = pre3_id,
              post_runner_1b_id   = post1_id,
              post_runner_2b_id   = post2_id,
              post_runner_3b_id   = post3_id,
              pre_runner_1b_name  = id_to_name(pre1_id),
              pre_runner_2b_name  = id_to_name(pre2_id),
              pre_runner_3b_name  = id_to_name(pre3_id),
              post_runner_1b_name = id_to_name(post1_id),
              post_runner_2b_name = id_to_name(post2_id),
              post_runner_3b_name = id_to_name(post3_id),
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

            out_rows[[length(out_rows) + 1L]] <- r
          }
        }

        pb$tick()
      }

      if (!length(out_rows)) return(tibble::tibble())

      data.table::rbindlist(out_rows, use.names = TRUE, fill = TRUE) %>%
        tibble::as_tibble()
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

    },
    get_pbp_season = function(season = c(2025),
                              start_date = NULL,
                              end_date = NULL,
                              sport_id = c(1),
                              game_type = c('R')){

      schedule <- self$get_schedule(
        season    = season,
        sport_id  = sport_id,
        game_type = game_type
      )  %>%
        filter(state %in% c("F", "D", "I"))

      if (!is.null(start_date)) {
        schedule <- schedule %>% dplyr::filter(date >= start_date)
      }
      if (!is.null(end_date)) {
        schedule <- schedule %>% dplyr::filter(date <= end_date)
      }

      ids_list <- schedule %>%
        distinct(game_id) %>%
        pull(game_id)


      if (length(ids_list) == 0) {
        tibble()
      }
      self$get_pbp_data(data_list = self$get_data_json(ids_list = ids_list))
    },
    get_pbp_player = function(player_id,
                              season = c(2025),
                              start_date = NULL,
                              end_date = NULL,
                              game_type = c('R'),
                              sport_id = c(1)){

      ids_list <- self$get_player_games_list(player_id = player_id,
                                             season = season,
                                             start_date = NULL,
                                             end_date = NULL,
                                             game_type = game_type,
                                             sport_id = sport_id)



      self$get_pbp_data(data_list = self$get_data_json(ids_list = ids_list))


    },
    get_run_expectancy = function(df, matrix_type = c(24, 288)) {



      df <- df %>%
        dplyr::mutate(
          half = paste(game_id, inning, top_bottom, sep = "_"),
          away_score = suppressWarnings(as.numeric(away_score)),
          home_score = suppressWarnings(as.numeric(home_score))
        ) %>%
        dplyr::arrange(ab_number, pitch_number) %>%
        dplyr::group_by(half) %>%
        tidyr::fill(away_score, home_score, .direction = "downup") %>%
        dplyr::mutate(
          is_top = grepl("top", tolower(dplyr::first(top_bottom))),
          bat_runs = ifelse(is_top, away_score, home_score),
          end_runs = max(bat_runs, na.rm = TRUE),
          runs_to_end = end_runs - bat_runs
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          base_state = paste0(as.integer(!is.na(pre_runner_1b_id)),
                              as.integer(!is.na(pre_runner_2b_id)),
                              as.integer(!is.na(pre_runner_3b_id))),
          outs    = pmin(pmax(as.integer(outs),    0L), 2L),
          balls   = pmin(pmax(as.integer(balls),   0L), 3L),
          strikes = pmin(pmax(as.integer(strikes), 0L), 2L),
          count   = paste0(balls, "-", strikes)
        )



      all_counts  <- as.vector(outer(0:3, 0:2, function(b, s) paste0(b, "-", s)))
      base_levels <- c("000","100","010","110","001","101","011","111")

      if (matrix_type == 24){
        agg <- df %>%
          dplyr::group_by(outs, base_state) %>%
          dplyr::summarise(run_expectancy = mean(runs_to_end, na.rm = TRUE), .groups = "drop")

        tidyr::expand_grid(
          outs = 0:2,
          base_state = base_levels) %>%
          dplyr::left_join(agg, by = c("outs","base_state")) %>%
          dplyr::arrange(outs, base_state) %>%
          dplyr::mutate(run_expectancy = round(run_expectancy, 2)) %>%
          dplyr::arrange(-run_expectancy)
      } else {

        agg <- df %>%
          dplyr::group_by(outs, count, base_state) %>%
          dplyr::summarise(run_expectancy = mean(runs_to_end, na.rm = TRUE), .groups = "drop")

        tidyr::expand_grid(
          outs = 0:2,
          count = all_counts,
          base_state = base_levels) %>%
          dplyr::left_join(agg, by = c("outs","count","base_state")) %>%
          dplyr::arrange(outs, count, base_state) %>%
          dplyr::mutate(run_expectancy = round(run_expectancy, 2)) %>%
          dplyr::arrange(-run_expectancy)
      }

    },
    get_guts = function(seasons = NULL) {
      url <- "https://www.fangraphs.com/tools/guts"
      resp <- httr2::request(url) %>%
        httr2::req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118 Safari/537.36") %>%
        httr2::req_perform()
      
      page <- httr2::resp_body_html(resp)
      
      tables <- page %>%
        rvest::html_elements("table") %>%
        lapply(rvest::html_table)
      
      result <- tables[[which.max(sapply(tables, nrow))]]
      
      if (!is.null(seasons)) {
        result <- result %>% dplyr::filter(Season %in% seasons)
      }
      
      result
    },
    apply_wOBA = function(df){

      guts <- self$get_guts()

      df %>%
        dplyr::mutate(year = lubridate::year(game_date)) %>%
        dplyr::left_join(guts %>%
                    dplyr::select(c(Season, w1B, w2B, w3B, wHR, wHBP, wBB)),
                  by = c("year" = "Season")) %>%
        dplyr::mutate(
          wOBA = dplyr::case_when(
            event_type == "single" ~ w1B,
            event_type == "double" ~ w2B,
            event_type == "triple" ~ w3B,
            event_type == "home_run" ~ wHR,
            event_type == "hit_by_pitch" ~ wHBP,
            event_type == "walk" ~ wBB,
            event_type == "intent_walk" ~ NA_real_,
            last_pitch_of_ab == 0 ~ NA_real_,
            TRUE ~ 0
          )
        ) %>%
        dplyr::select(-c(w1B, w2B, w3B, wHR, wHBP, wBB, year))

    },
    apply_re288 = function(df, full_season_data){

      re288 <- self$get_run_expectancy(full_season_data, 288)

      re_map <- re288 %>%
        dplyr::mutate(key = paste(outs, count, base_state, sep = "|")) %>%
        dplyr::select(key, run_expectancy)
      
      
      re_lookup <- setNames(re_map$run_expectancy, re_map$key)
      
      data %>%
        dplyr::mutate(
          half = paste(game_id, inning, top_bottom, sep = "_"),
          batting_score  = ifelse(top_bottom == "top", away_score, home_score),
          fielding_score = ifelse(top_bottom == "top", home_score, away_score),
          base_state = paste0(as.integer(!is.na(pre_runner_1b_id)),
                              as.integer(!is.na(pre_runner_2b_id)),
                              as.integer(!is.na(pre_runner_3b_id))),
          outs    = pmin(pmax(as.integer(outs),    0L), 2L),
          balls   = pmin(pmax(as.integer(balls),   0L), 3L),
          strikes = pmin(pmax(as.integer(strikes), 0L), 2L),
          count   = paste0(balls, "-", strikes),
      
          runs_on_play_text = str_count(play_description %||% "", regex("\\bscores\\b", ignore_case = TRUE))
        ) %>%
        dplyr::group_by(game_id) %>%
        dplyr::arrange(ab_number, pitch_number, .by_group = TRUE) %>%
        dplyr::mutate(
          is_game_last_play = dplyr::row_number() == dplyr::n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(re288, by = c("outs","count","base_state")) %>%
        dplyr::group_by(half) %>%
        dplyr::arrange(ab_number, pitch_number, .by_group = TRUE) %>%
        dplyr::mutate(
         
          runs_on_play_norm = dplyr::coalesce(lead(batting_score) - batting_score, 0),
      
         
          is_walkoff = is_game_last_play & (batting_score + runs_on_play_text > fielding_score),
      
          next_base_state_walk = paste0(as.integer(!is.na(post_runner_1b_id)),
                                        as.integer(!is.na(post_runner_2b_id)),
                                        as.integer(!is.na(post_runner_3b_id))),
          next_count_walk      = "0-0",
          next_outs_walk       = outs,
      
          RE_next_walk = re_lookup[paste(next_outs_walk, next_count_walk, next_base_state_walk, sep = "|")],
      
          RE_next_norm = ifelse(!is.na(lead(ab_number)), lead(run_expectancy), 0),
      
          RE_next = ifelse(is_walkoff, dplyr::coalesce(RE_next_walk, 0), RE_next_norm),
          runs_on_play = ifelse(is_walkoff, runs_on_play_text, runs_on_play_norm),
      
          delta_run_exp = RE_next - run_expectancy + runs_on_play
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(
          half, count,
          runs_on_play_norm, runs_on_play_text,
          is_game_last_play, is_walkoff,
          next_base_state_walk, next_count_walk, next_outs_walk,
          RE_next_walk, RE_next_norm, RE_next
        ))
}

  )
)


