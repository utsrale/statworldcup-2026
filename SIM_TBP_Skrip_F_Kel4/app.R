#  Team Based Project SIM

# Load .Renviron jika ada 
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(DT)
  library(plotly)
  library(leaflet)
  library(readr)
  library(memoise)
  library(cachem)
  library(digest)
})

# Suppress R CMD check / IDE linter warnings about undefined global variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "utcDate", "date_jkt", "group", "stage", "home", "away", "team",
    "goalDifference", "goalsFor", "position", "tla", "crest", "playedGames",
    "won", "draw", "lost", "goalsAgainst", "form", "goals", "assists", "status",
    "home_score", "away_score", "defense", "attack", "a_def", "a_atk", "h_def",
    "h_atk", "pts", "gd", "gf", "rand", "pos", "id", "p_champion", "p_final",
    "p_sf", "home_crest", "home_tla", "away_crest", "away_tla", "name", "city",
    "country", "capacity", "pct", "p_qf", "p_r32"
  ))
}

# 1. CONFIG & HELPERS
FD_KEYS <- c(
  Sys.getenv("FOOTBALL_DATA_KEY_1"),
  Sys.getenv("FOOTBALL_DATA_KEY_2")
) |> (\(x) x[nzchar(x)])()

FD_BASE <- "https://api.football-data.org/v4"
WC_CODE <- "WC"
SEASON_YEAR <- 2026

TR_KEY <- Sys.getenv("TOKENROUTER_API_KEY")
TR_BASE <- Sys.getenv("TOKENROUTER_BASE_URL", "https://api.tokenrouter.com/v1")
TR_MODEL <- Sys.getenv("TOKENROUTER_MODEL", "MiniMax-M3")

# Identitas Kelompok
GROUP_INFO <- list(
  course = "Sistem Informasi Manajemen",
  class = "F",
  year = 2026,
  project = "TBP SIM - StatWorldCup 2026",
  lecturer = "Muhammad Bayu Nirwana",
  members = list(
    list(name = "Ahmad Zaakiy Hidayat", nim = "M0724019"),
    list(name = "Fikri Adhiatma Nugroho", nim = "M0724027"),
    list(name = "Michael Petra Pakpahan", nim = "M0724065"),
    list(name = "Pancar Aura Zaki Ardika", nim = "M0724071"),
    list(name = "Naufal Fadhillah Pellu", nim = "M0724077")
  )
)


# In-memory cache (60 detik) - pendek supaya status LIVE cepat ter-update
cache <- cachem::cache_mem(max_size = 50 * 1024^2, max_age = 60)


`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# --- Generic fetch with key failover ---
fd_fetch_raw <- function(path, query = list()) {
  if (length(FD_KEYS) == 0) {
    stop("FOOTBALL_DATA_KEY belum di-set di .Renviron")
  }
  last_err <- NULL
  for (k in FD_KEYS) {
    res <- tryCatch(
      {
        req <- httr2::request(paste0(FD_BASE, path)) |>
          httr2::req_headers(`X-Auth-Token` = k) |>
          httr2::req_url_query(!!!query) |>
          httr2::req_timeout(15) |>
          httr2::req_error(is_error = \(r) FALSE) |>
          httr2::req_perform()
        list(status = httr2::resp_status(req), body = httr2::resp_body_string(req))
      },
      error = function(e) list(status = -1, body = conditionMessage(e))
    )

    if (!is.null(res$status) && res$status == 200) {
      return(jsonlite::fromJSON(res$body, simplifyVector = TRUE, flatten = TRUE))
    }
    last_err <- res
  }
  warning(sprintf("API gagal untuk %s (status %s)", path, last_err$status %||% "NA"))
  NULL
}

# Memoised wrapper
fd_fetch <- memoise::memoise(fd_fetch_raw, cache = cache)

# Helper: Material Symbols icon HTML
mat_icon <- function(name, class = "") {
  cls <- paste("material-symbols-outlined", class)
  tags$span(class = cls, name)
}

# Helper: format nama grup/babak dari API agar enak dibaca
# Contoh: "GROUP_A" -> "Grup A", "ROUND_OF_16" -> "16 Besar"
fmt_group <- function(g) {
  if (is.null(g) || length(g) == 0) {
    return("-")
  }
  out <- ifelse(is.na(g) | !nzchar(g), "-", g)
  out <- gsub("^GROUP_", "Grup ", out)
  out <- gsub("ROUND_OF_16", "16 Besar", out)
  out <- gsub("QUARTER_FINALS?", "Perempat Final", out)
  out <- gsub("SEMI_FINALS?", "Semi Final", out)
  out <- gsub("THIRD_PLACE", "Perebutan Juara 3", out)
  out <- gsub("^FINAL$", "Final", out)
  out <- gsub("PRELIMINARY_ROUND", "Babak Penyisihan", out)
  out <- gsub("_", " ", out)
  out
}


# 2. DATA LAYER
empty_matches <- function() {
  tibble(
    id = integer(), utcDate = as.POSIXct(character()),
    status = character(), matchday = integer(), stage = character(),
    group = character(), home = character(), home_tla = character(),
    home_crest = character(), away = character(), away_tla = character(),
    away_crest = character(), home_score = integer(), away_score = integer(),
    winner = character(), minute = character(), venue = character(),
    date_jkt = as.POSIXct(character()),
    date_label = character(), score_label = character()
  )
}



get_matches <- function() {
  raw <- fd_fetch(
    sprintf("/competitions/%s/matches", WC_CODE),
    list(season = SEASON_YEAR)
  )
  if (is.null(raw) || is.null(raw$matches) || length(raw$matches) == 0) {
    return(empty_matches())
  }
  m <- raw$matches
  tibble(
    id = m$id,
    utcDate = lubridate::ymd_hms(m$utcDate, quiet = TRUE),
    status = m$status,
    matchday = m$matchday,
    stage = m$stage,
    group = m$group %||% NA_character_,
    home = m$homeTeam.name,
    home_tla = m$homeTeam.tla,
    home_crest = m$homeTeam.crest,
    away = m$awayTeam.name,
    away_tla = m$awayTeam.tla,
    away_crest = m$awayTeam.crest,
    home_score = m$score.fullTime.home,
    away_score = m$score.fullTime.away,
    winner = m$score.winner %||% NA_character_,
    minute = if ("minute" %in% names(m)) as.character(m$minute) else NA_character_,
    venue = if ("venue" %in% names(m)) as.character(m$venue) else NA_character_
  ) |>
    mutate(
      date_jkt = lubridate::with_tz(utcDate, "Asia/Jakarta"),
      date_label = format(date_jkt, "%a, %d %b %Y %H:%M WIB"),
      # Skor: tampilkan X-Y untuk FINISHED + LIVE (IN_PLAY/PAUSED/LIVE), "vs" untuk lainnya
      score_label = dplyr::case_when(
        status %in% c("FINISHED", "IN_PLAY", "PAUSED", "LIVE") &
          !is.na(home_score) & !is.na(away_score) ~ paste0(home_score, " - ", away_score),
        TRUE ~ "vs"
      ),
      group = ifelse(is.na(group) | group == "", stage, group)
    )
}


get_standings <- function() {
  raw <- fd_fetch(
    sprintf("/competitions/%s/standings", WC_CODE),
    list(season = SEASON_YEAR)
  )
  if (is.null(raw) || is.null(raw$standings)) {
    return(NULL)
  }

  st <- raw$standings
  if (!is.data.frame(st) || !"table" %in% names(st)) {
    return(NULL)
  }

  idx <- if ("type" %in% names(st)) which(st$type == "TOTAL") else 1
  if (length(idx) == 0) idx <- 1
  tbl <- st$table[[idx[1]]]
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(NULL)
  }

  rows <- tibble(
    position       = tbl$position,
    team           = tbl$team.name,
    tla            = tbl$team.tla,
    crest          = tbl$team.crest,
    playedGames    = tbl$playedGames,
    won            = tbl$won,
    draw           = tbl$draw,
    lost           = tbl$lost,
    goalsFor       = tbl$goalsFor,
    goalsAgainst   = tbl$goalsAgainst,
    goalDifference = tbl$goalDifference,
    points         = tbl$points,
    form           = if ("form" %in% names(tbl)) tbl$form else NA_character_
  )

  m <- get_matches()
  if (nrow(m) > 0) {
    grp_long <- bind_rows(
      m |> filter(!is.na(group), grepl("^GROUP_", group)) |>
        select(team = home, group),
      m |> filter(!is.na(group), grepl("^GROUP_", group)) |>
        select(team = away, group)
    ) |> distinct(team, group)

    rows <- rows |>
      left_join(grp_long, by = "team") |>
      mutate(group = coalesce(group, "OVERALL")) |>
      group_by(group) |>
      arrange(desc(points), desc(goalDifference), desc(goalsFor),
        .by_group = TRUE
      ) |>
      mutate(position = row_number()) |>
      ungroup() |>
      select(
        group, position, team, tla, crest, playedGames,
        won, draw, lost, goalsFor, goalsAgainst,
        goalDifference, points, form
      )
  } else {
    rows$group <- "OVERALL"
  }
  rows
}

get_scorers <- function(limit = 20) {
  raw <- fd_fetch(
    sprintf("/competitions/%s/scorers", WC_CODE),
    list(season = SEASON_YEAR, limit = limit)
  )
  if (is.null(raw) || is.null(raw$scorers) || length(raw$scorers) == 0) {
    return(NULL)
  }
  s <- raw$scorers
  tibble(
    player = s$player.name,
    nationality = s$player.nationality,
    team = s$team.name,
    team_crest = s$team.crest,
    matches = s$playedMatches %||% NA_integer_,
    goals = s$goals %||% 0L,
    assists = s$assists %||% NA_integer_
  ) |> arrange(desc(goals), desc(assists %||% 0))
}

# Static fallback
venues <- tryCatch(
  readr::read_csv("data/wc2026_venues.csv", show_col_types = FALSE),
  error = function(e) tibble()
)

# 2b. PREDICTOR HUB - MONTE CARLO SIMULATION (VEKTORISASI TINGGI)
# Model: Poisson goals + Bayesian smoothing + Strength-of-Schedule.
# Implementasi fully-vectorized: 1.000 simulasi penuh dalam ~1-3 detik.
# Strategi:
#   - Lookup attack/defense via named vector (O(1), bukan filter dataframe).
#   - Batch rpois() utk seluruh λ sekaligus -> 1 panggilan, bukan ribuan.
#   - Klasemen grup dihitung pakai aggregate/matrix ops, bukan dplyr per iterasi.
#   - Loop hanya untuk knockout (32 → 16 → QF → SF → Final, total 5 ronde × 1000 sim).

PREDICTOR_CONFIG <- list(
  n_sim = 500,   # default: seimbang antara kecepatan dan akurasi
  prior_mu = 1.30, # rata-rata gol per tim per match (historis WC ~1.25-1.35)
  prior_weight = 2, # bobot prior (setara 2 match imajiner)
  home_advantage = 1.05, # multiplier kecil utk tuan rumah (USA/CAN/MEX)
  host_teams = c("United States", "Canada", "Mexico")
)

# Hitung kekuatan attack/defense tiap tim dari matches FINISHED
compute_team_strength <- function(matches) {
  cfg <- PREDICTOR_CONFIG
  # Kumpulkan semua tim yang tampil
  all_teams <- unique(c(matches$home, matches$away))
  all_teams <- all_teams[!is.na(all_teams) & nzchar(all_teams)]
  if (length(all_teams) == 0) {
    return(tibble(team = character(), attack = numeric(), defense = numeric(), n = integer()))
  }

  fin <- matches |>
    dplyr::filter(
      status == "FINISHED",
      !is.na(home_score), !is.na(away_score)
    )

  # Bayesian shrinkage: λ = (w * μ + Σ goals) / (w + n)
  mu <- cfg$prior_mu
  w <- cfg$prior_weight

  rows <- lapply(all_teams, function(t) {
    home_for <- fin$home_score[fin$home == t]
    home_ag <- fin$away_score[fin$home == t]
    away_for <- fin$away_score[fin$away == t]
    away_ag <- fin$home_score[fin$away == t]
    g_for <- c(home_for, away_for)
    g_ag <- c(home_ag, away_ag)
    n <- length(g_for)
    attack <- (w * mu + sum(g_for, na.rm = TRUE)) / (w + n)
    defense <- (w * mu + sum(g_ag, na.rm = TRUE)) / (w + n)
    tibble(team = t, attack = attack, defense = defense, n = n)
  })
  out <- dplyr::bind_rows(rows)

  # Strength-of-Schedule correction (1 iterasi cukup)
  if (nrow(fin) >= 5) {
    mu_atk <- mean(out$attack, na.rm = TRUE)
    mu_def <- mean(out$defense, na.rm = TRUE)
    # Untuk tiap match selesai: re-weight gol berdasarkan kekuatan lawan
    fin_with <- fin |>
      dplyr::left_join(out |> dplyr::select(team, h_def = defense, h_atk = attack),
        by = c("home" = "team")
      ) |>
      dplyr::left_join(out |> dplyr::select(team, a_def = defense, a_atk = attack),
        by = c("away" = "team")
      ) |>
      dplyr::mutate(
        # Gol home dinormalisasi terhadap def lawan
        adj_home_for = home_score * (mu_def / pmax(a_def, 0.4)),
        adj_home_ag  = away_score * (mu_atk / pmax(a_atk, 0.4)),
        adj_away_for = away_score * (mu_def / pmax(h_def, 0.4)),
        adj_away_ag  = home_score * (mu_atk / pmax(h_atk, 0.4))
      )
    rows2 <- lapply(all_teams, function(t) {
      af <- c(
        fin_with$adj_home_for[fin_with$home == t],
        fin_with$adj_away_for[fin_with$away == t]
      )
      ag <- c(
        fin_with$adj_home_ag[fin_with$home == t],
        fin_with$adj_away_ag[fin_with$away == t]
      )
      n <- sum(!is.na(af))
      attack <- (w * mu + sum(af, na.rm = TRUE)) / (w + n)
      defense <- (w * mu + sum(ag, na.rm = TRUE)) / (w + n)
      tibble(team = t, attack = attack, defense = defense, n = n)
    })
    out <- dplyr::bind_rows(rows2)
  }

  # Clamp utk stabilitas numerik
  out$attack <- pmin(pmax(out$attack, 0.4), 3.5)
  out$defense <- pmin(pmax(out$defense, 0.4), 3.5)
  out
}

# VEKTORISASI: hitung λ untuk vektor home/away sekaligus
# Return list(lh = numeric vec, la = numeric vec) sepanjang n_match
compute_lambdas <- function(home_vec, away_vec, atk, def) {
  cfg <- PREDICTOR_CONFIG
  mu <- cfg$prior_mu
  # Lookup O(1) via named vector
  h_atk <- atk[home_vec]
  a_def <- def[away_vec]
  a_atk <- atk[away_vec]
  h_def <- def[home_vec]
  # Tim yg tidak ada di str_df -> NA, ganti prior mu
  h_atk[is.na(h_atk)] <- mu
  a_def[is.na(a_def)] <- mu
  a_atk[is.na(a_atk)] <- mu
  h_def[is.na(h_def)] <- mu

  ha <- ifelse(home_vec %in% cfg$host_teams, cfg$home_advantage, 1)
  lh <- (h_atk * a_def / mu) * ha
  la <- (a_atk * h_def / mu)
  list(
    lh = pmin(pmax(lh, 0.15), 6),
    la = pmin(pmax(la, 0.15), 6)
  )
}

# Simulasi 1 pertandingan (legacy wrapper, dipakai utk knockout looped)
sim_match <- function(home, away, atk, def, knockout = FALSE) {
  cfg <- PREDICTOR_CONFIG
  mu <- cfg$prior_mu
  h_atk <- atk[home] %||% mu
  a_def <- def[away] %||% mu
  a_atk <- atk[away] %||% mu
  h_def <- def[home] %||% mu
  if (is.na(h_atk)) h_atk <- mu
  if (is.na(a_def)) a_def <- mu
  if (is.na(a_atk)) a_atk <- mu
  if (is.na(h_def)) h_def <- mu

  ha <- if (home %in% cfg$host_teams) cfg$home_advantage else 1
  lh <- min(max((h_atk * a_def / mu) * ha, 0.15), 6)
  la <- min(max((a_atk * h_def / mu), 0.15), 6)
  hg <- rpois(1, lh)
  ag <- rpois(1, la)
  if (knockout && hg == ag) {
    diff <- (h_atk - a_atk) - (h_def - a_def)
    p_home <- 1 / (1 + exp(-diff))
    if (runif(1) < p_home) hg <- hg + 1 else ag <- ag + 1
  }
  list(hg = hg, ag = ag)
}

# Tentukan klasemen 1 grup dari hasil match (FINISHED + simulasi)
build_group_table <- function(group_matches) {
  if (nrow(group_matches) == 0) {
    return(tibble())
  }
  teams <- unique(c(group_matches$home, group_matches$away))
  rows <- lapply(teams, function(t) {
    h <- group_matches |> dplyr::filter(home == t)
    a <- group_matches |> dplyr::filter(away == t)
    gf <- sum(h$home_score, a$away_score, na.rm = TRUE)
    ga <- sum(h$away_score, a$home_score, na.rm = TRUE)
    w <- sum(h$home_score > h$away_score, na.rm = TRUE) +
      sum(a$away_score > a$home_score, na.rm = TRUE)
    d <- sum(h$home_score == h$away_score, na.rm = TRUE) +
      sum(a$home_score == a$away_score, na.rm = TRUE)
    l <- sum(h$home_score < h$away_score, na.rm = TRUE) +
      sum(a$away_score < a$home_score, na.rm = TRUE)
    pts <- w * 3 + d
    tibble(
      team = t, pts = pts, gd = gf - ga, gf = gf, w = w, d = d, l = l,
      rand = runif(1)
    ) # tiebreaker terakhir = drawing of lots
  })
  dplyr::bind_rows(rows) |>
    dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd), dplyr::desc(gf), dplyr::desc(rand))
}

# 1 kali simulasi penuh turnamen -> return named vector babak terjauh
simulate_tournament_once <- function(matches, str_df) {
  # Extract named vectors untuk sim_match (atk dan def harus named vector, bukan tibble)
  atk_vec <- setNames(str_df$attack, str_df$team)
  def_vec <- setNames(str_df$defense, str_df$team)

  # 1. Kunci match FINISHED, simulasi sisa match grup
  grp_matches <- matches |>
    dplyr::filter(grepl("^GROUP_", group %||% ""))

  if (nrow(grp_matches) == 0) {
    # Tidak ada data grup -> kembalikan kosong
    return(setNames(rep("NONE", nrow(str_df)), str_df$team))
  }

  # Pisahkan finished vs to-simulate
  grp_sim <- grp_matches |>
    dplyr::mutate(
      home_score = ifelse(status == "FINISHED", home_score, NA_integer_),
      away_score = ifelse(status == "FINISHED", away_score, NA_integer_)
    )
  to_sim_idx <- which(is.na(grp_sim$home_score) | is.na(grp_sim$away_score))
  for (i in to_sim_idx) {
    r <- sim_match(grp_sim$home[i], grp_sim$away[i], atk_vec, def_vec, knockout = FALSE)
    grp_sim$home_score[i] <- r$hg
    grp_sim$away_score[i] <- r$ag
  }

  # 2. Klasemen per grup
  groups <- sort(unique(grp_sim$group))
  group_tables <- lapply(groups, function(g) {
    gm <- grp_sim |> dplyr::filter(group == g)
    bt <- build_group_table(gm)
    bt$group <- g
    bt$pos <- seq_len(nrow(bt))
    bt
  })
  all_st <- dplyr::bind_rows(group_tables)

  # 3. Pilih lolos: juara grup (pos==1) + runner-up (pos==2) + 8 peringkat-3 terbaik
  winners <- all_st |>
    dplyr::filter(pos == 1) |>
    dplyr::pull(team)
  runners <- all_st |>
    dplyr::filter(pos == 2) |>
    dplyr::pull(team)
  thirds_df <- all_st |>
    dplyr::filter(pos == 3) |>
    dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd), dplyr::desc(gf), dplyr::desc(rand)) |>
    head(8)
  thirds <- thirds_df$team

  qualified <- unique(c(winners, runners, thirds))
  # Babak yang dicapai (sementara semua R32 dulu, akan upgrade saat menang)
  reach <- setNames(rep("GROUP", nrow(str_df)), str_df$team)
  reach[qualified] <- "R32"

  # 4. Bracket 32 besar: pakai matches knockout dari API kalau ada, kalau tidak random pair
  # Untuk simplicity dan stabilitas: bagi qualified jadi 16 pasangan acak (deterministik per simulasi)
  pool <- sample(qualified)
  if (length(pool) %% 2 == 1) pool <- pool[-length(pool)] # safety
  rounds <- list()
  rounds[["R32"]] <- pool
  current <- pool

  # Helper: 1 ronde knockout
  play_round <- function(teams, label_next) {
    if (length(teams) < 2) {
      return(character())
    }
    winners <- character()
    for (i in seq(1, length(teams) - 1, by = 2)) {
      r <- sim_match(teams[i], teams[i + 1], atk_vec, def_vec, knockout = TRUE)
      w <- if (r$hg > r$ag) teams[i] else teams[i + 1]
      winners <- c(winners, w)
      reach[w] <<- label_next
    }
    winners
  }

  current <- play_round(current, "R16")
  current <- play_round(current, "QF")
  current <- play_round(current, "SF")
  current <- play_round(current, "FINAL")
  current <- play_round(current, "CHAMPION")
  reach
}

# Hash matches FINISHED -> cache key
matches_hash <- function(matches) {
  if (nrow(matches) == 0) {
    return("empty")
  }
  fin <- matches |>
    dplyr::filter(status == "FINISHED") |>
    dplyr::arrange(id) |>
    dplyr::transmute(s = paste(id, home_score, away_score, sep = ":"))
  digest::digest(paste(fin$s, collapse = "|"), algo = "md5")
}

# Wrapper Monte Carlo + cache (manual karena memoise butuh argumen sederhana)
# Cache key = hash hasil FINISHED + n_sim, sehingga hasil berbeda n_sim tidak saling timpa
.predictor_cache <- new.env()

run_monte_carlo <- function(matches, n_sim = PREDICTOR_CONFIG$n_sim, session = NULL) {
  key <- paste0(matches_hash(matches), "_n", n_sim)

  # Opsi B: Jika cache hit, langsung return tanpa progress bar
  if (!is.null(.predictor_cache[[key]])) {
    return(.predictor_cache[[key]])
  }

  # Cache miss: jalankan simulasi dengan progress bar 
  str_df <- compute_team_strength(matches)
  if (nrow(str_df) == 0) {
    return(NULL)
  }

  # Counter babak per tim
  rounds_levels <- c("GROUP", "R32", "R16", "QF", "SF", "FINAL", "CHAMPION")
  count_mat <- matrix(0,
    nrow = nrow(str_df), ncol = length(rounds_levels),
    dimnames = list(str_df$team, rounds_levels)
  )

  set.seed(42) # reprodusibel per snapshot data

  withProgress(
    message = sprintf("Menjalankan %d simulasi Monte Carlo...", n_sim),
    value = 0,
    {
      progress_step <- max(1, floor(n_sim / 10)) # update progress setiap 10%
      for (i in seq_len(n_sim)) {
        reach <- simulate_tournament_once(matches, str_df)
        for (t in names(reach)) {
          r <- reach[[t]]
          if (r %in% rounds_levels) {
            idx_max <- which(rounds_levels == r)
            for (j in 1:idx_max) {
              count_mat[t, j] <- count_mat[t, j] + 1
            }
          }
        }
        # Update progress bar setiap 10%
        if (i %% progress_step == 0) {
          setProgress(value = i / n_sim, detail = sprintf("%d / %d", i, n_sim))
        }
      }
      setProgress(1, detail = "Selesai!")
    }
  )

  prob <- count_mat / n_sim
  out <- tibble(
    team        = rownames(prob),
    p_group     = prob[, "GROUP"],
    p_r32       = prob[, "R32"],
    p_r16       = prob[, "R16"],
    p_qf        = prob[, "QF"],
    p_sf        = prob[, "SF"],
    p_final     = prob[, "FINAL"],
    p_champion  = prob[, "CHAMPION"]
  ) |>
    dplyr::arrange(dplyr::desc(p_champion), dplyr::desc(p_final), dplyr::desc(p_sf))

  # Gabung info logo + grup
  m <- matches
  if (nrow(m) > 0) {
    crest_map <- dplyr::bind_rows(
      m |> dplyr::select(team = home, crest = home_crest, tla = home_tla, group),
      m |> dplyr::select(team = away, crest = away_crest, tla = away_tla, group)
    ) |>
      dplyr::filter(grepl("^GROUP_", group %||% "")) |>
      dplyr::distinct(team, .keep_all = TRUE)
    out <- out |> dplyr::left_join(crest_map, by = "team")
  }

  .predictor_cache[[key]] <- out
  out
}

# 3. OPEN-METEO (no API key needed)
get_weather <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) {
    return(NULL)
  }
  url <- sprintf(
    "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current=temperature_2m,relative_humidity_2m,weather_code,wind_speed_10m&timezone=auto",
    lat, lon
  )
  tryCatch(
    {
      r <- httr2::request(url) |>
        httr2::req_timeout(10) |>
        httr2::req_perform()
      jsonlite::fromJSON(httr2::resp_body_string(r))$current
    },
    error = function(e) NULL
  )
}

# Return list(label, icon) Material Symbols
weather_info <- function(code) {
  if (is.null(code) || is.na(code)) {
    return(list(label = "Tidak diketahui", icon = "help"))
  }
  dict <- list(
    `0`  = list(label = "Cerah", icon = "wb_sunny"),
    `1`  = list(label = "Sebagian Berawan", icon = "partly_cloudy_day"),
    `2`  = list(label = "Berawan", icon = "partly_cloudy_day"),
    `3`  = list(label = "Mendung", icon = "cloud"),
    `45` = list(label = "Berkabut", icon = "foggy"),
    `48` = list(label = "Berkabut", icon = "foggy"),
    `51` = list(label = "Gerimis", icon = "grain"),
    `61` = list(label = "Hujan Ringan", icon = "rainy"),
    `63` = list(label = "Hujan", icon = "rainy"),
    `65` = list(label = "Hujan Lebat", icon = "rainy_heavy"),
    `71` = list(label = "Salju", icon = "weather_snowy"),
    `80` = list(label = "Hujan Lokal", icon = "rainy"),
    `95` = list(label = "Badai Petir", icon = "thunderstorm"),
    `96` = list(label = "Badai Hujan Es", icon = "thunderstorm")
  )
  dict[[as.character(code)]] %||% list(label = sprintf("Kode %s", code), icon = "help")
}

# 4. TOKENROUTER AI
# Bersihkan reply AI: hapus tag <think>...</think> (chain-of-thought MiniMax)
clean_ai_reply <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) {
    return("(jawaban kosong)")
  }
  out <- gsub("<think>[\\s\\S]*?</think>", "", txt, perl = TRUE)
  out <- gsub("(?i)<\\s*/?\\s*think\\s*>", "", out, perl = TRUE)
  out <- trimws(out)
  if (!nzchar(out)) out <- "(jawaban kosong)"
  out
}

ask_ai <- function(user_message, context_data = "") {
  if (!nzchar(TR_KEY)) {
    return("API key chatbot belum di-set. Hubungi administrator.")
  }
  system_prompt <- paste0(
    "Anda adalah asisten cerdas khusus FIFA World Cup 2026 (USA, Canada, Mexico). ",
    "Jawab dalam Bahasa Indonesia yang ringkas, akurat, dan ramah. ",
    "Jangan menampilkan proses berpikir, langsung berikan jawaban final. ",
    "Gunakan data berikut sebagai konteks jika relevan:\n", context_data
  )
  body <- list(
    model = TR_MODEL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_message)
    ),
    temperature = 0.5,
    max_tokens = 600
  )
  tryCatch(
    {
      res <- httr2::request(paste0(TR_BASE, "/chat/completions")) |>
        httr2::req_headers(
          Authorization = paste("Bearer", TR_KEY),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(45) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
      if (httr2::resp_status(res) != 200) {
        return(sprintf("Terjadi kesalahan (HTTP %s). Coba lagi nanti.", httr2::resp_status(res)))
      }
      parsed <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyVector = FALSE)
      raw <- parsed$choices[[1]]$message$content %||% "(jawaban kosong)"
      clean_ai_reply(raw)
    },
    error = function(e) paste("Koneksi AI gagal:", conditionMessage(e))
  )
}

# 5. UI
nav_item <- function(value, icon, label) {
  HTML(sprintf(
    '<span class="material-symbols-outlined">%s</span><span>%s</span>',
    icon, label
  ))
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css?v=7"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$meta(charset = "UTF-8"),
    tags$title("StatWorldCup - World Cup 2026 Analysis Hub"),
    # JavaScript: Enter-to-send + auto-scroll + auto-focus
    tags$script(HTML("
      $(document).on('keydown', '#chat_input', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault();
          if (!$('#chat_send').prop('disabled')) {
            $('#chat_send').click();
          }
        }
      });
      Shiny.addCustomMessageHandler('scrollChat', function(msg) {
        setTimeout(function() {
          var el = document.getElementById('chat_box');
          if (el) el.scrollTop = el.scrollHeight;
        }, 50);
      });
      Shiny.addCustomMessageHandler('focusChat', function(msg) {
        setTimeout(function() {
          var el = document.getElementById('chat_input');
          if (el) el.focus();
        }, 50);
      });
    "))
  ),

  # ---- Landing Page ----
  div(
    id = "landing_page",
    class = "landing-wrapper",
    div(
      class = "landing-glass-card",
      div(class = "landing-icon", mat_icon("stadium", "icon-landing")),
      HTML('<div class="brand-title" style="font-size:64px; font-weight:900; letter-spacing:2px; margin-bottom:10px; text-shadow: 0 4px 15px rgba(0,0,0,0.5);"><span style="color:#00e676;">Stat</span><span style="color:#f1f5f9;">World</span><span style="color:#ef4444;">Cup</span></div>'),
      p("Sistem Informasi Manajemen Prediksi & Analisis Data", style = "font-size:22px; color:#e2e8f0; margin-bottom:45px; font-weight:400; letter-spacing:0.5px; text-shadow: 0 2px 8px rgba(0,0,0,0.5);"),
      actionButton(
        "enter_app_btn", 
        label = tagList(mat_icon("sports_soccer"), "Masuk ke Dashboard"),
        class = "btn-enter-app",
        style = "background:linear-gradient(135deg, #8a1538, #c41e3a); color:#fff; font-weight:800; border:none; padding:18px 40px; border-radius:40px; font-size:18px; cursor:pointer; transition: all 0.3s ease; box-shadow: 0 10px 30px rgba(138,21,56,0.6); display:inline-flex; align-items:center; gap:10px;"
      )
    )
  ),

  # ---- App Dashboard (Hidden initially) ----
  div(
    id = "app_dashboard",
    style = "display:none;",
    
    # Sidebar
    div(
      class = "sidebar-panel",
      HTML('<div class="brand-title" style="font-size:21px; font-weight:900; letter-spacing:0.5px;"><span style="color:#00e676;">Stat</span><span style="color:#f1f5f9;">World</span><span style="color:#ef4444;">Cup</span></div>'),
      div(class = "brand-subtitle", "2026 - DATA INTELLIGENCE"),
      radioButtons("nav",
        label = NULL,
        choiceNames = list(
          nav_item("dash", "dashboard", "Dashboard"),
          nav_item("stadion", "stadium", "Stadion & Cuaca"),
          nav_item("predict", "online_prediction", "Predictor Hub"),
          nav_item("ai", "smart_toy", "AI Assistant")
        ),
        choiceValues = c("dash", "stadion", "predict", "ai"),
        selected = "dash"
      ),
      tags$hr(),
      div(
        class = "sidebar-info",
        HTML(
          "Data: <b style='color:var(--gold)'>football-data.org</b><br/>",
          "Cuaca: <b style='color:var(--gold)'>Open-Meteo</b><br/>",
          "AI: <b style='color:var(--gold)'>TokenRouter</b>"
        )
      ),
      div(
        class = "sidebar-group",
        div(class = "sidebar-group-title", "ANGGOTA KELOMPOK"),
        tags$ul(
          class = "sidebar-members",
          style = "list-style:none !important; padding:0 !important; margin:0 0 12px 0 !important;",
          lapply(GROUP_INFO$members, function(x) {
            tags$li(
              style = "list-style:none; display:flex; flex-direction:column; padding:4px 0; border-bottom:1px dotted rgba(255,255,255,0.04);",
              tags$span(
                class = "member-name",
                style = "font-size:10px; font-weight:500; color:var(--text-muted); line-height:1.25; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; display:block;",
                x$name
              ),
              tags$span(
                class = "member-nim",
                style = "font-size:9px; font-family:'Courier New',monospace; font-weight:600; color:var(--text-muted); letter-spacing:0.4px; opacity:0.75; margin-top:1px; display:block;",
                x$nim
              )
            )
          })
        ),
        div(
          class = "sidebar-copyright",
          HTML(sprintf(
            "&copy; %d &middot; TBP SIM &middot; Kelas %s",
            GROUP_INFO$year, GROUP_INFO$class
          ))
        )
      )
    ),

    # Main
    div(
      class = "main-content",
      uiOutput("page")
    )
  )
)

# 6. SERVER
server <- function(input, output, session) {
  
  # ---- Animasi Landing Page ----
  observeEvent(input$enter_app_btn, {
    shinyjs::hide("landing_page", anim = TRUE, animType = "fade", time = 0.5)
    shinyjs::show("app_dashboard", anim = TRUE, animType = "fade", time = 0.5)
  })

  # Reactive data sources (auto-refresh tiap 60 detik agar status LIVE cepat update)
  matches_r <- reactive({
    invalidateLater(60 * 1000)
    get_matches()
  })
  standings_r <- reactive({
    invalidateLater(5 * 60 * 1000)
    get_standings()
  })
  scorers_r <- reactive({
    invalidateLater(5 * 60 * 1000)
    get_scorers(20)
  })



  # PAGE ROUTER
  output$page <- renderUI({
    switch(input$nav,
      dash    = page_dashboard(),
      stadion = page_stadion(),
      predict = page_predictor(),
      ai      = page_ai()
    )
  })

  # Opsi A: Monte Carlo hanya berjalan saat user klik tombol
  sim_has_run <- reactiveVal(FALSE) # track apakah sudah pernah dijalankan
  predictions_data <- reactiveVal(NULL)

  predictions_r <- reactive({
    predictions_data()
  })

  observeEvent(input$run_sim_btn, {
    m <- matches_r()
    if (nrow(m) == 0) {
      return()
    }
    
    # Berikan visualisasi loading pada tombol secara instan
    shinyjs::disable("run_sim_btn")
    shinyjs::html("run_sim_btn", '<span class="material-symbols-outlined" style="vertical-align:middle; margin-right:4px;">hourglass_empty</span> <span style="vertical-align:middle;">Sedang Menghitung...</span>')
    
    # Kembalikan state tombol setelah selesai (on.exit memastikan selalu dipanggil meskipun error)
    on.exit({
      shinyjs::enable("run_sim_btn")
      shinyjs::html("run_sim_btn", '<span class="material-symbols-outlined" style="vertical-align:middle; margin-right:4px;">play_arrow</span> <span style="vertical-align:middle;">Jalankan Simulasi</span>')
    }, add = TRUE)
    
    n_sim <- as.integer(input$sim_count %||% PREDICTOR_CONFIG$n_sim)
    out <- run_monte_carlo(m, n_sim = n_sim, session = session)
    predictions_data(out)
    sim_has_run(TRUE)
  }, ignoreNULL = TRUE)


  # Helper UI
  card_heading <- function(icon, title) {
    h4(
      class = "card-heading",
      mat_icon(icon),
      span(title)
    )
  }

  stat_row <- function(icon, label, value) {
    div(
      class = "stat-row",
      div(
        class = "stat-row-label",
        mat_icon(icon),
        span(label)
      ),
      div(class = "stat-row-value", value)
    )
  }

  # PAGE: DASHBOARD
  page_dashboard <- function() {
    tagList(
      h1(class = "section-title", "Dashboard World Cup 2026"),
      p(
        style = "color: var(--text-muted); margin-bottom: 24px;",
        "Ringkasan turnamen FIFA World Cup 2026 - Amerika Serikat, Kanada & Meksiko."
      ),
      uiOutput("hero_match"),
      
      # Hasil 24 Jam Terakhir
      div(
        class = "glass-card",
        style = "margin-bottom: 24px;",
        card_heading("history", "Hasil 24 Jam Terakhir"),
        uiOutput("recent_matches_block")
      ),
      
      # Jadwal Mendatang
      div(
        class = "glass-card",
        card_heading("today", "Jadwal 24 Jam Mendatang"),
        uiOutput("today_block")
      ),
      fluidRow(
        column(8, div(
          class = "glass-card",
          card_heading("leaderboard", "Klasemen Grup"),
          uiOutput("group_selector"),
          DTOutput("dt_standings")
        )),
        column(
          4, div(
            class = "glass-card",
            card_heading("analytics", "Statistik Cepat"),
            uiOutput("quick_stats")
          )
        )
      )
    )
  }


  # Jadwal Hari Ini (WIB) - render sebagai tabel DT
  # Helper: cell tim dengan logo
  team_cell <- function(name, crest, align = c("left", "right")) {
    align <- match.arg(align)
    img_html <- sprintf(
      "<img src='%s' style='width:22px; height:22px; object-fit:contain; vertical-align:middle;' onerror=\"this.style.display='none'\"/>",
      crest %||% ""
    )
    if (align == "left") {
      sprintf(
        "<div style='text-align:left;'>%s <b>%s</b></div>",
        img_html, name
      )
    } else {
      sprintf(
        "<div style='text-align:right;'><b>%s</b> %s</div>",
        name, img_html
      )
    }
  }


  # Helper: cell skor (gold normal, merah live)
  score_cell <- function(label, is_live) {
    color <- if (isTRUE(is_live)) "#ff4757" else "var(--gold)"
    sprintf(
      "<div style='text-align:center; font-weight:900; font-size:15px; color:%s;'>%s</div>",
      color, label
    )
  }

  # Helper: cell status badge
  status_cell <- function(status, minute) {
    if (status %in% c("IN_PLAY", "PAUSED", "LIVE")) {
      txt <- if (!is.na(minute) && nzchar(minute)) sprintf("LIVE %s'", minute) else "LIVE"
      sprintf(
        "<span style='background:#ff4757; color:#fff; padding:3px 9px; border-radius:10px; font-size:11px; font-weight:800; letter-spacing:0.5px;'>%s</span>",
        txt
      )
    } else if (status == "FINISHED") {
      "<span style='background:rgba(0,230,118,0.18); color:#00e676; padding:3px 9px; border-radius:10px; font-size:11px; font-weight:700;'>SELESAI</span>"
    } else {
      "<span style='background:var(--surface-high); color:var(--text-muted); padding:3px 9px; border-radius:10px; font-size:11px; font-weight:700;'>BELUM MULAI</span>"
    }
  }

  # Window 24 jam terakhir (dari Sys.time() - 24h s/d Sys.time())
  output$recent_matches_block <- renderUI({
    m <- matches_r()
    if (nrow(m) == 0) {
      return(div(
        class = "empty-mini",
        mat_icon("history", "icon-lg"),
        tags$p("Data pertandingan tidak tersedia")
      ))
    }
    now <- Sys.time()
    start_window <- now - as.difftime(24, units = "hours")
    
    recent_24h <- m |>
      filter(status == "FINISHED", date_jkt >= start_window, date_jkt <= now) |>
      arrange(desc(utcDate))
      
    if (nrow(recent_24h) == 0) {
      return(div(
        class = "empty-mini",
        mat_icon("history", "icon-lg"),
        tags$p("Belum ada pertandingan selesai dalam 24 jam terakhir")
      ))
    }
    
    df <- recent_24h |> head(10)
    
    tbl <- tibble(
      Waktu = df$date_label,
      Home = mapply(team_cell, df$home, df$home_crest,
        MoreArgs = list(align = "right"), USE.NAMES = FALSE
      ),
      Skor = mapply(
        function(lbl, st) score_cell(lbl, st %in% c("IN_PLAY", "PAUSED", "LIVE")),
        df$score_label, df$status,
        USE.NAMES = FALSE
      ),
      Away = mapply(team_cell, df$away, df$away_crest,
        MoreArgs = list(align = "left"), USE.NAMES = FALSE
      ),
      Babak = sprintf(
        "<span style='background:rgba(138,21,56,0.25); color:var(--gold); padding:2px 8px; border-radius:4px; font-size:11px; font-weight:700;'>%s</span>",
        fmt_group(ifelse(is.na(df$group) | !nzchar(df$group), df$stage, df$group))
      )
    )

    datatable(
      tbl,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(targets = c(0, 2, 4), className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  # Window 24 jam mendatang (dari Sys.time() s/d +24h)
  output$today_block <- renderUI({
    m <- matches_r()
    if (nrow(m) == 0) {
      return(div(
        class = "empty-mini",
        mat_icon("event_busy", "icon-lg"),
        tags$p("Belum ada jadwal pertandingan")
      ))
    }
    now <- Sys.time()
    end_window <- now + as.difftime(24, units = "hours")
    upcoming_24h <- m |>
      filter(date_jkt >= now, date_jkt <= end_window) |>
      arrange(utcDate)

    # Tambah match LIVE walau utcDate sudah lewat
    live_now <- m |> filter(status %in% c("IN_PLAY", "PAUSED", "LIVE"))
    combined <- bind_rows(live_now, upcoming_24h) |>
      distinct(id, .keep_all = TRUE) |>
      arrange(utcDate)

    if (nrow(combined) == 0) {
      next_match <- m |>
        filter(status %in% c("SCHEDULED", "TIMED"), date_jkt > now) |>
        arrange(utcDate) |>
        head(1)
      next_info <- if (nrow(next_match) > 0) {
        tags$p(
          style = "font-size:12px; margin-top:6px;",
          tags$span(style = "color:var(--text-muted);", "Pertandingan berikutnya: "),
          tags$b(
            style = "color:var(--gold);",
            sprintf("%s vs %s", next_match$home, next_match$away)
          ),
          tags$br(),
          tags$span(style = "color:var(--text-muted);", next_match$date_label)
        )
      } else {
        NULL
      }
      return(div(
        class = "empty-mini",
        mat_icon("event_busy", "icon-lg"),
        tags$p("Tidak ada pertandingan dalam 24 jam ke depan"),
        next_info
      ))
    }
    DTOutput("dt_today")
  })

  output$dt_today <- renderDT({
    m <- matches_r()
    if (nrow(m) == 0) {
      return(NULL)
    }
    now <- Sys.time()
    end_window <- now + as.difftime(24, units = "hours")
    upcoming_24h <- m |>
      filter(date_jkt >= now, date_jkt <= end_window)
    live_now <- m |> filter(status %in% c("IN_PLAY", "PAUSED", "LIVE"))
    df <- bind_rows(live_now, upcoming_24h) |>
      distinct(id, .keep_all = TRUE) |>
      arrange(utcDate)
    if (nrow(df) == 0) {
      return(NULL)
    }


    tbl <- tibble(
      Waktu = format(df$date_jkt, "%H:%M WIB"),
      Home = mapply(team_cell, df$home, df$home_crest,
        MoreArgs = list(align = "right"), USE.NAMES = FALSE
      ),
      Skor = mapply(
        function(lbl, st) score_cell(lbl, st %in% c("IN_PLAY", "PAUSED", "LIVE")),
        df$score_label, df$status,
        USE.NAMES = FALSE
      ),
      Away = mapply(team_cell, df$away, df$away_crest,
        MoreArgs = list(align = "left"), USE.NAMES = FALSE
      ),
      Babak = sprintf(
        "<span style='background:rgba(138,21,56,0.25); color:var(--gold); padding:2px 8px; border-radius:4px; font-size:11px; font-weight:700;'>%s</span>",
        fmt_group(ifelse(is.na(df$group) | !nzchar(df$group), df$stage, df$group))
      )
    )

    datatable(
      tbl,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(targets = c(0, 2, 4), className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })


  # Hero Match Card
  # Logika: LIVE > paling-dekat-ke-now (FINISHED terbaru atau SCHEDULED terdekat)
  output$hero_match <- renderUI({
    m <- matches_r()
    now <- Sys.time()

    # Debug log
    message(sprintf(
      "[HeroMatch] now=%s | total=%d | status: %s",
      format(now, "%Y-%m-%d %H:%M:%S"),
      nrow(m),
      if (nrow(m) > 0) {
        paste(sprintf("%s=%d", names(table(m$status)), as.integer(table(m$status))),
          collapse = ", "
        )
      } else {
        "-"
      }
    ))

    if (nrow(m) == 0) {
      return(div(
        class = "hero-match-card",
        h3("Data turnamen belum tersedia"),
        p("Coba refresh beberapa saat lagi.")
      ))
    }

    # 1. LIVE (prioritas mutlak)
    live <- m |> filter(status %in% c("IN_PLAY", "PAUSED", "LIVE"))
    if (nrow(live) > 0) {
      sel <- live |> slice(1)
      badge_cls <- "hero-match-status-badge live"
      badge_txt <- "LIVE NOW"
      mnt <- sel$minute %||% NA_character_
      bottom_info <- if (!is.na(mnt) && nzchar(mnt)) {
        sprintf("Menit %s'", mnt)
      } else if (identical(sel$status, "PAUSED")) {
        "Half Time"
      } else {
        "Sedang Berlangsung"
      }
      message(sprintf("[HeroMatch] -> LIVE: %s vs %s", sel$home, sel$away))
    } else {
      # 2. Bandingkan: FINISHED terakhir vs SCHEDULED terdekat -> pilih yang lebih dekat ke now
      finished <- m |>
        filter(status == "FINISHED") |>
        arrange(desc(utcDate))
      upcoming <- m |>
        filter(status %in% c("SCHEDULED", "TIMED"), utcDate >= now) |>
        arrange(utcDate)

      pick_finished <- if (nrow(finished) > 0) finished |> slice(1) else NULL
      pick_upcoming <- if (nrow(upcoming) > 0) upcoming |> slice(1) else NULL

      # Hitung jarak waktu (detik)
      diff_fin <- if (!is.null(pick_finished)) {
        as.numeric(difftime(now, pick_finished$utcDate, units = "secs"))
      } else {
        Inf
      }
      diff_up <- if (!is.null(pick_upcoming)) {
        as.numeric(difftime(pick_upcoming$utcDate, now, units = "secs"))
      } else {
        Inf
      }

      if (is.null(pick_finished) && is.null(pick_upcoming)) {
        # Tidak ada apa-apa → pakai match pertama (pembukaan)
        sel <- m |>
          arrange(utcDate) |>
          slice(1)
        badge_cls <- "hero-match-status-badge"
        badge_txt <- "PEMBUKAAN"
        bottom_info <- sel$date_label
        message(sprintf("[HeroMatch] -> PEMBUKAAN: %s vs %s", sel$home, sel$away))
      } else if (diff_fin <= diff_up) {
        # FINISHED lebih dekat
        sel <- pick_finished
        badge_cls <- "hero-match-status-badge"
        badge_txt <- "HASIL TERAKHIR"
        bottom_info <- sel$date_label
        message(sprintf(
          "[HeroMatch] -> FINISHED: %s %s %s (%.1f jam lalu)",
          sel$home, sel$score_label, sel$away, diff_fin / 3600
        ))
      } else {
        # SCHEDULED lebih dekat → tampilkan countdown
        sel <- pick_upcoming
        badge_cls <- "hero-match-status-badge"
        badge_txt <- "AKAN DATANG"
        # Format countdown jam/menit
        hrs <- floor(diff_up / 3600)
        mins <- floor((diff_up %% 3600) / 60)
        countdown <- if (diff_up < 60) {
          "Beberapa saat lagi"
        } else if (hrs == 0) {
          sprintf("Mulai dalam %d menit", mins)
        } else if (hrs < 24) {
          sprintf("Mulai dalam %d jam %d menit", hrs, mins)
        } else {
          days <- floor(hrs / 24)
          sprintf("Mulai dalam %d hari %d jam", days, hrs %% 24)
        }
        bottom_info <- paste(sel$date_label, "·", countdown)
        message(sprintf(
          "[HeroMatch] -> UPCOMING: %s vs %s (%.1f jam lagi)",
          sel$home, sel$away, diff_up / 3600
        ))
      }
    }


    # Venue (optional, dari API). Jika NA / kosong -> skip
    venue_txt <- sel$venue %||% NA_character_
    venue_node <- if (!is.na(venue_txt) && nzchar(venue_txt)) {
      div(
        style = "font-size:11px; color:var(--text-muted); letter-spacing:0.5px; margin-bottom:14px; display:flex; align-items:center; gap:6px;",
        mat_icon("stadium"),
        tags$span(venue_txt)
      )
    } else {
      NULL
    }

    div(
      class = "hero-match-card",
      span(class = badge_cls, badge_txt),
      div(
        style = "font-size:11px; color:var(--text-muted); letter-spacing:1px; margin-bottom:6px;",
        sprintf("MATCHDAY %s - %s", sel$matchday %||% "-", fmt_group(sel$group %||% sel$stage))
      ),
      venue_node,
      div(
        style = "display:flex; align-items:center; justify-content:space-between; gap:20px;",
        div(
          style = "flex:1; text-align:center;",
          tags$img(
            src = sel$home_crest,
            style = "width:80px; height:80px; object-fit:contain; margin:0 auto;",
            onerror = "this.style.display='none'"
          ),
          div(style = "font-size:18px; font-weight:800; margin-top:10px;", sel$home)
        ),
        div(
          style = "font-size:42px; font-weight:900; color:var(--gold); padding:0 16px;",
          sel$score_label
        ),
        div(
          style = "flex:1; text-align:center;",
          tags$img(
            src = sel$away_crest,
            style = "width:80px; height:80px; object-fit:contain; margin:0 auto;",
            onerror = "this.style.display='none'"
          ),
          div(style = "font-size:18px; font-weight:800; margin-top:10px;", sel$away)
        )
      ),
      div(
        style = "margin-top:16px; text-align:center; color:var(--text-muted); font-size:12px;",
        bottom_info
      )
    )
  })


  output$group_selector <- renderUI({
    st <- standings_r()
    if (is.null(st)) {
      return(NULL)
    }
    groups <- sort(unique(st$group))
    # Label "Grup A" tapi value tetap raw "GROUP_A" untuk filter
    choices <- setNames(groups, fmt_group(groups))
    selectInput("group_sel", NULL,
      choices = choices, selected = groups[1], width = "200px"
    )
  })


  output$dt_standings <- renderDT({
    st <- standings_r()
    if (is.null(st)) {
      return(datatable(tibble(Pesan = "Menunggu pembaruan data dari server (API Rate Limit)..."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE),
        rownames = FALSE, class = "compact stripe"
      ))
    }
    req(input$group_sel)
    df <- st |>
      filter(group == input$group_sel) |>
      transmute(
        Pos = position,
        Tim = team,
        M = playedGames,
        W = won, D = draw, L = lost,
        GF = goalsFor, GA = goalsAgainst, GD = goalDifference,
        Pts = points,
        Form = form
      )
    datatable(df,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      rownames = FALSE, class = "compact stripe"
    )
  })

  output$quick_stats <- renderUI({
    m <- matches_r()
    if (nrow(m) == 0) {
      return(NULL)
    }
    finished <- m |> filter(status == "FINISHED")
    total_goals <- sum(c(finished$home_score, finished$away_score), na.rm = TRUE)
    n_done <- nrow(finished)
    n_total <- nrow(m)
    avg_goals <- if (n_done > 0) round(total_goals / n_done, 2) else 0
    tagList(
      stat_row("sports_soccer", "Total Gol", total_goals),
      stat_row("track_changes", "Rata-rata Gol/Match", avg_goals),
      stat_row("check_circle", "Pertandingan Selesai", paste0(n_done, " / ", n_total)),
      stat_row(
        "event", "Matchday Saat Ini",
        max(m$matchday[m$status %in% c("FINISHED", "IN_PLAY", "LIVE")], 0, na.rm = TRUE)
      )
    )
  })

  # PAGE: STADION & CUACA
  page_stadion <- function() {
    tagList(
      h1(class = "section-title", "Stadion & Cuaca Live"),
      p(
        style = "color: var(--text-muted); margin-bottom:24px;",
        "16 stadion tuan rumah di Amerika Serikat, Kanada & Meksiko, plus prakiraan cuaca real-time."
      ),
      fluidRow(
        column(8, div(
          class = "glass-card",
          card_heading("map", "Peta Stadion"),
          leafletOutput("stadion_map", height = "480px")
        )),
        column(4, div(
          class = "glass-card",
          card_heading("partly_cloudy_day", "Cuaca Stadion"),
          selectInput("stadion_sel", "Pilih Stadion",
            choices = setNames(venues$id, venues$name)
          ),
          uiOutput("weather_box")
        ))
      ),
      div(
        class = "glass-card",
        card_heading("list_alt", "Daftar Stadion"),
        DTOutput("dt_venues")
      )
    )
  }

  output$stadion_map <- renderLeaflet({
    if (nrow(venues) == 0) {
      return(NULL)
    }
    leaflet(venues) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      setView(lng = -98, lat = 39, zoom = 3) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 9, color = "#FFD700", weight = 2,
        fillColor = "#8a1538", fillOpacity = 0.85,
        label = ~name,
        popup = ~ sprintf(
          "<b style='color:#8a1538'>%s</b><br/>%s, %s<br/>Kapasitas: <b>%s</b>",
          name, city, country, format(capacity, big.mark = ",")
        )
      )
  })

  output$weather_box <- renderUI({
    req(input$stadion_sel)
    v <- venues |> filter(id == as.integer(input$stadion_sel))
    if (nrow(v) == 0) {
      return(NULL)
    }
    w <- get_weather(v$lat, v$lon)
    if (is.null(w)) {
      return(div(
        style = "padding:20px; text-align:center; color:var(--text-muted);",
        mat_icon("cloud_off", "icon-lg"),
        div("Data cuaca belum tersedia")
      ))
    }
    wi <- weather_info(w$weather_code)
    tagList(
      div(
        style = "padding:16px; background:var(--surface-high); border-radius:10px; margin-top:10px;",
        div(
          style = "font-size:13px; color:var(--text-muted);",
          paste0(v$city, ", ", v$country)
        ),
        div(
          style = "display:flex; align-items:center; gap:14px; margin:8px 0;",
          mat_icon(wi$icon, "icon-xl"),
          div(
            style = "font-size:42px; font-weight:900; color:var(--gold);",
            sprintf("%s°C", w$temperature_2m %||% "-")
          )
        ),
        div(style = "font-size:14px; color:var(--text-primary);", wi$label),
        tags$hr(),
        stat_row("water_drop", "Kelembapan", paste0(w$relative_humidity_2m %||% "-", "%")),
        stat_row("air", "Angin", paste0(round(w$wind_speed_10m %||% 0, 1), " km/j"))
      )
    )
  })

  output$dt_venues <- renderDT({
    venues |>
      transmute(
        Stadion = name, Kota = city, Negara = country,
        Kapasitas = format(capacity, big.mark = ",")
      ) |>
      datatable(
        options = list(pageLength = 8, dom = "ftp"),
        rownames = FALSE, class = "compact stripe"
      )
  })

  # PAGE: PREDICTOR HUB (Monte Carlo Championship Predictor)
  page_predictor <- function() {
    tagList(
      h1(class = "section-title", "Predictor Hub - Championship Odds"),
      p(
        style = "color: var(--text-muted); margin-bottom:24px;",
        "Prediksi peluang juara berbasis simulasi Monte Carlo. Klik tombol di bawah untuk menjalankan simulasi."
      ),
      uiOutput("predict_banner"),
      # Opsi A+C: Panel kontrol simulasi (tombol + pilihan jumlah)
      div(
        class = "glass-card",
        style = "background: linear-gradient(135deg, rgba(138,21,56,0.18), rgba(255,215,0,0.08)); overflow:visible; z-index:10; position:relative;",
        div(
          style = "display:flex; align-items:center; gap:16px; flex-wrap:wrap;",
          div(
            style = "flex:1; min-width:200px;",
            tags$div(
              style = "font-weight:800; font-size:14px; color:var(--gold); margin-bottom:4px;",
              mat_icon("settings"),
              tags$span(style = "vertical-align:middle; margin-left:6px;", "Kontrol Simulasi")
            ),
            tags$div(
              style = "color: var(--text-muted); font-size: 11px;",
              "Pilih jumlah simulasi lalu klik Jalankan. Semakin banyak simulasi, semakin akurat hasilnya."
            )
          ),
          div(
            style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
            div(
              style = "width: 280px; min-width: 280px;",
              selectInput("sim_count", NULL,
                choices = c(
                  "200x - Cepat"       = 200,
                  "500x - Rekomendasi" = 500,
                  "1000x - Presisi"    = 1000
                ),
                selected = 500,
                width = "100%"
              )
            ),
            actionButton(
              "run_sim_btn",
              label = tagList(mat_icon("play_arrow"), "Jalankan Simulasi"),
              class = "btn-sim-run",
              style = "background:linear-gradient(135deg, #8a1538, #c41e3a); color:#fff; font-weight:800; border:none; padding:10px 24px; border-radius:8px; font-size:14px; cursor:pointer; transition: all 0.3s ease; box-shadow: 0 4px 15px rgba(138,21,56,0.4);"
            )
          )
        )
      ),
      # ---- Hasil simulasi (hanya muncul setelah tombol diklik) ----
      uiOutput("predict_results_area")
    )
  }

  # Area hasil: tampilkan placeholder jika belum dijalankan
  output$predict_results_area <- renderUI({
    if (!isTRUE(sim_has_run())) {
      return(div(
        class = "glass-card",
        style = "text-align:center; padding:60px 20px;",
        mat_icon("query_stats", "icon-xl"),
        tags$div(
          style = "font-size:18px; font-weight:700; color:var(--text-muted); margin-top:16px;",
          "Klik \"Jalankan Simulasi\" untuk melihat prediksi"
        ),
        tags$div(
          style = "font-size:13px; color:var(--text-muted); margin-top:8px; max-width:500px; margin-left:auto; margin-right:auto;",
          "Simulasi Monte Carlo akan mensimulasikan seluruh sisa turnamen ratusan kali untuk menghitung probabilitas setiap tim menjadi juara."
        )
      ))
    }

    tagList(
      fluidRow(
        column(7, div(
          class = "glass-card",
          card_heading("emoji_events", "Top 15 - Peluang Juara"),
          plotlyOutput("predict_champ_plot", height = "480px")
        )),
        column(5, div(
          class = "glass-card",
          card_heading("trending_up", "Insight Cepat"),
          uiOutput("predict_insights")
        ))
      ),
      div(
        class = "glass-card",
        card_heading("table_chart", "Probabilitas Lengkap per Babak"),
        p(
          style = "color: var(--text-muted); font-size: 12px; margin-bottom: 12px;",
          "Setiap baris menampilkan peluang tim mencapai babak: Lolos Grup -> 32 Besar -> 16 Besar -> Perempat Final (QF) -> Semi Final (SF) -> Final -> JUARA. Klik header untuk sortir."
        ),
        DTOutput("dt_predictions")
      )
    )
  })

  # Banner: jumlah match selesai + tombol re-run
  output$predict_banner <- renderUI({
    m <- matches_r()
    n_fin <- if (nrow(m) > 0) sum(m$status == "FINISHED", na.rm = TRUE) else 0
    n_total <- nrow(m)

    if (n_fin == 0) {
      return(div(
        class = "glass-card",
        style = "background: linear-gradient(135deg, rgba(255,180,0,0.15), rgba(138,21,56,0.15)); border: 1px solid rgba(255,180,0,0.3);",
        div(
          style = "display:flex; align-items:center; gap:14px;",
          mat_icon("hourglass_empty", "icon-xl"),
          div(
            tags$div(
              style = "font-weight:700; color: var(--gold); font-size: 15px;",
              "Prediksi akan aktif setelah Matchday 1"
            ),
            tags$div(
              style = "color: var(--text-muted); font-size: 13px; margin-top: 4px;",
              "Model dinamis mempelajari kekuatan tim dari hasil pertandingan. Saat belum ada match selesai, semua 48 tim dianggap setara."
            )
          )
        )
      ))
    }

    div(
      class = "glass-card",
      style = "background: linear-gradient(135deg, rgba(0,230,118,0.12), rgba(138,21,56,0.12)); border: 1px solid rgba(0,230,118,0.3);",
      div(
        style = "display:flex; align-items:center; gap:14px; flex-wrap:wrap;",
        mat_icon("check_circle", "icon-xl"),
        div(
          style = "flex:1; min-width:200px;",
          tags$div(
            style = "font-weight:700; color: #00e676; font-size: 15px;",
            sprintf(
              "Data tersedia - %d / %d pertandingan sudah selesai",
              n_fin, n_total
            )
          ),
          tags$div(
            style = "color: var(--text-muted); font-size: 12px; margin-top: 4px;",
            "Model: Poisson + Bayesian smoothing + Strength-of-Schedule | On-demand Monte Carlo Simulation"
          )
        )
      )
    )
  })

  # Plot Top 15 peluang juara
  output$predict_champ_plot <- renderPlotly({
    pred <- predictions_r()
    if (is.null(pred) || nrow(pred) == 0) {
      return(plotly_empty() |>
        layout(
          title = list(text = "Data prediksi belum tersedia", font = list(color = "#aaa")),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ))
    }

    top <- pred |>
      dplyr::arrange(dplyr::desc(p_champion)) |>
      head(15) |>
      dplyr::mutate(
        team = factor(team, levels = rev(team)),
        pct = round(p_champion * 100, 1),
        lbl = paste0(pct, "%")
      )

    # Color scale: gold untuk top 3, gradient untuk sisanya
    colors <- ifelse(seq_len(nrow(top)) <= 3, "#FFD700",
      ifelse(seq_len(nrow(top)) <= 7, "#FFA500", "#8a1538")
    )
    colors <- rev(colors) # karena rev(team)

    plot_ly(top,
      x = ~pct, y = ~team, type = "bar", orientation = "h",
      text = ~lbl, textposition = "outside",
      textfont = list(color = "#fff", size = 12),
      marker = list(color = colors, line = list(color = "rgba(255,215,0,0.5)", width = 1)),
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Peluang Juara: %{x}%<extra></extra>"
      )
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#e0e0e0", family = "Inter"),
        xaxis = list(
          title = "Probabilitas Juara (%)",
          gridcolor = "rgba(255,255,255,0.08)",
          zerolinecolor = "rgba(255,255,255,0.15)"
        ),
        yaxis = list(title = "", tickfont = list(size = 11)),
        margin = list(l = 110, r = 50, t = 20, b = 50)
      ) |>
      config(displayModeBar = FALSE)
  })

  # Insight Cards (Favorit, Dark Horse, Underperformer)
  output$predict_insights <- renderUI({
    pred <- predictions_r()
    if (is.null(pred) || nrow(pred) == 0) {
      return(div(
        style = "padding:20px; text-align:center; color:var(--text-muted);",
        mat_icon("query_stats", "icon-lg"),
        div("Menunggu data pertandingan selesai...")
      ))
    }

    fav <- pred |>
      dplyr::arrange(dplyr::desc(p_champion)) |>
      head(1)
    # Dark Horse: peringkat 6-15 dengan peluang QF cukup tinggi
    dark <- pred |>
      dplyr::arrange(dplyr::desc(p_champion)) |>
      dplyr::slice(6:15) |>
      dplyr::arrange(dplyr::desc(p_qf)) |>
      head(1)
    # Tim dengan peluang lolos grup tertinggi (favorit fase grup)
    grp_king <- pred |>
      dplyr::arrange(dplyr::desc(p_r32)) |>
      head(1)

    insight_card <- function(icon, color, title, team, value_lbl, sub) {
      div(
        style = "padding:14px; margin-bottom:12px; background:var(--surface-high); border-radius:10px; border: 1px solid rgba(255,255,255,0.06);",
        div(
          style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
          mat_icon(icon, ""),
          tags$span(style = sprintf("font-size:11px; font-weight:800; letter-spacing:1px; color:%s;", color), title)
        ),
        div(
          style = "display:flex; align-items:center; gap:10px;",
          if (!is.null(team$crest) && !is.na(team$crest)) {
            tags$img(src = team$crest, style = "width:32px; height:32px; object-fit:contain;")
          } else {
            NULL
          },
          div(
            tags$div(style = "font-weight:800; font-size:15px;", team$team),
            tags$div(style = "color:var(--text-muted); font-size:11px;", sub)
          ),
          div(
            style = sprintf("margin-left:auto; font-size:22px; font-weight:900; color:%s;", color),
            value_lbl
          )
        )
      )
    }

    tagList(
      insight_card("emoji_events", "#FFD700", "FAVORIT JUARA",
        fav,
        paste0(round(fav$p_champion * 100, 1), "%"),
        sub = "Probabilitas tertinggi memenangkan trofi"
      ),
      insight_card("rocket_launch", "#00e676", "DARK HORSE",
        dark,
        paste0(round(dark$p_qf * 100, 1), "%"),
        sub = sprintf("Underrated, peluang lolos QF %s%%", round(dark$p_qf * 100, 1))
      ),
      insight_card("verified", "#4fc3f7", "RAJA FASE GRUP",
        grp_king,
        paste0(round(grp_king$p_r32 * 100, 1), "%"),
        sub = "Peluang lolos ke 32 Besar tertinggi"
      )
    )
  })

  # Tabel Probabilitas Lengkap
  output$dt_predictions <- renderDT({
    pred <- predictions_r()
    if (is.null(pred) || nrow(pred) == 0) {
      return(datatable(tibble(Pesan = "Data prediksi belum tersedia")))
    }

    # Bar inline berwarna untuk persentase
    pct_bar <- function(p, color = "#FFD700") {
      pct <- round(p * 100, 1)
      bar_w <- max(2, pct) # minimal 2% biar visible
      sprintf(
        "<div style='position:relative; width:100%%; height:22px; background:rgba(255,255,255,0.06); border-radius:4px; overflow:hidden;'>
           <div style='position:absolute; left:0; top:0; height:100%%; width:%.1f%%; background:linear-gradient(90deg, %s33, %s); border-radius:4px;'></div>
           <div style='position:relative; text-align:center; line-height:22px; font-weight:700; font-size:11px; color:#fff;'>%.1f%%</div>
         </div>",
        bar_w, color, color, pct
      )
    }

    team_with_logo <- function(team, crest) {
      has_crest <- !is.null(crest) && !is.na(crest) && nzchar(as.character(crest))
      img <- if (has_crest) {
        sprintf(
          "<img src='%s' style='width:22px; height:22px; object-fit:contain; vertical-align:middle; margin-right:6px;' onerror=\"this.style.display='none'\"/>",
          crest
        )
      } else {
        ""
      }
      sprintf("<div style='text-align:left;'>%s<b>%s</b></div>", img, team)
    }

    # Siapkan vector crest & group dengan aman (kolom mungkin NULL jika join gagal)
    crest_vec <- if ("crest" %in% names(pred)) pred$crest else rep(NA_character_, nrow(pred))
    group_vec <- if ("group" %in% names(pred)) pred$group else rep(NA_character_, nrow(pred))

    tbl <- tibble(
      Tim = mapply(team_with_logo, pred$team, crest_vec, USE.NAMES = FALSE),
      Grup = vapply(group_vec, fmt_group, character(1)),
      `Lolos Grup` = sapply(pred$p_r32, pct_bar, color = "#4fc3f7"),
      `16 Besar` = sapply(pred$p_r16, pct_bar, color = "#26a69a"),
      QF = sapply(pred$p_qf, pct_bar, color = "#9c27b0"),
      SF = sapply(pred$p_sf, pct_bar, color = "#ff6f00"),
      Final = sapply(pred$p_final, pct_bar, color = "#ef5350"),
      JUARA = sapply(pred$p_champion, pct_bar, color = "#FFD700"),
      # kolom numerik tersembunyi untuk sortir benar
      .p_r32 = pred$p_r32,
      .p_r16 = pred$p_r16,
      .p_qf = pred$p_qf,
      .p_sf = pred$p_sf,
      .p_final = pred$p_final,
      .p_champ = pred$p_champion
    )


    datatable(
      tbl,
      escape = FALSE,
      options = list(
        pageLength = 15,
        dom = "ftp",
        order = list(list(7, "desc")),
        columnDefs = list(
          list(targets = 0:1, className = "dt-left"),
          list(targets = 2:7, className = "dt-center", width = "100px"),
          list(targets = 8:13, visible = FALSE) # kolom .p_* untuk sortir
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  # PAGE: AI ASSISTANT
  chat_history <- reactiveVal(list(
    list(
      role = "ai",
      content = "Halo! Saya asisten World Cup 2026. Tanya saya tentang jadwal, prediksi, statistik, atau sejarah Piala Dunia. Contoh: \"Siapa top scorer saat ini?\" atau \"Kapan final WC 2026?\""
    )
  ))

  # Flag: sedang menunggu reply AI?
  ai_busy <- reactiveVal(FALSE)

  page_ai <- function() {
    tagList(
      h1(class = "section-title", "World Cup AI Assistant"),
      p(
        style = "color: var(--text-muted); margin-bottom:24px;",
        "Tanya apa saja tentang FIFA World Cup 2026 - didukung oleh AI MiniMax-M3."
      ),
      fluidRow(
        column(
          8,
          div(
            class = "chat-container",
            div(
              class = "chat-messages", id = "chat_box",
              uiOutput("chat_ui")
            ),
            div(
              class = "chat-input-area",
              textInput("chat_input", NULL,
                placeholder = "Tulis pertanyaan Anda lalu tekan Enter...",
                width = "100%"
              ),
              actionButton("chat_send",
                label = tagList(mat_icon("send"), "Kirim"),
                class = "chat-send-btn"
              )
            )
          )
        ),
        column(4, div(
          class = "glass-card",
          card_heading("tips_and_updates", "Pertanyaan Cepat"),
          actionButton("q1", "Siapa top scorer?",
            class = "action-button",
            style = "width:100%; margin-bottom:8px;"
          ),
          actionButton("q2", "Klasemen Grup A?",
            class = "action-button",
            style = "width:100%; margin-bottom:8px;"
          ),
          actionButton("q3", "Jadwal hari ini?",
            class = "action-button",
            style = "width:100%; margin-bottom:8px;"
          ),
          actionButton("q4", "Prediksi juara WC 2026?",
            class = "action-button",
            style = "width:100%; margin-bottom:8px;"
          )
        ))
      )
    )
  }

  # Render bubble (ada typing-dots khusus role == "typing")
  output$chat_ui <- renderUI({
    msgs <- chat_history()
    tagList(
      lapply(msgs, function(m) {
        if (identical(m$role, "typing")) {
          div(
            class = "chat-bubble ai",
            div(
              class = "typing-dots",
              tags$span(), tags$span(), tags$span()
            )
          )
        } else {
          div(class = paste0("chat-bubble ", m$role), shiny::markdown(m$content))
        }
      })
    )
  })

  # Auto-scroll ke bawah setiap kali history berubah
  observeEvent(chat_history(),
    {
      session$sendCustomMessage("scrollChat", list())
    },
    ignoreInit = FALSE
  )

  build_context <- function() {
    m <- matches_r()
    sc <- scorers_r()
    st <- standings_r()
    ctx <- c()
    if (!is.null(sc) && nrow(sc) > 0) {
      ctx <- c(
        ctx, "TOP SCORER:",
        paste(sprintf("- %s (%s) - %d gol", sc$player, sc$team, sc$goals)[seq_len(min(5, nrow(sc)))],
          collapse = "\n"
        )
      )
    }
    if (nrow(m) > 0) {
      upcoming <- m |>
        filter(status %in% c("SCHEDULED", "TIMED")) |>
        arrange(utcDate) |>
        head(3)
      if (nrow(upcoming) > 0) {
        ctx <- c(
          ctx, "\n3 PERTANDINGAN BERIKUTNYA:",
          paste(
            sprintf(
              "- %s vs %s (%s)",
              upcoming$home, upcoming$away, upcoming$date_label
            ),
            collapse = "\n"
          )
        )
      }
      finished <- m |>
        filter(status == "FINISHED") |>
        arrange(desc(utcDate)) |>
        head(3)
      if (nrow(finished) > 0) {
        ctx <- c(
          ctx, "\n3 HASIL TERAKHIR:",
          paste(
            sprintf(
              "- %s %s %s",
              finished$home, finished$score_label, finished$away
            ),
            collapse = "\n"
          )
        )
      }
    }
    if (!is.null(st) && nrow(st) > 0) {
      ctx <- c(
        ctx, "\nKLASEMEN (TOP 5 PTS):",
        paste(
          sprintf(
            "- %s: %d pts (%dW %dD %dL)",
            st$team, st$points, st$won, st$draw, st$lost
          )[seq_len(min(5, nrow(st)))],
          collapse = "\n"
        )
      )
    }
    paste(ctx, collapse = "\n")
  }

  # Pengiriman pesan halus: 2 fase (push user + typing, lalu fetch)
  pending_question <- reactiveVal(NULL)

  send_msg <- function(text) {
    if (!nzchar(text) || isTRUE(ai_busy())) {
      return()
    }
    ai_busy(TRUE)

    # Disable input & button
    shinyjs::disable("chat_send")
    shinyjs::disable("chat_input")
    shinyjs::disable("q1")
    shinyjs::disable("q2")
    shinyjs::disable("q3")
    shinyjs::disable("q4")

    # Push: user message + typing indicator
    hist <- chat_history()
    hist[[length(hist) + 1]] <- list(role = "user", content = text)
    hist[[length(hist) + 1]] <- list(role = "typing", content = "")
    chat_history(hist)
    updateTextInput(session, "chat_input", value = "")

    # Tunda fetch via reactiveVal -> observeEvent (next tick) supaya UI sempat render typing dots
    pending_question(list(text = text, stamp = Sys.time()))
  }

  observeEvent(pending_question(),
    {
      pq <- pending_question()
      if (is.null(pq)) {
        return()
      }

      ctx <- build_context()
      reply <- ask_ai(pq$text, ctx)

      # Ganti typing bubble dengan jawaban
      hist <- chat_history()
      hist[[length(hist)]] <- list(role = "ai", content = reply)
      chat_history(hist)

      # Re-enable UI
      shinyjs::enable("chat_send")
      shinyjs::enable("chat_input")
      shinyjs::enable("q1")
      shinyjs::enable("q2")
      shinyjs::enable("q3")
      shinyjs::enable("q4")
      ai_busy(FALSE)
      pending_question(NULL)
      session$sendCustomMessage("focusChat", list())
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(input$chat_send, send_msg(input$chat_input))
  observeEvent(input$q1, send_msg("Siapa top scorer FIFA World Cup 2026 saat ini?"))
  observeEvent(input$q2, send_msg("Bagaimana klasemen Grup A saat ini?"))
  observeEvent(input$q3, send_msg("Apa saja jadwal pertandingan terdekat?"))
  observeEvent(input$q4, send_msg("Menurut data sekarang, tim mana yang paling berpeluang juara WC 2026?"))
}


# RUN
shinyApp(ui, server)