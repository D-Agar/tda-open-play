# R script to generate the mapper data and the full data
library(tidyverse)
source("examples/open-play/helpers.R")

# cleaned data from Ballou
android <- read_csv("data/open-play/clean/android.csv.gz")
metadata <- read_csv("data/open-play/clean/game_metadata.csv.gz")
ios <- read_csv("data/open-play/clean/ios.csv.gz")
nintendo <- read_csv("data/open-play/clean/nintendo.csv.gz")
simon <- read_csv("data/open-play/clean/simon.csv.gz")
steam <- read_csv("data/open-play/clean/steam.csv.gz")
steam_visibility <- read_csv(
  "data/open-play/raw/telemetry_steam_account_linking_raw.csv.gz"
)
biweekly <- read_csv("data/open-play/clean/survey_biweekly_final.csv.gz")
daily <- read_csv("data/open-play/clean/survey_daily_final.csv.gz", guess_max = 10000)
intake_participants <- read_csv("data/open-play/clean/survey_intake_participants_final.csv.gz", guess_max = 34000)
participant_coords <- read_csv("data/open-play/clean/participant_coords.csv.gz")
time_use <- read_csv("data/open-play/clean/timeuse.csv.gz") |>
  filter(quality_flag == "Good quality")
xbox <- read_csv("data/open-play/clean/xbox.csv.gz")

# get the mapper data
# merge with the local time zone offsets from intake participants
# Note: We keep country and local_timezone for DST-aware conversion
tz_map <- intake_participants |>
  mutate(
    pid,
    country,
    local_timezone,
    .keep = "none"
  ) |>
  distinct(pid, .keep_all = TRUE)

# aggregate data at each level of granularity (session, hourly, daily)
# session level (Nintendo, Xbox, Steam already at session level)
session_telemetry <- bind_rows(
  xbox |> mutate(platform = "Xbox"),
  nintendo |> mutate(platform = "Nintendo"),
) |>
  left_join(tz_map, by = "pid") |>
  filter(!is.na(local_timezone)) |>
  mutate(
    # Calculate DST-aware offset for each timestamp
    offset_start = get_dst_offset(session_start, country, local_timezone),
    offset_end = get_dst_offset(session_end, country, local_timezone),
    # Add offset to get local time values (keeping UTC label for compatibility)
    start_local = session_start + offset_start,
    end_local = session_end + offset_end,
    duration_min = as.numeric(difftime(
      session_end,
      session_start,
      units = "mins"
    ))
  ) |>
  filter(
    !is.na(session_start),
    !is.na(session_end),
    session_end > session_start,
    duration_min >= 1
  )

# Hourly from sessions (Nintendo + Xbox)
# Join with timezone map and calculate DST-aware offsets
# Note: start_local/end_local have UTC labels but local time VALUES (for hour extraction)
sessions_telemetry <- bind_rows(
  xbox |> mutate(platform = "Xbox"),
  nintendo |> mutate(platform = "Nintendo")
) |>
  mutate(pid = as.character(pid)) |>
  left_join(tz_map, by = "pid") |>
  filter(!is.na(local_timezone)) |>
  mutate(
    # Calculate DST-aware offset for each timestamp
    offset_start = get_dst_offset(session_start, country, local_timezone),
    offset_end = get_dst_offset(session_end, country, local_timezone),
    # Add offset to get local time values (keeping UTC label for compatibility)
    start_local = session_start + offset_start,
    end_local = session_end + offset_end,
    duration_min = as.numeric(difftime(
      session_end,
      session_start,
      units = "mins"
    ))
  ) |>
  filter(
    !is.na(session_start),
    !is.na(session_end),
    session_end > session_start,
    duration_min >= 1
  )

hourly_from_sessions <- sessions_telemetry |>
  filter(!is.na(start_local), !is.na(end_local)) |>
  mutate(
    h0_local = floor_date(start_local, "hour"),
    h1_local = floor_date(end_local - seconds(1), "hour"),
    n_hours = as.integer(difftime(h1_local, h0_local, units = "hours")) + 1
  ) |>
  filter(!is.na(n_hours), n_hours > 0) |>
  tidyr::uncount(n_hours, .remove = FALSE, .id = "k") |>
  mutate(
    hour_start_local = h0_local + hours(k - 1),
    minutes = pmax(
      0,
      as.numeric(difftime(
        pmin(end_local, hour_start_local + hours(1)),
        pmax(start_local, hour_start_local),
        units = "mins"
      ))
    ),
    # Convert back to UTC (this preserves the instant, just changes label)
    hour_start_utc = with_tz(hour_start_local, tzone = "UTC")
  ) |>
  select(pid, platform, hour_start_local, hour_start_utc, minutes)

# --- Hourly from Steam (already hourly) -------------------------------------
hourly_from_steam <- steam |>
  select(pid, datetime_hour_start, minutes) |>
  mutate(pid = as.character(pid)) |>
  left_join(tz_map, by = "pid") |>
  filter(!is.na(local_timezone)) |>
  mutate(
    platform = "Steam",
    hour_start_utc = datetime_hour_start,
    # Calculate DST-aware offset and add to get local time values
    offset = get_dst_offset(datetime_hour_start, country, local_timezone),
    hour_start_local = datetime_hour_start + offset
  ) |>
  select(pid, platform, hour_start_local, hour_start_utc, minutes)

hourly_telemetry <- bind_rows(hourly_from_sessions, hourly_from_steam)

# Daily (Nintendo + Xbox + Steam; collapse hourly to days)
daily_telemetry <- hourly_telemetry |>
  mutate(
    day_local = as.Date(hour_start_local),
  ) |>
  group_by(pid, platform, day_local) |>
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop") |>
  bind_rows(ios) |>
  bind_rows(android)

daily_ios_android <- bind_rows(
  ios |>
    transmute(
      pid = as.character(pid),
      platform = "iOS",
      day_local = as.Date(day_local),
      minutes
    ),
  android |>
    transmute(
      pid = as.character(pid),
      platform = "Android",
      day_local = as.Date(day_local),
      minutes
    )
)

daily_all <- bind_rows(daily_telemetry, daily_ios_android) |>
  group_by(pid, day_local) |>
  summarise(total_minutes = sum(minutes, na.rm = TRUE), .groups = "drop")

# Weekly
weekly_all <- daily_telemetry |>
  mutate(
    week = floor_date(day_local, "week")
  ) |>
  group_by(pid, platform, week) |>
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop")

telemetry_spans <- daily_telemetry |>
  group_by(pid, platform) |>
  summarise(
    telemetry_start = min(day_local, na.rm = TRUE),
    # end of last hour bin
    telemetry_end = max(day_local, na.rm = TRUE) + hours(1),
    week = floor_date(telemetry_end, "week"),
    n_weeks = as.integer(difftime(
      telemetry_end,
      telemetry_start,
      units = "weeks"
    )) +
      1,
    .groups = "drop"
  )

platforms <- c("Steam", "Nintendo", "Xbox", "Android", "iOS")

# study lasts 12 weeks
# start at 0 for first day
study_duration_days <- 83

daily_start_dates <- daily |>
  group_by(pid) |>
  mutate(first_wave = min(wave)) |>
  ungroup() |>
  filter(wave == first_wave) |>
  select(pid, daily_start_date = date, first_wave)

biweekly_start_dates <- biweekly |>
  group_by(pid) |>
  mutate(first_wave = min(wave)) |>
  ungroup() |>
  filter(wave == first_wave) |>
  select(pid, biweekly_start_date = date, first_wave)

survey_start_dates <- biweekly_start_dates |>
  full_join(daily_start_dates, by = join_by(pid)) |>
  mutate(started_study = as.Date(pmin(biweekly_start_date, daily_start_date, na.rm = TRUE))) |>
  select(pid, started_study)

last_daily_survey <- daily |>
  group_by(pid) |>
  summarise(last_daily_survey = max(date, na.rm = TRUE))

last_biweekly_survey <- biweekly |>
  group_by(pid) |>
  summarise(last_biweekly_survey = max(date, na.rm = TRUE))

last_daily_telemetry <- daily_telemetry |>
  group_by(pid) |>
  summarise(last_ping = max(day_local, na.rm = TRUE))

participant_windows <- survey_start_dates |>
  left_join(last_daily_survey, by = "pid") |>
  left_join(last_biweekly_survey, by = "pid") |>
  left_join(last_daily_telemetry, by = "pid") |>
  mutate(
    last_survey_date = pmax(last_daily_survey, last_biweekly_survey, na.rm = TRUE),
    active_study_days = as.numeric(difftime(last_survey_date, started_study, units = "days")) + 1,
    active_study_days = pmin(active_study_days, study_duration_days)
  )

# participant timelines dynamically creating rows up till the last day of participation
study_timeline <- participant_windows |>
  filter(!is.na(active_study_days)) |> # drop pid with zero usable dates
  mutate(study_day_offset = map(active_study_days, ~ 0:(.x - 1))) |>
  unnest(study_day_offset) |>
  mutate(day_local = started_study + study_day_offset) |>
  select(pid, started_study, last_survey_date, active_study_days, day_local, study_day_offset)

wide_daily_telemetry <- daily_telemetry |>
  distinct(pid, platform, day_local, .keep_all = TRUE) |>
  pivot_wider(
    names_from = platform,
    names_glue = "{platform}_minutes",
    values_from = minutes,
    values_fill = 0
  ) |>
  rename_with(tolower)

complete_daily_telemetry <- study_timeline |>
  left_join(wide_daily_telemetry, by = join_by(pid, day_local)) |>
  mutate(
    across(ends_with("_minutes"), ~ replace_na(.x, 0)),
    day_of_week = wday(day_local)
  )

telemetry_traits <- complete_daily_telemetry |>
  group_by(pid, active_study_days) |>
  summarise(
    # activity stats
    mean_daily_steam_minutes = sum(steam_minutes, na.rm = TRUE) /
      first(active_study_days),
    mean_daily_nintendo_minutes = sum(nintendo_minutes, na.rm = TRUE) /
      first(active_study_days),
    mean_daily_xbox_minutes = sum(xbox_minutes, na.rm = TRUE) /
      first(active_study_days),
    mean_daily_mobile_minutes = sum(ios_minutes + android_minutes, na.rm = TRUE) /
      first(active_study_days),
    mean_daily_total = sum(
      steam_minutes + nintendo_minutes + xbox_minutes +
        ios_minutes + android_minutes,
      na.rm = TRUE
    ) / first(active_study_days),
    max_binge_day = max(steam_minutes + nintendo_minutes + xbox_minutes + ios_minutes + android_minutes, na.rm = TRUE),

    # gaming traits
    active_gaming_ratio = mean((steam_minutes + nintendo_minutes + xbox_minutes + ios_minutes + android_minutes) > 0),
    .groups = "drop_last"
  ) |>
  ungroup()

interested_deomographic_cols <- c(
  "adhd",
  "country", "long", "lat",
  "age", "gender", "ethnicity", "height", "weight",
  "edu_level", "employment", "marital_status"
)
telem_traits_dem <- intake_participants |>
  left_join(participant_coords, by = join_by(country, geo_area)) |>
  select(all_of(c("pid", interested_deomographic_cols))) |>
  right_join(telemetry_traits, by = "pid") |>
  drop_na()

# average session time for each day of the week

# join daily telemetry up to two weeks prior to the biweekly survey
# excluding the lower bound, to include the current day of the survey
wave_daily_telemetry <- biweekly |>
  left_join(select(intake_participants, pid, adhd), by = "pid") |>
  # cuts a lot of bangs missingness
  filter(played_any_games == "Yes") |>
  select(pid, survey_date = date, wave) |>
  drop_na() |>
  mutate(
    survey_date = as.Date(survey_date),
    min_twoweek_date = survey_date - weeks(2)
  ) |>
  left_join(
    daily_telemetry,
    by = join_by(
      pid,
      # get daily telemetry up to two weeks before response
      # excludes survey day (could play more after survey)
      min_twoweek_date <= day_local,
      survey_date > day_local
    ),
    # account for response delays impacting other responses
    relationship = "many-to-many"
  ) |>
  # start of the week will be Monday
  # merge mobile instances
  mutate(
    day_of_week = lubridate::wday(day_local, week_start = 1),
    platform = case_when(
      platform == "iOS" ~ "Mobile",
      platform == "Android" ~ "Mobile",
      TRUE ~ platform
    )
  ) |>
  # handle the duplicated "Mobile" rows
  group_by(across(c(-minutes))) |>
  summarise(minutes = sum(minutes), .groups = "drop") |>
  # complete cases (0 for missing days)
  group_by(pid, wave) |>
  complete(
    day_local = seq.Date(
      first(min_twoweek_date),
      first(survey_date),
      by = "day"
    ),
    survey_date = first(survey_date),
    min_twoweek_date = first(min_twoweek_date),
    platform = c("Mobile", "Steam", "Xbox", "Nintendo"),
    fill = list(minutes = 0)
  ) |>
  ungroup() |>
  mutate(day_of_week = lubridate::wday(day_local, week_start = 1)) |>
  filter(!is.na(day_local) & !is.na(platform))

wave_daily_totals <- wave_daily_telemetry |>
  pivot_wider(
    names_from = platform,
    values_from = minutes,
    names_glue = "{platform}_{.value}",
  ) |>
  rename_with(tolower) |>
  mutate(
    total_minutes = steam_minutes + xbox_minutes +
      mobile_minutes + nintendo_minutes
  )

biweekly_mapper_telemetry <- wave_daily_totals |>
  group_by(pid, wave) |>
  summarise(
    # telemetry frequency
    days_played = sum(total_minutes > 0),
    total_wave_minutes = sum(total_minutes),
    total_steam_minutes = sum(steam_minutes),
    total_nintendo_minutes = sum(nintendo_minutes),
    total_xbox_minutes = sum(xbox_minutes),
    total_mobile_minutes = sum(mobile_minutes),
    # central tendendencies and spread of telemetry
    mean_daily_minutes = mean(total_minutes),
    median_daily_minutes = median(total_minutes),
    sd_daily_minutes = sd(total_minutes),
    # hyperfocus/binge metrics
    max_binge_minutes = max(total_minutes),
    binge_day_of_week = if_else(
      max_binge_minutes > 0,
      day_of_week[which.max(total_minutes)],
      NA_real_ # unlikely to happen, but check
    ),
    # structure of telemetry metrics
    mean_weekday_minutes = mean(total_minutes[day_of_week %in% 1:5]),
    mean_weekend_minutes = mean(total_minutes[day_of_week %in% 6:7]),
    binge_day_of_week = factor(
      binge_day_of_week,
      levels = c(1, 2, 3, 4, 5, 6, 7),
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    binge_day_of_week = fct_na_value_to_level(binge_day_of_week, level = "None"),
    # keep pid and wave columns
    .groups = "drop"
  )

simon_traits <- simon |>
  filter(!is.na(meanrt_final)) |>
  group_by(pid) |>
  summarise(
    simon_waves_count = n_distinct(wave),
    rt_mean = mean(meanrt_final, na.rm = TRUE),
    rt_sd = ifelse(n() > 1, sd(meanrt_final, na.rm = TRUE), 0),
    # roc: rate of change (gradient of rt over time)
    rt_roc = ifelse(
      n() >= 2,
      coef(lm(meanrt_final ~ wave))[2],
      0
    ),
    score_mean = mean(score_final, na.rm = TRUE),
    .groups = "drop"
  )

biweekly_mapper_data <- biweekly |>
  left_join(select(intake_participants, pid, adhd), by = "pid") |>
  filter(!is.na(adhd)) |>
  mutate(adhd = as.factor(adhd)) |>
  select(
    pid, adhd, wave, date, affective_valence, life_sat,
    starts_with("bangs_"), -starts_with("bangs_dup"), -bangs_failed_att_check,
    starts_with("gaming_value"),
    starts_with("wemwbs"),
    played_any_games
  ) |>
  # cuts a lot of bangs missingness
  filter(played_any_games == "Yes") |>
  drop_na() |>
  select(-played_any_games) |>
  left_join(biweekly_mapper_telemetry, by = join_by(pid, wave)) |>
  # add reaction time data
  left_join(simon_traits, by = "pid") |>
  drop_na()

# Shortlisted, unscaled data
mapper_data <- biweekly_mapper_data |>
  mutate(
    wemwbs_total = rowSums(across(starts_with("wemwbs_")), na.rm = TRUE),
    bangs_auto_sat = rowSums(across(c(bangs_1, bangs_2, bangs_3)), na.rm = TRUE),
    bangs_auto_frus = rowSums(across(c(bangs_4, bangs_5, bangs_6)), na.rm = TRUE),
    bangs_comp_sat = rowSums(across(c(bangs_7, bangs_8, bangs_9)), na.rm = TRUE),
    bangs_comp_frus = rowSums(across(c(bangs_10, bangs_11, bangs_12)), na.rm = TRUE),
    bangs_rel_sat = rowSums(across(c(bangs_13, bangs_14, bangs_15)), na.rm = TRUE),
    bangs_rel_frus = rowSums(across(c(bangs_16, bangs_17, bangs_18)), na.rm = TRUE),
    gaming_value_sum = rowSums(across(starts_with("gaming_value_")), na.rm = TRUE)
  ) |>
  select(
    aff_val = affective_valence,
    wemwbs_total,
    bangs_auto_sat, bangs_auto_frus,
    bangs_comp_sat, bangs_comp_frus,
    bangs_rel_sat, bangs_rel_frus,
    gaming_value_sum,
    days_played,
    total_wave_minutes,
    total_steam_minutes,
    total_xbox_minutes,
    total_mobile_minutes,
    total_nintendo_minutes,
    max_binge_minutes,
    atten_score_mean = score_mean
  )

# Create reference data for post-construction analysis
mapper_full_data <- mapper_data |>
  add_column(
    pid = biweekly_mapper_data$pid,
    wave = biweekly_mapper_data$wave
  ) |>
  left_join(select(
    intake_participants,
    pid,
    adhd,
    linked_platforms
  ), by = "pid") |>
  mutate(any_adhd = if_else(adhd == "Neurotypical", 0, 1))
