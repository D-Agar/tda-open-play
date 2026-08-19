library(tidyverse)
library(tidymodels)
library(Rtsne)
library(xgboost)
library(isotree)
library(ggplot2)
library(GGally)
library(skimr)
source("examples/open-play/helpers.R")
source("utils.R")
sourceDir("mapper", exclude = "test.R")

### Data Setup and Preprocessing ###
message("[INFO] Preparing data:")
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
    total_mobile_minutes = sum(xbox_minutes),
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
    total_xbox_minutes,
    total_nintendo_minutes,
    max_binge_minutes,
    atten_score_mean = score_mean
  )

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

mapper_scaled <- mapper_data |>
  mutate(across(everything(), ~ scale(.)[, 1]))

skim(mapper_data)

message("[SUCCESS] Data setup")

### Filter Functions ###
message("[INFO] Calculating filter function values")

## Principal Component Analysis ##
message("[INFO] Performing PCA...")
n_components <- 2
mapper_pca <- prcomp(mapper_data, center = TRUE, scale = TRUE)
summary(mapper_pca)
message("[SUCCESS] PCA complete")

## t-SNE ##
message("[INFO] Performing t-SNE")
set.seed(30)
tsne_results <- Rtsne(
  mapper_data,
  dims = 1,
  pca_center = TRUE,
  pca_scale = TRUE
)

tsne_vec <- as.vector(tsne_results$Y)
message("[SUCCESS] t-SNE complete")

## Decision Tree
message("[INFO] Performing standard decision tree")
mapper_data_dt <- mapper_data |>
  add_column(adhd = mapper_full_data$adhd)
# prediction recipe
adhd_recipe <- recipe(adhd ~ ., data = mapper_data_dt)
# cross-validation for hyperparameter tuning
set.seed(30)
five_folds_cv <- vfold_cv(mapper_data_dt, v = 5, strata = adhd)
# metrics focussing on imbalanced classes
imbal_metrics <- metric_set(pr_auc, bal_accuracy, sens, spec)

adhd_dt_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) |>
  set_engine(
    "rpart",
    # equal priors
    parms = list(prior = c(1 / 3, 1 / 3, 1 / 3))
  ) |>
  set_mode("classification")
adhd_dt_spec |> translate()

adhd_dt_wf <- workflow() |>
  add_recipe(adhd_recipe) |>
  add_model(adhd_dt_spec)

adhd_dt_grid <- grid_regular(
  cost_complexity(range = c(-8, -3)),
  tree_depth(range = c(4, 8)),
  min_n(range = c(2, 50)),
  levels = 5
)

# tuning decision tree
adhd_dt_results <- tune_grid(
  adhd_dt_wf,
  resamples = five_folds_cv,
  grid = adhd_dt_grid,
  metrics = imbal_metrics,
)

adhd_dt_best_params <- select_best(adhd_dt_results, metric = "pr_auc")
adhd_dt_final_wf <- finalize_workflow(adhd_dt_wf, adhd_dt_best_params)
adhd_dt_fit <- fit(adhd_dt_final_wf, data = mapper_data_dt)
adhd_dt_best_params
adhd_dt <- extract_fit_engine(adhd_dt_fit)

full_preds <- predict(
  adhd_dt_fit,
  new_data = mapper_data_dt,
  type = "prob"
) |>
  bind_cols(mapper_data_dt |> select(adhd))

adhd_dt_adhd_probs <- full_preds |>
  select(pred_diagnosed_adhd = `.pred_Diagnosed ADHD`) |>
  pull(pred_diagnosed_adhd)
message("[SUCCESS] Standard decision tree complete")

### XGBoost ###
message("[INFO] Performing XGBoost")
set.seed(30)
data_split <- initial_split(
  mapper_data_dt,
  prop = 0.80,
  strata = adhd
)
train_data <- training(data_split)
test_data <- testing(data_split)

# 5-fold cross validation for hyperparameter tuning
cv_folds <- vfold_cv(train_data, v = 5, strata = adhd)

# note: XGBoost requires all categorical predictors to be dummified
adhd_xgb_recipe <- recipe(adhd ~ ., data = train_data) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  # remove zero-variance features
  step_zv(all_predictors())

# boosted tree with hyperparameter tuning
adhd_xgb_spec <- boost_tree(
  trees = tune(), # number of boosting iterations (nrounds)
  tree_depth = tune(), # max depth of each individual tree
  min_n = tune(), # minimum child weight to stop splitting
  loss_reduction = tune(), # minimum gain to split further (gamma)
  sample_size = tune(), # subsample ratio of training instances
  mtry = tune(), # fraction of features sampled per tree
  learn_rate = tune() # learning rate / shrinkage (eta)
) |>
  set_engine("xgboost", objective = "multi:softprob") |>
  set_mode("classification")
adhd_xgb_spec |> translate()

# workflow
adhd_xgb_wf <- workflow() |>
  add_recipe(adhd_xgb_recipe) |>
  add_model(adhd_xgb_spec)

# hyperparameter Tuning Grid
# using Space-Filling Design to efficiently sample the parameter space
adhd_xgb_grid <- grid_space_filling(
  trees(range = c(50, 500)),
  tree_depth(range = c(3, 8)),
  min_n(range = c(2, 15)),
  loss_reduction(range = c(1e-8, 10), trans = log10_trans()),
  sample_size = sample_prop(range = c(0.5, 0.9)),
  finalize(mtry(), train_data),
  learn_rate(range = c(0.01, 0.2)),
  size = 25 # Evaluates 25 distinct hyperparameter combinations
)

# grid search on folds
adhd_xgb_res <- tune_grid(
  adhd_xgb_wf,
  resamples = cv_folds,
  grid = adhd_xgb_grid,
  metrics = metric_set(roc_auc, mn_log_loss, accuracy),
  control = control_grid(save_pred = TRUE, verbose = FALSE)
)

# choosing the best hyperparameters
# ROC AUC multi-class metric evaluates class separation across thresholds
best_params <- select_best(adhd_xgb_res, metric = "roc_auc")

adhd_final_xgb_wf <- finalize_workflow(adhd_xgb_wf, best_params)

# Fit on full training set and evaluate on test set
adhd_final_fit <- last_fit(adhd_final_xgb_wf, data_split)
collect_metrics(adhd_final_fit)

# Now I can extract the continuous lens, the probabilities:
# Fit final workflow on entire dataset
fitted_xgb <- fit(adhd_final_xgb_wf, data = mapper_data_dt)

# Generate continuous class probability columns
adhd_xgb_probs <- augment(fitted_xgb, new_data = mapper_data_dt) |>
  select(
    adhd,
    pred_diagnosed_adhd = `.pred_Diagnosed ADHD`,
    pred_neurotypical = `.pred_Neurotypical`,
    pred_self_idenfitied_adhd = `.pred_Self-Identified ADHD`
  )
message("[SUCCESS] XGBoost complete")

## Isolation Forest ##
message("[INFO] Performing isolation forest")

set.seed(30)

is_outlier <- mapper_full_data$adhd %in% c("Diagnosed ADHD")

model_orig <- isolation.forest(
  mapper_data,
  ndim = 1,
  sample_size = 256,
  ntrees = 500,
  missing_action = "fail"
)
pred_orig <- predict(model_orig, mapper_data)

model_dens <- isolation.forest(
  mapper_data,
  ndim = 1,
  sample_size = 256,
  ntrees = 500,
  missing_action = "fail",
  scoring_metric = "density"
)
pred_dens <- predict(model_dens, mapper_data)

model_fcf <- isolation.forest(
  mapper_data,
  ndim = 1,
  sample_size = 32,
  prob_pick_pooled_gain = 1,
  ntrees = 500,
  missing_action = "fail"
)
pred_fcf <- predict(model_fcf, mapper_data)
message("[SUCCESS] Isolation forests complete")

## Dist Mean ##
message("[INFO] Performing dist_mean")

scaled_col_means <- colMeans(mapper_scaled)

# the euclidean distance between two single points is just the absolute difference
A_dist <- sweep(mapper_scaled, 2, scaled_col_means, "-")
dist_mean <- rowSums(A_dist^2)
message("[SUCCESS] dist_mean complete")

## Neurotypical distance ##
message("[INFO] Performing neurotypical_dist")

neurotypical_means <- mapper_scaled |>
  add_column(adhd = biweekly_mapper_data$adhd, .before = TRUE) |>
  filter(adhd == "Neurotypical") |>
  select(-adhd) |>
  colMeans()

neuro_diffs <- sweep(mapper_scaled, 2, neurotypical_means, "-")
neuro_dists <- rowSums(neuro_diffs^2)
message("[SUCCESS] Completed neurotypical_dist")

## Density Estimation ##
message("[INFO] Performing density estimation")
mapper_dmatrix <- dist(mapper_scaled)
mapper_density <- density_estimation(mapper_dmatrix)
density_lens <- as.vector(mapper_density$values)
message("[SUCCESS] Completed density estimation")

## L2 Norm ##
message("[INFO] L2 norm")
l2_norms <- mapper_data |>
  select(where(is.numeric)) |>
  apply(1, norm, type = "2")
message("[SUCCESS] L2 norm")

## Columns from data #### Visualising Filter functions ###
message("[INFO] Visualising Filter functions (pair plot)")
message("[INFO] Pearson Correlations:")

mapper_filters <- tibble(
  adhd = mapper_full_data$adhd,
  pc1 = mapper_pca$x[, 1],
  pc2 = mapper_pca$x[, 2],
  dt = adhd_dt_adhd_probs,
  xgb_adhd_pred = adhd_xgb_probs$pred_diagnosed_adhd,
  iso_forest = pred_orig,
  iso_forest_den = pred_dens,
  iso_forest_fc = pred_fcf,
  tsne = tsne_vec,
  dist_mean = dist_mean,
  density_estimated = density_lens,
  neuro_dist = neuro_dists,
  affective_val = mapper_data$aff_val,
  wemwbs_total = mapper_data$wemwbs_total,
  l2_norm = l2_norms
)
filters_cor <- cor(select(mapper_filters, -adhd))
filters_cor[] <- vapply(filters_cor, round, double(1), digits = 2)

message(filters_cor)

message("[INFO] Creating filter function pairplot")
# shortlisting functions for better visualisation
selected_filters <- mapper_filters |>
  select(
    adhd,
    `PC 1` = pc1,
    `PC 2` = pc2,
    `XGBoost ADHD` = xgb_adhd_pred,
    `Isolation Forest` = iso_forest,
    `Density Est.` = density_estimated,
    `Dist Mean` = dist_mean,
    `WEMWBS Total` = wemwbs_total,
    `L2 Norm` = l2_norm
  )
# custom upper-panel function: displays only the colored numeric values
cor_values_only <- function(data, mapping, digits = 2, size = 3.2, ...) {
  x_col <- rlang::as_name(mapping$x)
  y_col <- rlang::as_name(mapping$y)
  grp_col <- rlang::as_name(mapping$colour)

  # Calculate Pearson correlation for each factor level
  cor_df <- data |>
    group_by(grp = .data[[grp_col]]) |>
    summarise(
      r = cor(.data[[x_col]], .data[[y_col]], use = "pairwise.complete.obs"),
      .groups = "drop"
    ) |>
    mutate(
      label = sprintf(paste0("%.", digits, "f"), r),
      # Space numbers evenly from top to bottom
      y_pos = seq(0.75, 0.25, length.out = n())
    )

  ggplot(cor_df, aes(x = 0.5, y = y_pos, label = label, colour = grp)) +
    geom_text(size = size, fontface = "bold") +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
}

p_pairs <- ggpairs(
  selected_filters,
  columns = 2:ncol(selected_filters),
  aes(colour = adhd, fill = adhd),
  lower = list(
    continuous = wrap(
      "points",
      alpha = 0.25,
      size = 0.4,
      shape = 16
    )
  ),
  diag = list(
    continuous = wrap(
      "densityDiag",
      alpha = 0.4,
      linewidth = 0.4
    )
  ),
  upper = list(
    continuous = cor_values_only
  ),
  legend = c(1, 1) # legend from first plot
) +
  scale_color_brewer(palette = "Dark2", name = "ADHD Status") +
  scale_fill_brewer(palette = "Dark2", name = "ADHD Status") +
  theme_bw(base_size = 9, base_family = "sans") +
  theme(
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text       = element_text(face = "bold", size = 6),
    axis.text        = element_text(size = 6),
    axis.ticks       = element_line(linewidth = 0.3, color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 6),
    legend.text      = element_text(size = 6)
  )

scale <- 1.5
ggsave(
  filename = "filter_functions_pairs.pdf",
  plot     = p_pairs,
  width    = 13 * scale, # measurements for paper
  height   = 12 * scale,
  units    = "cm",
  device   = cairo_pdf
)
message(paste0("[SUCCESS] Visualisation created at '", filename, "'"))
message("[INFO] Filter functions completed")
