# BUAD 312: Genre Drivers of Spotify Streams
# Run this file from the folder containing tracks_simplified.csv.
# This standalone script reproduces the three genre comparisons in our table.
# Replace contributor placeholders before submission.
# Results describe associations, not the effect of changing or promoting a song.

# 1. SETUP AND CLEANING
# Contributed by: Sonly Nguyen

# Load packages. sandwich supplies the robust intervals used in the table.
# If needed, install these packages once in the Console before running the script.
# install.packages(c("tidyverse", "moderndive", "sandwich"))
library(tidyverse)
library(moderndive)
library(sandwich)

# Import the CSV; blank entries and NA text become missing values.
tracks <- read.csv("tracks_simplified.csv", na.strings=c("", "NA"))
output_folder <- "Genre Analysis Output"
dir.create(output_folder, showWarnings=FALSE)

# Convert duration to minutes and flag empty market lists.
# Read: an empty list is missing market-list information, not proof of unavailability.
tracks_prepared <- tracks |>
  mutate(duration_min=duration_ms/60000,
         empty_markets=available_markets=="[]")

# Extract the year from slash dates, full years, or year-month dates.
tracks_prepared <- tracks_prepared |>
  mutate(year_text=ifelse(grepl("/", album_release_date),
                         sub(".*/", "", album_release_date),
                         substr(album_release_date, 1, 4)),
         release_year=as.numeric(year_text))

# Treat 0 as missing; assign two-digit years using the original 2026 cutoff.
# Read: 19 becomes 2019, 95 becomes 1995, and a placeholder 0 stays missing.
tracks_prepared <- tracks_prepared |>
  mutate(release_year=case_when(
    album_release_date=="0" ~ NA_real_,
    release_year<=26 ~ release_year+2000,
    release_year<100 ~ release_year+1900,
    TRUE ~ release_year))
print(table(tracks_prepared$release_year, useNA="ifany"))

# Remove invalid counts and durations. Missing followers also fail this filter.
# Read: zero followers are valid; zero streams cannot be logged.
tracks_prepared <- tracks_prepared |>
  filter(streams>0, duration_min>0, album_total_tracks>0,
         artist_followers>=0)

# Create log variables and release-size categories.
# Read: logs express proportional differences; +1 allows zero followers.
# A one-track release does not necessarily identify a marketed lead single.
tracks_prepared <- tracks_prepared |>
  mutate(log_streams=log(streams),
         log_followers=log(artist_followers+1),
         release_size=case_when(
           album_total_tracks==1 ~ "1 track",
           album_total_tracks<=6 ~ "2-6 tracks",
           album_total_tracks>=7 ~ "7+ tracks"),
         explicit=factor(explicit, levels=c(FALSE, TRUE),
                         labels=c("Non-explicit", "Explicit")),
         release_size=factor(release_size,
                             levels=c("1 track", "2-6 tracks", "7+ tracks")))

# Keep the same complete-case requirements as the official analysis.
# Read: missing artist names do not exclude a track; unused audio columns preserve the sample.
tracks_model <- tracks_prepared |>
  select(track_id, streams, log_streams, explicit, genres, artist_followers,
         log_followers, artist_popularity, duration_min, release_year,
         release_size, empty_markets, danceability, energy, valence,
         acousticness, speechiness, liveness, instrumentalness, loudness, tempo) |>
  na.omit()
cleaning_counts <- data.frame(original=nrow(tracks), retained=nrow(tracks_model),
                             excluded=nrow(tracks)-nrow(tracks_model))
print(cleaning_counts)
# Current file: 5,870 original tracks, 5,824 retained, and 46 excluded.


# 2. DESCRIPTIVE STATISTICS AND GRAPHS
# Contributed by: [your name]

# Rank named genres by total streams and select the report's three focus genres.
# Read: this is a ranking within our sample, not a ranking of the whole industry.
genre_ranking <- tracks_model |>
  filter(genres!="Other/Unknown") |>
  group_by(genres) |>
  summarize(n=n(), total_streams=sum(streams), .groups="drop") |>
  arrange(desc(total_streams))
print(genre_ranking)
focus_tracks <- tracks_model |>
  filter(genres %in% c("Hip-Hop/Rap", "Pop", "Latin")) |>
  mutate(genres=factor(genres, levels=c("Hip-Hop/Rap", "Pop", "Latin")))
hiphop <- focus_tracks |> filter(genres=="Hip-Hop/Rap")
pop <- focus_tracks |> filter(genres=="Pop")
latin <- focus_tracks |> filter(genres=="Latin")

# Summarize each genre and each release-size group.
# Read: medians are less affected by very large hits than arithmetic means.
genre_summary <- focus_tracks |>
  group_by(genres) |>
  summarize(n=n(), mean_streams=mean(streams), median_streams=median(streams),
            sd_streams=sd(streams), .groups="drop")
release_summary <- focus_tracks |>
  group_by(genres, release_size) |>
  summarize(n=n(), median_streams=median(streams), .groups="drop")
print(genre_summary)
print(release_summary)

# Plot each proposed driver against log streams before adjusting for controls.
# Read: these plots show raw associations; the regression accounts for other variables.
hiphop_plot <- ggplot(hiphop, aes(log_followers, log_streams))+
  geom_point(alpha=0.25)+geom_smooth(method="lm", formula=y~x)+
  labs(title="Hip-Hop/Rap: Followers and Streams", x="Log(Followers + 1)", y="Log Streams")
pop_plot <- ggplot(pop, aes(release_size, log_streams))+
  geom_boxplot()+labs(title="Pop: Release Size and Streams", x="Release Size", y="Log Streams")
latin_plot <- ggplot(latin, aes(duration_min, log_streams))+
  geom_point(alpha=0.3)+geom_smooth(method="lm", formula=y~x)+
  labs(title="Latin: Duration and Streams", x="Duration (Minutes)", y="Log Streams")
print(hiphop_plot)
print(pop_plot)
print(latin_plot)

# Calculate numeric correlations separately for each genre.
# Read: correlation measures a pairwise linear association and does not control for other factors.
genre_data <- list("Hip-Hop/Rap"=hiphop, "Pop"=pop, "Latin"=latin)
for (genre_name in names(genre_data)) {
  cat("\nCorrelations:", genre_name, "\n")
  numeric_data <- genre_data[[genre_name]] |>
    select(log_streams, log_followers, release_year, duration_min, liveness)
  print(round(cor(numeric_data), 2))
}


# 3. SIMPLE AND MULTIPLE REGRESSIONS
# Contributed by: [your name]

# Fit simple models for the three main comparisons.
# Read: these coefficients do not account for artist and release differences.
simple_hiphop <- lm(log_streams~log_followers, data=hiphop)
simple_pop <- lm(log_streams~release_size, data=pop)
simple_latin <- lm(log_streams~duration_min, data=latin)

# Reproduce the final genre models specified in the supplied PDF.
# Read: coefficients compare tracks at the same modeled values of the other predictors.
# These formulas were selected in earlier exploratory work, not by a new selection test here.
model_hiphop <- lm(log_streams~log_followers+explicit+release_size+
                    release_year+duration_min, data=hiphop)
model_pop <- lm(log_streams~release_size+explicit+release_year+
                 log_followers+duration_min, data=pop)
model_latin <- lm(log_streams~duration_min+liveness+explicit+release_size+
                   release_year+log_followers, data=latin)
print(summary(model_hiphop))
print(summary(model_pop))
print(summary(model_latin))
# Read: followers are positive in Hip-Hop/Rap; larger releases are negative versus one track in Pop.
# Latin duration is negative. These signs do not establish causal effects.

# Compare simple and multiple models on the same genre samples and outcome scale.
# Read: adjusted R-squared penalizes complexity; a small p-value does not imply high accuracy.
simple_models <- list("Hip-Hop/Rap"=simple_hiphop, "Pop"=simple_pop, "Latin"=simple_latin)
models <- list("Hip-Hop/Rap"=model_hiphop, "Pop"=model_pop, "Latin"=model_latin)
model_fit_rows <- list()
for (genre_name in names(models)) {
  m <- models[[genre_name]]
  s <- simple_models[[genre_name]]
  model_fit_rows[[genre_name]] <- data.frame(
    genre=genre_name, n=nobs(m), simple_r_squared=summary(s)$r.squared,
    full_r_squared=summary(m)$r.squared, adjusted_r_squared=summary(m)$adj.r.squared,
    in_sample_log_rmse=sqrt(mean(residuals(m)^2)))
}
model_fit <- bind_rows(model_fit_rows)
print(model_fit)
# Current adjusted R-squared: Hip-Hop/Rap 5.7%, Pop 3.6%, Latin 12.6%.


# 4. SUPPORTING GENRE INTERACTION TESTS
# Contributed by: [your name]

# Test whether driver relationships vary by genre using one common set of controls.
# Read: these supporting models are separate from the PDF's final prediction models.
parallel_model <- lm(log_streams~genres+explicit+release_size+release_year+
                       log_followers+duration_min+liveness, data=focus_tracks)
followers_interaction <- update(parallel_model, .~.+genres:log_followers)
release_interaction <- update(parallel_model, .~.+genres:release_size)
duration_interaction <- update(parallel_model, .~.+genres:duration_min)
interaction_models <- list(Followers=followers_interaction,
                           Release_size=release_interaction,
                           Duration=duration_interaction)
interaction_rows <- list()
for (driver in names(interaction_models)) {
  test <- anova(parallel_model, interaction_models[[driver]])
  interaction_rows[[driver]] <- data.frame(driver=driver, F_value=test$F[2],
                                          p_value=test$`Pr(>F)`[2])
}
interaction_results <- bind_rows(interaction_rows) |>
  mutate(p_holm=p.adjust(p_value, method="holm"))
print(interaction_results)
# Read: a small p-value suggests a slope/category contrast differs somewhere across genres.
# These ordinary F-tests assume equal error variance; one significant genre alone is not a difference test.


# 5. PREDICTION SCENARIOS
# Contributed by: [your name]

# Hold controls at genre medians and use non-explicit tracks for all profiles.
# Read: Hip-Hop/Rap and Latin profiles use one-track releases; these are illustrative cases.
hiphop_scenarios <- data.frame(
  explicit="Non-explicit", release_size="1 track",
  release_year=median(hiphop$release_year), duration_min=median(hiphop$duration_min),
  log_followers=log(c(100000, 200000)+1))
pop_scenarios <- data.frame(
  explicit="Non-explicit", release_size=c("7+ tracks", "1 track"),
  release_year=median(pop$release_year), duration_min=median(pop$duration_min),
  log_followers=log(median(pop$artist_followers)+1))
latin_scenarios <- data.frame(
  explicit="Non-explicit", release_size="1 track",
  release_year=median(latin$release_year), duration_min=c(3.5, 3),
  log_followers=log(median(latin$artist_followers)+1), liveness=median(latin$liveness))
scenarios <- list("Hip-Hop/Rap"=hiphop_scenarios, "Pop"=pop_scenarios,
                  "Latin"=latin_scenarios)

# Transform predictions from log units back to stream levels.
# Read: exp(predicted log streams) is a fitted geometric mean, not an arithmetic mean.
# No future observation window is known, so these are not 28-day forecasts.
predictions <- data.frame(
  genre=names(models),
  comparison=c("100,000 to 200,000 followers", "7+ tracks to one-track release",
               "3.5 minutes to 3 minutes"),
  baseline=c(exp(predict(model_hiphop, hiphop_scenarios))[1],
             exp(predict(model_pop, pop_scenarios))[1],
             exp(predict(model_latin, latin_scenarios))[1]),
  comparison_prediction=c(exp(predict(model_hiphop, hiphop_scenarios))[2],
                          exp(predict(model_pop, pop_scenarios))[2],
                          exp(predict(model_latin, latin_scenarios))[2])) |>
  mutate(percent_difference=100*(comparison_prediction/baseline-1))
print(predictions)
# Current differences: Hip-Hop/Rap +7.7%, Pop +31.5%, Latin +22.4%.


# 6. ROBUST CONFIDENCE INTERVALS
# Contributed by: [your name]

# Each scenario changes just one coefficient; multiply it by the scenario's change.
# Read: -1 reverses Pop's larger-release comparison; -0.5 represents half a minute shorter.
focus_terms <- c("Hip-Hop/Rap"="log_followers", "Pop"="release_size7+ tracks",
                 "Latin"="duration_min")
scenario_changes <- c("Hip-Hop/Rap"=log(200001)-log(100001), "Pop"=-1, "Latin"=-0.5)
interval_rows <- list()
for (genre_name in names(models)) {
  m <- models[[genre_name]]
  term <- focus_terms[[genre_name]]
  change <- scenario_changes[[genre_name]]
  log_difference <- coef(m)[term]*change

  # HC3 permits unequal error variance; it does not fix omitted variables or artist dependence.
  robust_variance <- vcovHC(m, type="HC3")
  standard_error <- sqrt(robust_variance[term, term])*abs(change)
  critical_value <- qt(0.975, df.residual(m))
  lower_log <- log_difference-critical_value*standard_error
  upper_log <- log_difference+critical_value*standard_error
  interval_rows[[genre_name]] <- data.frame(
    genre=genre_name, lower_percent=100*(exp(lower_log)-1),
    upper_percent=100*(exp(upper_log)-1),
    robust_p_value=2*pt(-abs(log_difference/standard_error), df.residual(m)))
}
intervals <- bind_rows(interval_rows) |>
  mutate(robust_p_holm=p.adjust(robust_p_value, method="holm"))
predictions <- left_join(predictions, intervals, by="genre")
print(predictions)
# Read: an interval above 0 supports a positive modeled association under the assumptions.
# Intervals are individual 95% intervals; Holm adjusts the three p-values, not the intervals.
# They are not prediction intervals for a new song and do not measure a campaign's causal lift.

# Plot the scenario differences with robust intervals.
comparison_plot <- ggplot(predictions, aes(genre, percent_difference))+
  geom_hline(yintercept=0, linetype="dashed")+geom_point(size=3)+
  geom_errorbar(aes(ymin=lower_percent, ymax=upper_percent), width=0.15)+
  coord_flip()+labs(title="Modeled Differences for the Three Scenarios",
                   subtitle="Individual 95% HC3 intervals; associations, not causal effects",
                   x="Genre", y="Difference in Fitted Geometric-Mean Streams (%)")
print(comparison_plot)


# 7. PREDICTION VALIDATION
# Contributed by: [your name]

# Repeat five-fold validation 20 times using the same seeds as the earlier check.
# Read: each model trains on about 80% and predicts the other 20%; every track is held out once per repeat.
# This checks the fixed formulas, not the earlier variable-selection process.
# Folds separate tracks, not artists or years, so this is exploratory validation.
validation_rows <- list()
validation_details <- list()
for (genre_name in names(models)) {
  d <- genre_data[[genre_name]]
  model_formula <- formula(models[[genre_name]])
  repeat_rows <- list()
  for (iteration in 1:20) {
    set.seed(312000+iteration)
    fold <- sample(rep(1:5, length.out=nrow(d)))
    predicted <- rep(NA_real_, nrow(d))
    baseline <- rep(NA_real_, nrow(d))
    for (k in 1:5) {
      training <- d[fold!=k, ]
      testing <- d[fold==k, ]
      fitted <- lm(model_formula, data=training)
      predicted[fold==k] <- predict(fitted, newdata=testing)
      baseline[fold==k] <- mean(training$log_streams)
    }

    # Calculate held-out R-squared and log RMSE for each repetition.
    # Read: higher R-squared and lower RMSE are better; a negative held-out R-squared is possible.
    errors <- d$log_streams-predicted
    baseline_errors <- d$log_streams-baseline
    repeat_rows[[iteration]] <- data.frame(
      genre=genre_name, repetition=iteration,
      heldout_r_squared=1-sum(errors^2)/sum((d$log_streams-mean(d$log_streams))^2),
      log_rmse=sqrt(mean(errors^2)), baseline_log_rmse=sqrt(mean(baseline_errors^2)))
  }
  repeat_results <- bind_rows(repeat_rows)
  validation_details[[genre_name]] <- repeat_results
  validation_rows[[genre_name]] <- repeat_results |>
    summarize(genre=genre_name, heldout_r_squared=mean(heldout_r_squared),
              log_rmse=mean(log_rmse), baseline_log_rmse=mean(baseline_log_rmse))
}
validation <- bind_rows(validation_rows)
print(validation)
# Current held-out R-squared: Hip-Hop/Rap 4.7%, Pop 3.0%, Latin 9.3%.
# Read: the models capture some patterns but leave most track-level variation unexplained.
# These values are not percentages of prediction accuracy.


# 8. RESIDUAL CHECKS AND SENSITIVITY
# Contributed by: [your name]

# Collect residuals and refit on two restricted samples without changing the main models.
# Read: a large residual means an unusual prediction error, not necessarily a bad observation.
diagnostic_rows <- list()
raw_diagnostic_rows <- list()
outlier_rows <- list()
sensitivity_rows <- list()
for (genre_name in names(models)) {
  d <- genre_data[[genre_name]]
  m <- models[[genre_name]]
  points <- get_regression_points(m)
  diagnostic_rows[[genre_name]] <- data.frame(
    genre=genre_name, fitted=points$log_streams_hat, stdres=rstandard(m))
  d$stdres <- rstandard(m)
  outlier_rows[[genre_name]] <- d |>
    filter(abs(stdres)>3) |>
    select(track_id, genres, streams, stdres)

  # Compare raw-stream residuals to the log model; the outcomes have different scales.
  raw_model <- lm(update(formula(m), streams~.), data=d)
  raw_diagnostic_rows[[genre_name]] <- data.frame(
    genre=genre_name, fitted=fitted(raw_model), stdres=rstandard(raw_model))

  # Check whether the focal coefficient changes after excluding outliers or empty market lists.
  # Read: these subsets change the sample; a changed result signals sensitivity, not a preferred answer.
  no_outliers <- d |> filter(abs(stdres)<=3)
  listed_markets <- d |> filter(empty_markets==FALSE)
  fits <- list(Main=m, Without_residual_outliers=update(m, data=no_outliers),
               Listed_markets_only=update(m, data=listed_markets))
  for (sample_name in names(fits)) {
    fitted_model <- fits[[sample_name]]
    beta <- coef(fitted_model)[focus_terms[[genre_name]]]
    key <- paste(genre_name, sample_name)
    sensitivity_rows[[key]] <- data.frame(
      genre=genre_name, sample=sample_name, n=nobs(fitted_model),
      scenario_percent=100*(exp(beta*scenario_changes[[genre_name]])-1))
  }
}
diagnostics <- bind_rows(diagnostic_rows)
raw_diagnostics <- bind_rows(raw_diagnostic_rows)
flagged_tracks <- bind_rows(outlier_rows)
sensitivity <- bind_rows(sensitivity_rows)
print(flagged_tracks)
print(sensitivity)

# Look for curves or changing residual spread; Q-Q departures suggest non-normal errors.
# Read: a log transformation can help, but it does not guarantee valid assumptions.
residual_plot <- ggplot(diagnostics, aes(fitted, stdres))+
  geom_point(alpha=0.25)+geom_hline(yintercept=0, color="red")+
  facet_wrap(~genre, scales="free_x")+
  labs(title="Log-Model Residuals", x="Predicted Log Streams", y="Standardized Residual")
qq_plot <- ggplot(diagnostics, aes(sample=stdres))+
  geom_qq(alpha=0.4)+geom_qq_line(color="red")+facet_wrap(~genre)+
  labs(title="Log-Model Residual Q-Q Plots")
raw_residual_plot <- ggplot(raw_diagnostics, aes(fitted, stdres))+
  geom_point(alpha=0.25)+geom_hline(yintercept=0, color="red")+
  facet_wrap(~genre, scales="free_x")+
  labs(title="Raw-Stream Model Residuals", x="Predicted Streams", y="Standardized Residual")
print(residual_plot)
print(qq_plot)
print(raw_residual_plot)


# 9. FINAL TABLE AND OUTPUT FILES
# Contributed by: [your name]

# Join scenario results, sample sizes, and validation into one table.
# Read: the percentage column describes an association; the held-out R-squared describes overall prediction.
final_table <- predictions |>
  left_join(model_fit |> select(genre, n, adjusted_r_squared), by="genre") |>
  left_join(validation, by="genre") |>
  mutate(takeaway=case_when(
    genre=="Hip-Hop/Rap" ~ "Larger audiences show a modest advantage; test fan activation.",
    genre=="Pop" ~ "One-track releases show an advantage; test a single-first rollout.",
    genre=="Latin" ~ "Shorter tracks show an advantage; test a shorter edit."))
print(as_tibble(final_table), width=Inf)

# Export numeric tables and graphs for presentation preparation.
# Read: exported values retain full precision; round only for display.
write.csv(final_table, file.path(output_folder, "Final Genre Comparison.csv"), row.names=FALSE)
write.csv(model_fit, file.path(output_folder, "Model Fit.csv"), row.names=FALSE)
write.csv(interaction_results, file.path(output_folder, "Interaction Tests.csv"), row.names=FALSE)
write.csv(bind_rows(validation_details), file.path(output_folder, "Validation Repeats.csv"), row.names=FALSE)
write.csv(sensitivity, file.path(output_folder, "Sensitivity Checks.csv"), row.names=FALSE)
ggsave(file.path(output_folder, "Hip Hop Followers.png"), hiphop_plot, width=7, height=5)
ggsave(file.path(output_folder, "Pop Release Size.png"), pop_plot, width=7, height=5)
ggsave(file.path(output_folder, "Latin Duration.png"), latin_plot, width=7, height=5)
ggsave(file.path(output_folder, "Scenario Comparisons.png"), comparison_plot, width=8, height=5)
ggsave(file.path(output_folder, "Log Residuals.png"), residual_plot, width=11, height=4)
ggsave(file.path(output_folder, "QQ Plots.png"), qq_plot, width=11, height=4)
ggsave(file.path(output_folder, "Raw Residuals.png"), raw_residual_plot, width=11, height=4)

# BUSINESS INTERPRETATION
# The results support focused pilots, not promises of stream or revenue growth.
# A proposed 10% gain in streams per promotional dollar is a business target, not a regression result.
# Track total project/version streams so shifted listening is not mistaken for added listening.
# Spending and royalty data are absent, so this script cannot calculate campaign ROI or optimal budgets.
# Recheck profiles, data quality, assumptions, and sample sizes before applying results to new releases.
