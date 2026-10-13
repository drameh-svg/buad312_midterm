# BUAD 312 MIDTERM PROJECT
# UMG GENRE-SPECIFIC RELEASE AND ARTIST-INVESTMENT STRATEGY
# Developed with assistance from OpenAI GPT.
#
# PURPOSE
# Determine how release size, track duration, and artist following are
# associated with streams for Hip-Hop/Rap, Pop, and Latin music.
#
# IMPORTANT INTERPRETATION RULES
# 1. The models show associations, not causation.
# 2. Release size is categorical. Compare 2-6 tracks and 7+ tracks with
#    the 1-track reference; do not describe this as a one-category increase.
# 3. Duration is measured in minutes so its coefficient is easy to explain.
# 4. log_followers represents artist/audience position. It helps compare
#    emerging and established artists; it does not prove followers cause streams.
# 5. release_year identifies release cohorts. Because streams were measured
#    in one snapshot, it does not provide a true historical time series.
# 6. Models use log_streams because raw streams are extremely right-skewed.
#    Predictions are converted back to streams for business interpretation.
#
# RUN THIS FILE AFTER THE GROUP CLEANING SECTION CREATES tracks_prepared.

library(tidyverse)
library(moderndive)


############################################################
# 0. LOAD AND PREPARE tracks_simplified.csv
############################################################

# Keep the CSV in the same folder as this R script or set your working
# directory to the folder containing the CSV before running this line.
tracks <- read.csv(
  "tracks_simplified.csv",
  na.strings=c("", "NA")
)

# Create duration in minutes and identify values needed for cleaning.
tracks_prepared <- tracks |>
  mutate(
    duration_min=duration_ms/60000,
    year_text=ifelse(
      grepl("/", album_release_date),
      sub(".*/", "", album_release_date),
      substr(album_release_date, 1, 4)
    ),
    release_year=as.numeric(year_text)
  )

# Convert two-digit release years. The dataset ends in 2021, so values 00-26
# are treated as 2000s and the remaining two-digit values as 1900s.
tracks_prepared <- tracks_prepared |>
  mutate(
    release_year=case_when(
      album_release_date=="0" ~ NA_real_,
      release_year<=26 ~ release_year+2000,
      release_year<100 ~ release_year+1900,
      TRUE ~ release_year
    )
  )

# Keep observations with valid positive outcome and rollout values.
tracks_prepared <- tracks_prepared |>
  filter(
    streams>0,
    duration_min>0,
    album_total_tracks>0,
    artist_followers>=0
  )

# Create the variables used throughout the analysis.
tracks_prepared <- tracks_prepared |>
  mutate(
    log_streams=log(streams),
    log_followers=log(artist_followers+1),
    release_period=case_when(
      release_year<=2009 ~ "Through 2009",
      release_year<=2014 ~ "2010-2014",
      release_year<=2017 ~ "2015-2017",
      release_year>=2018 ~ "2018+"
    ),
    release_size=case_when(
      album_total_tracks==1 ~ "1 track",
      album_total_tracks<=6 ~ "2-6 tracks",
      album_total_tracks>=7 ~ "7+ tracks"
    )
  )

# Set categorical variables and their reference categories.
tracks_prepared <- tracks_prepared |>
  mutate(
    genres=factor(genres),
    genres=relevel(genres, ref="Hip-Hop/Rap"),
    release_period=factor(
      release_period,
      levels=c("Through 2009", "2010-2014", "2015-2017", "2018+")
    ),
    release_size=factor(
      release_size,
      levels=c("1 track", "2-6 tracks", "7+ tracks")
    )
  )

# Confirm that the CSV loaded and the variables were created correctly.
dim(tracks_prepared)
summary(tracks_prepared)
table(tracks_prepared$genres)
table(tracks_prepared$release_size)


############################################################
# 1. UMG COLOR PALETTE AND GRAPH THEME
############################################################

# UMG-inspired palette: black and white with blue, pink, and green accents.
umg_black <- "#000000"
umg_white <- "#FFFFFF"
umg_blue  <- "#1473E6"
umg_pink  <- "#E83E8C"
umg_green <- "#00A878"
umg_gray  <- "#D9D9D9"

genre_colors <- c(
  "Hip-Hop/Rap"=umg_black,
  "Pop"=umg_pink,
  "Latin"=umg_blue
)

umg_theme <- theme_minimal() +
  theme(
    plot.title=element_text(face="bold", color=umg_black, size=15),
    plot.subtitle=element_text(color="#444444", size=10),
    axis.title=element_text(face="bold", color=umg_black),
    axis.text=element_text(color=umg_black),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(color="#EEEEEE"),
    legend.position="bottom",
    legend.title=element_text(face="bold"),
    strip.text=element_text(face="bold", color=umg_black),
    plot.background=element_rect(fill=umg_white, color=NA),
    panel.background=element_rect(fill=umg_white, color=NA)
  )


############################################################
# 2. PREPARE THE FULL AND FOCUS DATASETS
############################################################

# tracks_prepared already contains log_streams, log_followers,
# duration_min, release_year, release_period, and release_size.

# Full cleaned dataset for the statistical overview slide.
tracks_overview <- tracks_prepared |>
  filter(
    streams>0,
    duration_min>0,
    artist_followers>=0,
    !is.na(release_year),
    !is.na(genres)
  )

# Show that Hip-Hop/Rap, Pop, and Latin are the three largest named genres.
# Other/Unknown is excluded because it is not an interpretable genre strategy.
genre_overview <- tracks_overview |>
  group_by(genres) |>
  summarize(
    tracks=n(),
    total_streams=sum(streams),
    mean_streams=mean(streams),
    median_streams=median(streams),
    .groups="drop"
  ) |>
  arrange(desc(tracks))

genre_overview

# Focus on the three selected genres and variables needed for this analysis.
tracks_focus <- tracks_overview |>
  filter(
    genres=="Hip-Hop/Rap" |
      genres=="Pop" |
      genres=="Latin"
  ) |>
  select(
    streams,
    log_streams,
    genres,
    release_size,
    album_total_tracks,
    duration_min,
    artist_followers,
    log_followers,
    release_year
  ) |>
  na.omit() |>
  mutate(
    genres=factor(
      genres,
      levels=c("Hip-Hop/Rap", "Pop", "Latin")
    ),
    release_size=factor(
      release_size,
      levels=c("1 track", "2-6 tracks", "7+ tracks")
    ),
    years_since_2010=release_year-2010,
    release_cohort=case_when(
      release_year<2010 ~ "Before 2010",
      release_year<=2014 ~ "2010-2014",
      release_year<=2017 ~ "2015-2017",
      release_year>=2018 ~ "2018+"
    ),
    release_cohort=factor(
      release_cohort,
      levels=c("Before 2010", "2010-2014", "2015-2017", "2018+")
    )
  )

# Confirm the number of observations used in the models.
table(tracks_focus$genres)
summary(tracks_focus)


############################################################
# 3. STATISTICAL OVERVIEW AND DATA-QUALITY CHECKS
############################################################

# Basic statistics for the full cleaned dataset.
full_numeric_summary <- tracks_overview |>
  summarize(
    tracks=n(),
    genres=n_distinct(genres),
    minimum_streams=min(streams),
    average_streams=mean(streams),
    median_streams=median(streams),
    maximum_streams=max(streams),
    average_duration=mean(duration_min),
    median_duration=median(duration_min),
    average_followers=mean(artist_followers),
    median_followers=median(artist_followers),
    earliest_release=min(release_year),
    latest_release=max(release_year)
  )

full_numeric_summary

# Missing-value counts for the variables used in this project.
missing_value_summary <- tracks_prepared |>
  summarize(
    missing_streams=sum(is.na(streams)),
    missing_genre=sum(is.na(genres)),
    missing_release_size=sum(is.na(release_size)),
    missing_duration=sum(is.na(duration_min)),
    missing_followers=sum(is.na(log_followers)),
    missing_release_year=sum(is.na(release_year))
  )

missing_value_summary

# Compare means and medians. A mean far above the median signals right skew.
focus_summary <- tracks_focus |>
  group_by(genres) |>
  summarize(
    tracks=n(),
    mean_streams=mean(streams),
    median_streams=median(streams),
    sd_streams=sd(streams),
    mean_duration=mean(duration_min),
    median_duration=median(duration_min),
    mean_followers=mean(artist_followers),
    median_followers=median(artist_followers),
    .groups="drop"
  )

focus_summary

# IQR rules identify potential outliers; they do not automatically prove an
# observation is incorrect.
stream_q1 <- quantile(tracks_focus$streams, 0.25)
stream_q3 <- quantile(tracks_focus$streams, 0.75)
stream_iqr <- IQR(tracks_focus$streams)
stream_upper_bound <- stream_q3+1.5*stream_iqr

duration_q1 <- quantile(tracks_focus$duration_min, 0.25)
duration_q3 <- quantile(tracks_focus$duration_min, 0.75)
duration_iqr <- IQR(tracks_focus$duration_min)
duration_lower_bound <- duration_q1-1.5*duration_iqr
duration_upper_bound <- duration_q3+1.5*duration_iqr

followers_q1 <- quantile(tracks_focus$artist_followers, 0.25)
followers_q3 <- quantile(tracks_focus$artist_followers, 0.75)
followers_iqr <- IQR(tracks_focus$artist_followers)
followers_upper_bound <- followers_q3+1.5*followers_iqr

potential_outliers <- data.frame(
  variable=c("Streams", "Duration", "Artist followers"),
  potential_outlier_count=c(
    sum(tracks_focus$streams>stream_upper_bound),
    sum(tracks_focus$duration_min<duration_lower_bound |
          tracks_focus$duration_min>duration_upper_bound),
    sum(tracks_focus$artist_followers>followers_upper_bound)
  )
)

potential_outliers

# OUTLIER TREATMENT:
# High streams and follower counts may represent real hit songs and major
# artists, so deleting them would remove observations central to UMG's question.
# We therefore retain valid observations and reduce their influence by using
# log_streams and log_followers. Later, standardized residuals are checked and
# a sensitivity model tests whether unusual observations change the conclusions.

plot_stream_distribution <- tracks_focus |>
  ggplot(aes(x=streams))+
  geom_histogram(bins=35, fill=umg_blue, color=umg_white)+
  labs(
    title="Raw Streams Are Strongly Right-Skewed",
    x="Streams",
    y="Track Count"
  )+
  umg_theme

plot_log_stream_distribution <- tracks_focus |>
  ggplot(aes(x=log_streams))+
  geom_histogram(bins=35, fill=umg_green, color=umg_white)+
  labs(
    title="Log Transformation Produces a More Balanced Outcome",
    x="Natural Log of Streams",
    y="Track Count"
  )+
  umg_theme

plot_stream_distribution
plot_log_stream_distribution


############################################################
# 4. DESCRIPTIVE TABLES AND CORRELATIONS
############################################################

# Release size is categorical, so compare its counts and stream summaries.
release_size_summary <- tracks_focus |>
  group_by(genres, release_size) |>
  summarize(
    tracks=n(),
    mean_streams=mean(streams),
    median_streams=median(streams),
    mean_log_streams=mean(log_streams),
    .groups="drop"
  ) |>
  group_by(genres) |>
  mutate(proportion=tracks/sum(tracks)) |>
  ungroup()

release_size_summary

# Numeric correlations provide initial evidence, not final model conclusions.
overall_correlations <- tracks_focus |>
  select(log_streams, duration_min, log_followers, release_year)

round(cor(overall_correlations), 2)

hiphop_correlations <- tracks_focus |>
  filter(genres=="Hip-Hop/Rap") |>
  select(log_streams, duration_min, log_followers, release_year)

pop_correlations <- tracks_focus |>
  filter(genres=="Pop") |>
  select(log_streams, duration_min, log_followers, release_year)

latin_correlations <- tracks_focus |>
  filter(genres=="Latin") |>
  select(log_streams, duration_min, log_followers, release_year)

round(cor(hiphop_correlations), 2)
round(cor(pop_correlations), 2)
round(cor(latin_correlations), 2)

# INTERPRETATION:
# Look at the log_streams row or column. 


############################################################
# 5. SIX VISUALIZATIONS OF THE ROLLOUT FACTORS
############################################################

# GRAPH 1: Streams by release size and genre.
plot_release_size <- tracks_focus |>
  ggplot(aes(x=release_size, y=streams, fill=genres))+
  stat_summary(
    fun=median,
    geom="col",
    position="dodge",
    color=umg_white
  )+
  scale_fill_manual(values=genre_colors)+
  labs(
    title="Median Streams by Release Size and Genre",
    subtitle="Median is used because streams are strongly right-skewed",
    x="Release Size",
    y="Median Streams",
    fill="Genre"
  )+
  umg_theme

# GRAPH 2: Streams by duration and genre.
plot_duration <- tracks_focus |>
  ggplot(aes(x=duration_min, y=log_streams, color=genres))+
  geom_point(alpha=0.22)+
  geom_smooth(method="lm", se=FALSE, linewidth=1.1)+
  scale_color_manual(values=genre_colors)+
  labs(
    title="Track Duration and Streams by Genre",
    x="Track Duration in Minutes",
    y="Natural Log of Streams",
    color="Genre"
  )+
  umg_theme

# GRAPH 3: Streams by artist following and genre.
plot_followers <- tracks_focus |>
  ggplot(aes(x=log_followers, y=log_streams, color=genres))+
  geom_point(alpha=0.22)+
  geom_smooth(method="lm", se=FALSE, linewidth=1.1)+
  scale_color_manual(values=genre_colors)+
  labs(
    title="Artist Following and Streams by Genre",
    subtitle="Following represents artist investment position",
    x="Natural Log of Artist Followers",
    y="Natural Log of Streams",
    color="Genre"
  )+
  umg_theme

# The next graphs focus on releases from 2010 onward.
tracks_since_2010 <- tracks_focus |>
  filter(release_year>=2010)

# GRAPH 4: Release size by release cohort.
plot_release_time <- tracks_since_2010 |>
  ggplot(aes(x=release_size, y=streams, fill=genres))+
  stat_summary(
    fun=median,
    geom="col",
    position="dodge",
    color=umg_white
  )+
  facet_wrap(~release_cohort)+
  scale_fill_manual(values=genre_colors)+
  labs(
    title="Median Streams by Release Size Across Release Cohorts",
    subtitle="Cohorts compare release years; they are not annual stream histories",
    x="Release Size",
    y="Median Streams",
    fill="Genre"
  )+
  umg_theme

# GRAPH 5: Duration by release cohort.
plot_duration_time <- tracks_since_2010 |>
  ggplot(aes(x=duration_min, y=log_streams, color=genres))+
  geom_point(alpha=0.18)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~release_cohort)+
  scale_color_manual(values=genre_colors)+
  labs(
    title="Duration and Streams Across Release Cohorts",
    x="Track Duration in Minutes",
    y="Natural Log of Streams",
    color="Genre"
  )+
  umg_theme

# GRAPH 6: Artist following by release cohort.
plot_followers_time <- tracks_since_2010 |>
  ggplot(aes(x=log_followers, y=log_streams, color=genres))+
  geom_point(alpha=0.18)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~release_cohort)+
  scale_color_manual(values=genre_colors)+
  labs(
    title="Artist Following and Streams Across Release Cohorts",
    x="Natural Log of Artist Followers",
    y="Natural Log of Streams",
    color="Genre"
  )+
  umg_theme

plot_release_size
plot_duration
plot_followers
plot_release_time
plot_duration_time
plot_followers_time

# Optional: save slide-ready images in the working directory.
ggsave("01_release_size_by_genre.png", plot_release_size,
       width=10, height=6, dpi=300, bg="white")
ggsave("02_duration_by_genre.png", plot_duration,
       width=10, height=6, dpi=300, bg="white")
ggsave("03_followers_by_genre.png", plot_followers,
       width=10, height=6, dpi=300, bg="white")
ggsave("04_release_size_by_cohort.png", plot_release_time,
       width=11, height=7, dpi=300, bg="white")
ggsave("05_duration_by_cohort.png", plot_duration_time,
       width=11, height=7, dpi=300, bg="white")
ggsave("06_followers_by_cohort.png", plot_followers_time,
       width=11, height=7, dpi=300, bg="white")


############################################################
# 6. FUNCTION USED TO EVALUATE EVERY MODEL CONSISTENTLY
############################################################

# This function returns the same evaluation measures for each model.
model_metrics <- function(model, model_name) {
  regression_points <- get_regression_points(model)
  f_values <- summary(model)$fstatistic

  data.frame(
    model=model_name,
    observations=nobs(model),
    r_squared=summary(model)$r.squared,
    adjusted_r_squared=summary(model)$adj.r.squared,
    rmse=sqrt(mean(regression_points$residual^2)),
    overall_p_value=pf(
      f_values[1],
      f_values[2],
      f_values[3],
      lower.tail=FALSE
    )
  )
}

# HOW TO READ THE METRICS:
# p-value: below 0.05 means the model provides statistically significant evidence.
# R-squared: percentage of log-stream variability explained by the model.
# Adjusted R-squared: preferred when comparing models with different numbers
#                     of predictors because it penalizes unnecessary complexity.
# RMSE: typical prediction error on the log-stream scale; lower is better when
#       comparing models fitted on the same outcome and observations.


############################################################
# 7. RELEASE-SIZE MODELS
############################################################

# BASIC: same release-size relationship for the full focus sample.
model_release_basic <- lm(
  log_streams~release_size,
  data=tracks_focus
)

# PARALLEL: genres may have different intercepts, but release-size differences
# are assumed to be the same for all genres.
model_release_parallel <- lm(
  log_streams~release_size+genres,
  data=tracks_focus
)

# INTERACTION: release-size differences may vary by genre.
model_release_interaction <- lm(
  log_streams~release_size*genres,
  data=tracks_focus
)

summary(model_release_basic)
summary(model_release_parallel)
summary(model_release_interaction)

# Test whether allowing release-size effects to vary by genre improves the model.
anova(model_release_parallel, model_release_interaction)

release_model_metrics <- bind_rows(
  model_metrics(model_release_basic, "Release size: basic"),
  model_metrics(model_release_parallel, "Release size: parallel"),
  model_metrics(model_release_interaction, "Release size: interaction")
)

release_model_metrics

# There is no numeric slope for release size because it is categorical.
# These predictions estimate each genre/release-size combination.
release_model_predictions <- expand.grid(
  release_size=levels(tracks_focus$release_size),
  genres=levels(tracks_focus$genres)
) |>
  mutate(
    release_size=factor(
      release_size,
      levels=levels(tracks_focus$release_size)
    ),
    genres=factor(
      genres,
      levels=levels(tracks_focus$genres)
    )
  )

release_model_predictions$predicted_log_streams <- predict(
  model_release_interaction,
  newdata=release_model_predictions
)

release_model_predictions <- release_model_predictions |>
  mutate(
    predicted_streams=exp(predicted_log_streams)
  ) |>
  group_by(genres) |>
  mutate(
    single_prediction=predicted_streams[release_size=="1 track"],
    percent_change_vs_single=
      100*(predicted_streams-single_prediction)/single_prediction
  ) |>
  ungroup()

release_model_predictions

# Genre-specific simple models make each genre's release-size coefficients
# and p-values easy to read.
model_release_hiphop <- lm(
  log_streams~release_size,
  data=tracks_focus |> filter(genres=="Hip-Hop/Rap")
)

model_release_pop <- lm(
  log_streams~release_size,
  data=tracks_focus |> filter(genres=="Pop")
)

model_release_latin <- lm(
  log_streams~release_size,
  data=tracks_focus |> filter(genres=="Latin")
)

summary(model_release_hiphop)
summary(model_release_pop)
summary(model_release_latin)

# INTERPRETATION TEMPLATE:
# Relative to a one-track release, [release size] is associated with an
# estimated [100*(exp(coefficient)-1)]% difference in typical streams for
# [genre]. Use "associated," not "causes."


############################################################
# 8. DURATION MODELS
############################################################

model_duration_basic <- lm(
  log_streams~duration_min,
  data=tracks_focus
)

model_duration_parallel <- lm(
  log_streams~duration_min+genres,
  data=tracks_focus
)

model_duration_interaction <- lm(
  log_streams~duration_min*genres,
  data=tracks_focus
)

summary(model_duration_basic)
summary(model_duration_parallel)
summary(model_duration_interaction)
anova(model_duration_parallel, model_duration_interaction)

duration_model_metrics <- bind_rows(
  model_metrics(model_duration_basic, "Duration: basic"),
  model_metrics(model_duration_parallel, "Duration: parallel"),
  model_metrics(model_duration_interaction, "Duration: interaction")
)

duration_model_metrics

# Construct the separate duration equation for each genre.
# Reference genre: Hip-Hop/Rap.
duration_b <- coef(model_duration_interaction)

duration_equations <- data.frame(
  genre=c("Hip-Hop/Rap", "Pop", "Latin"),
  intercept=c(
    duration_b["(Intercept)"],
    duration_b["(Intercept)"]+duration_b["genresPop"],
    duration_b["(Intercept)"]+duration_b["genresLatin"]
  ),
  duration_slope=c(
    duration_b["duration_min"],
    duration_b["duration_min"]+
      duration_b["duration_min:genresPop"],
    duration_b["duration_min"]+
      duration_b["duration_min:genresLatin"]
  )
) |>
  mutate(
    percent_change_per_minute=100*(exp(duration_slope)-1)
  )

duration_equations

# Genre-specific duration models.
model_duration_hiphop <- lm(
  log_streams~duration_min,
  data=tracks_focus |> filter(genres=="Hip-Hop/Rap")
)

model_duration_pop <- lm(
  log_streams~duration_min,
  data=tracks_focus |> filter(genres=="Pop")
)

model_duration_latin <- lm(
  log_streams~duration_min,
  data=tracks_focus |> filter(genres=="Latin")
)

summary(model_duration_hiphop)
summary(model_duration_pop)
summary(model_duration_latin)

# INTERPRETATION TEMPLATE:
# A one-minute increase in duration is associated with a
# [100*(exp(coefficient)-1)]% change in typical streams for [genre].


############################################################
# 9. ARTIST-FOLLOWING MODELS
############################################################

model_followers_basic <- lm(
  log_streams~log_followers,
  data=tracks_focus
)

model_followers_parallel <- lm(
  log_streams~log_followers+genres,
  data=tracks_focus
)

model_followers_interaction <- lm(
  log_streams~log_followers*genres,
  data=tracks_focus
)

summary(model_followers_basic)
summary(model_followers_parallel)
summary(model_followers_interaction)
anova(model_followers_parallel, model_followers_interaction)

followers_model_metrics <- bind_rows(
  model_metrics(model_followers_basic, "Followers: basic"),
  model_metrics(model_followers_parallel, "Followers: parallel"),
  model_metrics(model_followers_interaction, "Followers: interaction")
)

followers_model_metrics

# Construct the separate log-followers equation for each genre.
followers_b <- coef(model_followers_interaction)

followers_equations <- data.frame(
  genre=c("Hip-Hop/Rap", "Pop", "Latin"),
  intercept=c(
    followers_b["(Intercept)"],
    followers_b["(Intercept)"]+followers_b["genresPop"],
    followers_b["(Intercept)"]+followers_b["genresLatin"]
  ),
  followers_slope=c(
    followers_b["log_followers"],
    followers_b["log_followers"]+
      followers_b["log_followers:genresPop"],
    followers_b["log_followers"]+
      followers_b["log_followers:genresLatin"]
  )
)

followers_equations

# IMPORTANT:
# A one-unit increase in log_followers is not one additional follower.
# It represents multiplying followers+1 by approximately e=2.718.
# The percentile predictions later are easier to explain to a business audience.

# Genre-specific follower models.
model_followers_hiphop <- lm(
  log_streams~log_followers,
  data=tracks_focus |> filter(genres=="Hip-Hop/Rap")
)

model_followers_pop <- lm(
  log_streams~log_followers,
  data=tracks_focus |> filter(genres=="Pop")
)

model_followers_latin <- lm(
  log_streams~log_followers,
  data=tracks_focus |> filter(genres=="Latin")
)

summary(model_followers_hiphop)
summary(model_followers_pop)
summary(model_followers_latin)

model_pop_fol <- lm(log_streams~followers, data = filter(tracks_focus, genres=="Pop"))
model_pop_release <- lm(log_streams~release_size, data = filter(tracks_focus, genres=="Pop"))
model_pop_release <- lm(log_streams~release_size*genres, data = filter(tracks_focus))
summary(model_pop_release)

############################################################
# 10. COMBINED CONTROLLED MODEL
############################################################

# Do not use release_size*duration_min*log_followers*genres.
# That formula creates every possible 2-way, 3-way, and 4-way interaction.
# The following model is easier to explain and directly tests whether EACH
# rollout relationship differs by genre while controlling for the other factors.

model_full <- lm(
  log_streams~
    release_size*genres+
    duration_min*genres+
    log_followers*genres+
    years_since_2010,
  data=tracks_focus
)

summary(model_full)

# OVERALL INTERPRETATION:
# Each coefficient is interpreted while holding release size, duration,
# followers, genre, and release cohort constant.
# Interaction terms test whether a variable's relationship with streams differs
# from the Hip-Hop/Rap reference relationship.

# Reduced models test each rollout variable as a group.
model_full_no_release <- lm(
  log_streams~
    duration_min*genres+
    log_followers*genres+
    years_since_2010,
  data=tracks_focus
)

model_full_no_duration <- lm(
  log_streams~
    release_size*genres+
    log_followers*genres+
    years_since_2010,
  data=tracks_focus
)

model_full_no_followers <- lm(
  log_streams~
    release_size*genres+
    duration_min*genres+
    years_since_2010,
  data=tracks_focus
)

# The ANOVA p-values below test whether each variable and its genre interactions
# add significant information after controlling for the other predictors.
release_size_significance <- anova(model_full_no_release, model_full)
duration_significance <- anova(model_full_no_duration, model_full)
followers_significance <- anova(model_full_no_followers, model_full)

release_size_significance
duration_significance
followers_significance

# Compare full-model performance with each reduced model.
full_model_comparison <- bind_rows(
  model_metrics(model_full_no_release, "Without release size"),
  model_metrics(model_full_no_duration, "Without duration"),
  model_metrics(model_full_no_followers, "Without followers"),
  model_metrics(model_full, "Full controlled model")
)


# HOW TO IDENTIFY THE MOST IMPORTANT VARIABLE:
# Do not compare raw coefficient sizes because the variables use different units.
# Instead, consider all four pieces of evidence:
# 1. Its nested-model ANOVA p-value.
# 2. How much adjusted R-squared falls when it is removed.
# 3. How much RMSE rises when it is removed.
# 4. The predicted percentage change across a realistic observed range.


############################################################
# 11. GENRE-SPECIFIC CONTROLLED MODELS
############################################################

# These models use the same predictors for all three genres, which keeps the
# evaluation procedure consistent.

model_hiphop_full <- lm(
  log_streams~release_size+duration_min+log_followers+years_since_2010,
  data=tracks_focus |> filter(genres=="Hip-Hop/Rap")
)

model_pop_full <- lm(
  log_streams~release_size*duration_min*log_followers*years_since_2010,
  data=tracks_focus |> filter(genres=="Pop")
)

model_latin_full <- lm(
  log_streams~release_size+duration_min+log_followers+years_since_2010,
  data=tracks_focus |> filter(genres=="Latin")
)

summary(model_hiphop_full)
summary(model_pop_full)
summary(model_latin_full)

genre_model_metrics <- bind_rows(
  model_metrics(model_hiphop_full, "Hip-Hop/Rap controlled model"),
  model_metrics(model_pop_full, "Pop controlled model"),
  model_metrics(model_latin_full, "Latin controlled model")
)

genre_model_metrics

# Jointly test release size within each genre.
hiphop_no_release <- lm(
  log_streams~duration_min+log_followers+years_since_2010,
  data=tracks_focus |> filter(genres=="Hip-Hop/Rap")
)
pop_no_release <- lm(
  log_streams~duration_min+log_followers+years_since_2010,
  data=tracks_focus |> filter(genres=="Pop")
)
latin_no_release <- lm(
  log_streams~duration_min+log_followers+years_since_2010,
  data=tracks_focus |> filter(genres=="Latin")
)

anova(hiphop_no_release, model_hiphop_full)
anova(pop_no_release, model_pop_full)
anova(latin_no_release, model_latin_full)

# Duration, log_followers, and years_since_2010 each have one coefficient.
# Their individual p-values appear directly in each summary() output.

# Hip-Hop/Rap: Duration × Artist Followers Interaction Model
model_hiphop_interaction <- lm(
  log_streams ~ duration_min*log_followers*release_size,
  data = filter(tracks_focus, genres == "Hip-Hop/Rap")
)

# Model coefficients, p-values, and R-squared
summary(model_hiphop_interaction)

# Model evaluation: R², Adjusted R², RMSE, and overall p-value
model_metrics(
  model_hiphop_interaction,
  "Hip-Hop/Rap: Duration × Followers Interaction"
)

############################################################
# 12. CONTROLLED RELEASE-SIZE PREDICTIONS
############################################################

# Establish typical values within each genre.
genre_typical_values <- tracks_focus |>
  group_by(genres) |>
  summarize(
    typical_duration=median(duration_min),
    typical_log_followers=median(log_followers),
    typical_years_since_2010=median(years_since_2010),
    .groups="drop"
  )

# Predict every release size while holding duration, followers, and release
# cohort at the genre-specific median.
release_predictions <- expand.grid(
  release_size=levels(tracks_focus$release_size),
  genres=levels(tracks_focus$genres)
) |>
  mutate(
    release_size=factor(
      release_size,
      levels=levels(tracks_focus$release_size)
    ),
    genres=factor(
      genres,
      levels=levels(tracks_focus$genres)
    )
  ) |>
  left_join(genre_typical_values, by="genres") |>
  mutate(
    duration_min=typical_duration,
    log_followers=typical_log_followers,
    years_since_2010=typical_years_since_2010
  )

release_predictions$predicted_log_streams <- predict(
  model_full,
  newdata=release_predictions
)

release_predictions <- release_predictions |>
  mutate(
    predicted_streams=exp(predicted_log_streams)
  ) |>
  group_by(genres) |>
  mutate(
    one_track_prediction=predicted_streams[release_size=="1 track"],
    percent_change_vs_one_track=
      100*(predicted_streams-one_track_prediction)/one_track_prediction
  ) |>
  ungroup()

release_predictions

# The highest predicted release size within each genre.
best_release_size <- release_predictions |>
  group_by(genres) |>
  slice_max(predicted_streams, n=1, with_ties=FALSE) |>
  select(
    genres,
    recommended_release_size=release_size,
    predicted_streams,
    percent_change_vs_one_track
  )

best_release_size

# Only present a release-size recommendation if the joint release-size test for
# that genre is significant. Otherwise say the data does not provide strong
# evidence that release size affects streams for that genre.


############################################################
# 13. CONTROLLED DURATION RANGES AND PREDICTIONS
############################################################

# Use the 10th-90th percentile range instead of extreme minimum/maximum values.
# This keeps recommendations inside a common, realistic observed range.
duration_ranges <- tracks_focus |>
  group_by(genres) |>
  summarize(
    low_duration=quantile(duration_min, 0.10),
    typical_duration=median(duration_min),
    high_duration=quantile(duration_min, 0.90),
    typical_log_followers=median(log_followers),
    typical_years_since_2010=median(years_since_2010),
    .groups="drop"
  )

duration_ranges

# Create one prediction grid for each genre.
duration_grid_hiphop <- data.frame(
  genres=factor("Hip-Hop/Rap", levels=levels(tracks_focus$genres)),
  duration_min=seq(
    duration_ranges$low_duration[duration_ranges$genres=="Hip-Hop/Rap"],
    duration_ranges$high_duration[duration_ranges$genres=="Hip-Hop/Rap"],
    length.out=100
  ),
  release_size=factor("1 track", levels=levels(tracks_focus$release_size)),
  log_followers=duration_ranges$typical_log_followers[
    duration_ranges$genres=="Hip-Hop/Rap"
  ],
  years_since_2010=duration_ranges$typical_years_since_2010[
    duration_ranges$genres=="Hip-Hop/Rap"
  ]
)

duration_grid_pop <- data.frame(
  genres=factor("Pop", levels=levels(tracks_focus$genres)),
  duration_min=seq(
    duration_ranges$low_duration[duration_ranges$genres=="Pop"],
    duration_ranges$high_duration[duration_ranges$genres=="Pop"],
    length.out=100
  ),
  release_size=factor("1 track", levels=levels(tracks_focus$release_size)),
  log_followers=duration_ranges$typical_log_followers[
    duration_ranges$genres=="Pop"
  ],
  years_since_2010=duration_ranges$typical_years_since_2010[
    duration_ranges$genres=="Pop"
  ]
)

duration_grid_latin <- data.frame(
  genres=factor("Latin", levels=levels(tracks_focus$genres)),
  duration_min=seq(
    duration_ranges$low_duration[duration_ranges$genres=="Latin"],
    duration_ranges$high_duration[duration_ranges$genres=="Latin"],
    length.out=100
  ),
  release_size=factor("1 track", levels=levels(tracks_focus$release_size)),
  log_followers=duration_ranges$typical_log_followers[
    duration_ranges$genres=="Latin"
  ],
  years_since_2010=duration_ranges$typical_years_since_2010[
    duration_ranges$genres=="Latin"
  ]
)

duration_predictions <- bind_rows(
  duration_grid_hiphop,
  duration_grid_pop,
  duration_grid_latin
) 

duration_predictions$predicted_log_streams <- predict(
  model_full,
  newdata=duration_predictions
)

duration_predictions <- duration_predictions |>
  mutate(predicted_streams=exp(predicted_log_streams)) |>
  left_join(
    duration_ranges |>
      select(genres, typical_duration),
    by="genres"
  )

# Predict the median-duration baseline for each genre.
duration_baselines <- duration_ranges |>
  mutate(
    release_size=factor(
      "1 track",
      levels=levels(tracks_focus$release_size)
    ),
    genres=factor(genres, levels=levels(tracks_focus$genres)),
    duration_min=typical_duration,
    log_followers=typical_log_followers,
    years_since_2010=typical_years_since_2010
  )

duration_baselines$baseline_streams <- exp(predict(
  model_full,
  newdata=duration_baselines
))

duration_baselines <- duration_baselines |>
  select(genres, baseline_streams)

duration_predictions <- duration_predictions |>
  left_join(duration_baselines, by="genres") |>
  mutate(
    percent_change_from_typical=
      100*(predicted_streams-baseline_streams)/baseline_streams,
    distance_from_typical=abs(duration_min-typical_duration)
  )

duration_predictions

# Best model-supported duration inside each genre's 10th-90th percentile range.
best_duration <- duration_predictions |>
  group_by(genres) |>
  slice_max(predicted_streams, n=1, with_ties=FALSE) |>
  select(
    genres,
    recommended_duration_minutes=duration_min,
    predicted_streams,
    percent_change_from_typical
  )

best_duration

# Test possible business-success benchmarks without choosing one in advance.
duration_threshold_5 <- duration_predictions |>
  filter(percent_change_from_typical>=5) |>
  group_by(genres) |>
  slice_min(distance_from_typical, n=1, with_ties=FALSE) |>
  mutate(target_increase="5%")

duration_threshold_10 <- duration_predictions |>
  filter(percent_change_from_typical>=10) |>
  group_by(genres) |>
  slice_min(distance_from_typical, n=1, with_ties=FALSE) |>
  mutate(target_increase="10%")

duration_threshold_20 <- duration_predictions |>
  filter(percent_change_from_typical>=20) |>
  group_by(genres) |>
  slice_min(distance_from_typical, n=1, with_ties=FALSE) |>
  mutate(target_increase="20%")

duration_thresholds <- bind_rows(
  duration_threshold_5,
  duration_threshold_10,
  duration_threshold_20
) |>
  select(
    genres,
    target_increase,
    duration_min,
    predicted_streams,
    percent_change_from_typical
  )

duration_thresholds

plot_duration_predictions <- duration_predictions |>
  ggplot(aes(
    x=duration_min,
    y=predicted_streams,
    color=genres
  ))+
  geom_line(linewidth=1.2)+
  scale_color_manual(values=genre_colors)+
  labs(
    title="Controlled Predicted Streams Across Typical Duration Ranges",
    subtitle="Other model variables held at genre-specific typical values",
    x="Track Duration in Minutes",
    y="Predicted Typical Streams",
    color="Genre"
  )+
  umg_theme

plot_duration_predictions


############################################################
# 14. EMERGING VERSUS ESTABLISHED ARTIST PREDICTIONS
############################################################

# Define emerging, typical, and established positions using within-genre
# follower quartiles. These are descriptive thresholds, not universal labels.
follower_positions <- tracks_focus |>
  group_by(genres) |>
  summarize(
    emerging_log_followers=quantile(log_followers, 0.25),
    typical_log_followers=median(log_followers),
    established_log_followers=quantile(log_followers, 0.75),
    typical_duration=median(duration_min),
    typical_years_since_2010=median(years_since_2010),
    .groups="drop"
  )

follower_positions

follower_predictions <- follower_positions |>
  pivot_longer(
    cols=c(
      emerging_log_followers,
      typical_log_followers,
      established_log_followers
    ),
    names_to="artist_position",
    values_to="log_followers"
  ) |>
  mutate(
    artist_position=case_when(
      artist_position=="emerging_log_followers" ~ "Emerging: 25th percentile",
      artist_position=="typical_log_followers" ~ "Typical: median",
      artist_position=="established_log_followers" ~ "Established: 75th percentile"
    ),
    artist_position=factor(
      artist_position,
      levels=c(
        "Emerging: 25th percentile",
        "Typical: median",
        "Established: 75th percentile"
      )
    ),
    genres=factor(genres, levels=levels(tracks_focus$genres)),
    release_size=factor(
      "1 track",
      levels=levels(tracks_focus$release_size)
    ),
    duration_min=typical_duration,
    years_since_2010=typical_years_since_2010,
    follower_threshold=round(exp(log_followers)-1)
  )

follower_predictions$predicted_log_streams <- predict(
  model_full,
  newdata=follower_predictions
)

follower_predictions <- follower_predictions |>
  mutate(
    predicted_streams=exp(predicted_log_streams)
  ) |>
  group_by(genres) |>
  mutate(
    emerging_prediction=
      predicted_streams[artist_position=="Emerging: 25th percentile"],
    percent_change_vs_emerging=
      100*(predicted_streams-emerging_prediction)/emerging_prediction
  ) |>
  ungroup()

follower_predictions

plot_follower_predictions <- follower_predictions |>
  filter(genres=="Hip-Hop/Rap") |> 
  ggplot(aes(
    x=artist_position,
    y=predicted_streams
  ))+
  geom_col(position="dodge")+
  scale_fill_manual(values=genre_colors)+
  labs(
    title="Predicted Streams by Artist followers",
    subtitle="Comparison uses within-genre follower quartiles",
    x="Artist Position",
    y="Predicted Typical Streams",
  )+
  umg_theme+
  theme(axis.text.x=element_text(angle=15, hjust=1))

plot_follower_predictions

# BUSINESS INTERPRETATION:
# If log_followers is significant and the established-versus-emerging predicted
# gap is large, established artists are associated with stronger immediate
# stream potential. If the relationship is weak or insignificant, UMG may have
# more evidence that emerging artists can compete on streams within that genre.
# This still does not compare investment costs or prove causal return on spending.


############################################################
# 15. RELEASE-COHORT INTERACTIONS
############################################################

# These models ask whether each rollout relationship differs among songs from
# different release years. They do NOT track the same songs' streams over time.

# First compare a model with release cohort as a control.
model_time_parallel <- lm(
  log_streams~
    release_size*genres+
    duration_min*genres+
    log_followers*genres+
    years_since_2010,
  data=tracks_since_2010
)

# Allow each rollout relationship to change by release cohort.
# This adds only rollout-by-time terms, not an uninterpretable four-way product.
model_time_interaction <- lm(
  log_streams~
    release_size*genres+
    duration_min*genres+
    log_followers*genres+
    release_size*years_since_2010+
    duration_min*years_since_2010+
    log_followers*years_since_2010,
  data=tracks_since_2010
)

summary(model_time_interaction)
anova(model_time_parallel, model_time_interaction)

time_model_metrics <- bind_rows(
  model_metrics(model_time_parallel, "Release cohort as control"),
  model_metrics(model_time_interaction, "Rollout by release-cohort interactions")
)

time_model_metrics

# Genre-specific cohort models use identical procedures.
model_time_hiphop <- lm(
  log_streams~
    release_size*years_since_2010+
    duration_min*years_since_2010+
    log_followers*years_since_2010,
  data=tracks_since_2010 |> filter(genres=="Hip-Hop/Rap")
)

model_time_pop <- lm(
  log_streams~
    release_size*years_since_2010+
    duration_min*years_since_2010+
    log_followers*years_since_2010,
  data=tracks_since_2010 |> filter(genres=="Pop")
)

model_time_latin <- lm(
  log_streams~
    release_size*years_since_2010+
    duration_min*years_since_2010+
    log_followers*years_since_2010,
  data=tracks_since_2010 |> filter(genres=="Latin")
)

summary(model_time_hiphop)
summary(model_time_pop)
summary(model_time_latin)

# Significant variable:years_since_2010 terms suggest the association differs
# for newer release cohorts. Positive terms indicate the relationship becomes
# more positive for newer releases; negative terms indicate it becomes less
# positive or more negative. Use "release cohort," not "consumer tastes caused."


############################################################
# 16. RESIDUAL PLOTS, Q-Q PLOTS, RMSE, AND OUTLIERS
############################################################

# Collect regression points and standardized residuals for the full model.
full_points <- get_regression_points(model_full) |>
  mutate(stdres=rstandard(model_full))

full_rmse <- full_points |>
  summarize(rmse=sqrt(mean(residual^2)))

full_rmse

full_outliers <- full_points |>
  filter(abs(stdres)>3)

nrow(full_outliers)
full_outliers

# Residual-versus-predicted plot.
plot_full_residuals <- full_points |>
  ggplot(aes(x=log_streams_hat, y=stdres))+
  geom_hline(yintercept=0, color=umg_black)+
  geom_hline(yintercept=c(-3, 3), color=umg_pink, linetype="dashed")+
  geom_point(color=umg_blue, alpha=0.35)+
  labs(
    title="Full Model: Standardized Residual Plot",
    x="Predicted Natural Log of Streams",
    y="Standardized Residual"
  )+
  umg_theme

# Normal Q-Q plot.
plot_full_qq <- full_points |>
  ggplot(aes(sample=stdres))+
  geom_qq(color=umg_blue, alpha=0.45)+
  geom_qq_line(color=umg_pink, linewidth=1)+
  labs(
    title="Full Model: Normal Q-Q Plot",
    x="Theoretical Quantiles",
    y="Standardized Residual Quantiles"
  )+
  umg_theme

plot_full_residuals
plot_full_qq

# Repeat the same diagnostics for each final genre-specific model.
hiphop_points <- get_regression_points(model_hiphop_full) |>
  mutate(stdres=rstandard(model_hiphop_full))
pop_points <- get_regression_points(model_pop_full) |>
  mutate(stdres=rstandard(model_pop_full))
latin_points <- get_regression_points(model_latin_full) |>
  mutate(stdres=rstandard(model_latin_full))

genre_diagnostic_metrics <- data.frame(
  genre=c("Hip-Hop/Rap", "Pop", "Latin"),
  rmse=c(
    sqrt(mean(hiphop_points$residual^2)),
    sqrt(mean(pop_points$residual^2)),
    sqrt(mean(latin_points$residual^2))
  ),
  standardized_residuals_over_3=c(
    sum(abs(hiphop_points$stdres)>3),
    sum(abs(pop_points$stdres)>3),
    sum(abs(latin_points$stdres)>3)
  )
)

genre_diagnostic_metrics

plot_hiphop_residuals <- hiphop_points |>
  ggplot(aes(x=log_streams_hat, y=stdres))+
  geom_hline(yintercept=0, color=umg_black)+
  geom_hline(yintercept=c(-3, 3), color=umg_pink, linetype="dashed")+
  geom_point(color=umg_black, alpha=0.35)+
  labs(
    title="Hip-Hop/Rap: Standardized Residual Plot",
    x="Predicted Natural Log of Streams",
    y="Standardized Residual"
  )+
  umg_theme

plot_hiphop_qq <- hiphop_points |>
  ggplot(aes(sample=stdres))+
  geom_qq(color=umg_black, alpha=0.45)+
  geom_qq_line(color=umg_green, linewidth=1)+
  labs(title="Hip-Hop/Rap: Normal Q-Q Plot")+
  umg_theme

plot_pop_residuals <- pop_points |>
  ggplot(aes(x=log_streams_hat, y=stdres))+
  geom_hline(yintercept=0, color=umg_black)+
  geom_hline(yintercept=c(-3, 3), color=umg_blue, linetype="dashed")+
  geom_point(color=umg_pink, alpha=0.35)+
  labs(
    title="Pop: Standardized Residual Plot",
    x="Predicted Natural Log of Streams",
    y="Standardized Residual"
  )+
  umg_theme

plot_pop_qq <- pop_points |>
  ggplot(aes(sample=stdres))+
  geom_qq(color=umg_pink, alpha=0.45)+
  geom_qq_line(color=umg_green, linewidth=1)+
  labs(title="Pop: Normal Q-Q Plot")+
  umg_theme

plot_latin_residuals <- latin_points |>
  ggplot(aes(x=log_streams_hat, y=stdres))+
  geom_hline(yintercept=0, color=umg_black)+
  geom_hline(yintercept=c(-3, 3), color=umg_pink, linetype="dashed")+
  geom_point(color=umg_blue, alpha=0.35)+
  labs(
    title="Latin: Standardized Residual Plot",
    x="Predicted Natural Log of Streams",
    y="Standardized Residual"
  )+
  umg_theme

plot_latin_qq <- latin_points |>
  ggplot(aes(sample=stdres))+
  geom_qq(color=umg_blue, alpha=0.45)+
  geom_qq_line(color=umg_green, linewidth=1)+
  labs(title="Latin: Normal Q-Q Plot")+
  umg_theme

plot_hiphop_residuals
plot_hiphop_qq
plot_pop_residuals
plot_pop_qq
plot_latin_residuals
plot_latin_qq

#note there seems to be 3 distinct stream categories for pop songs
# DIAGNOSTIC INTERPRETATION:
# Residual plot: look for random scatter around zero with no curve or funnel.
# Q-Q plot: points close to the line support approximately normal residuals.
# Standardized residuals beyond +/-3 identify observations worth investigating.
# RMSE: lower values mean smaller typical prediction errors when comparing
# models using the same outcome and observations.


############################################################
# 17. OUTLIER SENSITIVITY CHECK
############################################################

# Do not automatically delete unusual observations. Instead, test whether the
# full-model findings materially change without |standardized residual| > 3.
tracks_focus_sensitivity <- tracks_focus[abs(rstandard(model_full))<=3, ]

model_full_sensitivity <- lm(
  log_streams~
    release_size*genres+
    duration_min*genres+
    log_followers*genres+
    years_since_2010,
  data=tracks_focus_sensitivity
)

summary(model_full_sensitivity)

sensitivity_comparison <- bind_rows(
  model_metrics(model_full, "Full sample"),
  model_metrics(model_full_sensitivity, "Excluding |standardized residual| > 3")
)

sensitivity_comparison

# If coefficient directions, significance conclusions, adjusted R-squared, and
# RMSE remain similar, the findings are not being driven by a few unusual tracks.
# If conclusions change, present that instability as a limitation.


############################################################
# 18. TABLES TO USE FOR THE PRESENTATION
############################################################

# Model selection tables.
all_candidate_model_metrics <- bind_rows(
  release_model_metrics,
  duration_model_metrics,
  followers_model_metrics,
  full_model_comparison,
  genre_model_metrics,
  time_model_metrics
)

all_candidate_model_metrics

# Controlled recommendations and scenario outputs.
best_release_size
best_duration
duration_thresholds
follower_predictions
genre_diagnostic_metrics
sensitivity_comparison

# Optional slide-ready model comparison graph.
plot_model_comparison <- all_candidate_model_metrics |>
  filter(
    model=="Release size: interaction" |
      model=="Duration: interaction" |
      model=="Followers: interaction" |
      model=="Full controlled model"
  ) |>
  ggplot(aes(
    x=reorder(model, adjusted_r_squared),
    y=adjusted_r_squared
  ))+
  geom_col(fill=umg_green)+
  coord_flip()+
  labs(
    title="Adjusted R-Squared Across Rollout Models",
    x="Model",
    y="Adjusted R-Squared"
  )+
  umg_theme

plot_model_comparison

ggsave("07_controlled_duration_predictions.png",
       plot_duration_predictions,
       width=10, height=6, dpi=300, bg="white")
ggsave("08_artist_position_predictions.png",
       plot_follower_predictions,
       width=10, height=6, dpi=300, bg="white")
ggsave("09_model_comparison.png",
       plot_model_comparison,
       width=10, height=6, dpi=300, bg="white")
ggsave("10_full_model_residuals.png",
       plot_full_residuals,
       width=9, height=6, dpi=300, bg="white")
ggsave("11_full_model_qq.png",
       plot_full_qq,
       width=9, height=6, dpi=300, bg="white")


############################################################
# 19. HOW TO WRITE THE FINDINGS AFTER RUNNING THE SCRIPT
############################################################

# OVERALL MODEL:
# "The full controlled model was [significant/not significant], with an
# adjusted R-squared of ___ and RMSE of ___. Removing [variable] produced the
# largest [decrease in adjusted R-squared/increase in RMSE], suggesting that it
# provided the strongest incremental information among the tested factors."

# RELEASE SIZE:
# "Holding duration, artist following, release cohort, and genre constant,
# [release size] was associated with ___% [higher/lower] predicted streams than
# a one-track release for [genre]. The joint release-size p-value was ___ ."

# DURATION:
# "Holding the other predictors constant, a one-minute increase in duration was
# associated with a ___% change in predicted streams for [genre]. Within the
# observed 10th-90th percentile range, the model's highest predicted streams
# occurred near ___ minutes."

# ARTIST INVESTMENT:
# "Holding release size, duration, and release cohort constant, an established
# artist at the genre's 75th follower percentile was predicted to receive ___%
# [more/fewer] streams than an emerging artist at the 25th percentile. Because
# followers and streams are measured together, this result supports portfolio
# positioning but does not prove that follower growth causes streams."

# MODEL RELIABILITY:
# "The model's RMSE was ___ on the log-stream scale. The residual plot showed
# [random scatter/pattern], while the Q-Q plot showed [approximate normality/
# deviations]. The sensitivity model [did/did not] materially change the results."


############################################################
# 20. REQUIRED LIMITATIONS AND BUSINESS TRADE-OFFS
############################################################

# 1. The data is observational, so the models show association, not causation.
# 2. Streams are a snapshot, not repeated annual observations.
# 3. release_year compares release cohorts; it does not directly measure a
#    change in consumer behavior over time.
# 4. Current followers may have been gained after a song became successful,
#    creating possible reverse causation.
# 5. The dataset does not contain marketing spend, playlist placement,
#    geographic reach, release costs, royalty rates, or profit margins.
# 6. Multiple tracks may come from the same artist or album, so observations
#    may not be fully independent.
# 7. Linear models identify the highest prediction within the observed range;
#    they do not prove a universal causal optimum.
# 8. A strategy with higher predicted streams may also carry higher production,
#    promotion, or opportunity costs. UMG should consider expected streams
#    together with investment cost and artist-development goals.


############################################################
# 21. OPTIONAL: EXPORT THE MAIN RESULTS AS CSV FILES
############################################################

write.csv(
  all_candidate_model_metrics,
  "UMG_model_metrics.csv",
  row.names=FALSE
)
write.csv(
  release_predictions,
  "UMG_release_size_predictions.csv",
  row.names=FALSE
)
write.csv(
  duration_thresholds,
  "UMG_duration_thresholds.csv",
  row.names=FALSE
)
write.csv(
  follower_predictions,
  "UMG_artist_position_predictions.csv",
  row.names=FALSE
)

# END OF ANALYSIS
