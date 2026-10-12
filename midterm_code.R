# BUAD 312 Midterm Project

library(tidyverse)
#####
# 1. Data Cleaning
# Contributed by: Sonly Nguyen

# SET WD TO FOLDER WITH CSV FILE
# import the CSV and read blank cells as missing values
tracks <- read.csv("tracks_simplified.csv", na.strings=c("", "NA"))

#order by genre popularity
genre_popularity <- tracks |>
  group_by(genres) |>
  summarize(
    track_count = n(),
    average_streams = mean(streams),
    median_streams = median(streams),
    total_streams = sum(streams)
  ) |>
  arrange(desc(median_streams))

genre_popularity

# Create the variables used in the cleaning and analysis sections.
tracks_audit <- tracks |>
  mutate(duration_min=duration_ms/60000,
         empty_markets=available_markets=="[]",
         zero_popularity=popularity==0)

# Extract the year from M/D/YY, YYYY, and YYYY-MM entries.
tracks_prepared <- tracks_audit |>
  mutate(year_text=ifelse(grepl("/", album_release_date),
                          sub(".*/", "", album_release_date),
                          substr(album_release_date, 1, 4)),
         release_year=as.numeric(year_text))

# Convert two-digit years; use 2026 as the century cutoff.
tracks_prepared <- tracks_prepared |>
  mutate(release_year=case_when(
    album_release_date=="0" ~ NA_real_,
    release_year<=26 ~ release_year+2000,
    release_year<100 ~ release_year+1900,
    TRUE ~ release_year))

# Check the converted years.
table(tracks_prepared$release_year, useNA="ifany")

# Keep positive streams, durations, and album sizes; allow zero followers.
tracks_prepared <- tracks_prepared |>
  filter(streams>0, duration_min>0, album_total_tracks>0,
         artist_followers>=0)

# Create natural-log variables and release categories.
tracks_prepared <- tracks_prepared |>
  mutate(log_streams=log(streams),
         log_followers=log(artist_followers+1),
         release_period=case_when(
           release_year<=2009 ~ "Through 2009",
           release_year<=2014 ~ "2010-2014",
           release_year<=2017 ~ "2015-2017",
           release_year>=2018 ~ "2018+"),
         release_size=case_when(
           album_total_tracks==1 ~ "1 track",
           album_total_tracks<=6 ~ "2-6 tracks",
           album_total_tracks>=7 ~ "7+ tracks"))

# Set categorical variables and their reference groups.
tracks_prepared <- tracks_prepared |>
  mutate(explicit=factor(explicit, levels=c(FALSE, TRUE),
                         labels=c("Non-explicit", "Explicit")),
         genres=factor(genres),
         genres=relevel(genres, ref="Hip-Hop/Rap"),
         release_period=factor(release_period,
                               levels=c("Through 2009", "2010-2014", "2015-2017", "2018+")),
         release_size=factor(release_size,
                             levels=c("1 track", "2-6 tracks", "7+ tracks")))

# Select analysis columns and remove rows missing those values.
# Missing artist names are not used to exclude tracks.
tracks_model <- tracks_prepared |>
  select(track_id, streams, log_streams, explicit, genres,
         artist_followers, log_followers, artist_popularity,
         duration_min, release_year, release_period, release_size,
         empty_markets, danceability, energy, valence, acousticness,
         speechiness, liveness, instrumentalness, loudness, tempo) |>
  na.omit()

# Count retained and excluded rows.
data.frame(original_rows=nrow(tracks),
           model_rows=nrow(tracks_model),
           excluded_rows=nrow(tracks)-nrow(tracks_model))

# Summarize the model dataset.
summary(tracks_model)

#######
# 2. Descriptive comparisons and graphs
# Contributed by: Geo Ji

# Count tracks and summarize streams within each group.
genre_summary <- tracks_model |>
  group_by(genres, explicit) |>
  summarize(count=n(),
            mean_streams=mean(streams),
            median_streams=median(streams),
            sd_streams=sd(streams),
            .groups="drop")
genre_summary

# Plot the number of tracks in each group.
genre_summary |>
  ggplot(aes(x=genres, y=count, fill=explicit))+
  geom_col(position="dodge")+
  coord_flip()+
  labs(title="Track Counts by Genre and Explicitness",
       x="Genre", y="Track Count", fill="Lyrics")

# Compare median streams within each genre.
genre_summary |>
  ggplot(aes(x=genres, y=median_streams, fill=explicit))+
  geom_col(position="dodge")+
  coord_flip()+
  labs(title="Median Streams by Genre and Explicitness",
       x="Genre", y="Median Streams", fill="Lyrics")

# Compare log-stream distributions within each genre.
tracks_model |>
  ggplot(aes(x=genres, y=log_streams, fill=explicit))+
  geom_boxplot()+
  coord_flip()+
  labs(title="Log Streams by Genre and Explicitness",
       x="Genre", y="Natural Log of Streams", fill="Lyrics")

# Plot the log-stream distribution used in the regressions.
tracks_model |>
  ggplot(aes(x=log_streams))+
  geom_histogram(bins=30, color="black", fill="lightblue")+
  labs(title="Distribution of Log Streams",
       x="Natural Log of Streams", y="Track Count")

# Calculate correlations among selected numeric variables.
tracks_numeric <- tracks_model |>
  select(streams, log_streams, log_followers, artist_popularity,
         duration_min, danceability, energy, valence)
round(cor(tracks_numeric), 2)

######
# 3. Simple and multiple regression
# Contributed by: [Brian Chun]

# Fit a simple regression using streams in their original units.
model_raw_simple <- lm(streams~explicit, data=tracks_model)
summary(model_raw_simple)

# Model 1: use log streams as the outcome.
model1 <- lm(log_streams~explicit, data=tracks_model)
summary(model1)

# Model 2: add genre, artist, release, and audio variables.
model2 <- lm(log_streams~explicit+genres+log_followers+
               artist_popularity+duration_min+release_period+release_size+
               empty_markets+danceability+energy+valence+acousticness+
               speechiness+liveness+instrumentalness+loudness+tempo,
             data=tracks_model)
summary(model2)

#####
# 4. Genre interaction and model comparison
# Contributed by: [your name]

# Model 3: allow the explicit coefficient to differ by genre.
model3 <- lm(log_streams~explicit*genres+log_followers+
               artist_popularity+duration_min+release_period+release_size+
               empty_markets+danceability+energy+valence+acousticness+
               speechiness+liveness+instrumentalness+loudness+tempo,
             data=tracks_model)
summary(model3)

# Compare R-squared and adjusted R-squared on the same outcome and rows.
model_comparison <- data.frame(
  model=c("1: Explicit only", "2: Controls", "3: Genre interaction"),
  n=c(nobs(model1), nobs(model2), nobs(model3)),
  r_squared=c(summary(model1)$r.squared,
              summary(model2)$r.squared,
              summary(model3)$r.squared),
  adjusted_r_squared=c(summary(model1)$adj.r.squared,
                       summary(model2)$adj.r.squared,
                       summary(model3)$adj.r.squared))
model_comparison

# Test the genre interaction terms together.
anova(model2, model3)

# Use the interaction model for the remaining checks.
# Choose the presentation model after reviewing comparisons and diagnostics.
model_check <- model3

#####
# 5. Explicit versus non-explicit comparisons within each genre
# Contributed by: [your name]

# Refit the same model with each genre as the reference group.
# update() repeats the formula using the supplied dataset.
tracks_latin <- tracks_model |>
  mutate(genres=relevel(genres, ref="Latin"))
model_latin <- update(model3, data=tracks_latin)

tracks_pop <- tracks_model |>
  mutate(genres=relevel(genres, ref="Pop"))
model_pop <- update(model3, data=tracks_pop)

tracks_dance <- tracks_model |>
  mutate(genres=relevel(genres, ref="Electronic/Dance"))
model_dance <- update(model3, data=tracks_dance)

tracks_rock <- tracks_model |>
  mutate(genres=relevel(genres, ref="Rock/Metal"))
model_rock <- update(model3, data=tracks_rock)

tracks_soul <- tracks_model |>
  mutate(genres=relevel(genres, ref="Soul/R&B"))
model_soul <- update(model3, data=tracks_soul)

tracks_other <- tracks_model |>
  mutate(genres=relevel(genres, ref="Other/Unknown"))
model_other <- update(model3, data=tracks_other)

# Extract each genre's explicit coefficient, standard error, t, and p-value.
# These are ordinary lm() p-values, not robust p-values.
genre_results <- rbind(
  coef(summary(model3))["explicitExplicit", ],
  coef(summary(model_latin))["explicitExplicit", ],
  coef(summary(model_pop))["explicitExplicit", ],
  coef(summary(model_dance))["explicitExplicit", ],
  coef(summary(model_rock))["explicitExplicit", ],
  coef(summary(model_soul))["explicitExplicit", ],
  coef(summary(model_other))["explicitExplicit", ])

genre_results <- as.data.frame(genre_results)
names(genre_results) <- c("estimate", "std_error", "t_value", "p_value")
genre_results$genre <- c("Hip-Hop/Rap", "Latin", "Pop", "Electronic/Dance",
                         "Rock/Metal", "Soul/R&B", "Other/Unknown")

# Adjust p-values for the seven within-genre comparisons.
genre_results$p_holm <- p.adjust(genre_results$p_value, method="holm")
genre_results

# Extract 95% confidence intervals from the same models.
genre_intervals <- rbind(
  confint(model3, "explicitExplicit"),
  confint(model_latin, "explicitExplicit"),
  confint(model_pop, "explicitExplicit"),
  confint(model_dance, "explicitExplicit"),
  confint(model_rock, "explicitExplicit"),
  confint(model_soul, "explicitExplicit"),
  confint(model_other, "explicitExplicit"))
genre_results$lower <- genre_intervals[, 1]
genre_results$upper <- genre_intervals[, 2]

# Plot the adjusted differences on the log-stream scale.
genre_results |>
  ggplot(aes(x=genre, y=estimate))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2)+
  coord_flip()+
  labs(title="Adjusted Explicit versus Non-explicit Differences",
       subtitle="95% intervals using ordinary regression standard errors",
       x="Genre", y="Difference in Predicted Log Streams")

#####
# 6. Residual checks and potential outliers
# Contributed by: [your name]

library(moderndive)

# Get fitted values and residuals using the class function.
model_points <- get_regression_points(model_check)
model_points <- model_points |>
  mutate(stdres=rstandard(model_check))

# Calculate in-sample RMSE in log-stream units.
model_points |>
  summarize(rmse=sqrt(mean(residual^2)))

# Plot standardized residuals against predicted log streams.
model_points |>
  ggplot(aes(x=log_streams_hat, y=stdres))+
  geom_point(alpha=0.3)+
  geom_hline(yintercept=0, color="red")+
  labs(title="Residuals versus Predicted Log Streams",
       x="Predicted Log Streams", y="Standardized Residual")

# Check the residual distribution with a Q-Q plot.
model_points |>
  ggplot(aes(sample=stdres))+
  geom_qq(color="darkblue")+
  geom_qq_line(color="red")+
  labs(title="Regression Residual Q-Q Plot")

# List tracks with standardized residuals beyond -3 or 3.
tracks_residuals <- tracks_model |>
  mutate(stdres=rstandard(model_check))
tracks_residuals |>
  filter(abs(stdres)>3) |>
  select(track_id, genres, explicit, streams, stdres)

# Refit without flagged residual outliers as a sensitivity check only.
# Keep all original model rows in the main analysis.
tracks_no_outliers <- tracks_residuals |>
  filter(abs(stdres)<=3)
model_no_outliers <- update(model_check, data=tracks_no_outliers)
summary(model_no_outliers)

# Fit the same interaction model to untransformed streams.
model_raw <- lm(streams~explicit*genres+log_followers+
                  artist_popularity+duration_min+release_period+release_size+
                  empty_markets+danceability+energy+valence+acousticness+
                  speechiness+liveness+instrumentalness+loudness+tempo,
                data=tracks_model)
summary(model_raw)

# Compare raw-model residual plots with the log-model plots above.
# Raw and log R-squared values describe different outcome scales.
raw_points <- get_regression_points(model_raw)
raw_points <- raw_points |>
  mutate(stdres=rstandard(model_raw))

raw_points |>
  ggplot(aes(x=streams_hat, y=stdres))+
  geom_point(alpha=0.3)+
  geom_hline(yintercept=0, color="red")+
  labs(title="Residuals versus Predicted Streams",
       x="Predicted Streams", y="Standardized Residual")

raw_points |>
  ggplot(aes(sample=stdres))+
  geom_qq(color="darkblue")+
  geom_qq_line(color="red")+
  labs(title="Raw-Stream Regression Residual Q-Q Plot")


# 7. Market-availability sensitivity check
# Contributed by: [your name]

# Restrict to tracks with at least one market listed.
tracks_available <- tracks_model |>
  filter(empty_markets==FALSE)
nrow(tracks_available)
table(tracks_available$genres, tracks_available$explicit)

# Refit without empty_markets, which is constant in this subset.
model_available <- lm(log_streams~explicit*genres+log_followers+
                        artist_popularity+duration_min+release_period+release_size+
                        danceability+energy+valence+acousticness+
                        speechiness+liveness+instrumentalness+loudness+tempo,
                      data=tracks_available)
summary(model_available)

# Repeat the Latin comparison in the restricted sample.
tracks_available_latin <- tracks_available |>
  mutate(genres=relevel(genres, ref="Latin"))
model_available_latin <- update(model_available, data=tracks_available_latin)

# Compare the two focal genres across the full and restricted samples.
rbind(
  HipHop_full=coef(summary(model3))["explicitExplicit", ],
  HipHop_available=coef(summary(model_available))["explicitExplicit", ],
  Latin_full=coef(summary(model_latin))["explicitExplicit", ],
  Latin_available=coef(summary(model_available_latin))["explicitExplicit", ])

# Add your model selection, interpretations, and recommendations separately.
