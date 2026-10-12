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
<<<<<<< HEAD
# Contributed by: [Your Name]
=======
# Contributed by: [Brian Chun]
>>>>>>> df729acbfe94138fd33605cb9321cb796b80bd8c

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

#####
# 8. Further Exploration
# Contributed by Zeinab Drameh


############################################################
# Definitions used in this section
############################################################

# Overall genre popularity is measured using total streams.
# Mean and median streams measure typical per-track performance.
# stream_cat measures the performance of each individual track.
#
# Low: fewer than 4,640.25 streams (bottom 25%).
# Medium: 4,640.25 through 39,327.50 streams (middle 50%).
# High: more than 39,327.50 streams (top 25%).


############################################################
# Create individual track stream categories
############################################################

# Display the quartiles used to create stream_cat.
summary(tracks_prepared$streams)


tracks_explore <- tracks_prepared |>
  mutate(
    stream_cat=ifelse(
      streams>39327.5,
      "High",
      ifelse(
        streams<4640.25,
        "Low",
        "Medium"
      )
    )
  )


# Count and proportion of tracks in each stream category.
table(tracks_explore$stream_cat)

proportions(
  table(tracks_explore$stream_cat)
)


############################################################
# Overall genre popularity and per-track performance
############################################################

# total_streams measures overall popularity in this dataset.
# mean_streams and median_streams measure per-track performance.
# high_stream_proportion controls for different genre sample sizes.

genre_popularity <- tracks_explore |>
  group_by(genres) |>
  summarize(
    track_count=n(),
    total_streams=sum(streams, na.rm=TRUE),
    mean_streams=mean(streams, na.rm=TRUE),
    median_streams=median(streams, na.rm=TRUE),
    mean_popularity=mean(popularity, na.rm=TRUE),
    median_popularity=median(popularity, na.rm=TRUE),
    high_stream_tracks=sum(stream_cat=="High"),
    high_stream_proportion=mean(stream_cat=="High"),
    .groups="drop"
  ) |>
  mutate(
    proportion_of_all_streams=
      total_streams/sum(total_streams)
  ) |> 
  arrange(desc(total_streams))

genre_popularity


# Genre with the most total streams, including Other/Unknown.
genre_popularity |>
  filter(
    total_streams==max(total_streams)
  )


# Most popular identifiable genre based on total streams.
# Other/Unknown is excluded because it is not one clear genre.
genre_popularity |>
  filter(genres!="Other/Unknown") |>
  filter(
    total_streams==max(total_streams)
  )


# Compare the three focal genres.
genre_popularity |>
  filter(
    genres=="Hip-Hop/Rap" |
      genres=="Pop" |
      genres=="Latin"
  )

# Among the three identifiable focus genres, Hip-Hop/Rap has the greatest
# total streaming volume. Pop has more total streams than Latin because Pop
# contains many more tracks in the dataset. However, Latin has stronger
# typical per-track performance and the highest proportion of High-stream
# tracks. Therefore, total popularity and per-track performance should not be
# treated as the same outcome.


# Genre with the highest mean streams per track.
genre_popularity |>
  filter(
    mean_streams==max(mean_streams)
  )

#Hip-Hop

# Genre with the highest median streams per track.
genre_popularity |>
  filter(
    median_streams==max(median_streams)
  )
#Hip-Hop

# Genre with the highest proportion of High-stream tracks.
genre_popularity |>
  filter(
    high_stream_proportion==
      max(high_stream_proportion)
  )
#Latin

############################################################
# Genre and stream-category probability tables
############################################################

genre_stream_table <- table(
  tracks_explore$genres,
  tracks_explore$stream_cat
)

genre_stream_table


# Conditional proportions within each genre.
# Each row adds to 1.
# This shows the probability that a track from each genre
# is classified as High, Medium, or Low.
genre_stream_proportions <- proportions(
  genre_stream_table,
  margin=1
)

round(
  genre_stream_proportions,
  digits=2
)




# Conditional proportions within each stream category.
# Each column adds to 1.
# This shows which genres contribute the tracks in each category.
stream_genre_proportions <- proportions(
  genre_stream_table,
  margin=2
)

round(
  stream_genre_proportions,
  digits=2
)

# Latin has the highest within-genre proportion of High-stream tracks
# (about 35%), followed closely by Hip-Hop/Rap (about 34%). Pop has a lower
# High-stream proportion (about 21%). These proportions compare track-level
# performance while controlling for the different number of tracks per genre.

############################################################
# Compare track factors across stream categories
############################################################

stream_category_summary <- tracks_explore |>
  group_by(stream_cat) |>
  summarize(
    track_count=n(),
    mean_streams=mean(streams, na.rm=TRUE),
    median_streams=median(streams, na.rm=TRUE),
    mean_popularity=mean(popularity, na.rm=TRUE),
    median_artist_followers=median(
      artist_followers,
      na.rm=TRUE
    ),
    mean_artist_popularity=mean(
      artist_popularity,
      na.rm=TRUE
    ),
    mean_album_tracks=mean(
      album_total_tracks,
      na.rm=TRUE
    ),
    mean_duration_min=mean(
      duration_min,
      na.rm=TRUE
    ),
    mean_release_year=mean(
      release_year,
      na.rm=TRUE
    ),
    explicit_proportion=mean(
      explicit=="Explicit",
      na.rm=TRUE
    ),
    mean_danceability=mean(
      danceability,
      na.rm=TRUE
    ),
    mean_energy=mean(
      energy,
      na.rm=TRUE
    ),
    mean_valence=mean(
      valence,
      na.rm=TRUE
    ),
    mean_acousticness=mean(
      acousticness,
      na.rm=TRUE
    ),
    mean_speechiness=mean(
      speechiness,
      na.rm=TRUE
    ),
    mean_liveness=mean(
      liveness,
      na.rm=TRUE
    ),
    mean_instrumentalness=mean(
      instrumentalness,
      na.rm=TRUE
    ),
    mean_loudness=mean(
      loudness,
      na.rm=TRUE
    ),
    mean_tempo=mean(
      tempo,
      na.rm=TRUE
    ),
    .groups="drop"
  )

stream_category_summary

############################################################
# Compare the same factors separately for each genre
############################################################

genre_factor_summary <- tracks_explore |>
  group_by(genres) |>
  summarize(
    track_count=n(),
    total_streams=sum(streams, na.rm=TRUE),
    mean_streams=mean(streams, na.rm=TRUE),
    median_streams=median(streams, na.rm=TRUE),
    mean_popularity=mean(popularity, na.rm=TRUE),
    median_artist_followers=median(
      artist_followers,
      na.rm=TRUE
    ),
    mean_artist_popularity=mean(
      artist_popularity,
      na.rm=TRUE
    ),
    mean_album_tracks=mean(
      album_total_tracks,
      na.rm=TRUE
    ),
    mean_duration_min=mean(
      duration_min,
      na.rm=TRUE
    ),
    mean_release_year=mean(
      release_year,
      na.rm=TRUE
    ),
    explicit_proportion=mean(
      explicit=="Explicit",
      na.rm=TRUE
    ),
    mean_danceability=mean(
      danceability,
      na.rm=TRUE
    ),
    mean_energy=mean(
      energy,
      na.rm=TRUE
    ),
    mean_valence=mean(
      valence,
      na.rm=TRUE
    ),
    mean_acousticness=mean(
      acousticness,
      na.rm=TRUE
    ),
    mean_speechiness=mean(
      speechiness,
      na.rm=TRUE
    ),
    mean_liveness=mean(
      liveness,
      na.rm=TRUE
    ),
    mean_instrumentalness=mean(
      instrumentalness,
      na.rm=TRUE
    ),
    mean_loudness=mean(
      loudness,
      na.rm=TRUE
    ),
    mean_tempo=mean(
      tempo,
      na.rm=TRUE
    ),
    .groups="drop"
  )

genre_factor_summary

############################################################
# Overall correlation matrix
############################################################

tracks_numeric <- select_if(
  tracks_explore,
  is.numeric
)

overall_cor <- round(
  cor(
    na.omit(tracks_numeric)
  ),
  digits=2
)

overall_cor

# Release year has the strongest positive correlation with streams, while duration has the strongest negative correlation. Speechiness, danceability, artist popularity, and liveness have smaller positive relationships. However, all correlations are weak, suggesting that streaming performance likely depends on multiple factors and that these relationships may differ by genre.
# 
# Important variables to exclude or avoid combining
# - log_streams is not an independent factor. It is calculated from streams.
# - duration_ms and duration_min are the same measurement. Use only duration_min.
# - artist_followers and log_followers are two versions of the same variable. Use only one.
# - artist_popularity and log_followers have a 0.91 correlation, so including both may be redundant.
# - Energy and loudness have a 0.72 correlation.
# - Energy and acousticness have a −0.58 correlation.


############################################################
# Correlation matrices for the three focal genres
############################################################

hiphop_numeric <- tracks_explore |>
  filter(genres=="Hip-Hop/Rap") |>
  select_if(is.numeric)

hiphop_cor <- round(
  cor(
    na.omit(hiphop_numeric)
  ),
  digits=2
)

hiphop_cor


pop_numeric <- tracks_explore |>
  filter(genres=="Pop") |>
  select_if(is.numeric)

pop_cor <- round(
  cor(
    na.omit(pop_numeric)
  ),
  digits=2
)

pop_cor


latin_numeric <- tracks_explore |>
  filter(genres=="Latin") |>
  select_if(is.numeric)

latin_cor <- round(
  cor(
    na.omit(latin_numeric)
  ),
  digits=2
)

latin_cor

# CORRELATION SUMMARY: HIP-HOP/RAP, POP, AND LATIN
# Most relationships with streams are weak, but the leading factors differ by
# genre. For Hip-Hop/Rap, the strongest useful correlations with raw streams
# are log_followers (r=0.16), artist_popularity (r=0.14), and release_year
# (r=0.10). Artist recognition and recency appear more relevant than audio
# characteristics, although none has a strong relationship with streams.
#
# For Pop, the strongest relationships with raw streams are release_year
# (r=0.14), album_total_tracks (r=-0.10), duration_min (r=-0.09), and track
# popularity (r=0.08). This supports further testing of whether newer,
# shorter Pop songs released on smaller projects perform better per track.
#
# For Latin, the strongest relationships with raw streams are duration_min
# (r=-0.26), liveness (r=0.25), and danceability (r=-0.19). Artist followers
# have essentially no relationship with Latin streams (r=0.00), suggesting
# that existing audience size alone does not distinguish the higher-streaming
# Latin tracks in this dataset.
#
# Overall, artist recognition and recency appear most relevant for Hip-Hop/Rap;
# release year, project size, and duration appear more relevant for Pop; and
# duration, liveness, and danceability show the clearest relationships for
# Latin. These are associations rather than evidence of causation.
#
# MODELING CAUTIONS:
# - Do not use log_streams to predict streams because it is the same outcome
#   on a transformed scale. Use log_streams as the outcome instead.
# - Do not include both duration_ms and duration_min.
# - Do not include both artist_followers and log_followers. Use log_followers.
# - Artist_popularity and log_followers are strongly related. Start with only
#   log_followers so the model does not repeatedly measure artist recognition.
# - Energy and loudness are strongly related. Do not automatically include
#   both in the same starting model.
# - stream_cat is calculated from streams, so it cannot predict log_streams.
# - Key, mode, and time_signature are category codes; their numerical
#   correlations should not be interpreted like continuous measurements.

############################################################
# Categorical comparisons within each genre
############################################################

# Explicitness within each genre.
genre_explicit_table <- table(
  tracks_explore$genres,
  tracks_explore$explicit
)

genre_explicit_table

round(
  proportions(
    genre_explicit_table,
    margin=1
  ),
  digits=2
)
# Hip-Hop/Rap and Latin have the highest proportions of explicit tracks among
# the three focus genres.

# Release size within each genre.
genre_release_size_table <- table(
  tracks_explore$genres,
  tracks_explore$release_size
)

genre_release_size_table

round(
  proportions(
    genre_release_size_table,
    margin=1
  ),
  digits=2
)

# Hip-Hop/Rap has the largest share of tracks belonging to 7+ track releases
# (67%), followed by Latin (60%) and Pop (56%). Pop is more evenly split, with
# 38% of its tracks belonging to one-track releases. Tracks from 2-6 track
# releases are rare across all three genres.
# IMPORTANT: each row represents a track, not a unique release. A ten-track
# album contributes ten rows while a single contributes one. Therefore, these
# values are proportions of tracks belonging to each release size, not the
# percentage of unique releases that are albums or singles.

# Release period within each genre.
genre_release_period_table <- table(
  tracks_explore$genres,
  tracks_explore$release_period
)

genre_release_period_table

round(
  proportions(
    genre_release_period_table,
    margin=1
  ),
  digits=2
)
# All three genres are concentrated in recent releases, but Hip-Hop/Rap is the most recent-heavy, with 59% released in 2018 or later, compared with 47% of Latin and 43% of Pop; Pop has the largest share of older tracks, with 12% released through 2009.

# Major and minor mode within each genre.
genre_mode_table <- table(
  tracks_explore$genres,
  tracks_explore$mode
)

genre_mode_table

round(
  proportions(
    genre_mode_table,
    margin=1
  ),
  digits=2
)


# For the three case-study genres, Hip-Hop/Rap is the only one leaning slightly minor (53%), while Latin and Pop lean major, with 60% and 58% of tracks in major keys, respectively.

############################################################
# Correlation of explicitness and release size with streams
# for each genre
############################################################

genre_stream_correlations <- tracks_explore |>
  mutate(
    explicit_num=ifelse(explicit=="Explicit", 1, 0)
  ) |>
  group_by(genres) |>
  summarize(
    explicit_streams_cor=cor(explicit_num, streams),
    explicit_log_streams_cor=cor(explicit_num, log_streams),
    release_size_streams_cor=cor(album_total_tracks, streams),
    release_size_log_streams_cor=cor(album_total_tracks, log_streams),
    .groups="drop"
  )

genre_stream_correlations


# The relationship between explicitness and streams differs by genre.
# Explicit Hip-Hop/Rap tracks tend to have slightly higher streams, explicit
# Latin tracks tend to have lower streams, and explicitness has almost no
# relationship with Pop streams. Release size has essentially no relationship
# with Hip-Hop/Rap streams, while larger releases are weakly associated with
# lower per-track streams for Pop. The Latin release-size relationship becomes
# much weaker after transforming streams, suggesting that its raw correlation
# may be influenced by unusually high-stream tracks. All correlations are
# weak, so these results show associations rather than strong predictive
# relationships or causation.

############################################################
# Graph overall genre popularity using total streams
############################################################

# This graph shows overall streaming volume in the dataset.
genre_popularity |>
  ggplot(aes(
    x=genres,
    y=total_streams,
    fill=genres
  )) +
  geom_bar(stat="identity") +
  labs(
    title="Total Streams by Genre",
    x="Genre",
    y="Total Streams",
    fill="Genre"
  )


############################################################
# Graph typical per-track performance using median streams
############################################################

# This graph does not measure total genre popularity.
genre_popularity |>
  ggplot(aes(
    x=genres,
    y=median_streams,
    fill=genres
  )) +
  geom_bar(stat="identity") +
  labs(
    title="Median Streams per Track by Genre",
    x="Genre",
    y="Median Streams",
    fill="Genre"
  )


############################################################
# Graph the proportion of High-stream tracks by genre
############################################################

# This graph controls for different genre sample sizes.
genre_popularity |>
  ggplot(aes(
    x=genres,
    y=high_stream_proportion,
    fill=genres
  )) +
  geom_bar(stat="identity") +
  labs(
    title="Proportion of High-Stream Tracks by Genre",
    x="Genre",
    y="Proportion of High-Stream Tracks",
    fill="Genre"
  )
############################################################
# Compare log-stream distributions by genre
############################################################

tracks_explore |>
  ggplot(aes(
    x=genres,
    y=log_streams,
    fill=genres
  )) +
  geom_boxplot() +
  labs(
    title="Log Streams by Genre",
    x="Genre",
    y="Natural Log of Streams",
    fill="Genre"
  )

# Hip-Hop/Rap and Latin have the highest typical per-track streaming
# performance, while Pop falls somewhat lower. However, the large amount of
# overlap between the boxes suggests that genre alone does not strongly
# determine streams. This graph does not show whether explicitness or release
# size is associated with streams; those factors require separate comparisons
# within each genre.
############################################################
# Compare explicitness and release size by genre
############################################################

genre_explicit_summary <- tracks_explore |>
  filter(
    genres=="Hip-Hop/Rap" |
      genres=="Latin" |
      genres=="Pop"
  ) |>
  group_by(genres, explicit) |>
  summarize(
    count=n(),
    mean_streams=mean(streams, na.rm=TRUE),
    median_streams=median(streams, na.rm=TRUE),
    high_stream_proportion=mean(stream_cat=="High", na.rm=TRUE),
    .groups="drop"
  )

genre_explicit_summary


genre_release_size_summary <- tracks_explore |>
  filter(
    genres=="Hip-Hop/Rap" |
      genres=="Latin" |
      genres=="Pop"
  ) |>
  group_by(genres, release_size) |>
  summarize(
    count=n(),
    mean_streams=mean(streams, na.rm=TRUE),
    median_streams=median(streams, na.rm=TRUE),
    high_stream_proportion=mean(stream_cat=="High", na.rm=TRUE),
    .groups="drop"
  )

genre_release_size_summary


genre_explicit_summary |>
  ggplot(aes(
    x=genres,
    y=median_streams,
    fill=explicit
  )) +
  geom_bar(
    stat="identity",
    position="dodge"
  ) +
  labs(
    title="Median Streams by Genre and Explicitness",
    x="Genre",
    y="Median Streams",
    fill="Lyrics"
  )

# Explicitness is associated with streaming performance differently across
# genres. Explicit Hip-Hop/Rap tracks have a higher median number of streams,
# while explicit Latin tracks have a substantially lower median. Explicit Pop
# tracks also have a higher median, but its near-zero correlation indicates
# that explicitness does not consistently separate high- and low-performing
# Pop tracks. A median difference and a weak correlation are not contradictory:
# the median compares the centers of two groups, while correlation also
# reflects the large spread and overlap of observations within those groups.


genre_release_size_summary |>
  ggplot(aes(
    x=genres,
    y=median_streams,
    fill=release_size
  )) +
  geom_bar(
    stat="identity",
    position="dodge"
  ) +
  labs(
    title="Median Streams by Genre and Release Size",
    x="Genre",
    y="Median Streams",
    fill="Release Size"
  )

# Release size has essentially no relationship with Hip-Hop/Rap streams. For
# Pop, larger releases have a consistent weak negative relationship with raw
# and log streams, making release strategy worth testing further. For Latin,
# the negative raw-stream relationship becomes much weaker after using
# log_streams, suggesting that unusually high-stream tracks influence the raw
# result. The 2-6 track category is rare, so its estimates may be less stable.


############################################################
# Explore numeric factors and streams by genre
############################################################

# Replace danceability with another numeric factor as needed.

tracks_explore_2 <- tracks_explore |>
  filter(
    genres=="Hip-Hop/Rap" |
      genres=="Latin" |
      genres=="Pop"
  )

tracks_explore_2 |>
  ggplot(aes(
    x=danceability,
    y=log_streams,
    color=genres
  )) +
  geom_point() +
  geom_smooth(
    method="lm",
    se=FALSE
  ) +
  labs(
    title="Danceability and Streams by Genre",
    x="Danceability",
    y="Natural Log of Streams",
    color="Genre"
  )

tracks_explore_2 |>
  ggplot(aes(
    x=duration_min,
    y=log_streams,
    color=genres
  )) +
  geom_point() +
  geom_smooth(
    method="lm",
    se=FALSE
  ) +
  labs(
    title="Duration and Streams by Genre",
    x="Duration in Minutes",
    y="Natural Log of Streams",
    color="Genre"
  )

tracks_explore_2 |>
  ggplot(aes(
    x=album_total_tracks,
    y=log_streams,
    color=genres
  )) +
  geom_point() +
  geom_smooth(
    method="lm",
    se=FALSE
  ) +
  labs(
    title="Album Size and Streams by Genre",
    x="Number of Tracks on Album",
    y="Natural Log of Streams",
    color="Genre"
  )



# The relationships between track characteristics and streams differ across
# the three focus genres. Danceability has almost no relationship with streams
# for Hip-Hop/Rap (r = 0.04) or Pop (r = 0.05), but it has a clearer negative
# relationship for Latin (r = -0.21). Duration is negatively associated with
# streams for all three genres, although the relationship is strongest for
# Latin (r = -0.27) and much weaker for Hip-Hop/Rap and Pop (both r = -0.08).
# Album size has essentially no relationship with Hip-Hop/Rap (r = -0.01) or
# Latin streams (r = -0.04), but larger Pop releases are weakly associated with
# lower streams per track (r = -0.11). Overall, the plots suggest that shorter,
# less danceable Latin tracks tend to perform better, while smaller Pop
# releases may perform somewhat better per track. None of these factors shows
# a strong relationship with Hip-Hop/Rap streams. The points remain widely
# scattered, so these are weak associations rather than reliable predictions
# or evidence of causation. The Pop album-size trend may also be affected by
# the small number of unusually large releases.    

#measure using duration, danceability, album size, release_size, 



############################################################
# Recommended next steps and focused models
############################################################

# PROJECT GOAL:
# Determine whether the factors associated with streaming performance differ
# across Hip-Hop/Rap, Pop, and Latin, then give the music company separate
# genre-based release and marketing recommendations.
#
# PRIMARY QUESTION:
# Does the relationship between explicitness and streams differ by genre?
# This should be the first model because it directly tests the clearest finding:
# explicitness is positively associated with Hip-Hop/Rap streams, negatively
# associated with Latin streams, and only weakly associated with Pop streams.
#
# SECONDARY QUESTIONS:
# 1. Does release size matter differently by genre, especially for Pop?
# 2. Are shorter tracks more strongly associated with streams for Latin?
# 3. Do danceability and liveness add useful information for Latin?
# 4. Do these relationships remain after controlling for artist audience and
#    release year?
#
# Use log_streams as the outcome because streams are highly skewed.
# Use adjusted R-squared when comparing models with different numbers of
# predictors. Only compare R-squared values when the models use the same
# outcome and the same observations.
#
# Do not put release_size and album_total_tracks in the same model because
# both measure project size. Do not use stream_cat as a predictor because it
# is created directly from streams.


# Keep the same complete observations for every focus-model comparison.
tracks_focus_model <- tracks_explore_2 |>
  select(
    log_streams,
    explicit,
    genres,
    release_size,
    duration_min,
    danceability,
    liveness,
    log_followers,
    release_year
  ) |>
  na.omit()


# STEP 1: Compare a parallel explicitness model with an interaction model.
# The parallel model assumes explicitness has the same relationship with
# streams in every genre. The interaction model allows that relationship to
# differ across genres.

model_explicit_parallel <- lm(
  log_streams~explicit+genres,
  data=tracks_focus_model
)

summary(model_explicit_parallel)
# Call:
#   lm(formula = log_streams ~ explicit + genres, data = tracks_focus_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1756 -1.0367  0.1878  0.9510  4.0242 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       9.96176    0.05889 169.163  < 2e-16 ***
#   explicitExplicit  0.14240    0.06310   2.257  0.02409 *  
#   genresLatin      -0.24793    0.08681  -2.856  0.00432 ** 
#   genresPop        -0.55476    0.06405  -8.661  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.346 on 3195 degrees of freedom
# Multiple R-squared:  0.04916,	Adjusted R-squared:  0.04827 
# F-statistic: 55.06 on 3 and 3195 DF,  p-value: < 2.2e-16
# 


model_explicit_interaction <- lm(
  log_streams~explicit*genres,
  data=tracks_focus_model
)
summary(model_explicit_interaction)

# Call:
#   lm(formula = log_streams ~ explicit * genres, data = tracks_focus_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2103 -1.0319  0.2063  0.9425  3.9896 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   9.87830    0.07076 139.597  < 2e-16 ***
#   explicitExplicit              0.26049    0.08418   3.095  0.00199 ** 
#   genresLatin                   0.03843    0.11044   0.348  0.72789    
# genresPop                    -0.48527    0.07885  -6.155 8.47e-10 ***
#   explicitExplicit:genresLatin -0.87235    0.18389  -4.744 2.19e-06 ***
#   explicitExplicit:genresPop    0.03714    0.14326   0.259  0.79548    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.341 on 3193 degrees of freedom
# Multiple R-squared:  0.05656,	Adjusted R-squared:  0.05508 
# F-statistic: 38.28 on 5 and 3193 DF,  p-value: < 2.2e-16

anova(
  model_explicit_parallel,
  model_explicit_interaction
)
# Analysis of Variance Table
# 
# Model 1: log_streams ~ explicit + genres
# Model 2: log_streams ~ explicit * genres
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1   3195 5784.9                                  
# 2   3193 5739.9  2    45.021 12.522 3.827e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# STEP 2: Test whether release-size categories behave differently by genre.
# This is more directly connected to a single-versus-album recommendation than
# using album_total_tracks as a continuous number. Interpret the 2-6 track
# category cautiously because it contains relatively few observations.

model_release_parallel <- lm(
  log_streams~release_size+genres,
  data=tracks_focus_model
)

summary(model_release_parallel)

# Call:
#   lm(formula = log_streams ~ release_size + genres, data = tracks_focus_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2912 -1.0387  0.2008  0.9462  4.0208 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            10.21774    0.05175 197.436  < 2e-16 ***
#   release_size2-6 tracks -0.33958    0.11861  -2.863  0.00423 ** 
#   release_size7+ tracks  -0.22127    0.05063  -4.370 1.28e-05 ***
#   genresLatin            -0.32239    0.08217  -3.924 8.91e-05 ***
#   genresPop              -0.65152    0.05126 -12.710  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model_release_interaction <- lm(
  log_streams~release_size*genres,
  data=tracks_focus_model
)

summary(model_release_interaction)

# Call:
#   lm(formula = log_streams ~ release_size * genres, data = tracks_focus_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.0309 -1.0316  0.2036  0.9422  4.0707 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        10.08968    0.06891 146.420  < 2e-16 ***
#   release_size2-6 tracks             -0.08566    0.28737  -0.298  0.76566    
# release_size7+ tracks              -0.03814    0.08319  -0.458  0.64666    
# genresLatin                        -0.30938    0.13768  -2.247  0.02470 *  
#   genresPop                          -0.42106    0.08757  -4.808 1.59e-06 ***
#   release_size2-6 tracks:genresLatin  1.31915    0.52517   2.512  0.01206 *  
#   release_size7+ tracks:genresLatin  -0.06849    0.17268  -0.397  0.69167    
# release_size2-6 tracks:genresPop   -0.51097    0.31835  -1.605  0.10858    
# release_size7+ tracks:genresPop    -0.33548    0.10870  -3.086  0.00204 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.338 on 3190 degrees of freedom
# Multiple R-squared:  0.06137,	Adjusted R-squared:  0.05901 
# F-statistic: 26.07 on 8 and 3190 DF,  p-value: < 2.2e-16



anova(
  model_release_parallel,
  model_release_interaction
)

# Analysis of Variance Table

# Model 1: log_streams ~ release_size + genres
# Model 2: log_streams ~ release_size * genres
# Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
# 1   3194 5754.0                                 
# 2   3190 5710.6  4    43.365 6.056 7.488e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# STEP 3: Test whether the track-level relationships differ by genre.
# Duration, danceability, and liveness are included because the exploratory
# results suggest that they are most relevant for Latin.

model_track_parallel <- lm(
  log_streams~duration_min+danceability+liveness+genres,
  data=tracks_focus_model
)

summary(model_track_parallel)

# Call:
#   lm(formula = log_streams ~ duration_min + danceability + liveness + 
#        genres, data = tracks_focus_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.3881 -1.0170  0.1798  0.9333  4.0205 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  10.64932    0.20065  53.073  < 2e-16 ***
#   duration_min -0.19203    0.03222  -5.960  2.8e-09 ***
#   danceability  0.04433    0.19053   0.233 0.816028    
# liveness      0.25674    0.16763   1.532 0.125716    
# genresLatin  -0.27172    0.08226  -3.303 0.000966 ***
#   genresPop    -0.61058    0.05295 -11.532  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.339 on 3193 degrees of freedom
# Multiple R-squared:  0.05914,	Adjusted R-squared:  0.05767 
# F-statistic: 40.14 on 5 and 3193 DF,  p-value: < 2.2e-16

model_track_interaction <- lm(
  log_streams~duration_min*genres+
    danceability*genres+
    liveness*genres,
  data=tracks_focus_model
)

summary(model_track_interaction)

# Call:
#   lm(formula = log_streams ~ duration_min * genres + danceability * 
#        genres + liveness * genres, data = tracks_focus_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.5590 -1.0109  0.1893  0.9326  4.1629 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              10.26008    0.31673  32.393  < 2e-16 ***
#   duration_min             -0.12115    0.04632  -2.615 0.008955 ** 
#   genresLatin               2.47618    0.68577   3.611 0.000310 ***
#   genresPop                -0.52846    0.42682  -1.238 0.215757    
#  danceability              0.28355    0.32051   0.885 0.376391    
#  liveness                  0.07826    0.27431   0.285 0.775427    
#  duration_min:genresLatin -0.29910    0.09398  -3.183 0.001473 ** 
#  duration_min:genresPop   -0.01754    0.07074  -0.248 0.804225    
#  genresLatin:danceability -2.66303    0.79509  -3.349 0.000819 ***
#  genresPop:danceability    0.07115    0.40816   0.174 0.861631    
#  genresLatin:liveness      1.42306    0.48174   2.954 0.003160 ** 
#   genresPop:liveness       -0.34432    0.37199  -0.926 0.354723    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.33 on 3187 degrees of freedom
# Multiple R-squared:  0.07313,	Adjusted R-squared:  0.06993 
# F-statistic: 22.86 on 11 and 3187 DF,  p-value: < 2.2e-16

anova(
  model_track_parallel,
  model_track_interaction
)

# Analysis of Variance Table
# 
# Model 1: log_streams ~ duration_min + danceability + liveness + genres
# Model 2: log_streams ~ duration_min * genres + danceability * genres + 
#   liveness * genres
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1   3193 5724.2                                  
# 2   3187 5639.0  6    85.108 8.0167 1.313e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# STEP 4: Candidate combined model.
# This model tests the main project findings together while controlling for
# artist audience and release year. Start with the smaller models above before
# interpreting this model because its many interaction coefficients make it
# more difficult to explain.

model_focus_combined <- lm(
  log_streams~explicit*genres+
    release_size*genres+
    duration_min*genres+
    danceability*genres+
    liveness*genres+
    log_followers+
    release_year,
  data=tracks_focus_model
)

summary(model_focus_combined)
# Call:
# lm(formula = log_streams ~ explicit * genres + release_size * 
#     genres + duration_min * genres + danceability * genres + 
#     liveness * genres + log_followers + release_year, data = tracks_focus_model)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.5037 -0.9702  0.1915  0.9039  4.0613 
# 
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        -41.505671   9.485129  -4.376 1.25e-05 ***
# explicitExplicit                     0.216913   0.083941   2.584 0.009807 ** 
# genresLatin                          2.920877   0.709725   4.116 3.96e-05 ***
# genresPop                           -0.349210   0.429809  -0.812 0.416579    
# release_size2-6 tracks               0.019336   0.282366   0.068 0.945408    
# release_size7+ tracks               -0.030899   0.083788  -0.369 0.712316    
# duration_min                        -0.098919   0.046144  -2.144 0.032134 *  
# danceability                         0.204608   0.319728   0.640 0.522255    
# liveness                             0.031107   0.270976   0.115 0.908614    
# log_followers                        0.038335   0.012154   3.154 0.001625 ** 
# release_year                         0.025307   0.004699   5.386 7.74e-08 ***
# explicitExplicit:genresLatin        -0.566950   0.192768  -2.941 0.003294 ** 
# explicitExplicit:genresPop          -0.010132   0.142038  -0.071 0.943135    
# genresLatin:release_size2-6 tracks   0.326597   0.539972   0.605 0.545328    
# genresPop:release_size2-6 tracks    -0.569563   0.312718  -1.821 0.068651 .  
# genresLatin:release_size7+ tracks   -0.394238   0.176600  -2.232 0.025659 *  
# genresPop:release_size7+ tracks     -0.242304   0.109551  -2.212 0.027052 *  
# genresLatin:duration_min            -0.298706   0.099147  -3.013 0.002609 ** 
# genresPop:duration_min               0.036634   0.070464   0.520 0.603175    
# genresLatin:danceability            -2.619603   0.789498  -3.318 0.000917 ***
# genresPop:danceability               0.011692   0.407326   0.029 0.977103    
# genresLatin:liveness                 1.214776   0.494871   2.455 0.014152 *  
# genresPop:liveness                  -0.275173   0.367208  -0.749 0.453691    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.311 on 3176 degrees of freedom
# Multiple R-squared:  0.1026,	Adjusted R-squared:  0.09637 
# F-statistic:  16.5 on 22 and 3176 DF,  p-value: < 2.2e-16

# Compare adjusted R-squared using the same outcome and observations.
focus_model_comparison <- data.frame(
  model=c(
    "Explicit parallel",
    "Explicit interaction",
    "Release-size interaction",
    "Track-factor interaction",
    "Combined interaction"
  ),
  adjusted_r_squared=c(
    summary(model_explicit_parallel)$adj.r.squared,
    summary(model_explicit_interaction)$adj.r.squared,
    summary(model_release_interaction)$adj.r.squared,
    summary(model_track_interaction)$adj.r.squared,
    summary(model_focus_combined)$adj.r.squared
  )
)

focus_model_comparison

# model adjusted_r_squared
# 1        Explicit parallel         0.04826621
# 2     Explicit interaction         0.05508162
# 3 Release-size interaction         0.05901220
# 4 Track-factor interaction         0.06993447
# 5     Combined interaction         0.09636833

############################################################
# Optional genre-specific starting models
############################################################

# These models make the final recommendations easier to explain. They should
# be used after the interaction models establish that genres differ.

model_hiphop_focus <- lm(
  log_streams~explicit+log_followers+release_year,
  data=tracks_focus_model |>
    filter(genres=="Hip-Hop/Rap")
)

model_pop_focus <- lm(
  log_streams~explicit+release_size+duration_min+
    log_followers+release_year,
  data=tracks_focus_model |>
    filter(genres=="Pop")
)

model_latin_focus <- lm(
  log_streams~explicit+duration_min+liveness+danceability+
    log_followers+release_year,
  data=tracks_focus_model |>
    filter(genres=="Latin")
)

summary(model_hiphop_focus)
summary(model_pop_focus)
summary(model_latin_focus)


# MODEL-SELECTION NOTES:
# - Prefer the simpler model if added interactions do not improve adjusted
#   R-squared and are not useful in the model comparison.
# - Focus interpretation on coefficient direction, size, and p-value rather
#   than selecting a model only because it has the highest ordinary R-squared.
# - Run the existing residual-versus-predicted and Q-Q checks on the selected
#   model before presenting it.
# - If the Pop release-size result disappears after controls are added, do not
#   make a strong recommendation about singles versus albums.
# - If the Latin duration, liveness, or danceability coefficients remain after
#   controls, they provide stronger support for a Latin-specific track strategy.
# - If the explicit-by-genre interaction remains useful after controls, it is
#   the strongest evidence for the main recommendation that explicit-content
#   strategy should differ across genres.
# - These models describe associations in an observational dataset. They do
#   not prove that changing lyrics, song length, or release size will cause a
#   change in streams.

