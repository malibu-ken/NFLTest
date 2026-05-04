NFLTest-RMD
================
Samuel Shomette
2026-05-04

## Loading Player Data

``` r
#Load 2025 GSIS ID information
rosters_2025 <- load_rosters(2025) %>%
  distinct(gsis_id)
```

Creating bridge table to bring all player information under same ID
system. Rosters and players have different ID systems, combining under
common IDs.

``` r
players_bridge <- load_players() %>%
  filter(!is.na(gsis_id), !is.na(pfr_id)) %>%
  distinct(pfr_id, .keep_all = TRUE) %>%
  dplyr::select(gsis_id, pfr_id, display_name, position)

combine_with_gsis <- load_combine() %>%
  left_join(players_bridge, by = "pfr_id")

combine_2025_players <- combine_with_gsis %>%
  semi_join(rosters_2025,by = "gsis_id")

combine_2025_skill <- combine_2025_players %>%
  filter(position %in% c("WR", "TE", "RB", "FB"))


#Renaming combine_season to season as we're assuming combine season is draft season.
combine_clean <- combine_2025_players %>% rename(combine_season = season)
```

Loading general play by play data with added player info including
position. For this and future queries we are limiting to PASS plays to
make scope manageable.

``` r
pbp <- load_pbp(2025) %>%
  filter(play_type == "pass",
         season_type == "REG")

rcvrs <- load_players() %>%
  filter(position %in% c("WR", "TE", "RB", "FB"))

full_pbp <- pbp %>%
  left_join(rcvrs, by = c("receiver_player_id" = "gsis_id")) %>%
  filter(!is.na(receiver_player_id))

player_2025_stats <- full_pbp %>%
  filter(season == 2025, play_type == "pass",
    !is.na(receiver_player_id) #Again, pass plays only - must have a receiver in the play
  ) %>% group_by(receiver_player_id) %>%
  summarise( nfl_season = 2025, targets = n(),    # receiver appeared as target
    receptions = sum(complete_pass == 1),         # completed to that receiver
    yards = sum(receiving_yards, na.rm = TRUE),
    .groups = "drop")

final_df <- player_2025_stats %>%
  left_join(combine_clean, by = c("receiver_player_id" = "gsis_id")) %>%
  filter(position %in% c("WR", "TE", "RB", "FB"))
```

Cleaning up data and adding additional metrics worth analysis.

``` r
#Cleaning up df - converting height to inches
final_df <- final_df %>%
  mutate(height_inches =
      as.numeric(sub("-.*$", "", ht)) * 12 +     # feet → inches
      as.numeric(sub("^.*-", "", ht))             # add remaining inches
  ) %>% dplyr::select(-ht)

#Additionally - adding yards per reception to df as this efficiency metric is useful
final_df <- final_df %>%
  mutate(ypr = yards/receptions)

#Adding AGE to df as well
players_age <- load_players() %>%
  dplyr::select(gsis_id, birth_date)

season_start <- as.Date("2025-09-04")

final_df <- final_df %>%
  left_join(players_age, by = c("receiver_player_id" = "gsis_id")) %>%
  mutate(
    birth_date = as.Date(birth_date),
    age_2025 = as.numeric(difftime(season_start, birth_date, units = "days")) / 365.25
  )
```

## Modeling Player Data

We now having combine data attached to regular season statistics for all
eligible receivers in the 2025 regular season. Re: combine - not all
players did all exercises. We must condition on non-missing values
moving forward.

``` r
#Example: 40-yard dash
df_forty <- final_df %>% filter(!is.na(forty))
fast_players <- df_forty %>% dplyr::select(display_name, forty, position, ypr, receptions, yards) %>%
  arrange(forty)
fast_players
```

    ## # A tibble: 282 × 6
    ##    display_name        forty position    ypr receptions yards
    ##    <chr>               <dbl> <chr>     <dbl>      <int> <dbl>
    ##  1 Xavier Worthy        4.21 WR        12.7          42   532
    ##  2 Tyquan Thornton      4.28 WR        23.1          19   438
    ##  3 Matthew Golden       4.29 WR        12.4          29   361
    ##  4 Dont'e Thornton Jr.  4.3  WR        13.5          10   135
    ##  5 Curtis Samuel        4.31 WR        11.6           7    81
    ##  6 Calvin Austin III    4.32 WR        12            31   372
    ##  7 De'Von Achane        4.32 RB         7.18         67   481
    ##  8 Bhayshul Tuten       4.32 RB         7.9          10    79
    ##  9 Brandin Cooks        4.33 WR        11.6          24   279
    ## 10 Mecole Hardman       4.33 WR       NaN             0     0
    ## # ℹ 272 more rows

``` r
player_names <- load_players() %>%
  dplyr::select(gsis_id, display_name)

#Those dudes are fast!

#Some example analyses from here:

#1. Linear regression model of receptions:
lm_rc <- lm(receptions ~ forty, data = df_forty)
summary(lm_rc)
```

    ## 
    ## Call:
    ## lm(formula = receptions ~ forty, data = df_forty)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -31.570 -18.988  -5.253  10.538  93.622 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   114.37      50.42   2.268   0.0241 *
    ## forty         -19.12      11.14  -1.716   0.0872 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.72 on 280 degrees of freedom
    ## Multiple R-squared:  0.01041,    Adjusted R-squared:  0.006876 
    ## F-statistic: 2.945 on 1 and 280 DF,  p-value: 0.08723

``` r
#2. Yards Per Reception:
lm_ypr <- lm(ypr ~ forty, data = df_forty)
summary(lm_ypr)
```

    ## 
    ## Call:
    ## lm(formula = ypr ~ forty, data = df_forty)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.9945 -2.7012 -0.1828  2.2011 21.0055 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   40.107      8.659   4.632 5.61e-06 ***
    ## forty         -6.587      1.913  -3.444 0.000664 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.206 on 273 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.04163,    Adjusted R-squared:  0.03812 
    ## F-statistic: 11.86 on 1 and 273 DF,  p-value: 0.0006637

``` r
#3. 40 times vs. receptions holding targets fixed:
lm_rec_controlled <- lm(receptions ~ forty + targets, data=df_forty)
summary(lm_rec_controlled)
```

    ## 
    ## Call:
    ## lm(formula = receptions ~ forty + targets, data = df_forty)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -20.1185  -2.5543   0.0519   2.3482  17.4602 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -39.24829   10.89932  -3.601 0.000375 ***
    ## forty         8.70234    2.39506   3.633 0.000333 ***
    ## targets       0.66643    0.00866  76.954  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.254 on 279 degrees of freedom
    ## Multiple R-squared:  0.9555, Adjusted R-squared:  0.9552 
    ## F-statistic:  2994 on 2 and 279 DF,  p-value: < 2.2e-16

Diving a bit deeper into this one, forty estimate is 8.70234, i.e. For
each second ADDED to the 40 there is to be expected 8.7 more receptions
in the regular season, holding targets constant. This suggests an
advantage of player archetype rather than a speed advantage.

``` r
by_pos <- lm(receptions ~  0 + position + forty:position, data = df_forty)
summary(by_pos)
```

    ## 
    ## Call:
    ## lm(formula = receptions ~ 0 + position + forty:position, data = df_forty)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -31.809 -19.745  -3.758  11.015  94.002 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                  Estimate Std. Error t value Pr(>|t|)  
    ## positionFB          8.000     24.485   0.327   0.7441  
    ## positionRB        263.143    134.349   1.959   0.0512 .
    ## positionTE        170.644    128.766   1.325   0.1862  
    ## positionWR         -8.142     97.392  -0.084   0.9334  
    ## positionFB:forty       NA         NA      NA       NA  
    ## positionRB:forty  -53.520     29.861  -1.792   0.0742 .
    ## positionTE:forty  -30.858     27.556  -1.120   0.2638  
    ## positionWR:forty    9.018     21.830   0.413   0.6798  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.48 on 275 degrees of freedom
    ## Multiple R-squared:  0.5793, Adjusted R-squared:  0.5686 
    ## F-statistic:  54.1 on 7 and 275 DF,  p-value: < 2.2e-16

There’s not much for fullbacks here, so it’s showing up as NA across the
board.

WR: A slower 40 time by one second leads to an average of 9 more
receptions. However, this is more descriptive than causal so we’ll
circle back to this. Also of note: p-value for positionWR:forty is .68!
So not very strong.

RB: A *faster* 40 of 1 second implies almost *54* more receptions.
Again, descriptive rather than causal - faster RBs are part of passing
game. Here, p-value *is* statistically significant.

TE: A faster 40 of 1 second implies 31 more receptions on the season.
However, p-value is relatively high and this is a less sensitive metric
than it was for RBs.

Zoning in on WR: let’s look instead at 3-cone drill for agility since
speed isn’t a smoking gun metric to determine receptions.

``` r
df_wr <- final_df %>%
  filter(position == "WR", !is.na(cone), receptions > 0) %>%
  mutate(yards_per_target = yards / targets, catch_rate = receptions / targets)

lm_ypt <- lm(yards_per_target ~ cone, data = df_wr)
summary(lm_ypt)
```

    ## 
    ## Call:
    ## lm(formula = yards_per_target ~ cone, data = df_wr)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5729 -1.4709 -0.2137  1.0741  5.5812 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   15.166     10.120   1.499    0.138
    ## cone          -1.077      1.460  -0.738    0.463
    ## 
    ## Residual standard error: 2.179 on 70 degrees of freedom
    ## Multiple R-squared:  0.007713,   Adjusted R-squared:  -0.006463 
    ## F-statistic: 0.5441 on 1 and 70 DF,  p-value: 0.4632

``` r
lm_cr <- lm(catch_rate ~ cone, data = df_wr)
summary(lm_cr)
```

    ## 
    ## Call:
    ## lm(formula = catch_rate ~ cone, data = df_wr)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.28509 -0.07907 -0.02148  0.08276  0.26273 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  1.16182    0.50561   2.298   0.0246 *
    ## cone        -0.08152    0.07293  -1.118   0.2674  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1088 on 70 degrees of freedom
    ## Multiple R-squared:  0.01754,    Adjusted R-squared:  0.003503 
    ## F-statistic:  1.25 on 1 and 70 DF,  p-value: 0.2674

Unfortunately, same story, as there is almost no correlation here and
p-values are not statistically significant in any way. This still shows
up even when accounting for age.

``` r
ypr_ageadjust <- lm(yards_per_target ~ cone + age_2025, data = df_wr)
summary(ypr_ageadjust)
```

    ## 
    ## Call:
    ## lm(formula = yards_per_target ~ cone + age_2025, data = df_wr)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8554 -1.6066 -0.2714  1.0349  5.3548 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 16.92654   10.28648   1.646    0.104
    ## cone        -0.98942    1.46312  -0.676    0.501
    ## age_2025    -0.08675    0.08961  -0.968    0.336
    ## 
    ## Residual standard error: 2.18 on 69 degrees of freedom
    ## Multiple R-squared:  0.02101,    Adjusted R-squared:  -0.007365 
    ## F-statistic: 0.7404 on 2 and 69 DF,  p-value: 0.4807

``` r
cr_ageadjust <- lm(catch_rate ~ cone + age_2025, data = df_wr)
summary(cr_ageadjust)
```

    ## 
    ## Call:
    ## lm(formula = catch_rate ~ cone + age_2025, data = df_wr)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.28820 -0.07585 -0.01987  0.07666  0.25808 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  1.223923   0.515675   2.373   0.0204 *
    ## cone        -0.078445   0.073348  -1.069   0.2886  
    ## age_2025    -0.003060   0.004492  -0.681   0.4980  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1093 on 69 degrees of freedom
    ## Multiple R-squared:  0.0241, Adjusted R-squared:  -0.004185 
    ## F-statistic: 0.852 on 2 and 69 DF,  p-value: 0.431

Since we’ve already explored a few bivariate models, let’s move towards
a fuller model to see what we can do.

``` r
#Defining full model for stepwise AIC

full_wr_model <- lm(
  yards_per_target ~ forty + cone + shuttle + vertical + age_2025, data = df_wr)
summary(full_wr_model)
```

    ## 
    ## Call:
    ## lm(formula = yards_per_target ~ forty + cone + shuttle + vertical + 
    ##     age_2025, data = df_wr)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0563 -1.6042  0.1701  0.9662  5.1614 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  23.0955    21.2662   1.086    0.282
    ## forty        -1.7358     3.3774  -0.514    0.609
    ## cone          0.5519     2.1404   0.258    0.797
    ## shuttle      -0.6548     2.6386  -0.248    0.805
    ## vertical     -0.1324     0.1366  -0.969    0.337
    ## age_2025     -0.1344     0.1078  -1.246    0.218
    ## 
    ## Residual standard error: 2.298 on 55 degrees of freedom
    ##   (11 observations deleted due to missingness)
    ## Multiple R-squared:  0.04052,    Adjusted R-squared:  -0.0467 
    ## F-statistic: 0.4646 on 5 and 55 DF,  p-value: 0.8009

``` r
#Again, initial returns not great but we can keep moving here.


null_wr_model <- lm(yards_per_target ~ 1, data = df_wr)
summary(null_wr_model)
```

    ## 
    ## Call:
    ## lm(formula = yards_per_target ~ 1, data = df_wr)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4537 -1.5806 -0.2903  0.9486  5.8296 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.7037     0.2559    30.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.172 on 71 degrees of freedom

``` r
#Initiating stepwise selection in both directions

step_wr_model <- stepAIC(
  null_wr_model,scope = list(
    lower = null_wr_model,
    upper = full_wr_model),
  direction = "both",
  trace = TRUE)
```

    ## Start:  AIC=112.66
    ## yards_per_target ~ 1

    ## Warning in add1.lm(object, scope = scope, scale = scale): using the 61/72 rows
    ## from a combined fit

    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  302.69 105.39
    ## + age_2025  1    6.7995 295.89 105.76
    ## + forty     1    1.2719 301.42 107.09
    ## + vertical  1    1.0370 301.65 107.15
    ## + cone      1    0.0498 302.64 107.38
    ## + shuttle   1    0.0305 302.66 107.39

``` r
summary(step_wr_model)
```

    ## 
    ## Call:
    ## lm(formula = yards_per_target ~ 1, data = df_wr)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4537 -1.5806 -0.2903  0.9486  5.8296 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.7037     0.2559    30.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.172 on 71 degrees of freedom

``` r
formula(step_wr_model)
```

    ## yards_per_target ~ 1

``` r
#>formula(step_wr_model)
#>yards_per_target ~ 1
#>mfw
```

So this is a problem of scope and noise. One season is very noisy,
combine data is a pretty crude instrument to determine results
downstream, even when accounting for age. We’re going to move to add
extra signal here and align the time-scale of these variables.

## REFINING INITIAL MODELING OF WR DATA

WR Efficiency by Season:

``` r
pbp_all <- load_pbp(2020:2025) %>% filter(play_type == "pass",season_type == "REG")

wr_stats_by_season <- pbp_all %>%
  left_join(load_players() %>% dplyr::select(gsis_id, position),
    by = c("receiver_player_id" = "gsis_id")) %>%
  filter(position == "WR") %>% group_by(receiver_player_id, season) %>%
  summarise(targets = n(), receptions = sum(complete_pass == 1, na.rm = TRUE),
    yards = sum(receiving_yards, na.rm = TRUE),.groups = "drop") %>%
  filter(targets >= 20) %>%  #Setting arbitrary cutoff for sample size
  mutate(ypt = yards / targets, catch_rate = receptions / targets)

#Collapse to player averages for players with at least 2 seasons:

wr_efficiency_avg <- wr_stats_by_season %>%
  group_by(receiver_player_id) %>% summarise(
    seasons = n(),avg_ypt = mean(ypt), avg_catch_rate = mean(catch_rate),
    avg_targets = mean(targets),.groups = "drop") %>%filter(seasons >= 2)

#Merging age into this dataframe
wr_panel <- wr_efficiency_avg %>%
  left_join(final_df %>% filter(position == "WR") %>%
      dplyr::select(
        receiver_player_id,
        forty, cone, shuttle, vertical, display_name,
        age_2025),by = "receiver_player_id")

#Merging names into the dataframe
wr_stats_by_season <- wr_stats_by_season %>%
  left_join(player_names,by = c("receiver_player_id" = "gsis_id"))

#Modeling baseline efficiency

lm_panel <- lm(avg_ypt ~ cone + shuttle + vertical + forty + age_2025, data = wr_panel)
summary(lm_panel)
```

    ## 
    ## Call:
    ## lm(formula = avg_ypt ~ cone + shuttle + vertical + forty + age_2025, 
    ##     data = wr_panel)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.44452 -0.55488  0.00334  0.58082  1.72606 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -2.6371017 10.9076448  -0.242    0.810
    ## cone         1.2574505  1.0326723   1.218    0.230
    ## shuttle     -0.7046035  1.2400461  -0.568    0.573
    ## vertical     0.0706243  0.0657488   1.074    0.289
    ## forty       -0.0004852  1.7193371   0.000    1.000
    ## age_2025     0.0769948  0.0584615   1.317    0.195
    ## 
    ## Residual standard error: 1.008 on 41 degrees of freedom
    ##   (155 observations deleted due to missingness)
    ## Multiple R-squared:  0.09395,    Adjusted R-squared:  -0.01655 
    ## F-statistic: 0.8503 on 5 and 41 DF,  p-value: 0.5224

``` r
vars_needed <- c(
  "avg_ypt",
  "forty",
  "cone",
  "shuttle",
  "vertical",
  "age_2025")

wr_panel_complete <- wr_panel %>%
  dplyr::select(all_of(vars_needed), receiver_player_id) %>%
  tidyr::drop_na()

#Setting up stepwise AIC analysis starting with null model for comparison
null_panel <- lm(avg_ypt ~ 1, data = wr_panel_complete)

full_panel <- lm(avg_ypt ~ forty + cone + shuttle + vertical + age_2025, data = wr_panel_complete)

step_panel <- stepAIC(null_panel,
  scope = list(lower = null_panel, upper = full_panel),
  direction = "both",trace = TRUE)
```

    ## Start:  AIC=0.98
    ## avg_ypt ~ 1
    ## 
    ##            Df Sum of Sq    RSS     AIC
    ## <none>                  45.987 0.97551
    ## + cone      1   1.07897 44.908 1.85962
    ## + vertical  1   1.05964 44.927 1.87985
    ## + age_2025  1   1.01919 44.967 1.92214
    ## + forty     1   0.15673 45.830 2.81505
    ## + shuttle   1   0.05292 45.934 2.92139

``` r
summary(step_panel)
```

    ## 
    ## Call:
    ## lm(formula = avg_ypt ~ 1, data = wr_panel_complete)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0991 -0.6232  0.1052  0.6871  1.9549 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.8933     0.1458   54.12   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9999 on 46 degrees of freedom

``` r
formula(step_panel)
```

    ## avg_ypt ~ 1

Not much here, either. Let’s test this again, this time controlling for
draft capital:

``` r
wr_draft <- load_combine() %>%
  dplyr::filter(pos == "WR") %>%
  dplyr::mutate(draft_ovr_clean = ifelse(is.na(draft_ovr),
      300,      # undrafted proxy
      draft_ovr)) %>% dplyr::select(pfr_id, draft_ovr_clean)

wr_draft_bridge <- wr_draft %>%
  left_join(players_bridge %>% dplyr::select(pfr_id, gsis_id),by = "pfr_id") %>%
  dplyr::select(receiver_player_id = gsis_id,draft_ovr_clean)


wr_panel_complete <- wr_panel_complete %>% left_join(wr_draft_bridge,by = "receiver_player_id")

lm_perf <- lm(
  avg_ypt ~ cone + shuttle + vertical + forty + age_2025 + draft_ovr_clean,
  data = wr_panel_complete)

summary(lm_perf)
```

    ## 
    ## Call:
    ## lm(formula = avg_ypt ~ cone + shuttle + vertical + forty + age_2025 + 
    ##     draft_ovr_clean, data = wr_panel_complete)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46231 -0.56846  0.02196  0.58973  1.71481 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)     -3.4238036 12.0292465  -0.285    0.777
    ## cone             1.2685950  1.0473369   1.211    0.233
    ## shuttle         -0.6998743  1.2553545  -0.558    0.580
    ## vertical         0.0710760  0.0665995   1.067    0.292
    ## forty            0.1736392  2.0363196   0.085    0.932
    ## age_2025         0.0745701  0.0609732   1.223    0.228
    ## draft_ovr_clean -0.0003434  0.0020858  -0.165    0.870
    ## 
    ## Residual standard error: 1.02 on 40 degrees of freedom
    ## Multiple R-squared:  0.09456,    Adjusted R-squared:  -0.04125 
    ## F-statistic: 0.6963 on 6 and 40 DF,  p-value: 0.654

``` r
lm_draft <- lm(draft_ovr_clean ~ forty + cone + shuttle + vertical, data = wr_panel_complete)

summary(lm_draft)
```

    ## 
    ## Call:
    ## lm(formula = draft_ovr_clean ~ forty + cone + shuttle + vertical, 
    ##     data = wr_panel_complete)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -135.228  -48.031   -8.099   38.601  215.012 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2453.905    835.149  -2.938 0.005341 ** 
    ## forty         469.005    130.411   3.596 0.000843 ***
    ## cone           33.264     79.677   0.417 0.678452    
    ## shuttle        31.712     94.990   0.334 0.740156    
    ## vertical        2.728      4.992   0.546 0.587632    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 77.78 on 42 degrees of freedom
    ## Multiple R-squared:  0.2679, Adjusted R-squared:  0.1982 
    ## F-statistic: 3.843 on 4 and 42 DF,  p-value: 0.009486

``` r
library(mediation)
```

    ## Warning: package 'mediation' was built under R version 4.5.3

    ## Loading required package: Matrix

    ## Loading required package: mvtnorm

    ## Loading required package: sandwich

    ## mediation: Causal Mediation Analysis
    ## Version: 4.5.1

``` r
med <- mediate(
  model.m = lm(draft_ovr_clean ~ forty + cone, data = wr_panel_complete),
  model.y = lm(avg_ypt ~ draft_ovr_clean + forty + cone, data = wr_panel_complete),
  treat = "forty",mediator = "draft_ovr_clean",boot = TRUE, sims = 1000)
```

    ## Running nonparametric bootstrap

``` r
summary(med)
```

    ## 
    ## Causal Mediation Analysis 
    ## 
    ## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
    ## 
    ##                 Estimate 95% CI Lower 95% CI Upper p-value
    ## ACME           -0.398604    -2.000656     1.712216   0.692
    ## ADE            -0.038875    -3.378697     2.899618   0.966
    ## Total Effect   -0.437479    -3.346519     2.739122   0.750
    ## Prop. Mediated  0.911139    -4.908023     8.122556   0.762
    ## 
    ## Sample Size Used: 47 
    ## 
    ## 
    ## Simulations: 1000

Sadly, not much here either.

## TAKEAWAYS SO FAR:

1.  In general, we’ve found no meaningful relationship between combine
    metrics and WR efficiency once a player is in the NFL. The layman’s
    explanation is that in order to be drafted, you have to be a stud
    athlete, generally speaking. This holds accounting for multiple
    seasons, accounting for age in each season, and draft capital
    allocated for each player drafted. The null hypothesis here -
    “combine performance has no correlation with efficiency at the NFL
    level” - tracks.

2.  We’ve established an association between 40 time and draft position.
    In general, the faster your 40 time, the higher you’re drafted. See
    above - athletes make it to the NFL.

3.  While we can establish a link between 40 time and draft position,
    neither of these clearly correlates with efficiency, so we can’t
    create a chain from these upstream metrics to downstream efficiency.

Lesser takeaways - RB speed correlates with receiving. (This is
intuitive as slower RBs are typically not provided passing plays.)

## A FURTHER INQUIRY INTO ROLE VS SPEED

We’ve established that speed doesn’t correlate to efficiency, but there
is a correlation to draft position. So, using an axiom we haven’t
established in data yet but can be commonly understood (essentially, the
higher a draft pick, the more likely they will be deployed and used, and
more often - at least very generally), we will need to explore usage.

So, we can ask here - if speed doesn’t make a receiver more efficient,
what does it buy them?

We’ll look at aDOT and QB metrics in one go here.

``` r
#Establishing ADOT metric for each season for each player
wr_adot_by_season <- pbp_all %>%
  filter(play_type == "pass") %>%
  left_join(load_players() %>% dplyr::select(gsis_id, position),
    by = c("receiver_player_id" = "gsis_id")) %>%
  filter(position == "WR") %>%
  group_by(receiver_player_id, season) %>%
  summarise(targets = n(), avg_adot = mean(air_yards, na.rm = TRUE),
    .groups = "drop") %>%filter(targets >= 20)
```

``` r
# Assign primary QB per WR-season
wr_qb_by_season <- pbp_all %>%
  filter(play_type == "pass") %>%
  group_by(receiver_player_id, season, passer_player_id) %>%
  summarise(qb_targets = n(), .groups = "drop") %>%
  group_by(receiver_player_id, season) %>%
  slice_max(qb_targets, n = 1, with_ties = FALSE) %>% ungroup()

# QB performance by season
qb_epa_by_season <- pbp_all %>%
  filter(play_type == "pass") %>%
  group_by(passer_player_id, season) %>%
  summarise(qb_epa_per_play = mean(epa, na.rm = TRUE), .groups = "drop")
```

``` r
# Combine WR aDOT with QB environment at season level
wr_adot_qb_by_season <- wr_adot_by_season %>%
  left_join(
    wr_qb_by_season,
    by = c("receiver_player_id", "season")) %>%
  left_join(
    qb_epa_by_season,
    by = c("passer_player_id", "season"))

wr_adot_qb_by_season <- wr_adot_qb_by_season %>%
  left_join(
    wr_panel_complete %>%
      dplyr::select(
        receiver_player_id,
        forty,
        age_2025),
    by = "receiver_player_id")

wr_adot_qb_by_season <- wr_adot_qb_by_season %>%
  mutate(
    age_season = age_2025 - (2025 - season))


lm_adot_season <- lm(
  avg_adot ~ forty + age_season + qb_epa_per_play,
  data = wr_adot_qb_by_season)

summary(lm_adot_season)
```

    ## 
    ## Call:
    ## lm(formula = avg_adot ~ forty + age_season + qb_epa_per_play, 
    ##     data = wr_adot_qb_by_season)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.1579 -1.9937 -0.0262  1.9802 10.2208 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     32.09724    9.45464   3.395 0.000824 ***
    ## forty           -4.59714    2.17500  -2.114 0.035750 *  
    ## age_season      -0.03312    0.07686  -0.431 0.667024    
    ## qb_epa_per_play -3.14146    1.61555  -1.945 0.053196 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.931 on 206 degrees of freedom
    ##   (611 observations deleted due to missingness)
    ## Multiple R-squared:  0.0463, Adjusted R-squared:  0.03241 
    ## F-statistic: 3.334 on 3 and 206 DF,  p-value: 0.02044

We finally have something here.

Holding for age and QB EPA, we can finally see that slower 40 times
account for lower average depth of target, which is, again, intuitive,
but good to see backed in the numbers. Controlling for QB play helps
here as a WR can’t force the QB to chuck the ball.

We have a lot more to work with here, sample-wise, by getting all these
individual seasons (206 df) which we didn’t have when we were dealing
with season averages. This also helps a bit against ecological
correlation.

QB EPA having the same correlation (Higher EPA, lower aDOT) - this is
slightly less significant statistically.

To be clear, adjusted R-squared in this instance is relatively small but
given the noisy sample, we’ll take it.

``` r
# Collapse to multi-season WR panel
wr_adot_panel <- wr_adot_qb_by_season %>%
  group_by(receiver_player_id) %>%
  summarise(
    seasons    = n(),
    avg_adot   = mean(avg_adot, na.rm = TRUE),
    avg_qb_epa = mean(qb_epa_per_play, na.rm = TRUE),
    .groups    = "drop") %>%
  filter(seasons >= 2) %>%
  left_join(wr_panel_complete %>% dplyr::select(receiver_player_id, forty, age_2025),
    by = "receiver_player_id")
```

``` r
lm_adot_qb <- lm(avg_adot ~ forty + age_2025 + avg_qb_epa, data = wr_adot_panel)

summary(lm_adot_qb)
```

    ## 
    ## Call:
    ## lm(formula = avg_adot ~ forty + age_2025 + avg_qb_epa, data = wr_adot_panel)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0367 -1.7612  0.1773  1.4351  5.0926 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  32.0158    15.8114   2.025   0.0491 *
    ## forty        -3.8053     3.6503  -1.042   0.3030  
    ## age_2025     -0.1464     0.1329  -1.101   0.2770  
    ## avg_qb_epa   -7.9359     4.2161  -1.882   0.0666 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.349 on 43 degrees of freedom
    ##   (155 observations deleted due to missingness)
    ## Multiple R-squared:  0.1372, Adjusted R-squared:  0.07697 
    ## F-statistic: 2.279 on 3 and 43 DF,  p-value: 0.09302

We don’t see this result play out as strongly when samples are combined
over seasons, which makes sense given that even before compression,
these samples are pretty noisy.
