library(dplyr)
library(rlang)
library(purrr)
library(labelled)

# library(desuctools)

d <- data.frame(x      = factor(c(rep(letters[1:3], 6),
                                  letters[1:2]),
                                levels = letters[1:4]),
                edad   = haven::labelled(c(rep(1, 8), rep(2, 12)),
                                         labels = c('young' = 1, 'old' = 2),
                                         label = 'Age'),
                fct    = factor(c(rep(1, 12), rep(0, 8)),
                                labels = c('No', 'Si')),
                lab    = haven::labelled(c(rep(1, 12), rep(0, 8)),
                                         labels = c('Si' = 1, 'No' = 0),
                                         label = 'pregunta 1'),
                lab_na = haven::labelled(c(rep(1, 12), rep(0, 6), rep(9, 1), NA),
                                         labels = c('Sí' = 1, 'No' = 0, 'NS/NR' = 9)),
                chr    = c(rep('Si', 12),
                           rep('No', 7), NA_character_),
                num    = c(rep(1, 12), rep(0, 8)),
                esc    = 1:20,
                esc_na = c(NA_integer_, 2:20),
                wgt    = c(rep(.5, 10), rep(1.5, 10))
)

library(survey)
library(srvyr)

## test ----

s <- srvyr::as_survey_design(d, weights = NULL)

tabla_var_segmento(d,
                   .var = 'lab',
                   .segmento = c('edad'),
                   .wt = NULL)

tabla_vars_segmentos(d,
                     .vars = lab,
                     miss = NULL,
                     .segmentos = c(x, edad),
                     .wt = NULL)

tabla_vars_segmentos(d,
                     .vars = lab,
                     miss = 'No',
                     .segmentos = c(x, edad),
                     .wt = NULL)

svy_tabla_vars_segmentos(s,
                         .vars = lab,
                         miss = 'No',
                         .segmentos = c(x, edad))

## numérico

tabla_var_segmento(d,
                   .var = 'esc',
                   .segmento = c('edad'),
                   total = TRUE,
                   .wt = NULL)

tabla_vars_segmentos(d,
                     .vars = c(esc, lab),
                     .segmentos = c(x, edad),
                     total = TRUE,
                     .wt = NULL)

svy_tabla_vars_segmentos(s,
                         .vars = esc,
                         .segmentos = c(x, edad))



df <- data.frame(var = c('a', 'a', 'b'),
                 num = c(0,  1, 1),
                 wgt = c(.5, .5, 2))

weighted.mean(c(0, 1), c(.5, .5))

df |>
  summarise(mean = weighted.mean(num, wgt),
            .by = var)

aggregate(wgt ~ var,
          df,
          # FUN = \(x) identity(x),
          FUN = \(x) mean(x, na.rm = FALSE),
          na.action = na.pass,
          drop = FALSE)


df_gpt <- data.frame(group=c("A", "A", "B", "B", "B"),
                     value=c(1, 2, 3, 4, 5),
                     weight=c(0.1, 0.2, 0.3, 0.4, 0.5))

aggregate(value * weight ~ group,
          df_gpt,
          # FUN = \(x) identity(x),
          FUN = \(x) sum(x, na.rm = FALSE),
          na.action = na.pass,
          drop = FALSE)



weighted_mean_agg <- function(.df, .group1, .weight) {

  aggregate(x = list('var' = .df[['value']]),
            by = list('segmento_cat' = .df[[.group1]]),
            FUN = \(x) c(mean = weighted.mean(x, .df$weight[match(x, .df$value)]),
                         sum  = sum(             .df$weight[match(x, .df$value)])
            ),
            simplify = TRUE
  )
}

x <- weighted_mean_agg(df_gpt, 'group', .weight = 'weight')

cbind(x,
      x$var)



df <- data.frame(group = rep(letters[1:3], each = 4), value = rnorm(12))

aggregate(x = df$value,
          by = list(df$group),
          FUN = function(x) c(sum = sum(x),
                              casos = length(x)))



# calculate weighted sum and weighted mean of mpg by number of cylinders (cyl)
aggregate(mpg ~ cyl,
          data = mtcars,
          FUN = function(x) c(wt_sum = sum(x * mtcars$wt),
                              wt_mean = sum(x * mtcars$wt) / sum(mtcars$wt)))



x <- bench::mark(
  aggregate = {
    aggregate(value ~ group,
              data = df_gpt,
              FUN = function(x) weighted.mean(x, df_gpt$weight[match(x, df_gpt$value)]))

  },
  summarise = {
    df_gpt |>
      summarise(value = weighted.mean(value, weight),
                .by = group)
  },
  check = TRUE
)

x |> ggplot2::autoplot(type = 'violin')

weighted.mean(x = c(0, 0, 1),
              w = c(.25, .25, .50))

