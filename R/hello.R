# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Modified Richard Equation
#'
#' @param t     Float : Time
#' @param p_max Float : Population Maximum (Upper Asymptote)
#' @param p_min Float : Population Minimum (Lower Asymptote)
#' @param r_min Float : Maximum Growth/Death rate (positive for growth|negative fo death)
#' @param s     Float : Shift (Time at which r_max occurs)
#' @return The population at time \code{t} Given by \deqn{P(t) = p_{min} + \frac{p_{max}-p_{min}}{1 + e^{4r_{max}.(t-s)/p_{min}- p_{max}}}}
#' @return
#' @examples
#' add(1, 1)
#' add(10, 1)
richard <- function(t, p_max, p_min, r_max, s){
  p_t <- p_min + (p_max - p_min) / (1 + exp(4 * r_max * (t - s)/(p_min - p_max)))
  return(p_t)
  }

linear <- function(x,a,b){a * x + b}

fit_richard <- function(y, t, method = "LAD") {

  lin <- lm(y ~ t)
  r_estimate <- lin$coefficients[2]
  sign <- sign(r_estimate)
  max_y <- max(y)
  min_y <- min(y)

  s_boundary <- max(as.numeric(t))

  estimates <- list()
  estimates$p_max <- rnorm(100, mean = max_y, sd = 0.01*(max_y-min_y))
  estimates$p_min <- rnorm(100, mean = min_y, sd = 0.01*(max_y-min_y))

  estimates$r_max <- sign*10^rnorm(100, mean = log(abs(r_estimate), base = 10), sd = 1)
  estimates$s <- rnorm(100, mean = max(t)/2, sd = 0.05*max(t))


  suggestions <- cbind(estimates$p_max, estimates$p_min, estimates$r_max, estimates$s)



  fitness_fun <- switch(method,
    "LAD" = \(p) -sum(abs(y - richard(t, p[1], p[2], p[3], p[4]))),
    "OLS" = \(p) -sum((y - richard(t, p[1], p[2], p[3], p[4]))^2),
    "MLE" = \(p) sum(log(dnorm(y, max_y, 0.01*(max_y-min_y))+dnorm(y, min_y, 0.01*(max_y-min_y))))
  )

#do.call(richard, append(list(t),p))
fit <- GA::de(type = "real-valued",
              fitness = fitness_fun,
              suggestions = suggestions,
              lower   = c(
                p_max = max_y - 0.05*(max_y-min_y),
                p_min = 0,
                r_max = min(0 , r_estimate * 100),
                s     = 0),
              upper = c(
                p_max = max_y + 0.05*(max_y-min_y),
                p_min = min_y + 0.05*(max_y-min_y),
                r_max = max(0 , r_estimate * 100),
                s     = s_boundary),
              names = c(
                "p_max",
                "p_min",
                "r_max",
                "s"),
              popSize = 100,
              monitor = FALSE,
              optim = FALSE,
              maxiter = 100)


  return(fit@solution[1,])

  # })
}

#'
#'
#' @param .data     Float : Dataframe
#' @param .time_col String : Name of the time column from the plate reader experiment
#' @return The dataframe with the formated time column as elapsed hours
#' @examples
clean_time <- function(.data, .time_col) {
  .data |>
    mutate(
      {{.time_col}} := lubridate::as_datetime({{.time_col}}),
      {{.time_col}} := {{.time_col}} - first({{.time_col}}),
      {{.time_col}} := as.numeric({{.time_col}}/3600)
    )
}

#' @param .data Float : Dataframe
#' @param wells Tidyselect : Tidyselect matching all the wells columns from your plate reader experiment
#' @return The dataframe in long/tidy format
#' @examples
format_wellplate_long <- function(.data, wells = matches(regex("^[A-Za-z]{1}\\d{1,2}"))) {
  .data |>
    pivot_longer(cols = {{wells}},
                 names_to = "well") |>
    drop_na(value)
}




fit_data <- function(.data, .groups ,.value, .time, method = "LAD") {
  .data |>
    select({{.groups}}, {{.value}}, {{.time}}) |>
    group_by(across(c({{.groups}}))) |>
    nest({{.time}}  := {{.time}},
         {{.value}} := {{.value}}) |>
    rowwise() |>
    mutate(fit = purrr::map2({{.value}}, {{.time}}, \(x,y) fit_richard(x, y, method = method))) |>
    unnest_wider(fit) |>
    select(-c({{.time}},
              {{.value}}))

}


m_score <- function(data) {.6745*(data-median(data))/mad(data)}
