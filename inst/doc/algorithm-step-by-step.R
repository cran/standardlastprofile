## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "95%",
  fig.align = "center"
)

## ----message=FALSE, include=FALSE---------------------------------------------
library(standardlastprofile)
library(ggplot2)

## ----message=FALSE------------------------------------------------------------
head(slp)

## ----small_multiples_vignette, echo = FALSE, fig.asp = 1.6, fig.retina=2------
# labeller
label_names <- c(
  "saturday" = "Saturday",
  "sunday" = "Sunday",
  "workday" = "Workday"
)

label_fun <- function(x) label_names[[x]]

# reorder facets
tmp <- slp
tmp$day <- factor(slp$day, levels = c("workday", "saturday", "sunday"))

# plot
ggplot(tmp,
       aes(x = as.POSIXct(x = paste(Sys.Date(), timestamp)),
           y = watts,
           color = period)) +
  geom_line() +
  facet_grid(profile_id ~ day, 
             scales = "free_y",
             labeller = labeller(day = as_labeller(label_names))) +
  scale_x_datetime(NULL, date_breaks = "6 hours", date_labels = "%k:%M") +
  scale_y_continuous(NULL, n.breaks = 3, limits = c(0, NA)) +
  scale_color_manual(name = NULL,
                     values = c(
                       "winter" = "#961BFA",
                       "summer" = "#FA9529",
                       "transition" = "#0CC792"
                       )) +
  labs(title = "Standard Load Profiles",
       subtitle = "96 x 1/4h measurements [in watts], based on consumption of 1,000 kWh/a",
       caption = "data: www.bdew.de") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(strip.text.y.right = element_text(angle = 0)) + 
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_line(
      linetype = "12",
      lineend = "round",
      colour = "#FAF6F4"
      )
  ) +
  NULL

## ----H0_data, message=FALSE---------------------------------------------------
library(standardlastprofile)
H0_2024 <- slp_generate(
  profile_id = "H0",
  start_date = "2024-01-01",
  end_date = "2024-12-31"
  )

## ----normalization-1, message=FALSE-------------------------------------------
sum(H0_2024$watts)

## ----normalization-2, message=FALSE-------------------------------------------
sum(H0_2024$watts / 4 / 1000)

## ----date_seq-1, message=FALSE, echo=TRUE-------------------------------------
start <- as.Date("2023-12-22")
end <- as.Date("2023-12-27")

(date_seq <- seq.Date(start, end, by = "day"))

## ----characteristic_days, message=FALSE---------------------------------------
wkday_period <- standardlastprofile:::get_wkday_period(date_seq)
data.frame(input = date_seq, output = wkday_period)

## ----G5_data_vignette, echo=TRUE----------------------------------------------
G5 <- slp_generate(
  profile_id = "G5",
  start_date = "2023-12-22",
  end_date = "2023-12-27"
  )

## -----------------------------------------------------------------------------
head(G5)

## ----G5_plot_vignette, echo=TRUE, eval=TRUE, message=FALSE, fig.retina=2, fig.asp=0.5----
library(ggplot2)
ggplot(G5, aes(start_time, watts)) +
  geom_line(color = "#0CC792") +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d") +
  labs(
    title = "'G5': bakery with bakehouse",
    subtitle = "1/4h measurements, based on consumption of 1,000 kWh/a",
    caption = "data: www.bdew.de",
    x = NULL,
    y = "[watts]") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_line(
      linetype = "12",
      lineend = "round",
      colour = "#FAF6F4"
      )
  ) +
  NULL

## ----H0_2024_daily, message=FALSE, echo=FALSE, fig.retina=2, fig.asp=0.5------
# aggregate by day of year as decimal number (1 - 365)
H0_2024_daily <- by(H0_2024, INDICES = format(H0_2024$start_time, "%j"), FUN = function(x) {
  data.frame(
    start_time = x[["start_time"]][1],
    watts = mean(x[["watts"]])
  )
})
H0_2024_daily <- do.call(rbind, args = H0_2024_daily)

## ----H0_2024_plot, message=FALSE, echo=FALSE, fig.retina=2, fig.asp=0.5-------
ggplot(H0_2024_daily, aes(start_time, watts)) +
  geom_line(color = "#0CC792") +
  labs(title = "Dynamic Load Profile 'H0': Households",
       subtitle = "Electrical power per day",
       caption = "data: www.bdew.de",
       x = NULL,
       y = "[watts]") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_line(
      linetype = "12",
      lineend = "round",
      colour = "#FAF6F4"
      )
    ) +
  NULL

## ----H0_dynamic, echo=FALSE, message=FALSE, fig.asp=0.8, fig.retina=2---------
# why these days? refer to page 9 in:
# https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf
lst <- Map(paste,
  list("1997-01", "1996-07", "1997-04"),
  list(17:19, 19:21, 18:20),
  sep = "-")

periods <- c("winter", "summer", "transition")
names(lst) <- periods

days <- c("workday", "saturday", "sunday")
lst <- lapply(lst, setNames, days)

# list to be populated
out <- vector("list", length(periods))
names(out) <- periods

# generates slp/s given params in lst
for(i in periods) {
  out[[i]] <- slp_generate("H0", lst[[i]][[1]], lst[[i]][[3]])
}

# add day column
out <- lapply(out, function(x) {
  tmp <- format.Date(x$start_time, "%A")
  tmp <- replace(tmp, tmp == "Friday", "workday")
  
  cbind(x, data.frame(day = tolower(tmp)))
})

# adds period column
H0 <- lapply(names(out), function(x) {
  cbind(out[[x]], data.frame(period = x))
})

H0 <- do.call(rbind, H0)

# remove date from start_time to make it "%H:%M"
H0$timestamp <- format(H0$start_time, "%H:%M")

# reorder, remove columns we do not need
H0 <- H0[, names(slp)]

# create variable for faceting
H0$type <- "dynamic"

# rbind with subset of H0 from slp
H0_slp <- subset(slp, subset = profile_id == "H0")
H0_slp$type <- "static"

H0_plot <- rbind(H0, H0_slp)

# reorder facets
H0_plot$day <- factor(H0_plot$day, levels = days)

# labeller
label_names <- c(
  "saturday" = "Saturday",
  "sunday" = "Sunday",
  "workday" = "Workday"
)

label_fun <- function(x) label_names[[x]]

# plot
library(ggplot2)
ggplot(H0_plot,
       aes(x = as.POSIXct(paste(Sys.Date(), timestamp)),
           y = watts,
           color = period)) +
  geom_line() +
  facet_grid(day ~ type,
             labeller = labeller(day = as_labeller(label_names))) +
  scale_x_datetime(NULL, date_breaks = "6 hours", date_labels = "%k:%M") +
  scale_y_continuous("[watts]") +
  scale_color_manual(name = NULL,
                     values = c(
                       "winter" = "#961BFA",
                       "summer" = "#FA9529",
                       "transition" = "#0CC792"
                     )) +
  labs(title = "Dynamic vs. Static Values of Standard Load Profile 'H0'",
       subtitle = "96 x 1/4h measurements, based on consumption of 1,000 kWh/a",
       caption = "data: www.bdew.de") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(strip.text.y.right = element_text(angle = 0)) + 
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
      panel.grid = element_line(
      linetype = "12",
      lineend = "round",
      colour = "#FAF6F4"
      )
  ) +
  NULL

