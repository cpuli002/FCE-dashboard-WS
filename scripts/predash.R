#---------- Load Libraries-----------------
library(tidyverse)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(fasstr)
library(tidyr)
#-----------General Arguments----------------------------------
# Create function to generate water year values,
# with a water year defined as starting in May
wtr_yr <- function(dates, start_month=5) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}



#Calculate calander year based on dates
cal_yr <-function(dates, start_month=1) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 
  # Return the water year
  adj.year
} 

#-----------Sites--------------------------
sites <- read.csv("sites.csv")

write_rds(sites, "sites.rds")
#-----------Hydro data-----------------------

hydro <- read_csv('sites_wse.csv',
                      na = "no data",
                      col_types = cols(
                        Date = col_date(format = '%m/%d/%Y')
                      ))
hydro <- hydro %>%
  pivot_longer(
    cols = - Date,
    names_to = "Site",
    values_to = "WSE"
  )


#Add calendar and water years
hydro <- hydro %>%
  mutate(WaterYear = wtr_yr(Date),
         CalendarYear = cal_yr(Date)) %>%
  drop_na()

# process for plot and linear regression
hydro <- hydro %>%
  mutate(YearMonth = floor_date(Date, "month"))


hydro_monthly <- hydro %>%
  group_by(YearMonth) %>%
  summarise(wse_mean = mean(WSE, na.rm = TRUE),
            wse_sd = sd(WSE, na.rm = TRUE))

hydro_monthly <- hydro_monthly %>%
  mutate(TimeIndex = as.numeric(YearMonth - min(YearMonth)))


# extract regression coefficients 
quad_model <- lm(wse_mean ~ poly(TimeIndex, 2, raw = TRUE), data = hydro_monthly)

r2_value <- summary(quad_model)$r.squared

coefs <- coef(quad_model)
eq <- sprintf("y = %.4f x² + %.4f x + %.4f", coefs[3], coefs[2], coefs[1])

y_min <- min(hydro_monthly$wse_mean, na.rm = TRUE) - 5  
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2024-04-30")

write_rds(hydro_monthly, "hydro_month.rds")

# ggplot of monthly mean water surface elevation with linear regression
ggplot(hydro_monthly, aes(x = YearMonth, y = wse_mean)) +
  geom_point(size = 2.5) +   
  geom_line() + 
  geom_errorbar(aes(ymin = wse_mean - wse_sd, ymax = wse_mean + wse_sd), width = 10) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linetype = "dashed") +
  annotate("text", x = as.Date("2015-06-01"), y = y_min,
           label = sprintf("r² = %.2f\n%s", r2_value, eq),
           size = 4, hjust = 0) +
  labs(x = "Year", y = "Water Surface Elevation (cm)", title = "Mean Monthly WSE") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


