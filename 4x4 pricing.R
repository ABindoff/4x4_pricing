library(openxlsx)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# read data collected from gumtree on cars that take my interest
cars <- read.xlsx("4x4 pricing.xlsx", sheet = 1)
options(digits = 3)
cars$age_of_car <- 2018 - cars$year
cars$price <- cars$price/1000
cars$mileage <- cars$mileage/1000
cars$id <- row.names(cars)
m1 <- lm(price ~ age_of_car +
             mileage*model +
             at_tyres +
             diesel:model +
             cosmetic_condition,
           data = cars)
summary(m1)


# check predictive power of model
cars$fit <- predict(m1, cars)

ggplot(cars, aes(x = fit, y = price, colour = model)) +
  geom_point() +
  geom_abline()


# what is the expected mileage per year for these vehicles

cars %>% group_by(model) %>% summarise(mpy = mean(mileage/age_of_car))

# now assume we keep the vehicle for {1, 3, 5} years, add 8000km/year mileage,
# assume body condition will be "poor" at the end, and that Commodore is sold for $2000

commodore <- 2

# 3 years
cars.pred <- cars
cars.pred$age_of_car <- cars.pred$age_of_car + 3
cars.pred$mileage <- cars.pred$mileage + 24
cars.pred$cosmetic_condition <- "poor"

fit <- predict(m1, cars.pred, se = T)
cars$fit3 <- fit$fit
cars$se3 <- fit$se.fit
cars$interest3 <- (cars$price-commodore)*(1+0.05/365)^(365*3)-(cars$price-commodore)
cars$cost_at_resale3 <- cars$price-cars$fit3+cars$interest3


# 1 years
cars.pred <- cars
cars.pred$age_of_car <- cars.pred$age_of_car + 1
cars.pred$mileage <- cars.pred$mileage + 8
cars.pred$cosmetic_condition <- "poor"

fit <- predict(m1, cars.pred, se = T)
cars$fit1 <- fit$fit
cars$se1 <- fit$se.fit
cars$interest1 <- (cars$price-commodore)*(1+0.05/365)^(365*1)-(cars$price-commodore)
cars$cost_at_resale1 <- cars$price-cars$fit1+cars$interest1

# 5 years
cars.pred <- cars
cars.pred$age_of_car <- cars.pred$age_of_car + 5
cars.pred$mileage <- cars.pred$mileage + 40
cars.pred$cosmetic_condition <- "poor"

fit <- predict(m1, cars.pred, se = T)
cars$fit5 <- fit$fit
cars$se5 <- fit$se.fit
cars$interest5 <- (cars$price-commodore)*(1+0.05/365)^(365*5)-(cars$price-commodore)
cars$cost_at_resale5 <- cars$price-cars$fit5+cars$interest5




head(arrange(cars, cost_at_resale1) %>% select(model, price, fit, year, mileage, cost_at_resale1), n = 8)
head(arrange(cars, cost_at_resale3) %>% select(model, price, fit, year, mileage, cost_at_resale3), n = 8)
head(arrange(cars, cost_at_resale5) %>% select(model, price, fit, year, mileage, cost_at_resale5), n = 8)


ts <- select(cars, model, id, price, diesel, cost_at_resale1, se1, cost_at_resale3, se3, cost_at_resale5, se5) %>%
  gather(key = se_time, value = se, se1, se3, se5) %>%
  gather(key = cost_time, value = cost, cost_at_resale1, cost_at_resale3, cost_at_resale5)
  
ggplot(ts, aes(x = cost_time, y = cost, group = id, colour = price)) +
  geom_line() +
  geom_errorbar(aes(max = cost + se, min = cost - se), width = 0.1) + 
  geom_hline(yintercept = 0) +
  scale_colour_viridis(option = "plasma") +
  facet_wrap(~model) +
  labs(y = "total cost $000's", x = "")
