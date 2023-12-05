### Creating a fake dataset

set.seed(42)

mmm_data = data.frame(week = 1:52,
                      revenue = 100,
                      newspaper_ads = c(rep(0, times = 9),
                                        rep(200, times = 4),
                                        rep(0, times = 5),
                                        rep(450, times = 6), 
                                        rep(0, times = 18), 
                                        rep(400, times = 6), 
                                        rep(0, times = 4)),
                      #search_ads = sample(c(0, 50, 70), size = 1, replace = FALSE, prob = c(0.6, 0.2, 0.2)),
                      #social_ads = sample(c(100, 150, 200, 400), size = 1, replace = FALSE, prob = c(0.6, 0.15, 0.15, 0.1)),
                      #price_change = 0,
                      holiday = 0,
                      ny_fashion_week = 0,
                      shopping_season = 0)

holiday_weeks = c(47:52)
mmm_data$holiday = ifelse(mmm_data$week %in% holiday_weeks, 1, 0)
mmm_data$ny_fashion_week[37] = 1
mmm_data$price_change = rnorm(52, mean = -4, sd = 5)
mmm_data$search_ads = sample(c(0, 50, 70), size = 52, replace = TRUE, prob = c(0.6, 0.2, 0.2))
mmm_data$social_ads = sample(c(100, 150, 200, 400), size = 52, replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.1))
mmm_data$shopping_season[c(1, 2, 3, 4, 20 ,21, 22, 23, 24)] = 1


plot(mmm_data)

### Generating revenue

mmm_data = mmm_data %>% 
  mutate(revenue = rnorm(52, mean = 15000, sd = 1000) +
           rnorm(52, 19.6, 6.75)*social_ads +
           rnorm(52, 25.18, 51.9)*search_ads +
           rnorm(52, 36.5, 5.1)*newspaper_ads +
           rnorm(52, 9000, 6000)*holiday +
           rnorm(52, 2000, 400)*shopping_season +
           rnorm(52, -682, 154)*price_change +
           rnorm(52, 1000, 500))

mmm_data$revenue[31] = mmm_data$revenue[31] + rnorm(1, 500, 200)
mmm_data$revenue[32] = mmm_data$revenue[32] + rnorm(1, 1000, 300)
mmm_data$revenue[33:35] = mmm_data$revenue[33:35] + rnorm(1, 1500, 400)
mmm_data$revenue[36:37] = mmm_data$revenue[36:37] + rnorm(1, 1000, 200)


ggplot(data=mmm_data, aes(x=week, y=revenue)) +
  geom_line()

corrplot(cor(mmm_data))

write.csv(mmm_data, "mmm_fashion_simulated.csv")
