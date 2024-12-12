# title: "US Treasury MA Modeling"
# date: "12/11/2024"
# author: Leonard Genders

# Observe the percent monthly change in the yield of US Treasury securities
treasury <- read_csv("https://jagelves.github.io/Data/treasury.csv")

# view
glimpse(treasury)

treasury %>%
  mutate(period=1:length(OBS)) -> treasury
as_tsibble(treasury, index = period) %>%
  autoplot() + theme_classic() +
  labs(title = "Percent Monthly Change in the Yield of US Treasury Securities",
       subtitle="1953-2008", x="", y="")
# Interpretation: appears to be white noise, ragged and unpredictable pattern

# observe ACF and PACF
as_tsibble(treasury, index = period) %>%
  ACF(lag_max = 12, changetreasury) %>%
  autoplot() + theme_bw() +
  labs(x="", y="", title="ACF for TCM5Y")
# ACF reflects the same pattern: one spike and all other lags not significantly
# different from zero in the ACF

as_tsibble(treasury, index = period) %>%
  PACF(lag_max = 12, changetreasury) %>%
  autoplot() + theme_bw() +
  labs(x="", y="", title="PACF for TCM5Y")
# PACF does not replicate the one from the simulation but the pattern created is the same
# Decaying PACF, a large lag 1 partial autocorrelation, followed by smaller ones until they are all insignificant
