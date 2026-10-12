#============================================================
# What 70 years of UK data say about booms, busts and the
# Phillips curve
#============================================================

#------------------------------------------------------------
# Setup: packages
#------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)



#------------------------------------------------------------
# Part 1: Seventy years of the UK business cycle
#------------------------------------------------------------

# Load Penn World Table data and convert to per-capita terms
df1<- read.csv("TREND_DATA_UK2.csv")

  df_long <- df1 |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 names_transform = list(year=as.integer),
                 values_to = "value"
                                    ) |>
    select(-c(Variable.name, ISO.code, Country)) |>
    pivot_wider(names_from = Variable.code,
                values_from = value) |>
    rename(Consumption = "rconna", Capital_Stock = "rnna", GDP = "rgdpna", Population = "pop")|>
    mutate(Consumption = Consumption/(Population))|>
    mutate(Capital_Stock = Capital_Stock/(Population))|>
    mutate(GDP = GDP/(Population))

# Fit a log-linear trend to each series (residuals = cyclical component)
models <- list()
residuals_list <- list()


for(i in c("Consumption", "Capital_Stock", "GDP")){
  models[[i]] <- lm(
    formula=paste("log(", i, ")", "~ year"),
    data = df_long,
    na.action = na.exclude)
  residuals_list[[i]] <- residuals(models[[i]])
}

# Charts: each series against its trend
for(i in c("Consumption", "Capital_Stock", "GDP")){
  df_long[[paste0("log_", i)]] <- log(df_long[[i]])
  df_long[[paste0("trend_", i)]] <- fitted(models[[i]])

  p1 <- ggplot(df_long, aes(x=year)) +
    geom_line(aes(y = .data[[paste0("log_", i)]], color = "Log"), size = 1.2) +
    geom_line(aes(y = .data[[paste0("trend_", i)]], color = "Trend"), size=1.2) +
    labs(title = paste("Log Series vs Trend for", i),
         y = "Value") +
    theme_minimal()

  print(p1)
}

# Chart: the three cyclical components side by side
for (i in c("Consumption", "Capital_Stock", "GDP")){
  df_long[[paste0("res_",i)]] <- residuals_list[[i]]
}

ggplot(df_long, aes(x=year, y=c(res_Consumption, res_Capital_Stock, res_GDP))) +
  geom_line(aes(y = res_Consumption, color = "Consumption Residuals"), size = 1.2) +
  geom_line(aes(y = res_Capital_Stock, color = "Capital Residuals"), size = 1.2) +
  geom_line(aes(y = res_GDP, color = "GDP Residuals"), size = 1.2) +
  labs(title = "Residuals of Consumption, Capital, and GDP",
       y = "Residual Value") +
  theme_minimal() +
  scale_color_manual(values = c("Consumption Residuals" = "blue",
                                 "Capital Residuals" = "red",
                                 "GDP Residuals" = "green"))


#------------------------------------------------------------
# Part 2: How flat is the UK Phillips curve?
#------------------------------------------------------------

# Load ONS inflation (MM23) and unemployment (VACS01) data
df2 <- read_xlsx("INFLATION_DATA_UK.xlsx", skip = 194)
df3 <- read_xlsx("UNEMPLOYMENT_DATA_UK.xlsx", skip = 284)

names(df2)[c(1,2)] <- c("year", "inflation")
names(df3)[c(1,2)] <- c("year", "unemployment")

df_m <- df2 |>
  inner_join(df3, by = "year")

df_m <- df_m |>
  mutate(year=seq(as.Date("1989-01-01"), by = "month", length.out=n())) |>
  rename(date = "year")


# NAIRU: 3%, midpoint of Melliss & Webb (1997) long-run estimate
df_m <- df_m |>
  mutate(unemployment_expectation = 3)

# Expected inflation: adaptive, inflation 12 months earlier
df_m <- df_m |>
  mutate(inflation_expectation = lag(inflation, 12))

# Estimate the Phillips curve
model_phillips <- lm(inflation ~inflation_expectation+I(unemployment-unemployment_expectation),
                     df_m, na.action = na.omit)

summary(model_phillips)

library(stargazer)
stargazer(model_phillips, type = "html", out = "phillips_table.doc")


