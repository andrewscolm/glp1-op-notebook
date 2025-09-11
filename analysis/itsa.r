library(marginaleffects)
library(patchwork)
library(lme4)
library(tinyplot)
tinytheme("ipsum")
set.seed(48103)

# number of days
n <- 365

# intervention at day
intervention <- 200

# time index from 1 to 365
time <- c(1:n)

# treatment variable: 0 before the intervention and 1 after
treatment <- c(rep(0, intervention), rep(1, n - intervention))

# outcome equation
outcome <- 
  10 +  # pre-intervention intercept
  15 * time +  # pre-intervention slope (trend)
  20 * treatment +  # post-intervention intercept (shift of 10)
  5 * treatment * time + # steeper slope after the intervention
  rnorm(n, mean = 0, sd = 100) # noise

dat <- data.frame(outcome, time, treatment)

tinyplot(outcome ~ time | treatment, type = "p", palette = "okabeito",
         data = transform(dat, treatment = factor(treatment)))


mod <- lm(outcome ~ time * treatment, data = dat)
summary(mod)

p0 <- predictions(mod, newdata = datagrid(time = 199, treatment = 0))
p1 <- predictions(mod, newdata = datagrid(time = 200, treatment = 1))

p0
p1

comparisons(mod, variables = "treatment", newdata = datagrid(time = 200))

avg_slopes(mod, variables = "time", by = "treatment")


df_tirzepatide <- df_input %>%
  filter(drug=="Tirzepatide") %>%
  mutate(brand_name = case_when(
    bnf_name == 'Tirzepatide 10mg/0.6ml inj 2.4ml pf dispos dev' ~ 'Mounjaro 10mg/0.6ml',
    bnf_name == 'Tirzepatide 12.5mg/0.6ml inj 2.4ml pf dispos dev' ~ 'Mounjaro 12.5mg/0.6ml',
    bnf_name == 'Tirzepatide 15mg/0.6ml inj 2.4ml pf dispos dev' ~ 'Mounjaro 15mg/0.6ml',
    bnf_name == 'Tirzepatide 2.5mg/0.5ml inj pre-filled disposable devices' ~ 'Mounjaro 2.5mg/0.5ml',
    bnf_name == 'Tirzepatide 2.5mg/0.6ml inj 2.4ml pf dispos dev' ~ 'Mounjaro 2.5mg/0.6ml',
    bnf_name == 'Tirzepatide 5mg/0.5ml inj pre-filled disposable devices' ~ 'Mounjaro 5mg/0.5ml',
    bnf_name == 'Tirzepatide 5mg/0.6ml inj 2.4ml pf dispos dev' ~ 'Mounjaro 5mg/0.6ml',
    bnf_name == 'Tirzepatide 7.5mg/0.6ml inj 2.4ml pf dispos dev' ~ 'Mounjaro 7.5mg/0.6ml',
    bnf_name == 'Mounjaro KwikPen 10mg/0.6ml inj 2.4ml pre-filled pens' ~ 'Mounjaro 10mg/0.6ml',
    bnf_name == 'Mounjaro KwikPen 12.5mg/0.6ml inj 2.4ml pre-filled pens' ~ 'Mounjaro 12.5mg/0.6ml',
    bnf_name == 'Mounjaro KwikPen 15mg/0.6ml inj 2.4ml pre-filled pens' ~ 'Mounjaro 15mg/0.6ml',
    bnf_name == 'Mounjaro KwikPen 2.5mg/0.6ml inj 2.4ml pre-filled pens' ~ 'Mounjaro 2.5mg/0.6ml',
    bnf_name == 'Mounjaro KwikPen 5mg/0.6ml inj 2.4ml pre-filled pens' ~ 'Mounjaro 5mg/0.6ml',
    bnf_name == 'Mounjaro KwikPen 7.5mg/0.6ml inj 2.4ml pre-filled pens' ~ 'Mounjaro 7.5mg/0.6ml',
    bnf_name == 'Mounjaro 5mg/0.5ml solution for injection pre-filled pens' ~ 'Mounjaro 5mg/0.5ml',
    bnf_name == 'Mounjaro 2.5mg/0.5ml solution for injection pre-filled pens' ~ 'Mounjaro 2.5mg/0.5ml',
    .default = bnf_name),
    brand = "Mounjaro") %>%
  group_by(month) %>%
  summarise(items = sum(items)) %>%
  mutate(guidance = case_when(month < as.Date("2024-12-01")~0,
                              month >= as.Date("2024-12-01") ~1),
         time = row_number())

tinyplot(items ~ month | guidance, type = "p", palette = "okabeito",
         data = transform(df_tirzepatide, guidance = factor(guidance)))

mod <- lm(items ~ time * guidance, data = df_tirzepatide)
summary(mod)

p0 <- predictions(mod, newdata = datagrid(time = 17, guidance = 0))
p1 <- predictions(mod, newdata = datagrid(time = 18, guidance = 1))

comparisons(mod, variables = "guidance", newdata = datagrid(time = 18))

avg_slopes(mod, variables = "time", by = "guidance")


newdata <- data.frame(time = c(1:22,1:22), guidance = c(rep(0,22),rep(1,22)))
predictions(model, newdata = newdata)

intervention <- 18
library(ggplot2)
p <- predictions(mod, variables = c("time", "guidance"),newdata = newdata)
p <- subset(p, time > intervention | guidance == 0)
ggplot(p, aes(x = time, y = estimate, color = factor(guidance))) +
  geom_line() +
  labs(title = "Predicted outcome over time",
       x = "Time",
       y = "Outcome")
            