library(scales)
library(readr)
library('tidyverse')
library('sf')
library(ggplot2)
library(stringr)
library(here)
library("glue")

# read OP data
df_input <- read_csv(here::here("data","tirzepatide_lists.csv"))


# combine with region name
df_regions <- read_csv(here::here("data","NHS_England_Names_and_Codes_in_England.csv")) %>%
  rename(regional_team = NHSER24CDH ,region = NHSER24NM) %>%
  select(regional_team,region)

df_input <- df_input %>%
  left_join(df_regions)

# plot group by drug
bydrug_region_plot <- df_input %>%
  ggplot(aes(x = month, y = rate, color = chemical)) +
  geom_line() +
  facet_wrap(~ region, ncol = 3)  +
  scale_y_continuous(labels = label_comma()) +
  scale_x_date(date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle =90)) +
  ylab("Rate")

ggsave(
  filename = here::here(
    "output",
    "isat",
    glue("tirzepatide_region_plot.png")),
  bydrug_region_plot,
  dpi = 600,
  width = 45,
  height = 30,
  units = "cm"
)

