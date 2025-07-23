# install.packages("tidyverse")
# install.packages("sf")
# install.packages("here")
library(scales)
library(readr)
library('tidyverse')
library('sf')
library(ggplot2)
library(stringr)
library(here)

# read OP data
df_input <- read_csv(here::here("data","vtm_matched.csv")) 
# %>%
#   mutate(across('bnf_name',str_replace,"/","-"))

# combine with region name
df_regions <- read_csv(here::here("data","NHS_England_Names_and_Codes_in_England.csv")) %>%
  rename(regional_team = NHSER24CDH ,region = NHSER24NM) %>%
  select(regional_team,region)

df_input <- df_input %>%
  left_join(df_regions)

# group by drug
df_input_bydrug <- df_input %>%
  group_by(drug,month) %>%
  summarise(quantity = sum(quantity))

# plot group by drug
bydrug_plot <- df_input_bydrug %>%
  ggplot(aes(x = month, y = quantity, color = drug)) +
  geom_line()

ggsave(
  filename = here::here(
    "output",
    "bydrug_plot.png"),
  bydrug_plot,
  dpi = 600,
  width = 25,
  height = 15,
  units = "cm"
)


# group by region
df_input_byregion <- df_input %>%
  group_by(region,month) %>%
  summarise(quantity = sum(quantity))

# plot group by region
byregion_plot <- df_input_byregion %>%
  ggplot(aes(x = month, y = quantity, color = region)) +
  geom_line()

ggsave(
  filename = here::here(
    "output",
    "byregion_plot.png"),
  byregion_plot,
  dpi = 600,
  width = 25,
  height = 15,
  units = "cm"
)


# group by region & drug
df_input_bydrug_region <- df_input %>%
  group_by(drug,region,month) %>%
  summarise(quantity = sum(quantity)) %>%
  drop_na(region)

# plot group by drug
bydrug_region_plot <- df_input_bydrug_region %>%
  ggplot(aes(x = month, y = quantity, color = drug)) +
  geom_line() +
  facet_wrap(~ region, ncol = 3)

ggsave(
  filename = here::here(
    "output",
    "bydrug_region_plot.png"),
  bydrug_region_plot,
  dpi = 600,
  width = 45,
  height = 30,
  units = "cm"
)




### by bnf name per drug
df_input_by_bnf <- df_input %>%
  group_by(bnf_name,month,drug) %>%
  summarise(quantity = sum(quantity))

dulaglutide <- df_input_by_bnf %>%
  filter(drug == "Dulaglutide")

dulaglutide_plot <- dulaglutide %>%
  ggplot(aes(x = month, y = quantity, color = bnf_name)) +
  geom_line()


drug_name<-df_input %>%
  group_by(drug) %>%
  summarise() %>%
  pull(drug)

for(i in 1:length(drug_name)){
  assign(drug_name[i],
         df_input_by_bnf %>%
          filter(drug == drug_name[i]))
  
  assign(paste0(drug_name[i],"_plot"),
         get(drug_name[i]) %>%
           ggplot(aes(x = month, y = quantity, color = bnf_name)) +
           geom_line()
         )
  
  ggsave(
    filename = here::here(
      "output",paste0(str_replace(drug_name[i],"/","-"),"_plot.png")
    ),
    get(paste0(drug_name[i],"_plot")),
    dpi = 600,
    width = 25,
    height = 15,
    units = "cm"
  )
  
}

# resize Semaglutide plot
ggsave(
  filename = here::here(
    "output","Semaglutide_plot.png"),
  Semaglutide_plot,
  dpi = 600,
  width = 35,
  height = 15,
  units = "cm"
)
