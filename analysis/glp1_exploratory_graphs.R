library(scales)
library(readr)
library('tidyverse')
library('sf')
library(ggplot2)
library(stringr)
library(here)
library("glue")

# read OP data
df_input <- read_csv(here::here("data","vtm_matched.csv"))


# combine with region name
df_regions <- read_csv(here::here("data","NHS_England_Names_and_Codes_in_England.csv")) %>%
  rename(regional_team = NHSER24CDH ,region = NHSER24NM) %>%
  select(regional_team,region)

df_input <- df_input %>%
  left_join(df_regions)

group_1 <- c("quantity","items")
group_2 <- c("drug","region")


by_2group_funct<-function(grouping_1,grouping_2){

  df_input_bydrug <- df_input %>%
    group_by(across(c(grouping_2,month))) %>%
    summarise(group = sum(get(grouping_1)))
  
  # plot group by drug
  bydrug_plot <- df_input_bydrug %>%
    ggplot(aes(x = month, y = group, color = get(grouping_2))) +
    geom_line()  +
    scale_y_continuous(labels = label_comma()) +
    guides(colour=guide_legend(title=grouping_2)) +
    ylab(grouping_1)
  
  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_by_{grouping_2}_plot.png")),
    bydrug_plot,
    dpi = 600,
    width = 25,
    height = 15,
    units = "cm"
  )

}


for(gr1 in group_1){
  for(gr2 in group_2){
    by_2group_funct(gr1,gr2)
    }
  }


region_facet_funct<-function(grouping_1){
  # group by region & drug
  df_input_bydrug_region <- df_input %>%
    group_by(drug,region,month) %>%
    summarise(group = sum(get(grouping_1))) %>%
    drop_na(region)
  
  # plot group by drug
  bydrug_region_plot <- df_input_bydrug_region %>%
    ggplot(aes(x = month, y = group, color = drug)) +
    geom_line() +
    facet_wrap(~ region, ncol = 3)  +
    scale_y_continuous(labels = label_comma()) +
    ylab(grouping_1)
  
  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_by_drug_region_plot.png")),
    bydrug_region_plot,
    dpi = 600,
    width = 45,
    height = 30,
    units = "cm"
  )
}

for(gr1 in group_1){
      region_facet_funct(gr1)
}


drug_name<-df_input %>%
  group_by(drug) %>%
  summarise() %>%
  pull(drug)


by_bnfname_per_drug_funct<-function(grouping_1){
  ### by bnf name per drug
  df_input_by_bnf <- df_input %>%
    group_by(bnf_name,month,drug) %>%
    summarise(group = sum(get(grouping_1)))
  
  for(i in 1:length(drug_name)){
    assign(drug_name[i],
           df_input_by_bnf %>%
            filter(drug == drug_name[i]))
    
    assign(paste0(drug_name[i],"_plot"),
           get(drug_name[i]) %>%
             ggplot(aes(x = month, y = group, color = bnf_name)) +
             geom_line() +
             scale_x_date(labels = date_format("%b %Y"), 
                          date_breaks = "3 months") + 
             theme(axis.text.x=element_text(angle=60, hjust=1)) +
      scale_y_continuous(labels = label_comma())  +
      ylab(grouping_1))
    
    if(glue("{drug_name[i]}_plot")=="Semaglutide_plot"){
      assign(glue("{grouping_1}_{drug_name[i]}_plot"),Semaglutide_plot,envir = .GlobalEnv)
      assign(glue("{grouping_1}_{drug_name[i]}"),Semaglutide,envir = .GlobalEnv)
    }
    
    ggsave(
      filename = here::here(
        "output",
        grouping_1,
        paste0(grouping_1,"_",str_replace(drug_name[i],"/","-"),"_plot.png")
      ),
      get(glue("{drug_name[i]}_plot")),
      dpi = 600,
      width = 25,
      height = 15,
      units = "cm"
    )
    
  }
  
  # resize Semaglutide plot
  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_Semaglutide_plot.png")),
    Semaglutide_plot,
    dpi = 600,
    width = 35,
    height = 15,
    units = "cm"
  )

  }

### Apply by_bnfname_per_drug_funct
for(gr1 in group_1){
  by_bnfname_per_drug_funct(gr1)
}


### Semaglutide key dates
semaglutide_dates<- tibble(dates=as.Date(c("2022-06-01","2023-03-01","2023-09-01")),
                           label = c("Final\nappraisal\ndocument","Published","Updated"),
                           alpha = c(0.2,0.3,0.4),
                           colour = "#00468BFF")

### function
Semaglutide_nice_funct<-function(grouping_1){
    max_val<-get(glue("{grouping_1}_Semaglutide")) %>%
      ungroup() %>%
      summarise(max=max(group))
      
    Semaglutide_nice_plot <- get(glue("{grouping_1}_Semaglutide_plot")) +
      geom_vline(xintercept = semaglutide_dates$dates, 
                 alpha = semaglutide_dates$alpha, 
                 colour = semaglutide_dates$colour) +
      annotate("text", x= semaglutide_dates$dates, 
              y = c(max_val$max*0.92, max_val$max*0.95, max_val$max*0.98), 
              label = semaglutide_dates$label)  +
      scale_y_continuous(labels = label_comma())  +
      ylab(grouping_1)
    
    
    ggsave(
      filename = here::here(
        "output",
        grouping_1,
        glue("{grouping_1}_Semaglutide_nice_plot.png")),
      Semaglutide_nice_plot,
      dpi = 600,
      width = 35,
      height = 15,
      units = "cm"
    )
}

### apply Semaglutide_nice_funct
for(gr1 in group_1){
  Semaglutide_nice_funct(gr1)
}


### semaglutide bnf_name function

bnfname_funct<-function(grouping_1){
  ## bnf name semaglutide
  
  Semaglutide_bnf <- get(glue("{grouping_1}_Semaglutide")) %>%
    filter(str_detect(bnf_name, 'Semaglutide')) %>%
    filter(!str_detect(bnf_name, "tablet"))
  
  max_val <- Semaglutide_bnf %>%
    ungroup() %>%
    summarise(max=max(group))
    
  Semaglutide_bnf <- Semaglutide_bnf %>%
    ggplot(aes(x = month, y = group, color = bnf_name)) +
    geom_line() +
    scale_x_date(labels = date_format("%b %Y"), 
                 date_breaks = "3 months") + 
    theme(axis.text.x=element_text(angle=60, hjust=1))  +
    geom_vline(xintercept = semaglutide_dates$dates, 
               alpha = semaglutide_dates$alpha, 
               colour = semaglutide_dates$colour) +
    annotate("text", x= semaglutide_dates$dates, 
             y = c(max_val$max*0.92,max_val$max*0.94,max_val$max*0.96), 
             label = semaglutide_dates$label)  +
    scale_y_continuous(labels = label_comma()) +
    ylab(grouping_1)
  
  
  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_Semaglutide_bnf_plot.png")),
    Semaglutide_bnf,
    dpi = 600,
    width = 25,
    height = 15,
    units = "cm"
  )
  
  ## bnf name wegovy
  Wegovy_bnf <-get(glue("{grouping_1}_Semaglutide")) %>%
    filter(str_detect(bnf_name, 'Wegovy')) 
  
  max_val <- Wegovy_bnf %>%
    ungroup() %>%
    summarise(max=max(group))
  
  
  Wegovy_bnf<- Wegovy_bnf  %>%
    ggplot(aes(x = month, y = group, color = bnf_name)) +
    geom_line() +
    scale_x_date(labels = date_format("%b %Y"), 
                 date_breaks = "3 months") + 
    theme(axis.text.x=element_text(angle=60, hjust=1))  +
    geom_vline(xintercept = semaglutide_dates$dates, 
               alpha = semaglutide_dates$alpha, 
               colour = semaglutide_dates$colour) +
    annotate("text", x= semaglutide_dates$dates, 
             y = c(max_val$max*0.92,max_val$max*0.94,max_val$max*0.96),
             label = semaglutide_dates$label)  +
    scale_y_continuous(labels = label_comma()) +
    ylab(grouping_1)
  
  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_Wegovy_bnf_plot.png")),
    Wegovy_bnf,
    dpi = 600,
    width = 25,
    height = 15,
    units = "cm"
  )

}

### apply bnfname_funct
for(gr1 in group_1){
  bnfname_funct(gr1)
}


## bnf region
bnf_region_funct<-function(grouping_1,bnf){

  ## individual wegovy by region
  df_region <- df_input %>%
    filter(str_detect(bnf_name, bnf)) %>%
    filter(!str_detect(bnf_name, "tablet")) %>%
    group_by(region,month,bnf_name) %>%
    summarise(group = sum(get(grouping_1))) 
  
  
  bnf_region_plot <- df_region %>%
    ggplot(aes(x = month, y = group, color = bnf_name)) +
    geom_line() +
    facet_wrap(~ region, ncol = 3)  +
    scale_y_continuous(labels = label_comma()) +
    geom_vline(xintercept = semaglutide_dates$dates) +
    scale_x_date(labels = date_format("%b %Y"), 
                 date_breaks = "6 months") +
    ylab(grouping_1)
  
  save_name <- str_replace(bnf,"[|]","_")
  
  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_{save_name}_region_plot.png")),
    bnf_region_plot,
    dpi = 600,
    width = 45,
    height = 30,
    units = "cm"
  )

  ## combined wegovy by region
  all_bnf_region <- df_input %>%
    filter(str_detect(bnf_name, bnf)) %>%
    filter(!str_detect(bnf_name, "tablet")) %>%
    group_by(region,month) %>%
    summarise(group = sum(get(grouping_1))) 
  
  
  all_bnf_region_plot <- all_bnf_region %>%
    ggplot(aes(x = month, y = group)) +
    geom_line() +
    facet_wrap(~ region, ncol = 3)  +
    scale_y_continuous(labels = label_comma()) +
    geom_vline(xintercept = semaglutide_dates$dates) +
    scale_x_date(labels = date_format("%b %Y"), 
                 date_breaks = "9 months") +
    ylab(grouping_1)
    
  


  ggsave(
    filename = here::here(
      "output",
      grouping_1,
      glue("{grouping_1}_all_{save_name}_region_plot.png")),
      all_bnf_region_plot,
    dpi = 600,
    width = 45,
    height = 30,
    units = "cm"
  )
}

for(gr1 in group_1){
  for(bnf_nam in c("Wegovy","Semaglutide","Wegovy|Semaglutide","Lixisenatide|Ozempic"))
    bnf_region_funct(gr1,bnf_nam)
}


