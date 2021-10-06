## PROJECT:  moz_achievement_table
## AUTHOR:   cadelson | USAID
## LICENSE:  MIT
## PURPOSE:  Create reproducable achievement tables for Mozambique field team

#Load packages
library(tidyverse)
library(glamr)
library(gt)
library(googledrive)
library(googlesheets4)
library(ICPIutilities)
library(glitr)
library(RColorBrewer)
library(scales)
library(here)

webshot::install_phantomjs()

# load and munge data
df_filepath <- "~/MERDATA/msd_fy21_q3_clean_psnu.txt"
df_raw<- read_msd(df_filepath)

df<-df_raw %>% 
  filter(operatingunit == "Mozambique",
         standardizeddisaggregate == "Total Numerator",
         indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW", "VMMC_CIRC", "PrEP_NEW"),
         fiscal_year %in% c(2020, 2021))%>%
  reshape_msd("long") %>%
  group_by(indicator, period, fundingagency, period_type) %>%
  summarise(value = sum(value))

#munge yield
df_positivity<-df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  pivot_wider(names_from=indicator, names_sep="_", values_from=value) %>% 
  mutate(Positivity=HTS_TST_POS/HTS_TST) %>% 
  pivot_longer(c(HTS_TST, HTS_TST_POS, Positivity), names_to="indicator") %>% 
  filter(indicator=="Positivity") 


#Combine main df with positivity df
df2<-bind_rows(df, df_positivity)

#continue to filter data and spread data wide

indicator_order<-c("HTS_TST","HTS_TST_POS","Positivity","TX_NEW","TX_CURR","TX_NET_NEW","VMMC_CIRC","PrEP_NEW")
fundingagency_order<-c("USAID","HHS/CDC")

df3 <- df2 %>% ##COMBINE PERIOD TYPE AND PERIOD COLUMNS
  filter(fundingagency == "USAID") %>% 
  tidyr::pivot_wider(names_from = c(period, period_type),
                     values_from = value) %>% 
  rename(`FY20 Q1` = `FY20Q1_results`,
         `FY20 Q2` = `FY20Q2_results`,
         `FY20 Q3` = `FY20Q3_results`,
         `FY20 Q4` = `FY20Q4_results`,
         `FY21 Q1` = `FY21Q1_results`,
         `FY21 Q2` = `FY21Q2_results`,
         `FY21 Q3` = `FY21Q3_results`,
         `FY20 Total`  = FY20_cumulative,
         `FY21 Total`  = FY21_cumulative,
         `FY20 Targets` = FY20_targets,
         `FY21 Targets` = FY21_targets) %>% 
  mutate(`FY20 Achieved` =  `FY20 Total`/`FY20 Targets`,
         `FY21 Achieved` =  `FY21 Total`/`FY21 Targets`) %>%
  mutate(`FY21 Achieved`=case_when(indicator=="Positivity" ~ NA_real_, TRUE ~`FY21 Achieved`)) %>% 
  relocate(`FY20 Total`, .before = `FY21 Targets`) %>%
  relocate(`FY20 Achieved`, .before = `FY21 Targets`) %>%
  relocate(`FY20 Targets`, .after = `FY20 Total`) %>%
  ungroup() %>% 
  mutate(indicator = fct_relevel(indicator, indicator_order)) %>% 
  arrange(indicator)


# Create table

save_path <- "Tables"


tbl<-df3 %>%
  select(`indicator`, `FY21 Q1`,`FY21 Q2`,`FY21 Q3`,`FY21 Total`,`FY21 Targets`, `FY21 Achieved`) %>%
  filter(`FY21 Q1`>0,) %>%
 gt(rowname_col = "indicator") %>% 
  fmt_number(
    columns = 2:7, 
    rows=indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_CURR", "TX_NET_NEW", "TX_NEW", "VMMC_CIRC"),
    decimals = 0) %>%
  fmt_percent(
    rows = indicator =="Positivity",
    columns = 2:7,
    decimals = 1) %>% 
  fmt_percent(
    columns="FY21 Achieved",
    decimals=0) %>% 
  fmt_missing(columns = everything(),
              missing_text = "-") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  cols_width(
    vars(indicator) ~ px(140),
    everything() ~ px(120)) %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),),
    locations = cells_body(
      columns = everything(),
      rows = everything())) %>% 
  tab_style(style = cell_fill(denim, alpha = 0.5),    
            locations = cells_body(                
              columns = vars(`FY21 Achieved`),      
              rows = indicator!="TX_CURR" & `FY21 Achieved` >= .65)) %>%   ## 
  tab_style(style = cell_fill(denim_light, alpha = 0.5),
            locations = cells_body(
              columns = vars(`FY21 Achieved`),
              rows = indicator!="TX_CURR" & `FY21 Achieved` <.65)) %>% 
  # tab_style(style = cell_fill(golden_sand_light, alpha = 0.5),
  #           locations = cells_body(
  #             columns = vars(`FY21 Achieved`),
  #             rows = indicator!="TX_CURR" &`FY21 Achieved` < .4)) %>% 
  tab_style(style = cell_fill(old_rose_light, alpha = 0.5),
            locations = cells_body(
              columns = vars(`FY21 Achieved`),
              rows = indicator!="TX_CURR" & `FY21 Achieved` < .5)) %>% 
  tab_options(
    table.font.size = 18,
    table.font.names = "SourceSansPro-Regular",
    footnotes.font.size = 8
  ) %>% 
  tab_header(title = "Mozambique results, performance, and targets FY21") %>% 
  tab_source_note("Source: DATIM MSD FY21Q3 Clean Data") 


tbl





gtsave(tbl, here(save_path,"ach_table_moz_partner_q3.png"))