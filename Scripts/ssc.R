# Sap Sugar Concentration


# Hannah Grace McNulty
# 07/10/2026

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Path to files
folder_path <-"CleanedData/LTS-Sugar_concentrations_copy.xlsx"


dates <- excel_sheets(folder_path) [-c(1:2)]

# Loop over sample dates to read data for each date ----
for (d in dates){
  
  # Read date-specific data ----
  tmp <- read_excel(
    path = folder_path, 
    sheet = d, 
    range = "A9:H102",
    na = "NA",
    col_names = c("t_number", "t_name", "tree_id", "ssc1", "ssc2", "ssc3", "ssc", "comments"),
    col_types = c("numeric", "text", "text", rep("numeric", 4), "text")) %>% 
    add_column (date = as_date(d))

  # Concatenate data ----
  if (d == dates[1]) {
    SS <- tmp
  } else {
    SS <- rbind (SS, tmp)
  }
} 

# Correct dates for a few trees (see comments of data file for reasons) ----
SS$date[SS$date == as_date("2025-03-05") & SS$t_number == 3] <- 
  as_date("2025-03-06")
SS$date[SS$date == as_date("2025-03-19") & SS$t_number == 3] <- 
  as_date("2025-03-20")
trees <- c("871", "873", "874","877", "879", "880", "883", "884", "886", "887", 
           "888", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH")
SS$date[SS$date == as_date("2025-04-03") & SS$tree_id %in% trees ] <- 
  as_date("2025-04-04")
trees1 <- c("836", "850", "870", "886", "808", "816", "862", "874", "877", "879", "N", "O")
SS$date[SS$date == as_date("2026-04-08") & SS$tree_id %in% trees1 ] <- 
  as_date("2026-04-09")




################################################ 
### Weekly ssc
# Create treatment means and standard deviations ----
means2024 <- SS %>% 
  filter(format(date, "%Y") == "2024") %>%
  mutate(week = date) %>% 
  group_by (t_number, week) %>% 
  summarise(m_ssc = mean (ssc, na.rm = TRUE),
            sd_ssc = sd (ssc, na.rm = TRUE), .groups = "keep")
means2025 <- SS  %>% 
  filter(format(date, "%Y") == "2025") %>%
  mutate(week = floor_date(date, unit = "week")) %>% 
  group_by (t_number, week) %>% 
  summarise(m_ssc = mean (ssc, na.rm = TRUE),
            sd_ssc = sd (ssc, na.rm = TRUE), .groups = "keep")
means2026 <- SS %>%
  filter(format(date, "%Y") == "2026") %>%
  group_by (t_number, date) %>% 
  summarise(m_ssc = mean (ssc, na.rm = TRUE),
            sd_ssc = sd (ssc, na.rm = TRUE), .groups = "keep")



################# SSC 2026
SS_2026 <- SS %>%
  filter(year(date) == 2026)


SS_2026 %>% ggplot(aes(x = date,
                       y = ssc)) +
  geom_line(aes(group = tree_id, 
                color = factor(t_number)),
            linewidth = 0.3, alpha = 0.2) +
  geom_point(aes(color = factor(t_number)),
             position = position_jitter(width = 0.2),
             size = 1.5, alpha = 0.9) +
  geom_point(data = means2026,
    aes(x = date,
        y = m_ssc, 
        color = factor(t_number)),
    size = 5, alpha = 0.7) +
  scale_color_manual(name = "Treatment",
                     labels = c("2" = "Gravity",
                                "3" = "Vacuum",
                                "4" = "Sugar Control"),
                     values = c("2" = "#1f77b4",
                                "3" = "#ff7f0e",
                                "4" = "#7f7f7f")) +
  scale_x_date(breaks = as.Date(c("2026-03-06", "2026-03-26", "2026-03-30", "2026-04-08", "2026-04-09")),
    labels = c("6 Mar", "26 Mar", "30 Mar", "8 Apr", "9 Apr")) +
  ylim(0, 7) +
  labs(x = "",
    y = "Soluble sugar concentration (°Brix)",
    x = "Date",
    color = "Treatment") +
  theme_classic()



################################################ 
### Histograms

ssc_26 <- SS %>% 
  filter(format(date, "%Y") == "2026") %>%
  ggplot(aes(x = ssc, fill = factor(t_number))) +
  geom_histogram(binwidth = 0.25,
                 color = "white") +
  facet_wrap(~ t_name) +
  scale_fill_manual(
    name = "Treatment",
    labels = c("2" = "Gravity",
               "3" = "Vacuum",
               "4" = "Sugar Control"),
    values = c("2" = "#1E90FF",
               "3" = "#004080",
               "4" = "#9B59B6")
  ) +
  labs(title = "SSC 2026",
       x = "Soluble sugar concentration (°Brix)",
       y = "Frequency") +
  theme_classic() +
  theme(legend.position = "none")


ssc_25 <- SS %>% 
  filter(format(date, "%Y") == "2025") %>%
  ggplot(aes(x = ssc, fill = factor(t_number))) +
  geom_histogram(binwidth = 0.25,
                 color = "white") +
  facet_wrap(~ t_name) +
  scale_fill_manual(
    name = "Treatment",
    labels = c("2" = "Gravity",
               "3" = "Vacuum",
               "4" = "Sugar Control"),
    values = c("2" = "#1E90FF",
               "3" = "#004080",
               "4" = "#9B59B6")
  ) +
  labs(title = "SSC 2025",
       x = "Soluble sugar concentration (°Brix)",
       y = "Frequency") +
  theme_classic() +
  theme(legend.position = "none")


ssc_24 <- SS %>% 
  filter(format(date, "%Y") == "2024") %>%
  ggplot(aes(x = ssc, fill = factor(t_number))) +
  geom_histogram(binwidth = 0.25,
                 color = "white") +
  facet_wrap(~ t_name) +
  scale_fill_manual(
    name = "Treatment",
    labels = c("2" = "Gravity",
               "3" = "Vacuum",
               "4" = "Sugar Control"),
    values = c("2" = "#1E90FF",
               "3" = "#004080",
               "4" = "#9B59B6")
  ) +
  labs(title = "SSC 2024",
       x = "Soluble sugar concentration (°Brix)",
       y = "Frequency") +
  theme_classic() +
  theme(legend.position = "none")



ssc_overall <- SS %>% 
  filter(!is.na(ssc)) %>%
  ggplot(aes(x = ssc, fill = factor(t_number))) +
  geom_histogram(binwidth = 0.25,
                 color = "white") +
  facet_wrap(~ t_name) +
  scale_fill_manual(
    name = "Treatment",
    labels = c("2" = "Gravity",
               "3" = "Vacuum",
               "4" = "Sugar Control"),
    values = c("2" = "#1E90FF",
               "3" = "#004080",
               "4" = "#9B59B6")
  ) +
  labs(title = "SSC Overall 2024-2026",
       x = "Soluble sugar concentration (°Brix)",
       y = "Frequency") +
  theme_classic() +
  theme(legend.position = "none")


ssc_overall_notreatment <- SS %>% 
  filter(!is.na(ssc)) %>%
  ggplot(aes(x = ssc, fill = factor(t_number))) +
  geom_histogram(binwidth = 0.25,
                 color = "white") +
  facet_wrap(~ t_name) +
  scale_fill_manual(
    name = "Treatment",
    labels = c("2" = "Gravity",
               "3" = "Vacuum",
               "4" = "Sugar Control"),
    values = c("2" = "#1E90FF",
               "3" = "#004080",
               "4" = "#9B59B6")
  ) +
  labs(title = "SSC Overall 2024-2026",
       x = "Soluble sugar concentration (°Brix)",
       y = "Frequency") +
  theme_classic() +
  theme(legend.position = "none")


ssc_overall_stacked <- SS %>% 
  filter(!is.na(ssc)) %>%
  ggplot(aes(x = ssc, fill = factor(t_number))) +
  geom_histogram(binwidth = 0.25,
                 color = "white",
                 alpha = 0.5,
                 position = "identity") +
  scale_fill_manual(
    labels = c("2" = "Gravity",
               "3" = "Vacuum",
               "4" = "Sugar Control"),
    values = c("2" = "#1E90FF",
               "3" = "#004080",
               "4" = "#9B59B6")
  ) +
  labs(title = "SSC Overall 2024-2026",
        fill = "Treatment type",
       x = "Soluble sugar concentration (°Brix)",
       y = "Frequency") +
  theme_classic()
