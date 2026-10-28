#Author: Leah Kim
#Date: 10/26/26 
#Class: PAI 741
#Topic: Policy Agendas PS #8 

#Load packages 
library(tidyverse)
library(haven)
library(lattice)
library(ggplot2)
library(grid)
library(srvyr)
library(dplyr)

#Set working directory and load all data 
setwd('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Assignments /Assignment 8')
exec <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/Data for Assingment 8/US-Executive-executive_orders_26.1.csv')
congress <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/Data for Assingment 8/US-Legislative_congressional_hearings-21.7 (1).csv')
nyt <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/Data for Assingment 8/US-Media_NYTimesIndex_16.1.csv')
gallup <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/Data for Assingment 8/US-Public-Gallups_Most_Important_Problem-21.3.csv')

#Check data needed 
table(exec$subtopic)
table(exec$subtopic[exec$majortopic ==1])
#Fig 1: Trend within Executive Orders between Macroeconomics (1) & Social Welfare (13)
fig1_data <- exec %>%
  filter(majortopic %in% c(1, 13)) %>%
  mutate(majortopic = if_else(majortopic ==1, "Macroeconomics", "Social Welfare")) %>%
  count(year, majortopic)
#Now graphing the plot1 using the fig1_data 
plot1 <- ggplot(fig1_data, aes(x = year, y = n, color = majortopic, group = majortopic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Macroeconomics" = "#FF6A6A", 
                                "Social Welfare" = "#00BFFF")) +
  scale_x_continuous(breaks = seq(1945, 2025, by = 10)) +
  labs(
    title = "Trend in Executive Orders: Macroeconomics vs. Social Welfare, 1945–2025",
    subtitle = "Annual count of executive directives (Comparative Agendas Project)",
    x = "Year",
    y = "Number of Executive Orders",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 11)
  )
plot1

#Fig 2: Trend in Executive Orders but within Subtopics of Macroeconomics: Price Control & Stabilization vs. National Budget and Debt
fig2_data <- exec %>%
  filter(majortopic ==1, subtopic %in% c(105, 110)) %>%
  mutate(subtopic = if_else(subtopic == 105, 
                            "National Budget and Debt", 
                            "Price Control and Stabilization")) %>%
  count(year, subtopic)
#Now graphing 
plot2 <- ggplot(fig2_data, aes(x = year, y = n, color = subtopic, group = subtopic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("National Budget and Debt" = "#FF6A6A", 
                                "Price Control and Stabilization" = "#00BFFF")) +
  scale_x_continuous(breaks = seq(1945, 2025, by = 10)) +
  labs(
    title = "Trend in Executive Orders: Macroeconomics Subtopics of National Budget and Debt vs. 
    Price Control and Stabilization, 1945–2025",
    subtitle = "Annual count of executive directives (Comparative Agendas Project)",
    x = "Year",
    y = "Number of Executive Orders",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 11)
  )
plot2

#Fig 3: Executive Orders vs. Congressional Hearings trends when focusing on the topic of Social Welfare
#Create variable that looks just at Social Welfare for the Executive Orders 
exec_share <- exec %>%
  group_by(year) %>%
  summarise(share = mean(majortopic == 13, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Executive Orders")
#Create variable that looks just at Social Welfare for the Congressional Hearings and dropping years that have NA 
hearings_share <- congress %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(share = mean(majortopic == 13, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Congressional Hearings")
#Now combine the datasets to have the data to make Fig 3 
fig3_data <- bind_rows(exec_share, hearings_share)

#Now graphing 
plot3 <- ggplot(fig3_data, aes(x=year, y=share, color=source, group=source)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Executive Orders" = "#00BFFF",
                                "Congressional Hearings" = "#FF6A6A")) + 
  scale_x_continuous(breaks = seq(1945, 2025, by = 10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Social Welfare: Executive Orders vs. Congressional Hearings, 1945-2025", 
    subtitle = "Share of each groups yearly contribution as Social Welfare", 
    x = "Year",
    x = "Share of yearly output", 
    color = NULL
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 11)
  )
plot3
#Fig 4: Executive Orders vs. Congressional Hearings trends when focusing on the topic of Macroeconomics
#Create variable that looks just at Macroeconomics for the Executive Orders 
exec_share_ecn <- exec %>%
  group_by(year) %>%
  summarise(share = mean(majortopic == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Executive Orders")
#Create variable that looks just at Macroeconomics for the Congressional Hearings and dropping years that have NA 
hearings_share_ecn <- congress %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(share = mean(majortopic == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Congressional Hearings")
#Now combine the datasets to have the data to make Fig 3 
fig4_data <- bind_rows(exec_share_ecn, hearings_share_ecn)

#Now graphing 
plot4 <- ggplot(fig4_data, aes(x=year, y=share, color=source, group=source)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Executive Orders" = "#00BFFF",
                                "Congressional Hearings" = "#FF6A6A")) + 
  scale_x_continuous(breaks = seq(1945, 2025, by = 10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Macroeconomics: Executive Orders vs. Congressional Hearings, 1945-2025", 
    subtitle = "Share of each groups yearly contribution as Macroeconomics", 
    x = "Year",
    x = "Share of yearly output", 
    color = NULL
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 11)
  )
plot4

#Saving all plots 
ggsave("figures/figure1.png", plot = plot1, width = 8, height = 5, dpi = 300)
ggsave("figures/figure2.png", plot = plot2, width = 8, height = 5, dpi = 300)
ggsave("figures/figure3.png", plot = plot3, width = 8, height = 5, dpi = 300)
ggsave("figures/figure4.png", plot = plot4, width = 8, height = 5, dpi = 300)