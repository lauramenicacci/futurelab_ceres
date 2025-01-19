##### stip policies

library(tidyverse)
library(readxl)
library(here)
library(dplyr)
library(countrycode)

here::i_am('code/stip_preprocessing.R')

s <- read_xlsx('data/raw/All-policy-initiatives.xlsx') %>% filter(!is.na(`Policy instrument type category`))

top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")

start_year <- 1996
end_year <- 2024

belgium <- s %>% filter(grepl('Belgium', Country)) %>% pull(Country) %>% unique

s1 <- s %>% 
  filter(!Country %in% c('European Union')) %>% 
  mutate(Country = case_when(
    Country %in% belgium ~ "Belgium",
    TRUE ~ as.character(Country)
  )) %>% 
  mutate(ISO = countrycode(Country, origin = "country.name", destination = "iso3c")) %>% 
  filter(ISO %in% top_25) %>% 
  group_by(ISO, `Policy instrument type category`, `Policy instrument type`) %>% 
  crossing(Year = seq(start_year, end_year)) %>% # Create time series
  # Add dummy variable: 1 if policy is active, 0 otherwise, switch off using end date
  mutate(IsActive = ifelse(
    Year >= `Start date` & (is.na(`End date`) | Year <= `End date`), 1, 0
  )) %>% 
  select(Country, ISO, Year, IsActive, `Policy instrument type category`, 
         `Policy instrument type`, `Yearly budget range (in EUR)`, `Policy instrument ID`) %>% 
  unique()

write.csv(s1, 'data/out/stip_policies.csv')


# cateogory

cat <- ggplot(s1, aes(x = Year, y = IsActive)) +
  geom_point(aes(color = `Policy instrument type category`), size = 1) + 
  geom_line(aes(group = `Policy instrument ID`), alpha = 0.7) + 
  scale_y_continuous(
    breaks = c(0, 1), 
    labels = c("Inactive", "Active")
  ) +
  facet_wrap(~ISO, scales = "free_y") + 
  labs(
    x = "Year",
    y = "Policy activation dummy",
    color = "Policy Type Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = 'bottom', 
    plot.background = element_rect(fill = 'white')
  )

ggsave("figs/stip_dummy_category.png", plot = cat, width = 12, height = 10, dpi = 500)



# type

ggplot(s1, aes(x = Year, y = IsActive)) +
  geom_point(aes(color = `Policy instrument type`), size = 1) + 
  geom_line(aes(group = `Policy instrument ID`), alpha = 0.7) + 
  scale_y_continuous(
    breaks = c(0, 1), 
    labels = c("Inactive", "Active")
  ) +
  facet_wrap(~ISO, scales = "free_y") + 
  labs(
    x = "Year",
    y = "Policy activation dummy",
    color = "Policy Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = 'bottom', 
    plot.background = element_rect(fill = 'white')
  )
