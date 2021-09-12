
names(csvData)[1]<-"state"
names(regions)[1]<-"state"
names(regions)[1]<-"home_of_record_state"
names(POP2000)[1]<-"FIPS"
names(Unemployment)[2]<-"state"
names(GWOTNames)[11]<-"county"

med_hou_income<- Unemployment %>% 
  select(FIPS, Median_Household_Income_2019)

pop2010<- POP2000 %>% 
  filter(year == 2010) %>% 
  select(FIPS, tot_pop)

county_stats<- county_key %>% 
  left_join(med_hou_income, by = "FIPS") %>% 
  left_join(csvData, by = "state") %>% 
  left_join(pop2010, by = "FIPS")

county_cas_stats<- GWOTNames %>% 
  group_by(county, home_of_record_state) %>% 
  summarize(number = n())

county_data<- county_stats %>% 
  left_join(county_cas_stats, by = c("county", "home_of_record_state")) %>% 
  left_join(regions, by = "home_of_record_state")

county_data[is.na(county_data)] <- 0

county_data<- county_data %>% 
  mutate(per_100 = number / tot_pop * 100000)

county_data %>% 
  filter(Median_Household_Income_2019 > 0) %>% 
  ggplot(aes(x= Median_Household_Income_2019, y=per_100, color = region)) +
  geom_point(size = 3, alpha = .75) +
  labs(title = "U.S. Fatality Patterns of the Global War on Terror (2001-2021)",
       subtitle = "Fatalities Per 100,000, by County, Median Household Income, and Region",
       color = "Region",
       size = "Total No. Fatalities",
       x = "Median Household Income (2019)",
       y = "Fatalities per 100,000") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks = c(25000, 50000, 75000, 100000, 125000, 150000),
                     labels = c ("$25,000", "$50,000", "$75,000", "$100,000", "$125,000", "$150,000")) +
  scale_size_continuous(breaks=c(1, 10, 20, 30),
                        labels=c("1", "10", "20", "30+")) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(nrow = 1),(override.aes = list(size=20))) +
  guides(size = guide_legend(nrow = 1),(override.aes = list(size=20))) +
  geom_hline(yintercept = 2.27, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 54460, linetype="dashed", color = "black", size = 1) +
  annotate("text", label = "Median National Income (2019)", x = 53341, y = 63, 
           color = "black", angle = 90, fontface="bold", size = 5)


county_data %>% 
  filter(tot_pop < 1000001) %>% 
  filter(per_100 < 60) %>%
  ggplot(aes(x= tot_pop, y= per_100, color = region, label = county)) + 
  geom_point(size = 3, alpha = .75) +
  labs(title = "U.S. Fatality Patterns of the Global War on Terror (2001-2021)",
       subtitle = "Fatalities Per 100,000, by County Population and Region (Counties Under 1M Residents)",
       color = "Region",
       x = "County Population",
       y = "Fatalities per 100,000",
       caption = "Counties Over 1M Residents and Under 60 Per 100k Not Shown for Scaling Purposes") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks = c(0,250000, 500000,750000, 1000000),
                     labels = c ("0","250,000", "500,000","750,000", "1,000,000")) +
  scale_size_continuous(breaks=c(1, 10, 20, 30),
                        labels=c("1", "10", "20", "30+")) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=20, color = "Black", face="bold")) +
  guides(color = guide_legend(nrow = 1),(override.aes = list(size=100))) +
  geom_vline(xintercept = 58815, linetype="dashed", color = "black", size = 1) +
  annotate("text", label = "U.S. Median County Population (2010)", x = 50800, y = 32, 
           color = "black", angle = 90, fontface="bold", size = 5)
region_pop<- county_data %>% 
  group_by(home_of_record_state) %>% 
  summarize(tot_pop = sum(tot_pop)) %>% 
  left_join(regions, by = "home_of_record_state") %>% 
  group_by(region) %>% 
  summarize(region_pop = sum(tot_pop))

state_pop<- co.est2019.alldata %>% 
  filter(COUNTY == 0) %>% 
  select(STNAME, CENSUS2010POP)

names(state_pop)[1]<-"state"
names(state_pop)[2]<-"pop"

sum(state_pop$pop, na.rm = TRUE)

7037 / 308745538 * 100000

region_pop<- state_pop %>% 
  left_join(csvData, by = "state") %>% 
  left_join(regions, by = "home_of_record_state") %>% 
  group_by(region) %>% 
  summarize(total_pop = sum(pop))
  
gwot_region<- GWOTNames %>% 
  group_by(home_of_record_state) %>% 
  left_join(regions, by = "home_of_record_state") %>% 
  group_by(region) %>% 
  summarize(number = n()) %>% 
  left_join(region_pop, by = "region") %>% 
  drop_na(total_pop) %>% 
  mutate(per_100 = number / total_pop * 100000)
