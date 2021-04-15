library(RSocrata)
library(lubridate)
library(zoo)

covid_data <- read.socrata("https://data.cityofchicago.org/Health-Human-Services/COVID-19-Daily-Cases-and-Deaths/naz8-j4nc/data") %>% as_tibble()
covid <- covid_data %>%
  select(date = lab_report_date, cases = cases_total, deaths = deaths_total) %>%
  mutate(date = as_date(date)) %>%
  arrange(desc(date)) %>% 
  filter(!is.na(cases) & !is.na(deaths)) %>%
  mutate(rolling_cases = rollmean(cases,k=7,fill = NA,align = "left"),
         rolling_deaths = rollmean(deaths,k=7,fill = NA,align = "left"))


#tests_raw <- read.socrata("https://data.cityofchicago.org/Health-Human-Services/COVID-19-Daily-Testing/t4hh-4ku9/data") %>%
#  as_tibble()
acs_block_group <- read_csv("projects/2020-019-quality-of-life-ta/output/prelim/block_group_data_2.csv")
over18_pop <- acs_block_group %>%
  mutate(over18 = population-under_18) %>%
  pull(over18) %>%
  sum

vaccinations <- read.socrata("https://data.cityofchicago.org/resource/2vhs-cf6b.csv") %>%
  mutate(date = ymd(date)) %>%
  select(date,daily_vaccines = total_doses_daily,
         cum_vaccines = total_doses_cumulative,
         first_dose = X_1st_dose_daily,
         first_dose_cum = X_1st_dose_cumulative,
         second_dose = vaccine_series_completed_daily,
         second_dose_cum = vaccine_series_completed_cumulative,
         first_dose_cum_pct = X_1st_dose_percent_population,
         second_dose_cum_pct = vaccine_series_completed_percent_population) %>%
  arrange(desc(date)) %>%
  mutate(rolling_vaccines_total = rollmean(daily_vaccines,k=7,fill = NA,align = "left"),
         rolling_first_dose = rollmean(first_dose,k=7,fill = NA,align = "left"),
         rolling_second_dose = rollmean(second_dose,k=7,fill = NA,align = "left"),
         first_dose_over18_pct = first_dose_cum/over18_pop,
         second_dose_over18_pct = second_dose_cum/over18_pop) %>%
  as_tibble(); vaccinations
  
vaccinations_by_dose <- vaccinations %>%
  select(date,rolling_vaccines_total,first_dose,second_dose) %>%
  pivot_longer(cols = contains("dose"),
               names_to = "dose",
               values_drop_na = F) 



ggplot(vaccinations_by_dose,aes(x=date,y=value)) +
  #geom_bar(stat = "identity",aes(fill = dose),color = "black") +
  #scale_fill_manual(values = c("blue","green"),labels = c("First Dose","Second Dose"), name = "Daily Vaccines\nAdministered") +
  geom_line(data = vaccinations,aes(x = date,y = first_dose_over18_pct,colour = "First Dose"), size = 1.5) +
  geom_line(data = vaccinations,aes(x = date,y = second_dose_over18_pct,colour = "Second Dose"), size = 1.5) +
  geom_hline(yintercept = .20,color = "limegreen", linetype = "dashed") +
  geom_hline(yintercept = .40, color = "blue", linetype = "dashed") +
  scale_color_manual(name = "Percent of Chicago Adult\nPopulation Vaccinated",
                     values = c("First Dose"="dodgerblue",
                                "Second Dose"="darkgreen")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(0,.5,.05),limits = c(0,.5)) +
  scale_x_date(date_breaks = "1 week",date_labels = "%b %d") +
  labs(title = "Chicago daily covid-19 vaccines administered",
       y = "Vaccines Administered",
       x = "")


ggplot(vaccinations_by_dose,aes(x=date,y=value)) +
  geom_bar(stat = "identity",aes(fill = dose),color = "black") +
  scale_fill_manual(values = c("blue","green"),labels = c("First Dose","Second Dose"), name = "Daily Vaccines\nAdministered") +
  geom_line(data = vaccinations,aes(x = date,y = rolling_vaccines_total,colour = "Total"), size = 1.5) +
  geom_line(data = vaccinations,aes(x = date,y = rolling_first_dose,colour = "First Dose"), size = 1.5) +
  geom_line(data = vaccinations,aes(x = date,y = rolling_second_dose,colour = "Second Dose"), size = 1.5) +
  scale_color_manual(name = "Daily vaccinations weekly\nrolling average",
                     values = c("Total"="red",
                                "First Dose"="dodgerblue",
                                "Second Dose"="darkgreen")) +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "1 week",date_labels = "%b %d") +
  labs(title = "Chicago daily covid-19 vaccines administered",
       y = "Vaccines Administered",
       x = "")

tests_updated <- read.socrata("https://data.cityofchicago.org/resource/gkdw-2tgv.csv")

tests <- tests_updated %>% 
  mutate(date = as_date(date)) %>% 
  select(date, tests = total_tests, positives = positive_tests) %>%
  arrange(desc(date)) %>% 
  mutate(percent_positive = positives/tests,
         rolling_tests = rollmean(tests,k=7,fill = NA,align = "left"),
         rolling_positives = rollmean(positives,k=7,fill = NA,align = "left"),
         rolling_percent_positive = rolling_positives/rolling_tests)

covid <- covid %>%
  left_join(tests) %>%
  filter(date >= ymd("2020-03-01"))

covid

covid <- covid %>% slice(-2)

hospital_data <- read.socrata("https://data.cityofchicago.org/Health-Human-Services/COVID-19-Hospital-Capacity-Metrics/f3he-c6sv") %>%
  as_tibble()

hospital <- hospital_data %>% 
  select(date,ventilators_available_total,ventilators_in_use_covid_19,ventilators_total_capacity,ventilators_in_use_total,
         icu_beds_available_adult,icu_beds_total_capacity,icu_beds_in_use_covid_19, icu_beds_in_use_total,
         acute_non_icu_beds_total_capacity,acute_non_icu_beds_in_use_total) %>%
  mutate(date = as_date(date),
         ventilator_percent = ventilators_in_use_total/ventilators_total_capacity * 100,
         icu_percent = icu_beds_in_use_total/icu_beds_total_capacity * 100,
         beds_percent = acute_non_icu_beds_in_use_total/acute_non_icu_beds_total_capacity * 100,
         ventilator_rolling = rollmean(ventilator_percent,k=7,fill = NA,align = "left"),
         icu_rolling = rollmean(icu_percent,k=7,fill = NA,align = "left"),
         beds_rolling = rollmean(beds_percent,k=7,fill = NA,align = "left"))


ggplot(hospital,aes(x=date)) +
  geom_line(aes(y=beds_percent,colour = "Total Hospital Beds Used")) +
  geom_line(aes(y=icu_percent,colour = "ICU Units Used")) +
  geom_line(aes(y=ventilator_percent,colour = "Ventilators Used")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100,linetype = "dashed") +
  labs(y = " ",
       title = "Chicago Hospital Capacity Measures during Covid-19") + 
  theme(legend.position="right") +
  scale_color_manual(name="Hospital Capacity (%)",
                     values = c("Total Hospital Beds Used"="darkgreen",
                                "ICU Units Used"="blue",
                                "Ventilators Used"="red"))

ggplot(hospital,aes(x=date)) +
  geom_line(aes(y=beds_rolling,colour = "Total Hospital Beds Used")) +
  geom_line(aes(y=icu_rolling,colour = "ICU Units Used")) +
  geom_line(aes(y=ventilator_rolling,colour = "Ventilators Used")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100,linetype = "dashed") +
  labs(y = " ",
       title = "Chicago Hospital Capacity Measures during Covid-19") + 
  theme(legend.position="right") +
  scale_color_manual(name="Hospital Capacity (%, 7-day rolling avg)",
                     values = c("Total Hospital Beds Used"="darkgreen",
                                "ICU Units Used"="blue",
                                "Ventilators Used"="red"))

#ggplot(hospital,aes(date,ventilators_total_capacity)) +
#  geom_line() +
  #geom_line(aes(y=ventilators_available_total),linetype = "dashed") +
#  geom_line(aes(y=ventilators_in_use_covid_19),linetype = "dashed",color="red") +
#  geom_line(aes(y=ventilators_in_use_total),linetype = "dashed",color="blue") +
#  geom_hline(yintercept = 0)# +
  #labs(y = "Daily covid-19 Cases")

#ggplot(hospital,aes(date,icu_beds_total_capacity)) +
#  geom_line() +
#  geom_line(aes(y=icu_beds_available_adult),linetype = "dashed") +
#  geom_line(aes(y=icu_beds_in_use_covid_19),linetype = "dashed",color="red") +
#  geom_line(aes(y=icu_beds_in_use_total),linetype = "dashed",color="blue") +
#  geom_hline(yintercept = 0)# +


ggplot(covid,aes(date,rolling_cases)) +
  geom_bar(stat = "identity",aes(y=cases),color = "NA",fill = "blue") +
  geom_line(color = "red",size = 1) +
  geom_hline(yintercept = 0) +
  labs(y = " ",
       title = "Chicago daily Covid-19 cases",
       x = "")


ggplot(covid,aes(date,rolling_deaths)) +
  geom_bar(stat = "identity",aes(y=deaths),fill = "blue",color="NA") +
  geom_line(color = "red", size = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Chicago daily Covid-19 deaths",
       y = " ",
       x = "")

ggplot(covid,aes(date,rolling_tests)) +
  geom_bar(stat = "identity",aes(y=tests),fill = "blue", color = "NA") +
  geom_line(color = "red",size = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Chicago daily covid-19 tests",
       y = "",
       x = "")

ggplot(covid %>% mutate(rolling_percent_positive = if_else(date <= ymd("2020-03-06"),NaN,rolling_percent_positive)) %>% mutate(rolling_percent_positive = 100 * rolling_percent_positive) %>% slice(-1),aes(date,rolling_percent_positive)) +
  geom_bar(stat = "identity",aes(y=percent_positive*100),fill = "blue",color="NA") +
  geom_line(color = "red", size = 1) +
  geom_hline(yintercept = 3,color = "darkgreen",linetype = "dashed",size = 1) +
  geom_hline(yintercept = 5,color = "green",linetype = "dashed",size = 1) +
  geom_hline(yintercept = 10,color = "goldenrod",linetype = "dashed",size = 1) +
  geom_hline(yintercept = 0) + 
  labs(title = "Chicago daily percent test positive",
       y = "",
       x = "")

# ggplot(covid %>% filter(date > ymd("2020-08-01")) %>% mutate(x = rolling_cases/500, y = cases/500),aes(date,x)) +
#   geom_bar(stat = "identity",aes(y=y),fill = "blue", color = "black") +
#   geom_line(color = "red",size = 1) +
#   geom_hline(yintercept = 0) +
#   labs(title = "Times Amanda has been wrong per day",
#        y = "",
#        x = "")
# 

