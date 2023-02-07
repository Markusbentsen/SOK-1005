#packages
rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(ggthemes)

#loading in datasets
lower_tro <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", fill=TRUE, header=TRUE)
mid_tro <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt", fill=TRUE, header=TRUE)
tro <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt", fill=TRUE, header=TRUE)
lower_str <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt", fill=TRUE, header=TRUE)

#cleaning up the datasets
lower_tro <- lower_tro[1:which(lower_tro$Year %in% "Year")-1, ]
mid_tro <- mid_tro[1:which(mid_tro$Year %in% "Year")-1, ]
tro <- tro[1:which(tro$Year %in% "Year")-1, ]
lower_str <- lower_str[1:which(lower_str$Year %in% "Year")-1, ]

#creating variable for lt
lt <- lower_tro %>% 
  mutate(Date = ymd(paste(lower_tro$Year, lower_tro$Mo, 1, sep="-"))) %>% 
  mutate(Variable = "Lower Troposphere") %>%
  select(Date, Globe, Variable) 
#creating variable for mt
mt <- mid_tro %>% 
  mutate(Date = ymd(paste(mid_tro$Year, mid_tro$Mo, 1, sep="-"))) %>% 
  mutate(Variable = "Mid Troposphere") %>%
  select(Date, Globe, Variable)
#creating variable for tp
tp <- tro %>% 
  mutate(Date = ymd(paste(tro$Year, tro$Mo, 1, sep="-"))) %>% 
  mutate(Variable = "Tropopause") %>%
  select(Date, Globe, Variable)
#creating variable for ls
ls <- lower_str %>% 
  mutate(Date = ymd(paste(lower_str$Year, lower_str$Mo, 1, sep="-"))) %>% 
  mutate(Variable = "Lower Stratosphere") %>%
  select(Date, Globe, Variable) 

#making one combined dataframe with rbind
df_temp <- rbind(lt, mt, tp, ls) 

#selecting months after december 1979 and making it numeric
df_temp <- df_temp %>% 
  filter(Date > "1979-12-01") %>%
  mutate(Globe=as.numeric(Globe))

#making a new dataset for the avg temp
df_tempavg <- df_temp %>% 
  group_by(Date) %>%
  summarize(Globe = mean(Globe)) %>%
  mutate(Variable = "Average")

#combining the two datasets
df_temp <- rbind(df_temp, df_tempavg)

#twelve month moving average using rollmean
df_temp <- df_temp %>% 
  group_by(Variable) %>%
  mutate(monthavg = rollmean(Globe, 12, fill=NA)) 

#plotting the graph
df_temp %>%
  mutate(isAverage = (Variable == 'Average')) %>% 
  ggplot(aes(x = Date, y = monthavg, color = Variable)) + 
  geom_line(aes(linetype = isAverage), size = 1.3, alpha = 0.6) + 
  labs(title = "12 Month average global temperature",
       subtitle = "Average global temperature from different areas",
       x = "Years",
       y = "Temperatures",
       color = "Variable") + 
  theme_bw() + 
  theme(axis.title = element_text(), text = element_text()) + 
  theme(legend.title = element_blank()) +
  scale_linetype_manual(values = c("dotted", "solid"), guide = "none")
  

