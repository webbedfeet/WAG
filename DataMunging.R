##%######################################################%##
#                                                          #
####                    Data Munging                    ####
#                                                          #
##%######################################################%##


# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(haven)

# Read data -----------------------------------------------------------------------------------

dat <- readRDS('data/rda/rawData.rds')


# New variables -------------------------------------------------------------------------------

dat <- dat %>% mutate(
  directroom = ifelse(sampletype=='direct reading' & samplelocation %in% c('room','center of room'),
                      direct, NA),
  directbz = ifelse(sampletype == 'direct reading' & samplelocation =='breathing zone', direct, NA),
  directvaporizer = ifelse(sampletype =='direct reading' & samplelocation =='vaporizer', direct, NA),
  badgevaporizer8hr = ifelse(sampletype =='area' & samplelocation=='vaporizer', 
                             # hourtwafixedppm, NA),
                             pmax(hourtwafixedppm,hourtwarangeaverageppm, na.rm=T), NA),
  badgeroom8hr = ifelse(sampletype == 'area' & samplelocation %in% c('room', 'center of room'), 
                        # hourtwafixedppm, NA),
                        pmax(hourtwafixedppm, hourtwarangeaverageppm, na.rm=T), NA),
  badgebz8hr = ifelse(sampletype == 'personal' & samplelocation == 'breathing zone', 
                      # hourtwafixedppm, NA)
                      pmax(hourtwafixedppm, hourtwarangeaverageppm, na.rm=T), NA)
)

dat1 <- select(dat, sampleid:samplelocation, ic, building, room, subject, directroom:badgebz8hr, chemical) %>% 
  mutate(subject = case_when(subject == 1 ~ 'human', subject == 2 ~ 'pig', subject == 3 ~ 'mice',
                             subject == 4 ~ 'nhp', subject == 5 ~ 'rats'))

dat2 <- dat1 %>% gather(variable, value, directroom:badgebz8hr) %>% 
  filter(!is.na(value)) %>% 
  select(sampleid,surveynumber,samplelocation, subject, variable, value, chemical) %>% 
  arrange(surveynumber)

dat3 <- dat2 %>% group_by(surveynumber, variable) %>% summarize(value = mean(value)) %>% ungroup() %>% 
  spread(variable, value) %>% 
  left_join(dat2 %>% select(surveynumber,  subject, chemical) %>% distinct()) 

dat4 <- dat2 %>% group_by(surveynumber, variable) %>% summarize(value = max(value)) %>% 
  spread(variable, value) %>% 
  left_join(dat2 %>% select(surveynumber, subject, chemical) %>% distinct())

write_dta(dat3, path = 'wag_mean.dta')
write_dta(dat4, path = 'wag_max.dta')
# variable              n
# <chr>             <int>
#   1 badgebz8hr          111
# 2 badgeroom8hr         36
# 3 badgevaporizer8hr    40
# 4 directbz            105
# 5 directroom           86
# 6 directvaporizer      54
# 
GGally::ggpairs(dat3 %>% mutate_at(vars(starts_with('direct')), funs(ifelse(. > 10, 10, .))) %>% 
                  filter(chemical == 'isoflurane')%>% 
                  mutate_at(vars(starts_with('direct')), funs(ifelse(. >=10, NA, .))) %>% 
                  select(starts_with('badge'), starts_with('direct')))
