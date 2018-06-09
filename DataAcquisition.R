ProjTemplate::reload()

dat <- read_dta('data/raw/wag4.dta', encoding = 'windows-1250')
dat <- dat %>% select(type:vaporizermodel, subject:patient, sampletype1:samplelocation1)
saveRDS(dat, file = 'data/rda/rawData.rds', compress = T)
