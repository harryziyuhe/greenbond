library(tidyverse)
library(readr) 

namelist <- list(
  Indonesia = c('ID_20180706_20210706'),
  Malaysia = c('MY_20191227_20291227', 'MY_20191227_20301227', 'MY_20191227_20311226', 'MY_20191227_20321227', 'MY_20171229_20321229'),
  Taiwan = c('TW_20171215_20241215', 'TW_20201115_20251115', 'TW_20221215_20251215', 'TW_20221215_20271215')
)

bondlist <- list(
  Indonesia = c('Indonesia (2018-2021)'),
  Malaysia = c('Malaysia (2019-2029)', 'Malaysia (2019-2030)', 'Malaysia (2019-2031)', 'Malaysia (2019-2032)', 'Malaysia (2017-2032)'),
  Taiwan = c('Taiwan (2017-2024)', 'Taiwan (2020-2025)', 'Taiwan (2021-2025)', 'Taiwan (2022-2027)')
)

colorlst = list(
  Indonesia = 'blue',
  Malaysia = 'green',
  Taiwan ='black'
)

plot_zspread <- function(country, name, bondname, ax = ggplot, bondcolor = 'blue') {
  data_dir <- paste0("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/",
                     country, "/")
  gbond_dir <- paste0(data_dir, name, "_G_z.csv")
  ngbond_dir <- paste0(data_dir, name, "_NG_z.csv")
  gbond <- read_csv(gbond_dir)
  ngbond <- read_csv(ngbond_dir)
  names(gbond) <- c("date", "GZ")
  names(ngbond) <- c("date", "NGZ")
  
  ngbond$date = as.Date(ngbond$date, format = '%d-%b-%Y')
  gbond$date = as.Date(gbond$date, format = '%d-%b-%Y')
  bondz <- ngbond |> 
    inner_join(gbond, by = 'date') |> 
    mutate(greenium = GZ-NGZ,
           bondname = bondname)
  bondcolor = bondcolor
  ax <- ax + 
    geom_boxplot(aes(y = factor(bondname), x = greenium, color = bondcolor), data = bondz)
  return(ax)
}

zspread <- ggplot()
for (country in names(namelist)){
  companies <- namelist[[country]]
  bonds <- bondlist[[country]]
  for (i in 1:length(companies)){
    zspread <- plot_zspread(country, companies[i], bonds[i], zspread, country)
  }
}

zspread <- zspread + 
  scale_color_manual(values = c('Indonesia' = '#e41a1c',
                                'Malaysia' = '#377eb8',
                                'Taiwan' = '#4daf4a')) +
  xlim(-10, 25) +
  labs(x = 'Greenium (bps)',
       y = 'Bonds',
       color = 'Country') +
  theme_classic() + theme(text = element_text(family = 'serif',
                                              color = 'black',
                                              size = 14))
zspread
ggsave('Twinbond Greenium.png')
