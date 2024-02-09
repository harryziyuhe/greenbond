library(readr)
library(tidyverse)

moving_avg <- function(df){
  zoo_data <- df |> 
    dplyr::select(-end, -spread)
  names(zoo_data) <- c("date", paste(colnames(zoo_data)[-1], "MA", sep = ""))
  zoo_data <- zoo(zoo_data[, -1], order.by = zoo_data$date)
  ma_seven_days <- rollapply(zoo_data, width = 30, FUN = mean, by = 1, align = "right", fill = NA)
  dfMA <- cbind(df, ma_seven_days) |> 
    drop_na()
  dfMA$spreadMA <- dfMA[, ncol(dfMA)] - dfMA[, ncol(dfMA) - 1]
  
  return(dfMA)
}

spread <- function(dfNG, dfG){
  dftotal <- inner_join(dfNG, dfG,
                        by = c("date", "end")) |> 
    as.data.frame()
  dftotal$spread <- dftotal[, 4] - dftotal[, 3]
  return(dftotal)
}

MY20192032NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20321227_NG_bidy.csv")
MY20192032Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20321227_G_bidy.csv")
names(MY20192032NGbidy) <- c("date", "NGBidY", "start", "end")
MY20192032NGbidy <- MY20192032NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(MY20192032Gbidy) <- c("date", "GBidY", "start", "end")
MY20192032Gbidy <- MY20192032Gbidy |> 
  dplyr::select(date, end, GBidY)
MY20192032bidy <- spread(MY20192032NGbidy, MY20192032Gbidy)
MY20192032bidyMA <- moving_avg(MY20192032bidy)
MY20192032bidyMA <- MY20192032bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

MY20172032NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20171229_20321229_NG_bidy.csv")
MY20172032Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20171229_20321229_G_bidy.csv")
names(MY20172032NGbidy) <- c("date", "NGBidY", "start", "end")
MY20172032NGbidy <- MY20172032NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(MY20172032Gbidy) <- c("date", "GBidY", "start", "end")
MY20172032Gbidy <- MY20172032Gbidy |> 
  dplyr::select(date, end, GBidY)
MY20172032bidy <- spread(MY20172032NGbidy, MY20172032Gbidy)
MY20172032bidyMA <- moving_avg(MY20172032bidy)
MY20172032bidyMA <- MY20172032bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

TW20142024NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20141215_20241215_NG_bidy.csv")
TW20172024Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20171215_20241215_G_bidy.csv")
names(TW20142024NGbidy) <- c("date", "NGBidY", "start", "end")
TW20142024NGbidy <- TW20142024NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(TW20172024Gbidy) <- c("date", "GBidY", "start", "end")
TW20172024Gbidy <- TW20172024Gbidy |> 
  dplyr::select(date, end, GBidY)
TW20142024bidy <- spread(TW20142024NGbidy, TW20172024Gbidy)
TW20142024bidyMA <- moving_avg(TW20142024bidy)
TW20142024bidyMA <- TW20142024bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

TW20202025NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20201215_20251215_NG_bidy.csv")
TW20222025Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20221215_20251215_G_bidy.csv")
names(TW20202025NGbidy) <- c("date", "NGBidY", "start", "end")
TW20202025NGbidy <- TW20202025NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(TW20222025Gbidy) <- c("date", "GBidY", "start", "end")
TW20222025Gbidy <- TW20222025Gbidy |> 
  dplyr::select(date, end, GBidY)
TW20202025bidy <- spread(TW20202025NGbidy, TW20222025Gbidy)
TW20202025bidyMA <- moving_avg(TW20202025bidy)
TW20202025bidyMA <- TW20202025bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

ID20182021NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Indonesia/ID_20180706_20210706_NG_bidy.csv")
ID20182021Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Indonesia/ID_20180706_20210706_G_bidy.csv")
names(ID20182021NGbidy) <- c("date", "NGBidY", "start", "end")
ID20182021NGbidy <- ID20182021NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(ID20182021Gbidy) <- c("date", "GBidY", "start", "end")
ID20182021Gbidy <- ID20182021Gbidy |> 
  dplyr::select(date, end, GBidY)
ID20182021bidy <- spread(ID20182021NGbidy, ID20182021Gbidy)
ID20182021bidyMA <- moving_avg(ID20182021bidy)
ID20182021bidyMA <- ID20182021bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

CN20232023NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20230831_20231209_NG_bidy.csv")
CN20232023Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20230831_20231209_G_bidy.csv")
names(CN20232023NGbidy) <- c("date", "NGBidY", "start", "end")
CN20232023NGbidy <- CN20232023NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(CN20232023Gbidy) <- c("date", "GBidY", "start", "end")
CN20232023Gbidy <- CN20232023Gbidy |> 
  dplyr::select(date, end, GBidY)
CN20232023bidy <- spread(CN20232023NGbidy, CN20232023Gbidy)
CN20232023bidyMA <- moving_avg(CN20232023bidy)
CN20232023bidyMA <- CN20232023bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

CN20222025NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20221213_20251213_NG_bidy.csv")
CN20222025Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20221213_20251213_G_bidy.csv")
names(CN20222025NGbidy) <- c("date", "NGBidY", "start", "end")
CN20222025NGbidy <- CN20222025NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(CN20222025Gbidy) <- c("date", "GBidY", "start", "end")
CN20222025Gbidy <- CN20222025Gbidy |> 
  dplyr::select(date, end, GBidY)
CN20222025bidy <- spread(CN20222025NGbidy, CN20222025Gbidy)
CN20222025bidyMA <- moving_avg(CN20222025bidy)
CN20222025bidyMA <- CN20222025bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

CN20212021NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20210610_20210908_NG_bidy.csv")
CN20212021Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20210610_20210908_G_bidy.csv")
names(CN20212021NGbidy) <- c("date", "NGBidY", "start", "end")
CN20212021NGbidy <- CN20212021NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(CN20212021Gbidy) <- c("date", "GBidY", "start", "end")
CN20212021Gbidy <- CN20212021Gbidy |> 
  dplyr::select(date, end, GBidY)
CN20212021bidy <- spread(CN20212021NGbidy, CN20212021Gbidy)
CN20212021bidyMA <- moving_avg(CN20212021bidy)
CN20212021bidyMA <- CN20212021bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)

HK20202021NGbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Hong Kong/HK_20200304_20210304_NG_bidy.csv")
HK20202021Gbidy <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Hong Kong/HK_20200304_20210304_G_bidy.csv")
names(HK20202021NGbidy) <- c("date", "NGBidY", "start", "end")
HK20202021NGbidy <- HK20202021NGbidy |> 
  dplyr::select(date, end, NGBidY)
names(HK20202021Gbidy) <- c("date", "GBidY", "start", "end")
HK20202021Gbidy <- HK20202021Gbidy |> 
  dplyr::select(date, end, GBidY)
HK20202021bidy <- spread(HK20202021NGbidy, HK20202021Gbidy)
HK20202021bidyMA <- moving_avg(HK20202021bidy)
HK20202021bidyMA <- HK20202021bidyMA |> 
  mutate(pspreadMA = spreadMA / NGBidYMA)


ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = MY20172032bidyMA) +
  geom_line(aes(x = date, y = spreadMA), color = "red", size = 1, data = MY20192032bidyMA) +
  labs(x = "Date",
       y = "Yield Spread",
       color = "Securities",
       title = "Yield Spread between Green and Non-Green Bonds in Malaysia") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
  
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread Malaysia.jpeg")

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = TW20142024bidyMA) +
  geom_line(aes(x = date, y = spreadMA), color = "red", size = 1, data = TW20202025bidyMA) +
  labs(title = "Yield Spread between Green and Non-Green Bonds in Taiwan",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread Taiwan.jpeg")

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = ID20182021bidyMA) +
  labs(title = "Yield Spread between Green and Non-Green Bonds in Indonesia",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread Indonesia.jpeg")

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = CN20232023bidyMA) +
  labs(title = "Yield Spread between Green and Non-Green Bonds in China",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread China(1).jpeg")

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = CN20222025bidyMA) +
  labs(title = "Yield Spread between Green and Non-Green Bonds in China",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread China(2).jpeg")

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = CN20212021bidyMA) +
  labs(title = "Yield Spread between Green and Non-Green Bonds in China",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread China(3).jpeg")

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = HK20202021bidyMA) +
  labs(title = "Yield Spread between Green and Non-Green Bonds in Hong Kong",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
ggsave("/Users/ziyuhe/Documents/GitHub/greenbond/figures/Yield Spread Hong Kong.jpeg")

MY20192032NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20321227_NG_bidp.csv")
MY20192032Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20321227_G_bidp.csv")
names(MY20192032NGbidp) <- c("date", "NGBidP", "start", "end")
MY20192032NGbidp <- MY20192032NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(MY20192032Gbidp) <- c("date", "GBidP", "start", "end")
MY20192032Gbidp <- MY20192032Gbidp |> 
  dplyr::select(date, end, GBidP)
MY20192032bidp <- spread(MY20192032NGbidp, MY20192032Gbidp)

MY20172032NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20171229_20321229_NG_bidp.csv")
MY20172032Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20171229_20321229_G_bidp.csv")
names(MY20172032NGbidp) <- c("date", "NGBidP", "start", "end")
MY20172032NGbidp <- MY20172032NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(MY20172032Gbidp) <- c("date", "GBidP", "start", "end")
MY20172032Gbidp <- MY20172032Gbidp |> 
  dplyr::select(date, end, GBidP)
MY20172032bidp <- spread(MY20172032NGbidp, MY20172032Gbidp)

TW20142024NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20141215_20241215_NG_bidp.csv")
TW20172024Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20171215_20241215_G_bidp.csv")
names(TW20142024NGbidp) <- c("date", "NGBidP", "start", "end")
TW20142024NGbidp <- TW20142024NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(TW20172024Gbidp) <- c("date", "GBidP", "start", "end")
TW20172024Gbidp <- TW20172024Gbidp |> 
  dplyr::select(date, end, GBidP)
TW20142024bidp <- spread(TW20142024NGbidp, TW20172024Gbidp)

TW20202025NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20201215_20251215_NG_bidp.csv")
TW20222025Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Taiwan/TW_20221215_20251215_G_bidp.csv")
names(TW20202025NGbidp) <- c("date", "NGBidP", "start", "end")
TW20202025NGbidp <- TW20202025NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(TW20222025Gbidp) <- c("date", "GBidP", "start", "end")
TW20222025Gbidp <- TW20222025Gbidp |> 
  dplyr::select(date, end, GBidP)
TW20202025bidp <- spread(TW20202025NGbidp, TW20222025Gbidp)

ID20182021NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Indonesia/ID_20180706_20210706_NG_bidp.csv")
ID20182021Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Indonesia/ID_20180706_20210706_G_bidp.csv")
names(ID20182021NGbidp) <- c("date", "NGBidP", "start", "end")
ID20182021NGbidp <- ID20182021NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(ID20182021Gbidp) <- c("date", "GBidP", "start", "end")
ID20182021Gbidp <- ID20182021Gbidp |> 
  dplyr::select(date, end, GBidP)
ID20182021bidp <- spread(ID20182021NGbidp, ID20182021Gbidp)

CN20232023NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20230831_20231209_NG_bidp.csv")
CN20232023Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20230831_20231209_G_bidp.csv")
names(CN20232023NGbidp) <- c("date", "NGBidP", "start", "end")
CN20232023NGbidp <- CN20232023NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(CN20232023Gbidp) <- c("date", "GBidP", "start", "end")
CN20232023Gbidp <- CN20232023Gbidp |> 
  dplyr::select(date, end, GBidP)
CN20232023bidp <- spread(CN20232023NGbidp, CN20232023Gbidp)

CN20222025NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20221213_20251213_NG_bidp.csv")
CN20222025Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20221213_20251213_G_bidp.csv")
names(CN20222025NGbidp) <- c("date", "NGBidP", "start", "end")
CN20222025NGbidp <- CN20222025NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(CN20222025Gbidp) <- c("date", "GBidP", "start", "end")
CN20222025Gbidp <- CN20222025Gbidp |> 
  dplyr::select(date, end, GBidP)
CN20222025bidp <- spread(CN20222025NGbidp, CN20222025Gbidp)

CN20212021NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20210610_20210908_NG_bidp.csv")
CN20212021Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/China/CN_20210610_20210908_G_bidp.csv")
names(CN20212021NGbidp) <- c("date", "NGBidP", "start", "end")
CN20212021NGbidp <- CN20212021NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(CN20212021Gbidp) <- c("date", "GBidP", "start", "end")
CN20212021Gbidp <- CN20212021Gbidp |> 
  dplyr::select(date, end, GBidP)
CN20212021bidp <- spread(CN20212021NGbidp, CN20212021Gbidp)

HK20202021NGbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Hong Kong/HK_20200304_20210304_NG_bidp.csv")
HK20202021Gbidp <- read_csv("Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Hong Kong/HK_20200304_20210304_G_bidp.csv")
names(HK20202021NGbidp) <- c("date", "NGBidP", "start", "end")
HK20202021NGbidp <- HK20202021NGbidp |> 
  dplyr::select(date, end, NGBidP)
names(HK20202021Gbidp) <- c("date", "GBidP", "start", "end")
HK20202021Gbidp <- HK20202021Gbidp |> 
  dplyr::select(date, end, GBidP)
HK20202021bidp <- spread(HK20202021NGbidp, HK20202021Gbidp)

ggplot(MY20172032bidp) +
  geom_line(aes(x = date, y = NGBidP, color = "Non-Green Bond"), size = 1) +
  geom_line(aes(x = date, y = GBidP, color = "Green Bond"), size = 1) +
  scale_color_manual(values = c("green", "black")) +
  labs(x = "Date",
       y = "Yield Spread",
       color = "Securities",
       title = "Yield Spread between Green and Non-Green Bonds in Malaysia") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))


ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = TW20142024bidyMA) +
  geom_line(aes(x = date, y = spreadMA), color = "red", size = 1, data = TW20202025bidyMA) +
  labs(title = "Yield Spread",
       x = "Date",
       y = "Yield Spread between Green and Non-Green Bonds in Taiwan",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = ID20182021bidyMA) +
  labs(title = "Yield Spread",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = CN20232023bidyMA) +
  labs(title = "Yield Spread",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = CN20222025bidyMA) +
  labs(title = "Yield Spread",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = CN20212021bidyMA) +
  labs(title = "Yield Spread",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

ggplot() +
  geom_line(aes(x = date, y = spreadMA), color = "blue", size = 1, data = HK20202021bidyMA) +
  labs(title = "Yield Spread",
       x = "Date",
       y = "Yield Spread",
       color = "Securities") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

companylist <- list(
  Indonesia = c('ID_20180706_20210706'),
  Malaysia = c('MY_20191227_20291227', 'MY_20191227_20301227', 'MY_20191227_20311226', 'MY_20191227_20321227', 'MY_20171229_20321229'),
  Taiwan = c('TW_20171215_20241215', 'TW_20201115_20251115', 'TW_20221215_20251215', 'TW_20221215_20271215')
)

colorlst = c('red', 'green', 'blue', 'black', 'darkred')

plot_zspread <- function(country, name, ax = ggplot, color = 'blue') {
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
    mutate(greenium = GZ-NGZ)

  median_greenium <- median(bondz$greenium, na.rm = TRUE)
  
  ax <- ax + 
    geom_histogram(aes(x = greenium), fill = color, data = bondz, alpha = 0.3) +
    #geom_freqpoly(aes(x = greenium, y = ..count../sum(..count..)*100), color = color, data = bondz, alpha = 0.5) +
    geom_density(aes(x = greenium), color = color, data = bondz, alpha = 0.5, adjust = 1, size = 1.2) +
    geom_vline(xintercept = median_greenium, color = color, linetype = "dashed", size = 1, lty = 2) +
    labs(x = 'Greenium (bps)', y = 'Percentage')
  return(ax)
}

plots <- list()

for (country in names(companylist)){
  companies <- companylist[[country]]
  zspread <- ggplot()
  for (i in 1:length(companies)){
    zspread <- plot_zspread(country, companies[i], zspread, colorlst[i])
  }
  plots[[country]] <- zspread + theme_classic() + theme(text = element_text(family = 'serif'))
}

for (plot in plots) {
  print(plot)
}

#Indonesia
ID20182021NGz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Indonesia/ID_20180706_20210706_NG_z.csv")
ID20182021Gz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Indonesia/ID_20180706_20210706_G_z.csv")
names(ID20182021NGz) <- c("date", "NGZ")
names(ID20182021Gz) <- c("date", "GZ")
ID20182021NGz$date = as.Date(ID20182021NGz$date, format = '%d-%b-%Y')
ID20182021Gz$date = as.Date(ID20182021Gz$date, format = '%d-%b-%Y')
ID20182021z <- ID20182021NGz |> 
  inner_join(ID20182021Gz, by = 'date') |> 
  mutate(greenium = GZ-NGZ)
  
ggplot(ID20182021z) +
  geom_line(aes(x = date, y = greenium))
mean(ID20182021z$greenium)
sd(ID20182021z$greenium)

plot_zspread('Malaysia', 'MY_20191227_20291227')

#Malaysia
MY20172032NGz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20171229_20321229_NG_z.csv")
MY20172032Gz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20171229_20321229_G_z.csv")
names(MY20172032NGz) <- c("date", "NGZ")
names(MY20172032Gz) <- c("date", "GZ")
MY20172032NGz$date = as.Date(MY20172032NGz$date, format = '%d-%b-%y')
MY20172032Gz$date = as.Date(MY20172032Gz$date, format = '%d-%b-%Y')
MY20172032z <- MY20172032NGz |> 
  inner_join(MY20172032Gz, by = 'date') |> 
  mutate(greenium = GZ-NGZ)

ggplot(MY20172032z) +
  geom_histogram(aes(x = greenium))
mean(MY20172032z$greenium)
sd(MY20172032z$greenium)

MY20192029NGz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20291227_NG_z.csv")
MY20192029Gz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20291227_G_z.csv")
names(MY20192029NGz) <- c("date", "NGZ")
names(MY20192029Gz) <- c("date", "GZ")
MY20192029NGz$date = as.Date(MY20192029NGz$date, format = '%d-%b-%Y')
MY20192029Gz$date = as.Date(MY20192029Gz$date, format = '%d-%b-%Y')
MY20192029z <- MY20192029NGz |> 
  inner_join(MY20192029Gz, by = 'date') |> 
  mutate(greenium = GZ-NGZ)

ggplot(MY20192029z) +
  geom_histogram(aes(x = greenium))
mean(MY20192029z$greenium)
sd(MY20192029z$greenium)

MY20192030NGz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20301227_NG_z.csv")
MY20192030Gz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20301227_G_z.csv")
names(MY20192030NGz) <- c("date", "NGZ")
names(MY20192030Gz) <- c("date", "GZ")
MY20192030NGz$date = as.Date(MY20192030NGz$date, format = '%d-%b-%Y')
MY20192030Gz$date = as.Date(MY20192030Gz$date, format = '%d-%b-%Y')
MY20192030z <- MY20192030NGz |> 
  inner_join(MY20192030Gz, by = 'date') |> 
  mutate(greenium = GZ-NGZ)

ggplot(MY20192030z) +
  geom_histogram(aes(x = greenium))
mean(MY20192030z$greenium)
sd(MY20192030z$greenium)

MY20192031NGz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20311226_NG_z.csv")
MY20192031Gz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20311226_G_z.csv")
names(MY20192031NGz) <- c("date", "NGZ")
names(MY20192031Gz) <- c("date", "GZ")
MY20192031NGz$date = as.Date(MY20192031NGz$date, format = '%d-%b-%Y')
MY20192031Gz$date = as.Date(MY20192031Gz$date, format = '%d-%b-%Y')
MY20192031z <- MY20192031NGz |> 
  inner_join(MY20192031Gz, by = 'date') |> 
  mutate(greenium = GZ-NGZ)

ggplot(MY20192031z) +
  geom_histogram(aes(x = greenium))
mean(MY20192031z$greenium)
sd(MY20192031z$greenium)

MY20192030NGz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20301227_NG_z.csv")
MY20192030Gz <- read_csv("/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/Malaysia/MY_20191227_20301227_G_z.csv")
names(MY20192030NGz) <- c("date", "NGZ")
names(MY20192030Gz) <- c("date", "GZ")
MY20192030NGz$date = as.Date(MY20192030NGz$date, format = '%d-%b-%Y')
MY20192030Gz$date = as.Date(MY20192030Gz$date, format = '%d-%b-%Y')
MY20192030z <- MY20192030NGz |> 
  inner_join(MY20192030Gz, by = 'date') |> 
  mutate(greenium = GZ-NGZ)

ggplot(MY20192030z) +
  geom_histogram(aes(x = greenium))
mean(MY20192030z$greenium)
sd(MY20192030z$greenium)
