
rm(list = ls())
gc()

library(ggplot2)
library(ggsci)
library(cowplot)
library(clipr)



# ------model fit--------
setwd("D:\\Downloads\\MSc Thesis\\hivst\\Model results")

fit <- readRDS("hivst_stan_fit_apr17.rds")



#---pop at the beginning of the year (as WPP reports mid year pop)----
countries <- c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
               "Sierra Leone", "Zambia", "Mali", "Uganda",
               "Lesotho", "Mozambique", "Rwanda",
               "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
               "Guinea", "Liberia", "Senegal", "South Africa", 
               "United Republic of Tanzania", "Namibia", "Botswana", 
               "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini", "Benin")

# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt) + 1
niter <- (end - start) / dt
n_yr <- end - start

# mapping hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}

yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind)


# survey and program data list 
cnt_data <- list(
  kenya = list(
    yr_svy = c(2012.5, 2018.5, 2022.5),
    ind_svy = (c(2012.5, 2018.5, 2022.5) - start) / dt, 
    den_svy_f = matrix(
      c(1900, 1983, 1897, 817, # 2012
        2491, 2554, 4338, 1145, # 2018 
        4980, 5037, 4523, -999), # 2022
      nrow = 3, byrow = TRUE),
    num_svy_f = matrix(
      c(29, 45, 33, 14, # 2012
        89, 111, 102, 12, # 2018 
        203, 328, 195, -999), # 2022
      nrow = 3, byrow = TRUE),
    den_svy_m = matrix(
      c(253, 888, 611, 390, # 2012 
        2245, 1187, 3448, 622,# 2018
        2691, 1181, 2334, 556), # 2022 
      nrow = 3, byrow = TRUE),
    num_svy_m = matrix(
      c(10, 31, 18, 7, # 2012
        54, 54, 106, 14, # 2018 
        108, 181, 227, 36), # 2022
      nrow = 3, byrow = TRUE),
    yr_hts = c(2018,  2019,   2020,    2021,   2022,  2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
    se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
  ),
  
  ghana = list(
    yr_svy = c(2017.5, 2022.5),
    ind_svy = (c(2017.5, 2022.5) - start) / dt,
    den_svy_f = matrix(
      c(3303, 1908, 2386, -999, # 2017
        3133, 2389, 2509, -999), # 2022
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(52, 72, 47, -999, # 2017
        42, 97, 51, -999), # 2022
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(1927, 645, 856, -999, # 2017 
        1604, 1547, 1222, 617), # 2022 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(9, 21, 13, -999, # 2017
        9, 48, 29, 7), # 2022
      nrow = 2, byrow = TRUE),
    yr_hts = c(2020,  2021,   2022,  2023) + 0.5,
    ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(20000, 1323, 235000, 140500),
    se_hts = c(20000, 1323, 235000, 140500) * 0.1
  ),
  malawi = list(
    yr_svy = c(2015.5, 2019.5, 2020.5),
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
    den_svy_f = matrix(
      c( 6698, 4260, 4117, -999, # 2015
         8372, 5690, 5570, -999, # 2019
         3544, 2504, 3145, 1530), # 2020
      nrow = 3, byrow = TRUE),
    num_svy_f = matrix(
      c(50, 48, 39, -999, # 2015
        540, 412, 273, -999, # 2019
        273, 193, 169, 24), # 2020
      nrow = 3, byrow = TRUE),
    den_svy_m = matrix(
      c(858, 1041, 1061, 148, # 2015 DHS
        2546, 1368, 1519, -999,# 2019
        2217, 2199, 2377, 1440), # 2020 
      nrow = 3, byrow = TRUE),
    num_svy_m = matrix(
      c(6, 17, 12, 2, # 2015 DHS
        230, 131, 109, -999, # 2019 
        190, 219, 163, 49), # 2020
      nrow = 3, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(408900, 101256, 561282, 602657, 735385, 910088),
    se_hts =  c(408900, 101256, 561282, 602657, 735385, 910088) * 0.1 
  ),
  madagascar = list(
    yr_svy = c(2018.5, 2021.5),
    ind_svy = (c(2018.5, 2021.5) - start) / dt,
    den_svy_f = matrix(
      c(3796, 1712, 2117, -999, # 2018
        4793, 1859, 4414, -999), # 2021
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c( 41, 48, 30, -999, # 2018
         10, 9, 10, -999), # 2021
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(1188, 888, 1711, -999, # 2018 
        2971, 2607, 1610, 749), # 2021 DHS
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(9, 17, 18, -999, # 2018
        13, 18, 17, 7), # 2021 DHS
      nrow = 2, byrow = TRUE),
    yr_hts = c(2022,  2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2500, 2500),
    se_hts = c(2500, 2500) * 0.1
  ),
  
  zimbabwe = list(
    yr_svy = c(2015.5, 2019.5, 2020.5),
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
    den_svy_f = matrix(
      c(4078, 2387, 1661, -999, # 2015
        2332, 1954, 1978, -999, # 2019
        2759, 2343, 3081, 1876), # 2020
      nrow = 3, byrow = TRUE),
    num_svy_f = matrix(
      c(8, 11, 3, -999, # 2015
        116, 137, 95, -999, # 2019
        166, 213, 177, 42), # 2020
      nrow = 3, byrow = TRUE),
    den_svy_m = matrix(
      c(2254, 1950, 1453, 333, # 2015 dhs
        998, 611, 870, -999,# 2019
        2124, 1340, 1960, 1137), # 2020 
      nrow = 3, byrow = TRUE),
    num_svy_m = matrix(
      c(22, 34, 35, 6, # 2015 dhs
        39, 46, 47, -999, # 2019 
        112, 117, 122, 31), # 2020
      nrow = 3, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(197408, 174566, 240434, 459517, 414499, 513090),
    se_hts = c(197408, 174566, 240434, 459517, 414499, 513090) * 0.1
  ),
  
  sierraleone = list(
    yr_svy = c(2017.5, 2019.5),
    ind_svy = (c(2017.5, 2019.5) - start) / dt,
    den_svy_f = matrix(
      c(5854, 4421, 4019, -999, # 2017
        2203, 1346, 2236, -999), # 2019
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(167, 170, 125, -999, # 2017
        70, 68, 80, -999), # 2019
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(5854, 4421, 4019, -999, # 2017 
        1609, 541, 1157, 773), # 2019 DHS
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(167, 170, 125, -999, # 2017
        22, 17, 25, 15), # 2019 DHS
      nrow = 2, byrow = TRUE),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2678, 1173, 50340),
    se_hts = c(2678, 1173, 50340) * 0.1
  ),
  zambia = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy_f = matrix(
      c(1845, 1419, 1394, -999), # 2018
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(47, 51, 36, -999), # 2018
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1555, 1531, 1820, 579), # 2018 dhs
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(38, 52, 53, 9), # 2018 dhs
      nrow = 1, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(315348, 781175, 639225, 23750, 33153, 95559),
    se_hts = c(315348, 781175, 639225, 23750, 33153, 95559) * 0.1
  ),
  mali = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy_f = matrix(
      c(2046, 1434, 1526, -999), # 2018
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(17, 15, 20, -999), # 2018
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1361, 1189, 943, 484), # 2018 dhs
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(2, 4, 4, 5), # 2018 dhs
      nrow = 1, byrow = TRUE),
    yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7763, 169962, 11375, 235729),
    se_hts = c(7763, 169962, 11375, 235729) * 0.1
  ),
  uganda = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy_f = matrix(
      c(6404, 2748, 3072, -999), # 2016
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(223, 205, 97, -999), # 2016
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1639, 1009, 1094, 257), # 2016 dhs
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(58, 95, 59, 8), # 2016 dhs
      nrow = 1, byrow = TRUE),
    yr_hts = c(2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(42570, 306421, 750698, 681602),
    se_hts = c(42570, 306421, 750698, 681602) * 0.1
  ),
  lesotho  = list(
    yr_svy =  c(2020.5, 2023.5),
    ind_svy = (c(2020.5, 2023.5) - start) / dt,
    den_svy_f = matrix(
      c(2024, 1786, 2170, 1824, # 2020
        1498, 889, 1464, -999), # 2023
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(226, 224, 134, 34, # 2020
        852, 538, 482, -999), # 2023
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(1449, 1254, 1643, 1014, # 2020
        388, 478, 490, 214), # 2023 dhs
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(146, 182, 126, 24, # 2020
        132, 241, 145, 33), # 2023 dhs
      nrow = 2, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(58917, 42650, 164236, 281277, 301762, 262915),
    se_hts = c(58917, 42650, 164236, 281277, 301762, 262915) * 0.1
  ),
  mozambique = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy_f = matrix(
      c(2372, 1943, 2263, 1156), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(227, 259, 215, 43), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1828, 1294, 1709, 899),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(110, 149, 170, 56), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(67883, 203966, 683345),
    se_hts = c(67883, 203966, 683345) * 0.1
  ),
  rwanda = list(
    yr_svy =  2019.5,
    ind_svy = (2019.5 - start) / dt,
    den_svy_f = matrix(
      c(4995, 3401, 4987, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(57, 70, 43, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1809, 1046, 1173, 715),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(14, 39, 17, 3), 
      nrow = 1, byrow = TRUE),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 62683,
    se_hts = 62683 * 0.1
  ),
  burkinafaso = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy_f = matrix(
      c(3870, 5248, 3926, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(7, 17, 10, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(2336, 1026, 1451, 622),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(4, 9, 9, 2), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2022, 2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(968, 3367),
    se_hts = c(968, 3367) * 0.1
  ),
  burundi = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy_f = matrix(
      c(3182, 4642, 1929, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(7, 16, 3, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(2756, 1372, 2230, 3009),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 10, 10, 1), 
      nrow = 1, byrow = TRUE),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 78013,
    se_hts = 78013 * 0.1
  ),
  cameroon = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy_f = matrix(
      c(3721, 2750, 2138, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(48, 88, 50, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1381, 1151, 1008, 665),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(27, 45, 52, 22), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2021, 2022) + 0.5,
    ind_hts = (c(2021,2022) - start + 0.5) / dt,
    hts_dat = c(15000, 33073),
    se_hts = c(15000,33073) * 0.1
  ),
  cotedivoire = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy_f = matrix(
      c(3313, 2067, 1231, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(17, 31, 16, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(872, 815, 853, 597),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 12, 16, 3), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(1159, 111184, 117556, 41774, 60154),
    se_hts = c(1159, 111184, 117556, 41774, 60154) * 0.1
  ),
  guinea = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy_f = matrix(
      c(2279, 1720, 2194, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(14, 20, 15, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1006, 904, 959, 632),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(3, 6, 5, 1), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2019, 2022) + 0.5,
    ind_hts = (c(2019, 2022) - start + 0.5) / dt,
    hts_dat = c(12, 152),
    se_hts = c(12, 152) * 0.1
  ),
  liberia = list(
    yr_svy =  2019.5,
    ind_svy = (2019.5 - start) / dt,
    den_svy_f = matrix(
      c(2313, 829, 1095, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(18, 18, 19, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(952, 444, 849, 284),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 9, 10, 7), 
      nrow = 1, byrow = TRUE),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 12129,
    se_hts = 12129 * 0.1
  ),
  senegal = list(
    yr_svy =  2017.5,
    ind_svy = (2017.5 - start) / dt,
    den_svy_f = matrix(
      c(4458, 3271, 5904, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(4, 10, 10, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(3384, 655, 3226, 330),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(1, 1, 2, 3), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7307, 18860, 5505, 4056, 11932),
    se_hts = c(7307, 18860, 5505, 4056, 11932) * 0.1
  ),
  southafrica = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy_f = matrix(
      c(1734, 1637, 1470, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(39, 62, 42, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(264, 526, 469, 307),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(6, 17, 17, 2), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2018, 2019, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(1200000, 794034, 913418, 212000),
    se_hts = c(1200000, 794034, 913418, 212000) * 0.1
  ),
  tanzania = list(
    yr_svy =  2022.5,
    ind_svy = (2022.5 - start) / dt,
    den_svy_f = matrix(
      c(2937, 2173, 2523, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(68, 103, 71, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1439, 916, 1096, -999),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(28, 61, 66, -999), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(25810, 14940, 19000, 38717, 809603, 1447029),
    se_hts = c(25810, 14940, 19000, 38717, 809603, 1447029) * 0.1
  ),
  
  namibia = list(
    yr_svy =  2017.5,
    ind_svy = (2017.5 - start) / dt,
    den_svy_f = matrix(
      c(1851, 2159, 2696, 650), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(66, 105, 70, 6), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1050, 1342, 1782, 398),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(15, 59, 50, 4), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(3910, 40075, 47258, 130000, 27974),
    se_hts = c(3910, 40075, 47258, 130000, 27974) * 0.1
  ),
  
  botswana = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy_f = matrix(
      c(1958, 2002, 3199, 811), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(50, 71, 46, 6), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(1559, 1401, 2348, 475),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(22, 30, 50, 3), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7000, 3848, 8403, 16405),
    se_hts = c(7000, 3848, 8403, 16405) * 0.1
  ),
  
  guineabissau = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy_f = matrix(
      c(2347, 1097, 1339, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(38, 44, 28, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(279, 233, 210, -999),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(7, 11, 11, -999), 
      nrow = 1, byrow = TRUE),
    yr_hts = 2020 + 0.5,
    ind_hts = (2020 - start + 0.5) / dt,
    hts_dat = 37500,
    se_hts = 37500 * 0.1
  ),
  
  drc = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy_f = matrix(
      c(1733, 1626, 1404, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(22, 37, 24, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(244, 419, 259, -999),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 16, 11, -999), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2018, 2020, 2021) + 0.5,
    ind_hts = (c(2018, 2020, 2021) - start + 0.5) / dt,
    hts_dat = c(500, 7158, 7480),
    se_hts = c(500, 7158, 7480) * 0.1
  ),
  eswatini = list(
    yr_svy = c(2021.5, 2022.5),
    ind_svy = (c(2021.5, 2022.5) - start) / dt,
    den_svy_f = matrix(
      c(642, 484, 479, -999, # 2021
        1552, 1348, 1612, 1076), # 2022 phia
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(194, 178, 101, -999, #2021
        470, 360, 213, 54), # 2022 pia
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(601, 377, 349, -999, # 2021 MICS
        1425, 924, 1110, 587),  # 2022 PHIA
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(118, 122, 85, -999, # 2021
        271, 283, 177, 22), # 2022 phia
      nrow = 2, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022) - start + 0.5) / dt,
    hts_dat = c(33159, 32531, 191990, 78570, 111912),
    se_hts = c(33159, 32531, 191990, 78570, 111912) * 0.1
  ),
  
  benin = list(
    yr_svy =  2017.5,
    ind_svy = (2017.5 - start) / dt,
    den_svy_f = matrix(
      c(4180, 5512, 3754, -999), 
      nrow = 1, byrow = TRUE),
    num_svy_f = matrix(
      c(20, 34, 20, -999), 
      nrow = 1, byrow = TRUE),
    den_svy_m = matrix(
      c(3054, 1961, 1868, 1079),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(3, 17, 15, 3), 
      nrow = 1, byrow = TRUE),
    yr_hts = c(2022, 2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(5173, 8149),
    se_hts = c(5173, 8149) * 0.1
  )
)

# adding svy_dat,lci,uci after list as the operations cant be performed inside list
cnt_data <- lapply(cnt_data, function(x) {
  x$svy_dat_f <- x$num_svy_f / x$den_svy_f
  x$lci_svy_f <- x$svy_dat_f - qnorm(0.975) * sqrt(x$svy_dat_f * (1 - x$svy_dat_f) / x$den_svy_f)
  x$uci_svy_f <- x$svy_dat_f + qnorm(0.975) * sqrt(x$svy_dat_f * (1 - x$svy_dat_f) / x$den_svy_f)
  return(x)
})

cnt_data <- lapply(cnt_data, function(x) {
  x$svy_dat_m <- x$num_svy_m / x$den_svy_m
  x$lci_svy_m <- x$svy_dat_m - qnorm(0.975) * sqrt(x$svy_dat_m * (1 - x$svy_dat_m) / x$den_svy_m)
  x$uci_svy_m <- x$svy_dat_m + qnorm(0.975) * sqrt(x$svy_dat_m * (1 - x$svy_dat_m) / x$den_svy_m)
  return(x)
})

# combining svy and pgm data for all countries
# survey by age (combining gender)
num_svy1 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 1], x$num_svy_f[, 1]))) #rows=countries, cols=sex
den_svy1 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 1], x$den_svy_f[, 1])))
num_svy2 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 2], x$num_svy_f[, 2])))
den_svy2 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 2], x$den_svy_f[, 2])))
num_svy3 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 3], x$num_svy_f[, 3])))
den_svy3 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 3], x$den_svy_f[, 3])))
num_svy4 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 4], x$num_svy_f[, 4])))
den_svy4 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 4], x$den_svy_f[, 4])))

# to check that every missing value (-999) is both in female or male
# if (any(!apply(rbind(num_svy4, den_svy4), 1, function(row) all(row > 0) || all(row < 0)))) { 
#   print('stop, missing 50+ age inconsistent by sex') }



# the survey
ind_svy <- unlist(lapply(cnt_data, function(x) x$ind_svy))
hts_dat <- unlist(lapply(cnt_data, function(x) x$hts_dat)) #rows=countries, cols=time points
se_hts <- unlist(lapply(cnt_data, function(x) x$se_hts))
ind_hts <- unlist(lapply(cnt_data, function(x) x$ind_hts))

# survey and hts indices
n_cnt <- length(cnt_data)
n_svy_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$yr_svy))) 
n_hts_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$ind_hts)))

# hts
hts_idx_s <- NULL
hts_idx_e <- NULL
hts_idx_s[1] <- 1  
hts_idx_e[1] <- n_hts_by_cnt[1]

# survey
# cnt_no_age50 <- unlist(lapply(cnt_data, function(x) ifelse(all(x$num_svy_m[, 4] == -999), 0, 1)))
# age_grp_na <- unlist(lapply(cnt_data, function(x) ifelse(x$num_svy_m[, 4] == -999, 0, 1)))

svy_idx_s <- NULL
svy_idx_e <- NULL
svy_idx_s[1] <- 1
svy_idx_e[1] <- n_svy_by_cnt[1]

# remaining countries
for (c in 2:n_cnt) {
  # survey
  svy_idx_s[c] <- svy_idx_s[c - 1] + n_svy_by_cnt[c - 1]
  svy_idx_e[c] <- svy_idx_s[c] + n_svy_by_cnt[c] - 1   
  # hts
  hts_idx_s[c] <- hts_idx_s[c - 1] + n_hts_by_cnt[c - 1]
  hts_idx_e[c] <- hts_idx_s[c] + n_hts_by_cnt[c] - 1
}

idx_pop <- seq(from = 1, to = (n_cnt * 4), by = 4)


#-----------------plots from fit--------------------

# testing rate
pars_beta_t <- matrix(paste0("beta_t[", rep(1:data_stan$n_cnt, each = data_stan$n_yr), ",", rep(1:data_stan$n_yr, data_stan$n_cnt), "]"), 
                      nrow = data_stan$n_cnt, ncol = data_stan$n_yr, byrow = TRUE)
r <- as.data.frame(rstan::summary(fit, pars = pars_beta_t, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
r$`50%`
max(r$`50%`)
max(r$`97.5%`)

# ------ retesting rate ratio --------
rr_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_retest_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`50%`)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`2.5%`)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`97.5%`)

rr <- as.data.frame(rstan::summary(fit, pars = c("beta_retest"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr$`50%`
rr$`2.5%`
rr$`97.5%`

# dataframe for forest plot of RR retesting
df_rr_rt <- data.frame(country = names(cnt_data),
                       median = rr$`50%`,
                       lci = rr$`2.5%`,
                       uci = rr$`97.5%`)
df_rr_ov <- rbind(df_rr_rt,
                  data.frame( country = "overall",
                              median = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`50%`),
                              lci = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`2.5%`),
                              uci = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`97.5%`)))

# Renaming selected countries
df_rr_ov$country <- as.character(df_rr_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_ov$country[df_rr_ov$country == old_name] <- rename_map[[old_name]]
}

cap_first_letter <- function(x) {
  if (x == "overall") return("Overall")   
  if (nchar(x) == 0)   return(x)      
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
df_rr_ov$country <- sapply(df_rr_ov$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_ov$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))
df_rr_ov$country     <- factor(df_rr_ov$country, levels = new_levels)
df_rr_ov$style <- ifelse(df_rr_ov$country == "Overall", "pooled", "individual")

rr_retesting_forest <- ggplot(df_rr_ov, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "green3", "pooled" = "midnightblue")) +
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Re-testing rate ratio", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  theme(legend.position = "right")
rr_retesting_forest

#write_clip(df_rr_ov, object_type = "table")


#ggsave("rr_retest_plot.png", plot = rr_retesting_forest, width = 8, height = 6, dpi = 300)

# ------- phi -----------------
phi_overall <- as.data.frame(rstan::summary(fit, pars = c("phi_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (1 - 0.5) * plogis(phi_overall$`50%`)
0.5 + (1 - 0.5) * plogis(phi_overall$`2.5%`)
0.5 + (1 - 0.5) * plogis(phi_overall$`97.5%`)
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# df and forest plot for phi
df_phi <- data.frame(country = names(cnt_data),
                     median = phi$`50%`,
                     lci = phi$`2.5%`,
                     uci = phi$`97.5%`)
df_phi_ov <- rbind(df_phi,
                   data.frame( country = "overall",
                               median = 0.5 + (1 - 0.5) * plogis(phi_overall$`50%`),
                               lci = 0.5 + (1 - 0.5) * plogis(phi_overall$`2.5%`),
                               uci = 0.5 + (1 - 0.5) * plogis(phi_overall$`97.5%`)))

# Renaming selected countries
df_phi_ov$country <- as.character(df_phi_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_phi_ov$country[df_phi_ov$country == old_name] <- rename_map[[old_name]]
}

df_phi_ov$country <- sapply(df_phi_ov$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_phi_ov$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_phi_ov$country     <- factor(df_phi_ov$country, levels = new_levels)
df_phi_ov$style <- ifelse(df_phi_ov$country == "Overall", "pooled", "individual")


phi_forest <- ggplot(df_phi_ov, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "palevioletred2", "pooled" = "tomato4")) +  # Colors
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Proportion of distributed HIVST kits that are used", color = "Estimates (95% CrI)") +
  #geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2)))
phi_forest

#write_clip(df_phi_ov, object_type = "table")

#ggsave("phi_plot.png", plot = phi_forest, width = 8, height = 6, dpi = 300)


#---rr retest and phi side by side panel-----
rrretest_phi <- plot_grid(rr_retesting_forest, phi_forest, ncol = 2)
#ggsave("rrretest_phi_plot.png", plot = rrretest_phi, width = 16, height = 6, dpi = 300)

# library(patchwork)
# 
# rrretest_phi <- rr_retesting_forest +
#   phi_forest +
#   plot_annotation(tag_levels = "A")
# 
# ggsave("rrretest_phi_plot.png",
#        plot   = rrretest_phi,
#        width  = 13, height = 13, dpi = 600)


#-------- rate ratio male 15-24 years ---------------
rr_m_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_male_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m_overall$`50%`)
exp(rr_m_overall$`2.5%`)
exp(rr_m_overall$`97.5%`)

rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_m$`50%`
rr_m$`2.5%`
rr_m$`97.5%`

# dataframe and forest plot for RR male
df_rrm_ <- data.frame(country = names(cnt_data),
                      median = rr_m$`50%`,
                      lci = rr_m$`2.5%`,
                      uci = rr_m$`97.5%`)
df_rr_m <- rbind(df_rrm_,
                 data.frame( country = "overall",
                             median = exp(rr_m_overall$`50%`),
                             lci = exp(rr_m_overall$`2.5%`),
                             uci = exp(rr_m_overall$`97.5%`)))

# Renaming selected countries
df_rr_m$country <- as.character(df_rr_m$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_m$country[df_rr_m$country == old_name] <- rename_map[[old_name]]
}

df_rr_m$country <- sapply(df_rr_m$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_m$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_m$country     <- factor(df_rr_m$country, levels = new_levels)
df_rr_m$style <- ifelse(df_rr_m$country == "Overall", "pooled", "individual")


rr_male_forest <- ggplot(df_rr_m, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "darkorchid3", "pooled" = "mediumblue")) +  # Colors
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio for 15-24 year old men (reference: 15-24 year old women) ", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right")
rr_male_forest

#write_clip(df_rr_m, object_type = "table")
#ggsave("rr_male_plot.png", plot = rr_male_forest, width = 8, height = 6, dpi = 300)

#-------- rate ratio age ---------------
#---men----
rr_age_overall_m <- as.data.frame(rstan::summary(fit, pars = c("beta_age_male_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_age_overall_m$`50%`)
exp(rr_age_overall_m$`2.5%`)
exp(rr_age_overall_m$`97.5%`)

rr_age_m <- as.data.frame(rstan::summary(fit, pars = c("beta_age_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_age_m$`50%`
rr_age_m$`2.5%`
rr_age_m$`97.5%`

# data.frame and forest plot for RR male
df_rr_a_m <- NULL
for (i in 1:n_cnt) {
  df_rr_a_m_i <- data.frame(country = names(cnt_data)[i],
                          age = c("25-34", "35-49", "50+"),
                          median = rr_age_m$`50%`[grepl(paste(paste0("beta_age_male\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_m))],
                          lci = rr_age_m$`2.5%`[grepl(paste(paste0("beta_age_male\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_m))],
                          uci = rr_age_m$`97.5%`[grepl(paste(paste0("beta_age_male\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_m))])
  df_rr_a_m <- rbind(df_rr_a_m, df_rr_a_m_i)
}
df_rr_age_m <- rbind(df_rr_a_m,
                   data.frame( age = c("25-34", "35-49", "50+"),
                               country = "overall",
                               median = exp(rr_age_overall_m$`50%`),
                               lci = exp(rr_age_overall_m$`2.5%`),
                               uci = exp(rr_age_overall_m$`97.5%`)))

# Renaming selected countries
df_rr_age_m$country <- as.character(df_rr_age_m$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_age_m$country[df_rr_age_m$country == old_name] <- rename_map[[old_name]]
}

df_rr_age_m$country <- sapply(df_rr_age_m$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_age_m$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_age_m$country     <- factor(df_rr_age_m$country, levels = new_levels)
df_rr_age_m$style <- ifelse(df_rr_age_m$country == "Overall", "Pooled", "Individual")
df_rr_age_m$age <- factor(df_rr_age_m$age, levels = c("50+", "35-49", "25-34")) # to match with position.dodge()

rr_age_forest_m <- ggplot(df_rr_age_m, aes(x = country, y = median, color = age, size = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("25-34" = "#845699", 
                                "35-49" = "#66BBBB",
                                "50+" = "#D61717E6")) +  # Colors
  scale_size_manual(values = c("individual" = 0.2, "pooled" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio by age for men (reference: 15-24 year old men)", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right") +
  guides(color = guide_legend(reverse = TRUE), size = "none")
rr_age_forest_m

#write_clip(df_rr_age_m, object_type = "table")
#ggsave("rr_age_plot_men.png", plot = rr_age_forest_m, width = 8, height = 6, dpi = 300)

#---women----
rr_age_overall_f <- as.data.frame(rstan::summary(fit, pars = c("beta_age_female_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_age_overall_f$`50%`)
exp(rr_age_overall_f$`2.5%`)
exp(rr_age_overall_f$`97.5%`)

rr_age_f <- as.data.frame(rstan::summary(fit, pars = c("beta_age_female"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_age_f$`50%`
rr_age_f$`2.5%`
rr_age_f$`97.5%`

# data.frame and forest plot for RR male
df_rr_a_f <- NULL
for (i in 1:n_cnt) {
  df_rr_a_f_i <- data.frame(country = names(cnt_data)[i],
                            age = c("25-34", "35-49", "50+"),
                            median = rr_age_f$`50%`[grepl(paste(paste0("beta_age_female\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_f))],
                            lci = rr_age_f$`2.5%`[grepl(paste(paste0("beta_age_female\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_f))],
                            uci = rr_age_f$`97.5%`[grepl(paste(paste0("beta_age_female\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_f))])
  df_rr_a_f <- rbind(df_rr_a_f, df_rr_a_f_i)
}
df_rr_age_f <- rbind(df_rr_a_f,
                     data.frame( age = c("25-34", "35-49", "50+"),
                                 country = "overall",
                                 median = exp(rr_age_overall_f$`50%`),
                                 lci = exp(rr_age_overall_f$`2.5%`),
                                 uci = exp(rr_age_overall_f$`97.5%`)))

# Renaming selected countries
df_rr_age_f$country <- as.character(df_rr_age_f$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_age_f$country[df_rr_age_f$country == old_name] <- rename_map[[old_name]]
}

df_rr_age_f$country <- sapply(df_rr_age_f$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_age_f$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_age_f$country     <- factor(df_rr_age_f$country, levels = new_levels)
df_rr_age_f$style <- ifelse(df_rr_age_f$country == "Overall", "Pooled", "Individual")
df_rr_age_f$age <- factor(df_rr_age_f$age, levels = c("50+", "35-49", "25-34")) # to match with position.dodge()

rr_age_forest_f <- ggplot(df_rr_age_f, aes(x = country, y = median, color = age, size = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("25-34" = "#845699", 
                                "35-49" = "#66BBBB",
                                "50+" = "#D61717E6")) +  # Colors
  scale_size_manual(values = c("individual" = 0.2, "pooled" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio by age for women (reference: 15-24 year old women)", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right") +
  guides(color = guide_legend(reverse = TRUE), size = "none")
rr_age_forest_f

#write_clip(df_rr_age_f, object_type = "table")
#ggsave("rr_age_plot_women.png", plot = rr_age_forest_f, width = 8, height = 6, dpi = 300)

#---rr ref, rr age men women side by side panel-----
#rrage_malefemale <- plot_grid(rr_age_forest_m, rr_age_forest_f, ncol = 2)
#ggsave("rrage_malefemale.png", plot = rrage_malefemale, width = 13, height = 6, dpi = 300)

library(patchwork)

# rrage_malefemale <- rr_male_forest + rr_age_forest_m +
#   rr_age_forest_f +
#   plot_annotation(tag_levels = "A")


top_row <- rr_male_forest +
  theme(
    plot.margin = margin(t = 5, r = 150, b = 5, l = 150)
  )

# Wrap it so that layout doesn't propagate to others
top_row_wrapped <- wrap_elements(top_row)

# Bottom row as is
bottom_row <- rr_age_forest_m + rr_age_forest_f

# Combine
rrage_malefemale <- 
  top_row_wrapped /                   # top row isolated
  bottom_row +                        # bottom row normal
  plot_layout(ncol = 1, heights = c(1, 1.2)) +
  plot_annotation(tag_levels = "A")
rrage_malefemale



ggsave("rrage_malefemale.png",
       plot   = rrage_malefemale,
       width  = 13, height = 13, dpi = 600)




# all parameters side by side panel

# library(patchwork)
# 
# 
# top row: 3 plots
top <- wrap_plots(
  rr_male_forest,
  rr_age_forest_m,
  rr_age_forest_f,
  ncol = 3
)

# bottom row: a blank cell + your two plots
bottom <- wrap_plots(
  plot_spacer(),       # empty left cell
  rr_retesting_forest, # 4th plot
  phi_forest,          # 5th plot
  ncol = 3,
  widths = c(0.2, 1, 1)
)

allpar <- top / bottom +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(1.2, 1))  # make top row taller if you like

allpar

ggsave("all_par.png",
       plot   = allpar,
       width  = 25, height = 15, dpi = 600)


#-----plot code for trends--------

# overall trend by sex (age aggregated)
# svy_prd_m [country=1(kenya), 2(ghana).., niter=130, agegrp=1:4]
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

dim(ext_fit_m) # draws, country, iterations (time), age groups
n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
niter   <- dim(ext_fit_m)[3]
n_age   <- dim(ext_fit_m)[4]

data_stan$pop #  4 rows per country (one per age group)
# pop_mat_m[c, a] => population of males in country c, age group a
# pop_mat_f[c, a] => population of females in country c, age group a

pop_mat_m <- matrix(NA, nrow = n_cnt, ncol = 4)
pop_mat_f <- matrix(NA, nrow = n_cnt, ncol = 4)
for (c in seq_len(n_cnt)) {
  row_start <- (c - 1)*4 + 1
  row_end   <- row_start + 3
  # male population in column 1 of data_stan$pop
  pop_mat_m[c, ] <- data_stan$pop[row_start:row_end, 1]
  # female population in column 2 of data_stan$pop
  pop_mat_f[c, ] <- data_stan$pop[row_start:row_end, 2]
}
# total region‐level population (across all countries and all age groups)
total_pop_male <- sum(pop_mat_m)
total_pop_female <- sum(pop_mat_f)


# getting overall proportion for trend stratified by sex
male_prp <- matrix(NA, nrow = n_draws, ncol = niter)
female_prp <- matrix(NA, nrow = n_draws, ncol = niter)
for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    # summing for men
    sum_m <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        # ext_fit_m[i, c, t, a] = predicted proportion (0 to 1)
        # multiplying proportion by population in that group
        sum_m <- sum_m + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
      }
    }
    male_prp[i, t] <- sum_m / total_pop_male
    
    # summing for women
    sum_f <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        sum_f <- sum_f + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
    }
    female_prp[i, t] <- sum_f / total_pop_female
  }
}

# median and 95% CrI
male_lci <- apply(male_prp, 2, quantile, 0.025)
male_med <- apply(male_prp, 2, quantile, 0.5)
male_uci <- apply(male_prp, 2, quantile, 0.975)
female_lci <- apply(female_prp, 2, quantile, 0.025)
female_med <- apply(female_prp, 2, quantile, 0.5)
female_uci <- apply(female_prp, 2, quantile, 0.975)

# plotting
#dev.off()
df_sextrend <- data.frame(
  time       = time,
  male_med   = male_med * 100,
  male_lci   = male_lci * 100,
  male_uci   = male_uci * 100,
  female_med = female_med * 100,
  female_lci = female_lci * 100,
  female_uci = female_uci * 100
)

sex_trend_plot <- ggplot(df_sextrend, aes(x = time)) +
  geom_ribbon(aes(ymin = male_lci, ymax = male_uci),
              fill = "lightblue", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = male_med, color = "Male"), size = 1.1) +
  geom_ribbon(aes(ymin = female_lci, ymax = female_uci),
              fill = "pink", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = female_med, color = "Female"), size = 1.1) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = "deepskyblue4", "Female" = "deeppink1")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("HIVST uptake by sex")

sex_trend_plot

# write.table(df_sextrend, "clipboard", sep = "\t", row.names = FALSE)
#ggsave("trend_sex_plot.png", plot = sex_trend_plot, width = 8, height = 6, dpi = 300)

#------------- overall trend by age groups ------------------
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
niter   <- dim(ext_fit_m)[3]
n_age   <- dim(ext_fit_m)[4]

pop_mat_m <- matrix(NA, nrow = n_cnt, ncol = n_age)  # male population
pop_mat_f <- matrix(NA, nrow = n_cnt, ncol = n_age)  # female population
for (c in seq_len(n_cnt)) {
  row_start <- (c - 1)*4 + 1
  row_end   <- row_start + 3
  # male population in column 1
  pop_mat_m[c, ] <- data_stan$pop[row_start:row_end, 1]
  # female population in column 2
  pop_mat_f[c, ] <- data_stan$pop[row_start:row_end, 2]
}

#  array to store sex-aggregated proportions by age group
# prp_age[i, t, a] = proportion who have used HIVST in age group a, time t, draw i, aggregated over all countries & sexes
prp_age <- array(NA, dim = c(n_draws, niter, n_age))
for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    for (a in seq_len(n_age)) {
      sum_counts <- 0
      for (c in seq_len(n_cnt)) {
        # male
        sum_counts <- sum_counts + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
        # female
        sum_counts <- sum_counts + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
      total_pop_age <- sum(pop_mat_m[, a]) + sum(pop_mat_f[, a])
      
      prp_age[i, t, a] <- sum_counts / total_pop_age
    }
  }
}

prp_age_median <- apply(prp_age, c(2, 3), median)   
prp_age_lci    <- apply(prp_age, c(2, 3), quantile, 0.025)
prp_age_uci    <- apply(prp_age, c(2, 3), quantile, 0.975)


# plotting
age_labels <- c("15-24 years", "25-34 years", "35-49 years", "50+ years")
df_age <- data.frame()
for (a in seq_len(n_age)) {
  df_a <- data.frame(
    time   = time,
    median = prp_age_median[, a] * 100,  
    lci    = prp_age_lci[, a]    * 100,
    uci    = prp_age_uci[, a]    * 100,
    age_grp = age_labels[a]  
  )
  
  df_age <- rbind(df_age, df_a)
}

p_age <- ggplot(df_age, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = age_grp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = median, color = age_grp),
            size = 1.1) +
  scale_color_manual(
    values = c("15-24 years" = "#326df9", 
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332")
  ) +
  scale_fill_manual(
    values = c("15-24 years" = "#326df9",
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332")
  )+
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) + 
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)",
    color = "Age Group",
    fill  = "Age Group"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("HIVST uptake by age groups")

p_age

#write_clip(df_age, object_type = "table")



#ggsave("trend_age_plot.png", plot = p_age, width = 8, height = 6, dpi = 300)



#------overall trend---------

prp_total <- matrix(NA, nrow = n_draws, ncol = niter)
total_pop <- sum(pop_mat_m) + sum(pop_mat_f)

for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    sum_counts <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        sum_counts <- sum_counts +
          ext_fit_m[i, c, t, a] * pop_mat_m[c, a] +
          ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
    }
    prp_total[i, t] <- sum_counts / total_pop
  }
}

prp_total_median <- apply(prp_total, 2, median)
prp_total_lci    <- apply(prp_total, 2, quantile, 0.025)
prp_total_uci    <- apply(prp_total, 2, quantile, 0.975)


df_total <- data.frame(
  time   = time,
  median = prp_total_median * 100,
  lci    = prp_total_lci    * 100,
  uci    = prp_total_uci    * 100
)

p_total <- ggplot(df_total, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), 
              fill = "palegreen2", alpha = 0.2) +
  geom_line(aes(y = median), color = "palegreen2", size = 1.2) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Overall HIVST uptake")

p_total

#write_clip(df_total, object_type = "table")


#---region wise trend-----

# ---- regional classification from GBD 2015-----
country_to_region <- c(
  "Burundi"                       = "Eastern", 
  "Kenya"                         = "Eastern", 
  "Madagascar"                    = "Eastern", 
  "Malawi"                        = "Eastern", 
  "Mozambique"                    = "Eastern", 
  "Rwanda"                        = "Eastern", 
  "United Republic of Tanzania"   = "Eastern",  
  "Uganda"                        = "Eastern", 
  "Zambia"                        = "Eastern", 
  "Democratic Republic of the Congo" = "Central",
  
  "Botswana"                      = "Southern", 
  "Eswatini"                      = "Southern", 
  "Lesotho"                       = "Southern", 
  "Namibia"                       = "Southern", 
  "South Africa"                  = "Southern", 
  "Zimbabwe"                      = "Southern", 
  
  "Benin"                         = "Western", 
  "Burkina Faso"                  = "Western", 
  "Cameroon"                      = "Western", 
  "Cote d'Ivoire"                 = "Western", 
  "Ghana"                         = "Western", 
  "Guinea"                        = "Western", 
  "Guinea-Bissau"                 = "Western", 
  "Liberia"                       = "Western", 
  "Mali"                          = "Western", 
  "Senegal"                       = "Western", 
  "Sierra Leone"                  = "Western"
)


is_ESA <- function(region) {
  #  Eastern + Southern as "ESA"
  region %in% c("Eastern", "Southern")
}

is_WCA <- function(region) {
  # We'll treat Western + Central as "WCA"
  region %in% c("Western", "Central")
}

# Now for each country in `countries`, find if it belongs to ESA or WCA:
country_super_region <- sapply(countries, function(ctry) {
  if (is_ESA(country_to_region[ctry])) {
    return("ESA")
  } else if (is_WCA(country_to_region[ctry])) {
    return("WCA")
  } else {
    # If, for some reason, there's a mismatch or new region, handle it
    return(NA_character_)
  }
})

stopifnot(length(country_super_region) == n_cnt)

# We'll create region-level population mats:
pop_mat_m_ESA <- matrix(0, nrow = n_cnt, ncol = n_age)
pop_mat_f_ESA <- matrix(0, nrow = n_cnt, ncol = n_age)
pop_mat_m_WCA <- matrix(0, nrow = n_cnt, ncol = n_age)
pop_mat_f_WCA <- matrix(0, nrow = n_cnt, ncol = n_age)

for (c in seq_len(n_cnt)) {
  if (country_super_region[c] == "ESA") {
    pop_mat_m_ESA[c, ] <- pop_mat_m[c, ]
    pop_mat_f_ESA[c, ] <- pop_mat_f[c, ]
  } else if (country_super_region[c] == "WCA") {
    pop_mat_m_WCA[c, ] <- pop_mat_m[c, ]
    pop_mat_f_WCA[c, ] <- pop_mat_f[c, ]
  }
}

# Region-level total population
pop_total_ESA <- sum(pop_mat_m_ESA) + sum(pop_mat_f_ESA)
pop_total_WCA <- sum(pop_mat_m_WCA) + sum(pop_mat_f_WCA)

# We'll store draws in:
esa_prp <- matrix(NA, nrow = n_draws, ncol = niter)
wca_prp <- matrix(NA, nrow = n_draws, ncol = niter)

for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    # ESA
    sum_esa <- 0
    for (c in seq_len(n_cnt)) {
      if (country_super_region[c] == "ESA") {
        for (a in seq_len(n_age)) {
          # sum men
          sum_esa <- sum_esa + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
          # sum women
          sum_esa <- sum_esa + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
        }
      }
    }
    esa_prp[i, t] <- sum_esa / pop_total_ESA
    
    # WCA
    sum_wca <- 0
    for (c in seq_len(n_cnt)) {
      if (country_super_region[c] == "WCA") {
        for (a in seq_len(n_age)) {
          sum_wca <- sum_wca + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
          sum_wca <- sum_wca + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
        }
      }
    }
    wca_prp[i, t] <- sum_wca / pop_total_WCA
  }
}

esa_lci <- apply(esa_prp, 2, quantile, 0.025)
esa_med <- apply(esa_prp, 2, quantile, 0.5)
esa_uci <- apply(esa_prp, 2, quantile, 0.975)

wca_lci <- apply(wca_prp, 2, quantile, 0.025)
wca_med <- apply(wca_prp, 2, quantile, 0.5)
wca_uci <- apply(wca_prp, 2, quantile, 0.975)

df_regions <- data.frame(
  time = rep(time, times = 2),
  region = rep(c("ESA", "WCA"), each = length(time)),
  
  # Put proportions in percent
  median = c(esa_med, wca_med) * 100,
  lci    = c(esa_lci,  wca_lci)  * 100,
  uci    = c(esa_uci,  wca_uci)  * 100
)


p_regions <- ggplot(df_regions, aes(x = time, y = median, color = region, fill = region)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"), 
    labels = c("ESA" = "Eastern & Southern Africa", 
               "WCA" = "Western & Central Africa")
  ) +
  scale_fill_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"),
    labels = c("ESA" = "Eastern & Southern Africa", 
               "WCA" = "Western & Central Africa")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)",
    color = "Region",
    fill  = "Region"
  ) +
  theme_classic(base_size = 14) +
  ggtitle("HIVST uptake by region") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # center the plot title
  )

p_regions

#write_clip(df_regions, object_type = "table")


#ggsave("trend_region_plot.png", plot = p_regions, width = 8, height = 6, dpi = 300)


#---3 trend plots in panel-----------
# removing legend titles for panel
# region
p_regions <- ggplot(df_regions, aes(x = time, y = median, color = region, fill = region)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"),
    labels = c("ESA" = "Eastern & Southern Africa",
               "WCA" = "Western & Central Africa"),
    name   = NULL            # <--- remove legend title
  ) +
  scale_fill_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"),
    labels = c("ESA" = "Eastern & Southern Africa",
               "WCA" = "Western & Central Africa"),
    name   = NULL            # <--- remove legend title
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
    # we omit color= or fill= to avoid legend titles here
  ) +
  theme_classic(base_size = 14) +
  ggtitle("HIVST uptake by region") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

p_regions

# sex
sex_trend_plot <- ggplot(df_sextrend, aes(x = time)) +
  geom_ribbon(aes(ymin = male_lci, ymax = male_uci),
              fill = "lightblue", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = male_med, color = "Male"), size = 1.1) +
  geom_ribbon(aes(ymin = female_lci, ymax = female_uci),
              fill = "pink", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = female_med, color = "Female"), size = 1.1) +
  scale_color_manual(
    name = NULL,
    values = c("Male" = "deepskyblue4", "Female" = "deeppink1")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("HIVST uptake by sex")

sex_trend_plot

# age
p_age <- ggplot(df_age, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = age_grp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = median, color = age_grp), size = 1.1) +
  scale_color_manual(
    values = c("15-24 years" = "#326df9", 
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332"),
    name   = NULL 
  ) +
  scale_fill_manual(
    values = c("15-24 years" = "#326df9",
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332"),
    name   = NULL 
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),     # remove any remaining title
    plot.title      = element_text(hjust = 0.5)
  ) +
  guides(
    color = guide_legend(nrow = 2),
    fill  = guide_legend(nrow = 2)
  ) +
  ggtitle("HIVST uptake by age groups")

p_age

# combined plot (all 3)
library(patchwork)

combined_trend <- p_regions + sex_trend_plot + p_age +
  plot_layout(ncol = 3) +
  plot_annotation(
    tag_levels = "A",              # gives A, B, C
    theme = theme(
      plot.tag.position = c(0.08, 0.92),   # x, y in npc units
      plot.tag           = element_text(size = 16, face = "bold")
    )
  )

combined_trend

combined_trend <- combined_trend &
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15)  # in pt by default
  )

ggsave("trend_all3_plot.png", plot = combined_trend, width = 13, height = 6, dpi = 300)


# combined plot (all 4)

library(patchwork)

combined_trend2 <- p_total + p_regions + sex_trend_plot + p_age +
  plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "A",              # gives A, B, C
    theme = theme(
      plot.tag.position = c(0.08, 0.92),   # x, y in npc units
      plot.tag           = element_text(size = 16, face = "bold")
    )
  )

combined_trend2

combined_trend2 <- combined_trend2 &
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15)  # in pt by default
  )

#ggsave("trend_all4_plot.png", plot = combined_trend2, width = 13, height = 13, dpi = 600)


#--- function code to check for survey and program fit--------
#  no separate sex dimension in svy_prd_m because it is only for males
# svy_prd_m [country=1(kenya), 2(ghana).., niter=130, agegrp=1:4]

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe",
                   "sierraleone", "zambia", "mali", "uganda",
                   "lesotho", "mozambique", "rwanda",
                   "burkinafaso", "burundi", "cameroon", "cotedivoire",
                   "guinea", "liberia", "senegal", "southafrica","tanzania",
                   "namibia", "botswana", "guineabissau","drc", "eswatini", "benin")
alphabetical_cnt <- order(cnt_lowercase)


# ------ men results --------
svy_m_full <- rstan::summary(fit, "svy_prd_m", probs=c(0.025, 0.5, 0.975))$summary
svy_m_full <- as.data.frame(svy_m_full)
svy_m_full$param <- rownames(svy_m_full)

# splitting by countries
n_cnt <- length(countries)
svy_m_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_m_full$param)   # only country c
  tmp_c <- svy_m_full[ix_c, ]
  
  #  within that subset, separating each age group
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_m_list[[c]] <- ages
}

# plotting for each country
png("survey_fit_men.png",
    width = 20, height = 28,
    units = "in", res = 320)
par(mfrow = c(7, 4))
par(
  mar = c(3, 4, 2, 1),  
  oma = c(0, 0, 0, 0) 
)

for (c_idx in alphabetical_cnt) {
  plot(NA, xlim = range(time), ylim = c(0, 0.5),
       main = paste("Men -", countries[c_idx]),
       xlab = "Year", ylab = "Ever used HIVST")
  offsets <- c(-0.2, -0.1, 0.1, 0.2)
  for (a in 1:4) {
    df_age <- svy_m_list[[c_idx]][[a]] 
    lines(df_age$`50%` ~ time, col = a, lwd = 2)
    polygon(
      x = c(time, rev(time)),
      y = c(df_age$`2.5%`, rev(df_age$`97.5%`)),
      col = adjustcolor(a, alpha.f = 0.3), border = NA
    )
    
    obs <- cnt_data[[c_idx]]$svy_dat_m[, a]   
    lci <- cnt_data[[c_idx]]$lci_svy_m[, a]
    uci <- cnt_data[[c_idx]]$uci_svy_m[, a]
    t_obs <- cnt_data[[c_idx]]$yr_svy
    x_dodge <- t_obs + offsets[a]
    points(obs ~ x_dodge, pch = 16, col = a)
    segments(x_dodge, lci, x_dodge, uci, col = a)
  }
  
  legend("topleft",
         legend = c("15-24", "25-34", "35-49", "50+"),
         col = 1:4, lwd = 2, bty = "n")
}

dev.off()

#------ women results --------
svy_f_full <- rstan::summary(fit, "svy_prd_f", probs=c(0.025, 0.5, 0.975))$summary
svy_f_full <- as.data.frame(svy_f_full)
svy_f_full$param <- rownames(svy_f_full)

svy_f_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_f_full$param)
  tmp_c <- svy_f_full[ix_c, ]
  
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_f_list[[c]] <- ages
}

# plotting for each country
png("survey_fit_women.png",
    width = 20, height = 28,
    units = "in", res = 320)
par(mfrow = c(7, 4))
par(
  mar = c(3, 4, 2, 1),  
  oma = c(0, 0, 0, 0) 
)
for (c_idx in alphabetical_cnt) {
  plot(NA, xlim = range(time), ylim = c(0, 0.5),
       main = paste("Women -", countries[c_idx]),
       xlab = "Year", ylab = "Ever used HIVST")
  
  offsets <- c(-0.2, -0.1, 0.1, 0.2)
  
  for (a in 1:4) {
    df_age <- svy_f_list[[c_idx]][[a]]
    
    lines(df_age$`50%` ~ time, col=a, lwd=2)
    polygon(
      x = c(time, rev(time)),
      y = c(df_age$`2.5%`, rev(df_age$`97.5%`)),
      col = adjustcolor(a, alpha.f=0.3),
      border=NA
    )
    
    obs <- cnt_data[[c_idx]]$svy_dat_f[, a]
    lci <- cnt_data[[c_idx]]$lci_svy_f[, a]
    uci <- cnt_data[[c_idx]]$uci_svy_f[, a]
    t_obs <- cnt_data[[c_idx]]$yr_svy
    
    x_dodge <- t_obs + offsets[a]
    
    points(obs ~ x_dodge, pch=16, col=a)
    segments(x_dodge, lci, x_dodge, uci, col=a)
  }
  
  legend("topleft", legend=c("15-24","25-34","35-49","50+"),
         col=1:4, lwd=2, bty="n")
}

dev.off()


#--- hts results ----
hts_full <- as.data.frame(rstan::summary(fit, "hivst_prd")$summary)
hts_full$param <- rownames(hts_full)

# Splitting by countries
n_cnt <- length(countries)
hts_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), hts_full$param)
  hts_list[[c]] <- hts_full[ix_c, ]
}

png("program_data_fit.png",
    width = 14, height = 28,
    units = "in", res = 320)

par(mfrow = c(7, 4))
par(mar = c(3, 4, 2, 1),  
    oma = c(0, 0, 0, 0))

for (c_idx in alphabetical_cnt) {
  df_c <- hts_list[[c_idx]]
  
  plot(time, df_c$`50%`, type = "l", col = "blue", lwd = 2,
       main = paste("HTS -", countries[c_idx]),
       xlab = "Year", ylab = "Number of HIVST kits")
  
  polygon(x = c(time, rev(time)),
          y = c(df_c$`2.5%`, rev(df_c$`97.5%`)),
          col = adjustcolor("blue", alpha.f = 0.3),
          border = NA)
  
  # observed program data
  t_obs   <- cnt_data[[c_idx]]$yr_hts
  obs_hts <- cnt_data[[c_idx]]$hts_dat
  points(obs_hts ~ t_obs, pch = 16, col = "red")
}

dev.off()

#--------plot code for national estimates of proportion of hivst users in 2024 (when model stops)----------------
#---- Overall proportion at the dt, end of 2024 (last niter 130)---------------
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f
n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
n_iter  <- dim(ext_fit_m)[3]   
n_age   <- dim(ext_fit_m)[4]

res_last_dt <- matrix(NA, nrow = n_draws, ncol = n_cnt,
                      dimnames = list(NULL, c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
                                              "Sierra Leone", "Zambia", "Mali", "Uganda",
                                              "Lesotho", "Mozambique", "Rwanda",
                                              "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
                                              "Guinea", "Liberia", "Senegal", "South Africa", 
                                              "United Republic of Tanzania", "Namibia", "Botswana", 
                                              "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini", "Benin")
                      ))

for (i in seq_len(n_draws)) {
  for (c in seq_len(n_cnt)) {
    men_count   <- 0
    women_count <- 0
    for (a in seq_len(n_age)) {
      men_count   <- men_count   + ext_fit_m[i, c, n_iter, a] * pop_mat_m[c, a]
      women_count <- women_count + ext_fit_f[i, c, n_iter, a] * pop_mat_f[c, a]
    }
    total_pop_c <- sum(pop_mat_m[c, ]) + sum(pop_mat_f[c, ])
    
    res_last_dt[i, c] <- (men_count + women_count) / total_pop_c
  }
}

res_last_dt_median <- apply(res_last_dt, 2, median)
res_last_dt_lci    <- apply(res_last_dt, 2, quantile, probs = 0.025)
res_last_dt_uci    <- apply(res_last_dt, 2, quantile, probs = 0.975)

df_last_dt <- data.frame(
  Country = c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
              "Sierra Leone", "Zambia", "Mali", "Uganda",
              "Lesotho", "Mozambique", "Rwanda",
              "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
              "Guinea", "Liberia", "Senegal", "South Africa", 
              "United Republic of Tanzania", "Namibia", "Botswana", 
              "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini", "Benin"), 
  Median  = res_last_dt_median * 100,
  LCI     = res_last_dt_lci * 100,
  UCI     = res_last_dt_uci * 100
)
df_last_dt

# overall_country_estimates <- ggplot(df_last_dt, aes(x = Country, y = Median)) +
#   geom_col(fill = "#CC0066", color = "black") + 
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), 
#                 width = 0.2, color = "black", size = 0.7) +
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     ggtitle = "HIVST Use by Country"
#   ) +
#   theme_classic(base_size = 14)
# 
# overall_country_estimates



# overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median)) +
#   geom_col(fill = "#CC0066", color = "black") + 
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "black", size = 0.7) +
#   coord_flip() +  # Flip the coordinates so that country names are on the y-axis
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     title = "National estimates of HIVST uptake in 2024"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   )
# 
# overall_country_estimates


# overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median, fill = Median)) +
#   geom_col(color = "black") +
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "black", size = 0.7) +
#   scale_fill_gradient(low = "#B3CDE3", high = "#011F4B") +
#   coord_flip() +
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     title = "HIVST Use by Country (2024)"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   )
# 
# overall_country_estimates


# overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median, fill = Median)) +
#   geom_col(color = "black") +
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "black", size = 0.7) +
#   scale_fill_gradientn(colors = c("#B3CDE3", "#66B2FF", "#1E90FF", "#00509E")) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     title = "National estimates of HIVST uptake in 2024"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   )
# 
# overall_country_estimates

overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median, fill = Median)) +
  geom_col(color = NA) +  # Remove borders from the bars
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "#8B0000", size = 0.7) +  # Deep red error bars
  scale_fill_gradientn(colors = c("#FFF5E1", "#FFA500", "#FF4500", "#FF0000")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Proportion of people who have used HIVST (%)",
    title = "National estimates of HIVST uptake in 2024"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

overall_country_estimates

#write_clip(df_last_dt, object_type = "table")

#ggsave("country-estimate_2024.png", plot = overall_country_estimates, width = 8, height = 6, dpi = 300)




