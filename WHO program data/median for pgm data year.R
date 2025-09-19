

# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt) + 1
niter <- (end - start) / dt
n_yr <- end - start

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
    yr_svy =  c(2021.5, 2022.5),
    ind_svy = (c(2021.5, 2022.5) - start) / dt,
    den_svy_f = matrix(
      c(2372, 1943, 2263, 1156, # 2021
        4269, 3126, 3071, -999), # 2022
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(227, 259, 215, 43, # 2021
        69, 89, 66, -999), # 2022
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(1828, 1294, 1709, 899, # 2021
        1640, 1118, 1150, 146), # 2022 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(110, 149, 170, 56, # 2021
        34, 51, 51, 5), # 2022 dhs
      nrow = 2, byrow = TRUE),
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
    yr_svy =  c(2017.5, 2023.5),
    ind_svy = (c(2017.5, 2023.5) - start) / dt,
    den_svy_f = matrix(
      c(4458, 3271, 5904, -999, # 2017
        2965, 4634, 2078, -999), # 2023
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(4, 10, 10, -999, # 2017
        12, 31, 14, -999), # 2023
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(3384, 655, 3226, 330, # 2017
        2170, 1087, 1289, 511), # 2023 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(1, 1, 2, 3, # 2017
        1, 3, 3, 2), # 2023 
      nrow = 2, byrow = TRUE),
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
      c(1552, 1348, 1612, 1076, # 2021 phia
        642, 484, 479, -999), # 2022
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(470, 360, 213, 54,# 2021 phia
        194, 178, 101, -999), # 2022 mics 
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(1425, 924, 1110, 587, # 2021 PHIA
        601, 377, 349, -999), # 2022 MICS 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(271, 283, 177, 22, # 2021 phia
        118, 122, 85, -999), # 2022 mics
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


hts_list <- lapply(cnt_data, `[[`, "yr_hts")
hts_counts <- sapply(hts_list, length)
median_count <- median(hts_counts)


# total number of country-years from HTS data
country_years <- sum(sapply(cnt_data, function(x) length(x$yr_hts)))
country_years



