shock_with_si_and_cat <- shock_original %>% 
         mutate(SI = round(HR/sbp, digits = 2), 
                           sex = as.factor(sex),
                           SI_cat=cut(SI,
                                     breaks = c(-Inf, 0.50, 0.70, +Inf),
                                     labels=c("low","normal","high")))

shock_normal <- shock_with_si_and_cat %>%
                filter(SI_cat == "normal")

shock_mean <- mean(shock_with_si_and_cat$SI)

female_only <- shock_with_si_and_cat %>%
               filter(sex=="F")

male_only <- shock_with_si_and_cat %>%
            filter(sex=="M")

median_female <- round(median(female_only$SI), digits = 2)
median_male <- median(male_only$SI)

female_over_50 <- female_only %>% filter(age > 50)

female_over_50_mean <- round(mean(female_over_50$SI), digits = 2)

female_over_50_SD <- round(sd(female_over_50$SI), digits = 2)

males_with_sbp_over_135 <- shock_with_si_and_cat %>%
                           filter(sbp > 135, sex == "M")

males_with_sbp_over_135_mean <- round(mean(males_with_sbp_over_135$HR), digits = 1)

males_with_sbp_over_135_sd <- round(sd(males_with_sbp_over_135$HR), digits = 1)

shock_low_and_high <- shock_with_si_and_cat %>%
                      filter(SI_cat == "low" | SI_cat =="high")

shock_low_and_high_mean <- round(mean(shock_low_and_high$age), digits = 0)



rx <- round(rnorm(10, 21, 1), 1)
ct <- round(rnorm(10, 24, 1), 1)
id <- c(1:10)
oag <- data.frame(cbind(id, rx, ct))

sapply(oag, is.unsorted)

str(sapply(oag, is.unsorted))

str(lapply(oag, is.unsorted))


mat <- matrix(1:9, nrow = 3, ncol = 3)
mat

apply(mat, 2, mean)

mapply(mean, [[2]], mat)


mtrx <- matrix(nrow = 5, ncol = 5)
mtrx

mtrx[1,3] = 3
mtrx