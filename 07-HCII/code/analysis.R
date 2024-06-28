library(tidyverse)

source("code/data_cleaning.R")

results <- results |>
  group_by(participantID,db) |>
  arrange(plotStartTime) |>
  mutate(trial_no = 1:n())|>
  mutate(plot_lab = as.character(plot) |> as.factor()) |>
  mutate(ratio = factor(ratioLabel)) |>
  mutate(participantno = factor(participantID))


library(lme4)
library(ggridges)
ggplot(results, aes(x = absdiff, y = plot, fill = plot)) + 
  geom_density_ridges() + facet_grid(factor(ratioLabel)~.) +
  scale_fill_discrete(guide = 'none') + 
  ylab("") + 
  xlab("Estimate - True Ratio")

mod <- lmer(data = results, absdiff ~ (1|participantID) + ratio +
                                        graphtype + plot_lab)

summary(mod)

library(modelsummary)


modelsummary(models = list("Estimation Errors" = mod),
             fmt = fmt_decimal(digits = 2, pdigits = 4),
             estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}")


library(mgcv)
library(tidygam)
gamdat <- select(results, appStartTime, absdiff, absdiffnorm, byHowMuch, graphtype, ratioLabel, plot, plot_lab, participantno) 
  # filter(year(appStartTime)<2024)

gammod <- gam(absdiffnorm ~ #plot_lab + 
                s(ratioLabel, by = plot_lab, bs = "tp", k = 4) +
                s(participantno, bs = 're'),
              data = gamdat, method = 'REML')


predict_gam(gammod, exclude_terms = "s(participantno)") %>%
  # mutate(graphtype = factor(graphtype, levels = c("Type1", "Type3"))) %>%
  ggplot(aes(x = ratioLabel, y = absdiffnorm, ymin = lower_ci, ymax = upper_ci, 
             fill = plot_lab, color = plot_lab)) +
  geom_line() + 
  geom_ribbon(alpha = .05, color = NA) + 
  # facet_wrap(~graphtype) + 
  ylab("(Estimate - True Ratio)/(True Ratio)") + xlab("Ratio") 
