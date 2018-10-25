library(RCurl)
library(tidyverse)

# This isn't the prettiest way of doing it, but it'll do...

theme_dfp <- function() {
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  if(get_os() == "osx") {
    theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.caption=element_text(hjust=1, size=9,
                                      margin=margin(t=10),
                                      face="italic"),
            plot.title=element_text(hjust=0, size=18,
                                    margin=margin(b=10),
                                    face="bold", family='FuturaBT-Heavy'),
            plot.subtitle=element_text(hjust=0,
                                       family='Montserrat-Regular'),
            axis.title.y=element_text(size=10,hjust=1,
                                      face="italic", family="Montserrat-Thin"),
            axis.title.x=element_text(hjust=1, size=10, face="italic", family="Montserrat-Thin",
                                      margin = margin(t = 10)), # , margin = margin(t = 10)
            legend.position = "bottom",
            legend.title = element_text(face="bold", family="Montserrat-Regular"),
            text=element_text(family="Montserrat-Regular"))
  }
  else {
    theme_bw() +
      theme(panel.border = element_blank(),
            plot.caption=element_text(hjust=1, size=9,
                                      margin=margin(t=10),
                                      face="italic"),
            plot.title=element_text(hjust=0, size=18,
                                    margin=margin(b=10),
                                    face="bold", family='FuturaBT-Heavy'),
            plot.subtitle=element_text(hjust=0,
                                       family='Montserrat-Regular'),
            axis.title.y=element_text(size=10,hjust=1,
                                      face="italic", family="Montserrat-Thin"),
            axis.title.x=element_text(hjust=1, size=10, face="italic", family="Montserrat-Thin",
                                      margin = margin(t = 10)), # , margin = margin(t = 10)
            legend.position = "bottom",
            legend.title = element_text(face="bold", family="Montserrat-Regular"),
            text=element_text(family="Montserrat-Regular"))
  }
}

# Load data...

ca48_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-ca48-1.csv")) %>% 
  tbl_df() %>% rename_all(tolower) %>% mutate(district = "CA-48")
il12_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-il12-1.csv")) %>% 
  tbl_df() %>% rename_all(tolower) %>%  mutate(district = "IL-12")
il06_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-il06-1.csv")) %>% 
  tbl_df() %>% rename_all(tolower) %>%  mutate(district = "IL-06")
ky06_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-ky06-1.csv")) %>% 
  tbl_df() %>% rename_all(tolower) %>%  mutate(district = "KY-06")
mn03_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-mn03-1.csv")) %>%
  tbl_df() %>% rename_all(tolower) %>%  mutate(district = "MN-03")
mn08_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-mn08-1.csv")) %>% 
  tbl_df() %>% rename_all(tolower) %>%  mutate(district = "MN-08")
wv03_1 <- read.csv(
  text=getURL("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-wv03-1.csv")) %>%
  tbl_df() %>% rename_all(tolower) %>%  mutate(district = "WV-03")

# Clean data...

bind_rows(ca48_1, il12_1, il06_1, ky06_1, mn03_1, mn08_1, wv03_1) -> Data

Data %>%
  mutate(votegop = ifelse(response == "Rep", 1, NA),
         votegop = ifelse(response == "Dem", 0, votegop),
         female = ifelse(gender == "Female", 1, 0),
         approvepotus = dplyr::recode(approve,
                                      'Approve' = 1,
                                      'Disapp.' = 0,
                                      "Don't know" = NULL),
         raceeth = dplyr::recode(race_eth,
                                 'Asian' = 'Asian',
                                 'Black' ='Black',
                                 'Hispanic' = 'Hispanic',
                                 'Other' = 'Other',
                                 'White' = 'White',
                                 "[DO NOT READ] Don't know/Refused" = NULL),
         educcat = dplyr::recode(educ4,
                                 '4-year College Grad' = '4-year College Grad',
                                 'Some College Educ.' ='Some College Educ.',
                                 'Postgraduate Degree' = 'Postgraduate Degree',
                                 'High School Grad. or Less' = 'High School Grad. or Less',
                                 "[DO NOT READ] Don't know/Refused" = NULL),
         collegeed = ifelse(educcat == "4-year College Grad." | educcat == "Postgraduate Degree", 1, 0),
         pid = dplyr::recode(partyid,
                             '[DO NOT READ] Refused' = NULL,
                             'Democrat' = 'Democrat',
                             'Republican' = 'Republican',
                             'Independent (No party)' = 'Pure Independent',
                             'or as a member of another political party' = 'Other Party'),
         districtpid = paste0(district,", ", pid),
         gopid = ifelse(pid == "Republican", 1, 0),
         demid = ifelse(pid == "Democrat", 1, 0),
         struggleopioid = dplyr::recode(opioid,
                                        'yes' = 1,
                                        'no' = 0,
                                        "Don't know" = NULL),
         agegroup = dplyr::recode(ager,
                                  '[DO NOT READ] Refused' = NULL,
                                  '18 to 34' = 0,
                                  '35 to 49' = 1,
                                  '50 to 64' = 2,
                                  '65 and older' = 3),
         z_agegroup = arm::rescale(agegroup),
         fiftyabove = ifelse(agegroup >= 2, 1, 0)) %>% 
  mutate(whitedim = dplyr::recode(reverseracis,
                                  'agree' = 1,
                                  'disagree' = 0 ,
                                  "Don't know" = NULL)) -> Data

# Run models...
# Also: Add a Trump approval variable into this equation and you'll basically dominate everything else, sans the white discrimination variable. Go on; try it!

library(blme)


M1 <- bglmer(votegop ~ fiftyabove + female + whitedim + struggleopioid + collegeed + 
               (1 + whitedim | district) + (1 | pid) + (1 | raceeth), data=Data, family=binomial(link="logit"),
             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))

M2 <- bglmer(votegop ~ fiftyabove + female + whitedim + struggleopioid + collegeed + 
               (1 | district) + (1 + whitedim | pid) + (1 | raceeth), data=Data, family=binomial(link="logit"),
             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))


# Get dot-whisker plot...

library(dotwhisker)
library(broom)


m1df <- tidy(M1) %>% filter(group == "fixed") %>% mutate(model = "Random Slope on District")
m2df <- tidy(M2) %>% filter(group == "fixed") %>% mutate(model = "Random Slope on Party ID")

bind_rows(m1df, m2df) %>%
  relabel_predictors(c(fiftyabove = "50 years old or Above",
                       female = "Female",
                       whitedim = "Believes in Discrimination\nAgainst Whites",
                       struggleopioid = "Knows Someone Who\nStruggled With Opioids",
                       collegeed = "College Educated",
                       approvepotus = "Approves of Trump's\nJob Performance")) %>%
  dwplot(.,dot_args = list(aes(colour = model, shape = model))) + theme_dfp() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey60") +
  # scale_color_discrete(name = "Model") +
  scale_color_manual(values = c("#124073", "#B71D1A"),
                     name = "Model") +
  scale_shape_discrete(name = "Model") +
  labs(title = "Support for the GOP Congressional Candidate in Seven Competitive House Races",
       # subtitle = "Belief in discrimination against whites has the largest magnitude effect in both estimations.",
       caption = "Data: NYT/Upshot. Races: CA-48, IL-06, IL-12, KY-06, MN-03, MN-08, WV-03.
       Both models have random intercepts for the Congressional district, party ID (Democrat, Republican, independent, other), and race-ethnicity (white, black, Asian, Hispanic, other).")

# Get simulations for M1...

merTools::draw(M1, type="average") %>%
  mutate(whitedim = 0) %>%
  merTools::wiggle(., varlist="whitedim", valueslist = list(c(0,1))) %>%
  merTools::wiggle(., varlist="district", valueslist = list(unique(Data$district))) %>%
  mutate(votegop = 0, fiftyabove = 0, female = 0, struggleopioid = 0, collegeed = 0, pid = 0, raceeth = 0) -> m1_newdat




merTools::predictInterval(M1,
                          newdata=m1_newdat, 
                          include.resid.var=F,
                          type="probability", 
                          level = 0.8,
                          n.sims=1000, seed=8675309,
                          returnSims = TRUE) -> m1_sims

bind_cols(m1_newdat, m1_sims) -> m1_newdat

attributes(m1_sims)$sim.results %>% tbl_df() %>%
  mutate(district = c(rep("CA-48", 2), rep("IL-12", 2), rep("IL-06", 2), 
                      rep("KY-06", 2), rep("MN-03", 2), rep("MN-08", 2),
                      rep("WV-03", 2)),
         whitedim = rep(seq(0, 1), 7)) %>%
  select(district, whitedim, everything()) %>%
  group_by(district, whitedim) %>%
  gather(sim, fit, V1:V1000) %>%
  mutate(fit = boot::inv.logit(fit)) %>%
  group_by(district, sim) %>%
  summarize(diff = fit[2]-fit[1]) %>% 
  group_by(district) %>%
  mutate(meanfd = paste0("Mean First Difference:\n ", round(mean(diff), 3))) -> m1fds

m1fds %>% distinct(district, meanfd) %>% ungroup() %>%
  mutate(x = .6,
         y = seq(1.5, 7.5)) -> annotate_me_m1

m1fds %>%
  ggplot(., aes(x = diff, y = district, fill=factor(..quantile..))) + 
  theme_dfp() +
  stat_density_ridges(geom = "density_ridges_gradient",
                      quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975),
                      alpha = 0.4, 
                      calc_ecdf = TRUE) +
  scale_fill_manual(name = "Probability", values = c("#A6CEE3", "#1F78B4", "#A6CEE3"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  geom_text(data=annotate_me_m1, aes(x=x, y=y, label=meanfd), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=2.75,
            family = "Montserrat") +
  geom_vline(xintercept=0, linetype="dashed") +
  xlab("") + ylab("") + guides(fill=FALSE) +
  scale_x_continuous(breaks = seq(-.2, .4, by=.1)) + 
  labs(caption = "Note: each ridge annotated with the mean of first differences.",
       title = "The Varying Effect of Belief in Discrimination Against Whites by Congressional District" #,
       # subtitle = "The distribution of first differences is diffuse for all districts, but the effect is generally highest in CA-48 and lowest in WV-03."
       )

# Get simulations for M2...



merTools::draw(M2, type="average") %>%
  mutate(whitedim = 0) %>%
  merTools::wiggle(., varlist="whitedim", valueslist = list(c(0,1))) %>%
  merTools::wiggle(., varlist="pid", valueslist = list(c("Democrat", "Pure Independent", "Republican", "Other Party"))) %>%
  mutate(votegop = 0, fiftyabove = 0, female = 0, struggleopioid = 0, collegeed = 0, district=0, raceeth = 0) -> m2_newdat




merTools::predictInterval(M2,
                          newdata=m2_newdat, 
                          include.resid.var=F,
                          type="probability", 
                          level = 0.8,
                          n.sims=1000, seed=8675309,
                          returnSims = TRUE) -> m2_sims

bind_cols(m2_newdat, m2_sims) -> m2_newdat

attributes(m2_sims)$sim.results %>% tbl_df() %>%
  mutate(pid = c(rep("Democrat", 2), rep("Pure Independent", 2), 
                      rep("Republican", 2), 
                      rep("Other Party", 2)),
         whitedim = rep(seq(0, 1), 4)) %>%
  select(pid, whitedim, everything()) %>%
  group_by(pid, whitedim) %>%
  gather(sim, fit, V1:V1000) %>%
  mutate(fit = boot::inv.logit(fit)) %>%
  group_by(pid, sim) %>%
  summarize(diff = fit[2]-fit[1]) %>% 
  group_by(pid) %>%
  mutate(meanfd = paste0("Mean First Difference:\n ", round(mean(diff), 3))) -> m2fds


m2fds %>% distinct(pid, meanfd) %>% ungroup() %>%
  mutate(x = .5,
         y = seq(1.5, 4.5)) -> annotate_me_m2

m2fds %>%
  ggplot(., aes(x = diff, y = pid, fill=factor(..quantile..))) + 
  theme_dfp() +
  stat_density_ridges(geom = "density_ridges_gradient",
                      quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975),
                      alpha = 0.4, 
                      calc_ecdf = TRUE) +
  scale_fill_manual(name = "Probability", values = c("#A6CEE3", "#1F78B4", "#A6CEE3"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  geom_text(data=annotate_me_m2, aes(x=x, y=y, label=meanfd), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=2.75,
            family="Montserrat") +
  geom_vline(xintercept=0, linetype="dashed") +
  xlab("") + ylab("") + guides(fill=FALSE) +
  scale_x_continuous(breaks = seq(-.2, .4, by=.1)) + 
  labs(caption = "Note: each ridge annotated with the mean of first differences.",
       title = "The Varying Effect of Belief in Discrimination Against Whites by Party ID"#,
       #subtitle = "The effect is lowest among Republicans, and discernibly more precise/positive among independents and supporters of other parties."
       )
