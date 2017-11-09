library(tidyverse)

library(hrbrthemes)

rats <- read_csv("rats.csv")

## data
windows(3,3)
ggplot(data=rats, aes(x=Age, y=Weight)) +
  geom_line(aes(group=Rat.ID)) +
  theme_bw() +
  xlab("Age (days)") + ylab("Weight (g)")

rats.d <- as.list(rats)
rats.d$n <- nrow(rats)
rats.d$J <- max(rats$Rat.ID)

library(rjags)

rats.m <- jags.model(file="rats.bug", data = rats.d)
rats.b <- jags.samples(rats.m, c("beta.00"), n.iter = 1e4)
rats.p <- coda.samples(rats.m, c("beta.0", "beta.1"), n.iter = 1e4)

windows()
plot(rats.p, ask=T)

library(broom)
library(mmcc)

rats.tidy <- tidy(rats.p)

library(magrittr)

windows(width = 7, height = 3.5)
rats.tidy %<>% separate(parameter, into= c("parameter", "Rat.ID"), sep = "(\\[|\\])") %>%
  mutate_at(.vars = vars(Rat.ID) , .funs = funs(parse_number))

ggplot(data=rats.tidy, aes(x=Rat.ID, y=mean)) +
  geom_pointrange(aes(ymin=`2.5%`,
                      ymax=`97.5%`)) +
  facet_wrap( ~ parameter, scales="free_y") +
  theme_bw() +
  xlab("Rat ID") +
  ylab("Parameter value")

## rat caterpillar
rats.tidy %>% 
  filter(parameter == "beta.1") %>%
  arrange(mean) %>%
  mutate(Rat.ID2 = 1:n()) %>%
  dplyr::select(Rat.ID, Rat.ID2) %>%
  dplyr::inner_join(rats.tidy) %>%
  ggplot(data=., aes(x=Rat.ID2, y=mean)) +
  geom_pointrange(aes(ymin=`2.5%`,
                      ymax=`97.5%`)) +
  facet_wrap( ~ parameter, scales="free_y") +
  theme_bw() +
  xlab("Rat ID ordered by slope") +
  ylab("Parameter value")
  
rats.dt <- mcmc_to_dt(rats.p) %>%
  separate(parameter, into= c("parameter", "Rat.ID"), sep = "(\\[|\\])") %>%
  mutate_at(.vars = vars(Rat.ID) , .funs = funs(parse_number))

windows(height=2)
# overlaid densities
ggplot(data=filter(rats.dt, Rat.ID < 31), aes(x=value)) +
  geom_density(aes(group=Rat.ID), fill="black", color=NA, alpha=1/30) +
  facet_wrap( ~ parameter, scales="free") + theme_bw()
# 
# big.dens <- rats.dt %>% split(.$parameter) %>%
#   map( ~density(.x$value)) %>%
#   map_df(~data.frame(x = .x$x, y = .x$y), .id="parameter")
# 
# ggplot(data=rats.dt, aes(x=value)) +
#   geom_density(aes(group=Rat.ID), fill="black", color=NA, alpha=1/30) +
#   facet_wrap( ~ parameter, scales="free") + theme_bw() +
#   geom_line(data=big.dens, aes(x=x, y=y))

rats.p0 <- coda.samples(rats.m, c("beta.00", "beta.10"), n.iter = 1e4)


rats.dt0 <- mcmc_to_dt(rats.p0) %>%
  mutate(parameter = substr(parameter, 1, 6))

ggplot(data=rats.dt, aes(x=value)) +
  geom_density(aes(group=Rat.ID), fill="black", color=NA, alpha=1/30) +
  facet_wrap( ~ parameter, scales="free") + theme_bw() +
  

  
# ecdf - aitken
windows(7, 2)
ggplot(data=rats.dt,
       aes(x=value)) +
  stat_ecdf(aes(group=Rat.ID), n = 101, geom="line") +
  facet_wrap( ~ parameter, scales="free_x") +
  ylab("Cumulative density") + xlab("Parameter value") +
  theme_bw() +
  geom_hline(yintercept = 0.975, lty=2) +
  geom_hline(yintercept = 0.025, lty=2)


# lines of best fit
library(purrr)
rats.tidy %>%
  select(Rat.ID, parameter, mean) %>%
  spread(parameter, mean) %>%
  split(.$Rat.ID) %>%
  map_df( ~ data.frame(Age = c(0, 36)) %>%
         mutate(Weight = .x$beta.0 + .x$beta.1 * Age),
         .id="Rat.ID") %>%
  ggplot(data=., aes(x=Age, y=Weight)) +
  geom_line(aes(group=Rat.ID))
  

## winbugs style density plots
g0 <- ggplot(data=filter(rats.dt, parameter=="beta.0"), aes(x=value)) +
  geom_density() +
  facet_wrap( ~ Rat.ID, scales="free_x", ncol = 5) + theme_bw() +
  #theme(axis.text = element_text(size=6)) +
  scale_x_continuous(breaks=seq(0,200,by=20)) +
  xlab(expression(beta[0]))

g1 <- ggplot(data=filter(rats.dt, parameter=="beta.1"), aes(x=value)) +
  geom_density() +
  facet_wrap( ~ Rat.ID, scales="free_x", ncol = 5) + theme_bw() +
  #theme(axis.text = element_text(size=6)) +
  scale_x_continuous(breaks=seq(0,8,by=0.5)) +
  xlab(expression(beta[1]))

windows(7, 3.5)
library(gridExtra)
grid.arrange(g0, g1, nrow=1)

# stan
windows(7, 3.5)
inner_join(tidy(rats.p, conf_level = 0.8),
           tidy(rats.p, conf_level = 0.95)) %>%
  separate(parameter, into= c("parameter", "Rat.ID"), sep = "(\\[|\\])") %>%
  mutate_at(.vars = vars(Rat.ID) , .funs = funs(parse_number)) %>%
  ggplot(data=., aes(x=Rat.ID, y=median)) +
  geom_linerange(aes(ymin=`2.5%`,
                     ymax=`97.5%`)) +
  geom_linerange(aes(ymin=`10.0%`,
                     ymax=`90.0%`), color="red", size=2) +
  geom_point(size=2) +
  facet_wrap( ~ parameter, scales="free_x", nrow=1) +
  theme_bw() +
  coord_flip() +
  xlab("Rat ID") +
  ylab("Parameter value")
 
## make a table

rats.tidy %>%
  kable(format = "markdown", digits = 2) %>%
  write(., file="output.md")