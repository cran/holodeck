## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE----------------------------------------
library(holodeck)
library(dplyr)
library(purrr)
library(ropls)
library(iheatmapr)
library(mice)

## ------------------------------------------------------------------------
df <- tibble(Y = rep(c("a", "b"), each = 5))

df %>% sim_covar(n_vars = 5, var = 1, cov = 0.5)

## ------------------------------------------------------------------------
sim_covar(n_obs = 10, n_vars = 5, var = 1, cov = 0.5)

## ------------------------------------------------------------------------
sim_cat(n_obs = 10, n_groups = 2)

## ------------------------------------------------------------------------
df %>%
  group_by(Y) %>% 
  sim_discr(n_vars = 5, var = 1, cov = 0.1, group_means = c(1, -1))

## ------------------------------------------------------------------------
df <-
  sim_covar(n_obs = 20, n_vars = 5, var = 1, cov = 0.1, name = "low") %>% #5 variables with low covariance
  sim_covar(n_vars = 5, var = 1, cov = 0.8, name = "high")            #5 variables with high covariance

## ------------------------------------------------------------------------
df1 <-
  df %>% 
  sim_cat(n_groups = 2, name = "factor") %>% 
  group_by(factor) %>% 
  sim_discr(n_vars = 5, var = 1, cov = 0.1, group_means = c(-1, 1), name = "discr") %>% 
  ungroup()

## ------------------------------------------------------------------------
df2 <-
  df1 %>% 
  sim_missing(prop = 0.1)
df2

## ----fig.width=6, fig.height=5-------------------------------------------
df1 %>%
  select(-factor) %>% 
  cov() %>% 
  iheatmap(row_labels = TRUE, col_labels = TRUE, name = "cov or var")

## ------------------------------------------------------------------------
df2 <- 
  sim_cat(n_obs = 40, n_groups = 3, name = "factor") %>% 
  sim_covar(n_vars = 3, var = 1, cov = 0.0, name = "noise") %>% 
  group_by(factor) %>% 
  sim_discr(n_vars = 5, var = 1, cov = 0, group_means = c(-1, 0, 1), name  = "signal") %>%
  sim_discr(n_vars = 5, var = 1, cov = 0, group_means = c(0, 0.5, 1), name = "signal2") %>% 
  ungroup()
df2

## ------------------------------------------------------------------------
set.seed(100)
dfs <-
  map(1:20, 
      ~sim_cat(n_obs = 40, n_groups = 3, name = "factor") %>% 
        sim_covar(n_vars = 3, var = 1, cov = 0.0, name = "noise") %>% 
        group_by(factor) %>% 
        sim_discr(n_vars = 5, var = 1, cov = 0, group_means = c(-1, 0, 1), name  = "signal") %>%
        sim_discr(n_vars = 5, var = 1, cov = 0, group_means = c(0, 0.5, 1), name = "signal2") %>% 
        ungroup())

## ------------------------------------------------------------------------
set.seed(101)
dfs.missing <-
  map(dfs, ~sim_missing(., prop = 0.05))

## ----warning=FALSE-------------------------------------------------------
# this might take a few seconds
dfs.imputed <-
  map(dfs.missing, ~mice(., printFlag = FALSE) %>% complete())

## ------------------------------------------------------------------------
head(dfs[[1]])
head(dfs.missing[[1]])
head(dfs.imputed[[1]])

## ------------------------------------------------------------------------
my_opls<- partial(opls, predI = 2, permI = 1, printL = FALSE, plotL = FALSE)
plsda.q2 <- 
  map_dbl(dfs,
      ~my_opls(select(., -factor), .$factor) %>%
        getSummaryDF() %>% .$`Q2(cum)`)

missing.q2 <-
  map_dbl(dfs.missing,
      ~my_opls(select(., -factor), .$factor) %>%
        getSummaryDF() %>% .$`Q2(cum)`)

imputed.q2 <- 
  map_dbl(dfs.imputed,
      ~my_opls(select(., -factor), .$factor) %>%
        getSummaryDF() %>% .$`Q2(cum)`)

## ------------------------------------------------------------------------
tibble(plsda.q2, missing.q2, imputed.q2) %>% 
  mutate(missing.q2.diff = plsda.q2 - missing.q2,
         imputed.q2.diff = plsda.q2 - imputed.q2) %>% 
  summarize_all(mean, na.rm = TRUE)

