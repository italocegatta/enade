library(tidyverse)
library(readxl)
library(janitor)

x <- list()
for (i in as.character(2010:2016)) {
 x[[i]] <- read_excel("data-raw/base_original_enade.xlsx", i) %>%
   clean_names() %>%
   names() %>%
   data_frame(
     ano = i,
     var = .
   )
}

vars <- bind_rows(x)

n_vars <- vars %>%
  group_by(var) %>%
  tally()

vars %>%
  filter(ano == 2016) %>%
  left_join(n_vars) %>%
  filter(n == 7)

vars %>%
  filter(var == "nota_bruta_idd")

# nomes de 2016, referencia
nomes_padrao <- vars %>%
  filter(ano == 2016) %>%
  left_join(n_vars) %>%
  filter(n == 7) %>%
  pull(var)

usethis::use_data(nomes_padrao, internal = TRUE, overwrite = TRUE)
