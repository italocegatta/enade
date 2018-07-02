library(tidyverse)
library(readxl)
library(janitor)

load("R/sysdata.rda")

vars_q <- quos(nomes_padrao)

x <- list()
for (i in as.character(2010:2016)) {
  x[[i]] <- read_excel("data-raw/base_original_enade.xlsx", i) %>%
    clean_names() %>%
    select(!!!vars_q) %>%
    select(
      -c(
        nota_bruta_organizacao_didatico_pedagogica,
        nota_bruta_infraestrutura_e_instalacoes_fisicas,
        nota_bruta_mestres, nota_bruta_doutores,
        nota_bruta_regime_de_trabalho, cpc_faixa
      )
    )
}

base_enade <- bind_rows(x)


usethis::use_data(base_enade, overwrite = FALSE)
