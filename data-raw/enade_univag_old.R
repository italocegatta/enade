
# pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(ggrepel)
library(ggdendro)
library(ggraph)
library(corrr)
library(olsrr)


# funcao auxiliar ---------------------------------------------------------

limpa_nome <- function(x) {
  make.names(x) %>%
    snakecase::to_any_case(
      case = "snake",
      sep_in = "\\.",
      transliterations = c("Latin-ASCII"),
      parsing_option = 4
    )
}

gera_dendro <- function(df, var, k = 4) {
  var <- enquo(var)
  
  dados_dendro <- df %>% 
    select_if(is.numeric) %>% 
    as.data.frame()
  
  row.names(dados_dendro) <- df %>% 
    pull(!!var)
  
  hc <- hclust(dist(dados_dendro), "average") 
  dendr <- dendro_data(hc)
  clust <- cutree(hc, k)
  clust.df <- data.frame(label = names(clust), cluster = factor(clust))
  dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")
  
  return(dendr)
}


# importacao --------------------------------------------------------------

df <- read_excel("resultado_cpc_2016_portal_27_11_2017.xlsx") %>% 
  clean_names() %>% # retira acento, espaco em branco e converte para caixa-baixa
  select(-c(
    ano, nome_da_ies, organizacao_academica,
    categoria_administrativa, codigo_do_curso, codigo_da_area, 
    modalidade_de_ensino, codigo_do_municipio, concluintes_inscritos,
    concluintes_participantes, concluintes_participantes_com_nota_no_enem,
    percentual_de_concluintes_participantes_com_nota_no_enem, 
    
    nota_bruta_idd, nota_bruta_organizacao_didatico_pedagogica, 
    nota_bruta_infraestrutura_e_instalacoes_fisicas, 
    nota_bruta_oportunidades_de_ampliacao_da_formacao, nota_bruta_mestres,
    nota_bruta_doutores, nota_bruta_regime_de_trabalho, cpc_faixa
  )) %>% # retira variaveis que nao serao utilizadas
  select_all(~str_replace(., "_padronizada", "")) %>% # retira o termo padronizada do nome
  rename(
    nota_organizacao_dp = nota_organizacao_didatico_pedagogica,  
    nota_infraestrutura = nota_infraestrutura_e_instalacoes_fisicas,
    nota_oportunidades = nota_oportunidades_de_ampliacao_da_formacao
  ) %>% # renomeia para um nome menor
  filter(nr_de_docentes < 150) # filtra numero absurso de professores

glimpse(df) # estrutura final da base utiliada na analise


# componentes principais --------------------------------------------------


# semelhanca dos cursos com universidades do brasil -----------------------

lista_cursos <- df %>% 
  filter(sigla_da_ies == "UNIVAG") %>% 
  pull(area_de_enquadramento) %>% 
  unique()

i = "AGRONOMIA"
for (i in lista_cursos) {
  
  pca <- df %>% 
    filter(area_de_enquadramento == i) %>% 
    select(
      nota_idd, cpc_continuo, 
      nota_continua_do_enade, nota_infraestrutura,
      nota_mestres, nota_doutores, nota_oportunidades
    ) %>% 
    na.omit() %>% 
    princomp(cor = T)
  
  loadings(pca)
  
  df_pca <- df %>% 
    filter(area_de_enquadramento == i) %>% 
    cbind(predict(pca, newdata = .)[ , 1:2]) %>% 
    as_tibble()
  
  v_max <- c(df_pca$Comp.1, df_pca$Comp.2) %>% 
    range(na.rm = TRUE) %>% 
    abs() %>% max()
  
  df_pca %>% 
    ggplot(aes(Comp.1, Comp.2)) +
      geom_vline(xintercept = 0, size = 1.2, linetype = 6) +
      geom_hline(yintercept = 0, size = 1.2, linetype = 6) +
      geom_point(
        aes(color = sigla_da_uf == "MT"), 
        show.legend = FALSE
      ) +
      geom_label_repel(
        data = df_pca %>% filter(sigla_da_uf == "MT"),
        aes(label = sigla_da_ies)
      ) +
      coord_equal(ratio = 1, xlim = c(-v_max, v_max), ylim = c(-v_max, v_max)) +
      scale_color_manual(values = c("grey80", "red")) +
      theme_bw() +
      ggsave(str_glue("pca_{str_to_lower(limpa_nome(i))}_brasil.png"), width = 8, height = 8)
}

# semelhanca dos cursos com universidades do brasil -----------------------

base_compara_cursos <- df %>% 
  select(
    codigo_da_ies, sigla_da_ies, municipio_do_curso,
    area_de_enquadramento, nota_idd, cpc_continuo, 
    nota_continua_do_enade, nota_infraestrutura,
    nota_mestres, nota_doutores, nota_oportunidades
  ) %>% 
  inner_join(
    .,
    filter(., sigla_da_ies == "UNIVAG") %>% select(area_de_enquadramento) 
  ) %>% 
  mutate(ies = str_c(codigo_da_ies, sigla_da_ies, municipio_do_curso, sep = "-")) %>% 
  select(-c(codigo_da_ies, sigla_da_ies, municipio_do_curso))

i = "AGRONOMIA"
for (i in unique(base_compara_cursos$area_de_enquadramento)) {
  
  dados_dendro <- base_compara_cursos %>% 
    filter(area_de_enquadramento == i) %>% 
    distinct(ies, .keep_all = TRUE) %>% 
    select_if(is.numeric) %>% 
    as.data.frame()
  
  row.names(dados_dendro) <-  base_compara_cursos %>% 
    filter(area_de_enquadramento == i) %>% 
    distinct(ies, .keep_all = TRUE) %>% 
    pull(ies)
  
  n <- nrow(dados_dendro)
  f <- 12
  size = case_when(
    n <= 150 ~ f,
    n >= 350 & n < 550 ~ f * 1.5,
    n >= 550 & n < 750 ~ f * 2.5,
    n >= 950 ~ f * 2.5
  )
  
  dendrogram <- as.dendrogram(hclust(dist(dados_dendro)))
  
  ggraph(dendrogram,  layout = 'dendrogram', repel = F, circular = TRUE, 
         ratio = 0.5) + 
    geom_edge_elbow() + 
    geom_node_text(
      aes(
        x = x * 1.005, y = y * 1.005, filter = leaf, 
        angle = -((-node_angle(x, y) + 90) %% 180) + 90, 
        label = label %>% str_split("-", 3, TRUE) %>% '['( , 2),
        color = label %>% str_split("-", 3, TRUE) %>% '['( , 2) == "UNIVAG"
      ), 
      size = 3, hjust='outward', show.legend = FALSE
    ) +
    labs(title = str_glue("Curso: {i}"), subtitle = str_glue("N° de Universidades: {n}")) +
    scale_color_manual(values = c("grey20", "red")) +
    coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    theme_void() +
    ggsave(str_glue("cluster_{str_to_lower(limpa_nome(i))}_brasil.png"), width = size, height = size)
}


# semelhanca com universidades do brasil ----------------------------------

resumo_ies <- df %>% 
  select(
    codigo_da_ies, sigla_da_ies, municipio_do_curso,
    nota_idd, cpc_continuo, 
    nota_continua_do_enade, nota_infraestrutura,
    nota_mestres, nota_doutores, nota_oportunidades
  ) %>% 
  group_by(codigo_da_ies, sigla_da_ies, municipio_do_curso) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(ies = str_c(codigo_da_ies, sigla_da_ies, municipio_do_curso, sep = "-")) %>% 
  select(-c(codigo_da_ies, sigla_da_ies, municipio_do_curso))

dados_dendro <- resumo_ies %>% 
  select_if(is.numeric) %>% 
  as.data.frame()

row.names(dados_dendro) <- resumo_ies %>% 
  pull(ies)

dendrogram <- as.dendrogram(hclust(dist(dados_dendro)))

ggraph(dendrogram,  layout = 'dendrogram', repel = F, circular = TRUE, 
       ratio = 0.5) + 
  geom_edge_elbow(edge_width = 0.5) + 
  geom_node_text(
    aes(
      x = x * 1.005, y = y * 1.005, filter = leaf, 
      angle = -((-node_angle(x, y) + 90) %% 180) + 90, 
      label = label %>% str_split("-", 3, TRUE) %>% '['( , 2),
      color = label %>% str_split("-", 3, TRUE) %>% '['( , 2) == "UNIVAG"
    ), 
    size = 1, hjust='outward', show.legend = FALSE
  ) +
  scale_color_manual(values = c("grey20", "red")) +
  coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_void() +
  ggsave("cluster_univag_brasil.png", width = 20, height = 20)


# semelhanca dentro do univag ------------------------------------------------------------

dendr <- df %>% 
  filter(sigla_da_ies == "UNIVAG") %>% # filtra so os cursos da univag
  select(
    area_de_enquadramento, nota_idd, cpc_continuo, 
    nota_continua_do_enade, nota_infraestrutura,
    nota_mestres, nota_doutores, nota_oportunidades
  ) %>% 
  gera_dendro(area_de_enquadramento, 4)

ggplot() +
  geom_segment(
    data = segment(dendr), 
    aes(x, y, xend = xend, yend = yend)
  ) + 
  geom_text(
    data = label(dendr), 
    aes(x, y, label = label), hjust = 0, 
    size = 3, show.legend = FALSE
  ) +
  coord_flip() + scale_y_reverse(expand = c(0.5, 0)) + 
  theme_void() +
  ggsave("cluster_dentro_univag.png", width = 12, height = 6)


# comparativo geral -------------------------------------------------------

df %>% 
  select_if(function(x) is.numeric(x) | any(str_detect(x, "UNIVAG"))) %>%        # seleciona notas e variavel informa a universidade
  gather(indice, valor, -sigla_da_ies) %>%                                       # reformata de wide para long-data
  mutate(univag = ifelse(str_detect(sigla_da_ies, "UNIVAG"), "sim", "nao")) %>%  # cria variavel para identificar valores da univag
  ggplot(aes(indice, valor)) +
    geom_boxplot(outlier.color = NA) +
    geom_jitter(aes(color = univag, alpha = univag, size = univag), show.legend = FALSE) +
    facet_wrap(~indice, ncol = 1, scales = "free") +
    labs(title = "UNIVAG x Brasil") +
    coord_flip() +
    scale_color_manual(values = c("sim" = "#084594", "nao" = "grey70")) +
    scale_alpha_manual(values = c("sim" = 0.8, "nao" = 0.05)) +
    scale_size_manual(values = c("sim" = 2, "nao" = 1)) +
    theme_bw() +
    theme(strip.text = element_blank()) +
    ggsave("indices_brasil_univag.png", width = 8, height = 10)

df %>% 
  filter(sigla_da_uf == "MT") %>%  # filtra so dados de mt
  select_if(function(x) is.numeric(x) | any(str_detect(x, "UNIVAG"))) %>%      
  gather(indice, valor, -sigla_da_ies) %>%                                    
  mutate(univag = ifelse(str_detect(sigla_da_ies, "UNIVAG"), "sim", "nao")) %>% 
  ggplot(aes(indice, valor)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(color = univag, alpha = univag, size = univag), show.legend = FALSE) +
  facet_wrap(~indice, ncol = 1, scales = "free") +
  labs(title = "UNIVAG x MT") +
  coord_flip() +
  scale_color_manual(values = c("sim" = "#084594", "nao" = "grey70")) +
  scale_alpha_manual(values = c("sim" = 0.8, "nao" = 0.3)) +
  scale_size_manual(values = c("sim" = 2, "nao" = 1)) +
  theme_bw() +
  theme(strip.text = element_blank()) +
  ggsave("indices_mt_univag.png", width = 8, height = 10)

df %>% 
  filter(municipio_do_curso %in% c("CUIABÁ", "VÁRZEA GRANDE")) %>% 
  select_if(function(x) is.numeric(x) | any(str_detect(x, "UNIVAG"))) %>%
  gather(indice, valor, -sigla_da_ies) %>%
  mutate(univag = ifelse(str_detect(sigla_da_ies, "UNIVAG"), "sim", "nao")) %>% 
  ggplot(aes(indice, valor)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(color = univag, alpha = univag, size = univag), show.legend = FALSE) +
  facet_wrap(~indice, ncol = 1, scales = "free") +
  labs(title = "UNIVAG x (CBA + VG)") +
  coord_flip() +
  scale_color_manual(values = c("sim" = "#084594", "nao" = "grey70")) +
  scale_alpha_manual(values = c("sim" = 0.8, "nao" = 0.5)) +
  scale_size_manual(values = c("sim" = 2, "nao" = 1)) +
  theme_bw() +
  theme(strip.text = element_blank()) +
  ggsave("indices_cba_vg__univag.png", width = 8, height = 10)
  

# corelacoes --------------------------------------------------------------

df %>% 
  select_if(is.numeric) %>%                 # seleciona so variaveis numericas
  correlate() %>%                           # calcula correlacao
  rearrange() %>%                           # ordena variaveis em funcao da correlacao (facilita a visualisacao)
  rplot(legend = TRUE, print_cor = TRUE) +  # cria o grafico e faz edicoes
  labs(title = "Todos os dados", color = "Correlação de Pearson") +
  theme(
    axis.text.x = element_text(angle = 30, vjus = 1, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(alpha = "none", size = "none") +
  ggsave("cor_brasil.png", width = 8, height = 8)

df %>% 
  filter(sigla_da_ies == "UNIVAG") %>% # filtra so os cursos da univag
  select_if(is.numeric) %>%               
  correlate() %>%                         
  rearrange() %>%                         
  rplot(legend = TRUE, print_cor = TRUE) +
  labs(title = "Cursos da UNIVAG", color = "Correlação de Pearson") +
  theme(
    axis.text.x = element_text(angle = 30, vjus = 1, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(alpha = "none", size = "none") +
  ggsave("cor_univag.png", width = 8, height = 8)


# stepwise ----------------------------------------------------------------

df_step <- df %>% 
  filter(sigla_da_ies == "UNIVAG") %>%
  select_if(is.numeric)               

mod_cpc <- lm(cpc_continuo ~ ., data = df_step) # determina o modelo, cpc contra todas as variaveis

step_mod_cpc <- ols_step_both_p(mod_cpc) # aplica metodo stepwise para identificar variaveis influentes
step_mod_cpc

mod_idd <- lm(nota_idd ~ ., data = df_step)

step_mod_idd <- ols_step_both_p(mod_idd)
step_mod_idd


# nota dos cursos ----------------------------------------------------------

df %>% 
  filter(sigla_da_ies == "UNIVAG") %>% 
  ggplot(aes(nota_idd, cpc_continuo, label = area_de_enquadramento)) +
  geom_smooth(method = "lm", se = FALSE, color = "#6baed6", fullrange = TRUE) +  
  geom_point(size = 3, size = 2, color = "#084594") +
  geom_label_repel(size = 3, alpha = 0.7, seed = 50) +
  lims(x = c(0, 5), y = c(0, 5)) +
  theme_bw() +
  ggsave("idd_cpc_univag.png", width = 9, height = 7)

df %>% 
  filter(sigla_da_ies == "UNIVAG") %>% 
  ggplot(aes(nota_idd, cpc_continuo, label = area_de_enquadramento)) +
    geom_smooth(data = df, method = "lm", se = FALSE, color = "black", alpha = 0.6) +
    geom_point(data = df, alpha = 0.05) +
    geom_smooth(method = "lm", se = FALSE, color = "#6baed6", fullrange = TRUE) +  
    geom_point(size = 3, size = 2, color = "#084594") +
    geom_label_repel(size = 3, alpha = 0.7, seed = 50) +
    lims(x = c(0, 5), y = c(0, 5)) +
    theme_bw() +
    ggsave("idd_cpc_brasil_univag.png", width = 9, height = 7)


# loop notas por curso ----------------------------------------------------

lista_cursos <- df %>% 
  filter(sigla_da_ies == "UNIVAG") %>% 
  pull(area_de_enquadramento) %>% 
  unique()

for (i in seq_along(lista_cursos)) {
  
  df %>% 
    filter(area_de_enquadramento == lista_cursos[i]) %>% 
    select_if(function(x) is.numeric(x) | any(str_detect(x, "UNIVAG"))) %>%       
    gather(indice, valor, -sigla_da_ies) %>%                                      
    mutate(univag = ifelse(str_detect(sigla_da_ies, "UNIVAG"), "sim", "nao")) %>% 
    ggplot(aes(indice, valor)) +
      geom_boxplot(outlier.color = NA) +
      geom_jitter(aes(color = univag, alpha = univag, size = univag), show.legend = FALSE) +
      facet_wrap(~indice, ncol = 1, scales = "free") +
      labs(title = lista_cursos[i]) +
      coord_flip() +
      scale_color_manual(values = c("sim" = "#084594", "nao" = "grey70")) +
      scale_alpha_manual(values = c("sim" = 1, "nao" = 0.2)) +
      scale_size_manual(values = c("sim" = 3, "nao" = 1.5)) +
      theme_bw() +
      theme(strip.text = element_blank()) +
      ggsave(
        str_glue('indices_curso_{limpa_nome(lista_cursos[i])}.png'), 
        width = 8, height = 10
      )
}


