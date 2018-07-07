
# main pacotes -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(magrittr)


# main input -------------------------------------------------------------------

#load("C:/Users/Italo/Github/enade/data/base_enade.rda")
# dplyr::glimpse(base_enade)


# main ui --------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "ENADE"),
  dashboardSidebar(
      width = 300,
      uiOutput("ui_ano"),
      uiOutput("ui_universidade"),
      uiOutput("ui_curso")
  ),
  dashboardBody(
    fluidPage(
      box(
        title = "Filtros", width = 2, height = "450px",
        uiOutput("ui_modalidade"),
        uiOutput("ui_org_academica"),
        uiOutput("ui_categ_adminstrativa"),
        uiOutput("ui_estado"),
        uiOutput("ui_municipio")
      ),
      #tags$style(type = "text/css", ".tabbox-body {height:85vh}"),
      tabBox(
        width = 10,
        tabPanel(
          "Desempenho",
          plotOutput("out_plot_boxplot", height = "800px")
        ),
        tabPanel(
          "Similaridade",
          plotOutput("out_plot_dendro", height = "800px")
        )
      )
      #box(plotOutput("out_plot_dendro", height = "100%"))
      #verbatimTextOutput("deb")
    )
  )
)


# main server -------------------------------------------------------------

server <- function(input, output) {


  # sidebar define ano de consulta ------------------------------------------

  l_ano <- base_enade %>%
    dplyr::pull(ano) %>%
    unique()

  output$ui_ano = renderUI({
    selectInput(
      "in_ano", "Ano de referência",
      choices = l_ano,  selected = 2016
    )
  })


  # sidebar define universidade ---------------------------------------------

  l_universidade <- reactive({

    req(input$in_ano)

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::pull(nome_da_ies) %>%
      unique()
  })

  output$ui_universidade = renderUI({
    selectInput(
      "in_universidade", "Instituição de Ensino",
      choices = l_universidade(),  selected = "CENTRO UNIVERSITÁRIO DE VÁRZEA GRANDE"
    )
  })



  # sidebar define curso ----------------------------------------------------

  l_curso <- reactive({

    req(input$in_ano)
    req(input$in_universidade)

    base_enade %>%
      dplyr::filter(
        ano == input$in_ano,
        nome_da_ies == input$in_universidade
      ) %>%
      dplyr::pull(area_de_enquadramento) %>%
      unique()
  })

  output$ui_curso = renderUI({

    selectInput(
      "in_curso", "Curso",
      choices = l_curso(),  selected = "AGRONOMIA"
    )
  })

  #filtro de modalidade
  # l_modalidade <- reactive({
  #
  #   req(input$in_universidade)
  #
  #   base_enade %>%
  #     dplyr::filter(ano == input$in_ano) %>%
  #     dplyr::pull(modalidade_de_ensino) %>%
  #     unique()
  # })
  #
  # output$ui_modalidade = renderUI({
  #   pickerInput(
  #     "in_modalidade", "Modalidade de Ensino",
  #     choices = l_modalidade(), multiple = TRUE, selected = l_modalidade(),
  #     options = list(`actions-box` = TRUE)
  #   )
  # })


  # filtro de org academica -------------------------------------------------

  l_org_academica <- reactive({

    req(input$in_ano)

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::pull(organizacao_academica) %>%
      unique()
  })

  output$ui_org_academica = renderUI({
    pickerInput(
      "in_org_academica", "Organização Acadêmica",
      choices = l_org_academica(), multiple = TRUE, selected = l_org_academica(),
      options = list(`actions-box` = TRUE)
    )
  })


  # filtro de categ adminstrativa -------------------------------------------

  l_categ_adminstrativa <- reactive({

    req(input$in_ano)
    req(input$in_org_academica)

    base_enade %>%
      dplyr::filter(
        ano == input$in_ano,
        organizacao_academica %in% input$in_org_academica
      ) %>%
      dplyr::pull(categoria_administrativa) %>%
      unique()
  })

  output$ui_categ_adminstrativa = renderUI({
    pickerInput(
      "in_categ_adminstrativa", "Categoria Administrativa",
      choices = l_categ_adminstrativa(), multiple = TRUE, selected = l_categ_adminstrativa(),
      options = list(`actions-box` = TRUE)
    )
  })


  # filtro de estado --------------------------------------------------------

  l_estado <- reactive({

    req(input$in_ano)
    req(input$in_org_academica)
    req(input$in_categ_adminstrativa)

    base_enade %>%
      dplyr::filter(
        ano == input$in_ano,
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa
      ) %>%
      dplyr::arrange(sigla_da_uf) %>%
      dplyr::pull(sigla_da_uf) %>%
      unique()
  })

  output$ui_estado = renderUI({
    pickerInput(
      "in_estado", "Estado",
      choices = l_estado(), multiple = TRUE, selected = l_estado(),
      options = list(`actions-box` = TRUE)
    )
  })


  # filtro de municipio -----------------------------------------------------

  l_municipio <- reactive({

    req(input$in_ano)
    req(input$in_org_academica)
    req(input$in_categ_adminstrativa)
    req(input$in_estado)

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::filter(
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa,
        sigla_da_uf %in% input$in_estado
      ) %>%
      dplyr::arrange(municipio_do_curso) %>%
      dplyr::pull(municipio_do_curso) %>%
      unique()
  })

  output$ui_municipio = renderUI({
    pickerInput(
      "in_municipio", "Município",
      choices = l_municipio(), multiple = TRUE, selected = l_municipio(),
      options = list(`actions-box` = TRUE)
    )
  })

  # output$debbug <- renderPrint({
  #   base_enade %>%
  #     dplyr::select(-codigo_da_ies) %>%
  #     dplyr::filter(
  #       modalidade_de_ensino %in% input$in_modalidade,
  #       organizacao_academica %in% input$in_org_academica,
  #       categoria_administrativa %in% input$in_categ_adminstrativa,
  #       sigla_da_uf %in% input$in_estado,
  #       municipio_do_curso %in% input$in_municipio
  #     ) %>%
  #     dplyr::filter(area_de_enquadramento == input$in_curso) %>%
  #     dplyr::select_if(function(x) is.numeric(x) | any(stringr::str_detect(x, input$in_universidade))) %>%
  #     tidyr::gather(indice, valor, -nome_da_ies) %>%
  #     dplyr::mutate(dummy = ifelse(stringr::str_detect(nome_da_ies, input$in_universidade), "sim", "nao")) %>%
  #     dplyr::glimpse()
  # })


  # body boxplot ------------------------------------------------------------

  base_boxplot <- reactive({
    req(input$in_ano)
    req(input$in_universidade)
    req(input$in_curso)
    req(input$in_org_academica)
    req(input$in_categ_adminstrativa)
    req(input$in_estado)
    req(input$in_municipio)

    base_enade %>%
      dplyr::filter(
        ano == input$in_ano,
        area_de_enquadramento == input$in_curso,
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa,
        sigla_da_uf %in% input$in_estado,
        municipio_do_curso %in% input$in_municipio
      ) %>%
      dplyr::select(
        nome_da_ies,
        nota_bruta_fg, nota_bruta_ce, nota_padronizada_idd,
        nota_padronizada_organizacao_didatico_pedagogica,
        nota_padronizada_infraestrutura_e_instalacoes_fisicas,
        nota_padronizada_mestres, nota_padronizada_doutores,
        nota_padronizada_regime_de_trabalho,
        cpc_continuo
      ) %>%
      tidyr::gather(indice, valor, -nome_da_ies) %>%
      dplyr::mutate(dummy = ifelse(stringr::str_detect(nome_da_ies, input$in_universidade), "sim", "nao"))
  })

  output$out_plot_boxplot <- renderPlot({

    label_notas <- c(
      nota_bruta_fg = "Nota Bruta - FG",
      nota_bruta_ce = "Nota Bruta - CE",
      nota_padronizada_idd = "Nota Padronizada - IDD",
      nota_padronizada_organizacao_didatico_pedagogica = "Nota Padronizada - Organização Didático-Pedagógica",
      nota_padronizada_infraestrutura_e_instalacoes_fisicas = "Nota Padronizada - Infraestrutura e Instalações Físicas",
      nota_padronizada_mestres = "Nota Padronizada - Mestres",
      nota_padronizada_doutores = "Nota Bruta - Doutores",
      nota_padronizada_regime_de_trabalho = "Nota Padronizada - Regime de Trabalho",
      cpc_continuo = "CPC Contínuo"
    )

    base_boxplot() %>%
      ggplot2::ggplot(ggplot2::aes(indice, valor)) +
        ggforce::geom_sina(
          data = dplyr::filter(base_boxplot(), dummy == "nao"),
          color = "grey50", alpha = 0.5, size = 2
        ) +
        ggplot2::geom_point(
          data = dplyr::filter(base_boxplot(), dummy == "sim"),
          color = "white", fill = "#084594", alpha = 1, size = 7, shape = 21
        ) +
        ggplot2::facet_wrap(~indice, ncol = 2, scales = "free", labeller = ggplot2::labeller(indice = label_notas)) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::coord_flip() +
        ggplot2::theme_bw(14) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()
        )

  })


  # body dendrograma -------------------------------------------------------

  base_df_dendro <- reactive({
    req(input$in_ano)
    req(input$in_universidade)
    req(input$in_curso)
    req(input$in_org_academica)
    req(input$in_categ_adminstrativa)
    req(input$in_estado)
    req(input$in_municipio)

    base_enade %>%
      dplyr::mutate(
        nome_da_ies = ifelse(is.na(nome_da_ies), "xxxx", nome_da_ies),
        sigla_da_ies = ifelse(is.na(sigla_da_ies), "xxxx", sigla_da_ies)
      ) %>%
      dplyr::filter(
        ano == input$in_ano,
        area_de_enquadramento == input$in_curso,
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa,
        sigla_da_uf %in% input$in_estado,
        municipio_do_curso %in% input$in_municipio
      ) %>%
      dplyr::select(
        sigla_da_ies, nome_da_ies,
        nota_bruta_fg, nota_bruta_ce, nota_padronizada_idd,
        nota_padronizada_organizacao_didatico_pedagogica,
        nota_padronizada_infraestrutura_e_instalacoes_fisicas,
        nota_padronizada_mestres, nota_padronizada_doutores,
        nota_padronizada_regime_de_trabalho,
        cpc_continuo
      ) %>%
      dplyr::distinct(sigla_da_ies, .keep_all = TRUE)  # verificar se eh precisamos mesmo
  })

  base_dendro <- reactive({

    dados_dendro <- base_df_dendro() %>%
      dplyr::select_if(is.numeric) %>%
      as.data.frame()

    row.names(dados_dendro) <-  base_df_dendro() %>%
      dplyr::pull(sigla_da_ies)

    as.dendrogram(hclust(dist(dados_dendro)))
  })

  output$out_plot_dendro <- renderPlot({

    aux_sigla_universdade <- base_df_dendro() %>%
      dplyr::filter(nome_da_ies == input$in_universidade) %>%
      dplyr::pull(sigla_da_ies)

    ggraph::ggraph(
        base_dendro(),  layout = 'dendrogram',
        repel = F, circular = TRUE, ratio = 0.5
      ) +
      ggraph::geom_edge_elbow() +
      ggraph::geom_node_text(
        ggplot2::aes(
          x = x * 1.005, y = y * 1.005, filter = leaf,
          angle = -((-ggraph::node_angle(x, y) + 90) %% 180) + 90,
          label = label,
          color = label == aux_sigla_universdade,
          size = label == aux_sigla_universdade
        ),
        hjust ='outward', show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = c("grey10", "#084594")) +
      ggplot2::scale_size_manual(values = c(3, 5)) +
      ggplot2::coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
      ggraph::theme_graph()
  })
}

shinyApp(ui, server)
