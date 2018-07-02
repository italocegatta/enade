
# pacotes -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(magrittr)


# input -------------------------------------------------------------------

# load("data/base_enade.rda")

# dplyr::glimpse(base_enade)


# dash --------------------------------------------------------------------

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
      tags$style(type = "text/css", ".box-body {height:85vh}"),
      box(plotOutput("out_plot_boxplot", height = "100%"), width = 10)
      #verbatimTextOutput("deb")
    )
  )
)

server <- function(input, output) {

  #sidebar define ano de consulta
  l_ano <- base_enade %>%
    dplyr::pull(ano) %>%
    unique()

  output$ui_ano = renderUI({
    selectInput(
      "in_ano", "Ano de referência",
      choices = l_ano,  selected = 2016
    )
  })


  #sidebar define universidade
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


  # sidebar define curso
  l_curso <- reactive({

    req(input$in_universidade)

    base_enade %>%
      dplyr::filter(nome_da_ies == input$in_universidade) %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::pull(area_de_enquadramento) %>%
      unique()
  })

  output$ui_curso = renderUI({

    selectInput(
      "in_curso", "Curso",
      choices = l_curso(),  selected = "AGRONOMIA"
    )
  })

  # filtro de modalidade
  l_modalidade <- base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::pull(modalidade_de_ensino) %>%
      unique()

  output$ui_modalidade = renderUI({
    pickerInput(
      "in_modalidade", "Modalidade de Ensino",
      choices = l_modalidade, multiple = TRUE, selected = l_modalidade,
      options = list(`actions-box` = TRUE)
    )
  })


  # filtro de org academica
  l_org_academica <- reactive({

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::filter(modalidade_de_ensino %in% input$in_modalidade) %>%
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


  # filtro de categ adminstrativa
  l_categ_adminstrativa <- reactive({

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::filter(
        modalidade_de_ensino %in% input$in_modalidade,
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


  # filtro de estado
  l_estado <- reactive({

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::filter(
        modalidade_de_ensino %in% input$in_modalidade,
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa
      ) %>%
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


  # filtro de municipio
  l_municipio <- reactive({

    base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::filter(
        modalidade_de_ensino %in% input$in_modalidade,
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa,
        sigla_da_uf %in% input$in_estado
      ) %>%
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

  # body boxplot
  output$out_plot_boxplot <- renderPlot({

    req(input$in_municipio)

    base_plot <- base_enade %>%
      dplyr::filter(ano == input$in_ano) %>%
      dplyr::select(-codigo_da_ies) %>%
      dplyr::filter(
        modalidade_de_ensino %in% input$in_modalidade,
        organizacao_academica %in% input$in_org_academica,
        categoria_administrativa %in% input$in_categ_adminstrativa,
        sigla_da_uf %in% input$in_estado,
        municipio_do_curso %in% input$in_municipio
      ) %>%
      dplyr::filter(area_de_enquadramento == input$in_curso) %>%
      dplyr::select_if(function(x) is.numeric(x) | any(stringr::str_detect(x, input$in_universidade))) %>%
      tidyr::gather(indice, valor, -nome_da_ies) %>%
      dplyr::mutate(dummy = ifelse(stringr::str_detect(nome_da_ies, input$in_universidade), "sim", "nao"))

    label_notas <- c(
      nota_bruta_fg = "Nota Bruta - FG",
      nota_bruta_ce = "Nota Bruta - CE",
      nota_continua_do_enade = "Nota Contínua do Enade",
      nota_idd = "Nota Padronizada - IDD",
      nota_organizacao_dp = "Nota Padronizada - Organização Didático-Pedagógica",
      nota_infraestrutura = "Nota Padronizada - Infraestrutura e Instalações Físicas",
      nota_oportunidades = "Nota Padronizada - Oportunidades de Ampliação da Formação",
      nr_de_docentes = "Nr. de Docentes",
      nota_mestres = "Nota Padronizada - Mestres",
      nota_doutores = "Nota Bruta - Doutores",
      nota_regime_de_trabalho = "Nota Padronizada - Regime de Trabalho",
      cpc_continuo = "CPC Contínuo"
    )

      ggplot2::ggplot(base_plot, ggplot2::aes(indice, valor)) +
        ggplot2::geom_boxplot(outlier.color = NA) +
        ggplot2::geom_jitter(
          data = dplyr::filter(base_plot, dummy == "nao"),
          color = "grey50", alpha = 1, size = 1.5
        ) +
        ggplot2::geom_point(
          data = dplyr::filter(base_plot, dummy == "sim"),
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
}

shinyApp(ui, server)
