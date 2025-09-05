# app.R  ---------------------------------------------------------------
# Painel de Meningite (Belém) - Shiny (tema claro, KPIs, dygraphs e CSS)

# ---------------------- Pacotes ---------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, bslib, tidyverse, lubridate, rio, dygraphs, xts, kableExtra,
  ISOweek, DT, stringr, ggplot2, zoo, scales, bsicons, knitr
)

# ---------------------- CSS custom (tema claro) ------------------------
app_css <- HTML("
  :root {
    --brand: #0ea5e9;           /* azul/ciano principal */
    --brand-2: #22d3ee;         /* ciano claro */
    --bg: #f8fafc;
    --card: #ffffff;
    --muted: #64748b;
    --border: #e2e8f0;

    /* Navbar */
    --nav-bg: #0ea5e9;          /* fundo da navbar */
    --nav-fg: #ffffff;          /* textos/links da navbar */
    --nav-pill-active: #0369a1; /* aba ativa (azul escuro) */
    --nav-height: 48px;
    --nav-py: 4px;
    --nav-link-py: 4px;
    --nav-link-px: 12px;
    --nav-brand-size: 2rem;

    /* Gradiente exclusivo da 'pílula' do título */
    --title-pill-1: #22d3ee;    /* ciano claro */
    --title-pill-2: #0ea5e9;    /* ciano/azul médio */
    --title-pill-3: #0369a1;    /* azul escuro */

    /* KPIs (value boxes) */
    --kpi-primary: #0ea5e9;
    --kpi-success: #16a34a;
    --kpi-warning: #f59e0b;
    --kpi-danger:  #ef4444;     /* usar apenas em 'danger/alerta' */
    --kpi-info:    #22d3ee;
    --kpi-height: 140px;
  }

  body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    background: linear-gradient(180deg, var(--bg) 0%, #f3f4f6 100%);
    color: #111827; margin: 0; padding: 20px; line-height: 1.6;
  }

  .container, .container-fluid { max-width: 100% !important; margin: 0 auto; padding: 20px; }

  /* ---------------- Navbar ---------------- */
  .navbar {
    background: var(--nav-bg) !important; color: var(--nav-fg) !important;
    border-bottom: 1px solid rgba(0,0,0,.08) !important;
    box-shadow: 0 6px 18px rgba(0,0,0,.08);
    min-height: var(--nav-height);
    padding-top: var(--nav-py) !important; padding-bottom: var(--nav-py) !important;
    position: relative;
  }
  .navbar .navbar-brand, .navbar .nav-link {
    display: flex; align-items: center; font-weight: 600; color: #ffffff !important;
    text-shadow: 1px 1px 4px rgba(0,0,0,0.7);
  }
  .navbar .navbar-brand {
    font-weight: 800; letter-spacing: .2px; font-size: var(--nav-brand-size);
    padding-top: 0 !important; padding-bottom: 0 !important;
  }
  .navbar .nav-link {
    border-radius: 999px !important; padding: var(--nav-link-py) var(--nav-link-px) !important;
    margin: 4px 6px; transition: all .2s ease;
  }
  .navbar .nav-link:hover { background: rgba(255,255,255,.15) !important; }
  .navbar .nav-link.active, .navbar .nav-item .active {
    background: var(--nav-pill-active) !important; color: #ffffff !important;
    box-shadow: 0 6px 14px rgba(0,0,0,.20);
  }
  /* Borda/gradiente do título + filete inferior */
  .navbar::after{
    content: '';
    position: absolute; left: 12px; right: 12px; bottom: -1px; height: 4px;
    border-radius: 999px;
    background: linear-gradient(90deg, var(--title-pill-1), var(--title-pill-2), var(--title-pill-3));
    opacity: .95;
  }
  .navbar .navbar-brand{
    padding: 6px 14px !important;
    border: 2px solid transparent;
    background:
      linear-gradient(180deg, rgba(255,255,255,.22), rgba(255,255,255,.06)) padding-box,
      linear-gradient(90deg, var(--title-pill-1), var(--title-pill-2), var(--title-pill-3)) border-box;
    border-radius: 999px;
    box-shadow:
      inset 0 1px 0 rgba(255,255,255,.6),
      0 8px 18px rgba(0,0,0,.15);
  }

  /* ---------------- Cards ---------------- */
  .card {
    background-color: var(--card); color: #111827; border: 1px solid var(--border);
    box-shadow: 0 6px 18px rgba(0,0,0,.06); border-radius: 14px;
    min-height: 400px; display: flex; flex-direction: column;
  }
  .card .card-body { overflow: visible !important; padding: 20px 24px; flex: 1; }

  /* ---------------- KPIs (value boxes) ---------------- */
  .bslib-value-box.kpi {
    position: relative; height: var(--kpi-height); min-height: var(--kpi-height);
    display: flex; align-items: center; gap: 16px; padding: 18px 20px 18px 24px;
    border: 1px solid var(--border); border-radius: 20px;
    background: radial-gradient(120% 160% at 100% -10%, rgba(255,255,255,.9) 0%, rgba(255,255,255,.8) 40%, rgba(255,255,255,.75) 100%),
                linear-gradient(180deg, #ffffff 0%, #f6f9fc 100%);
    box-shadow: 0 8px 22px rgba(2, 8, 20, .06),
                inset 0 0 0 1px color-mix(in srgb, var(--kpi-accent, var(--brand)) 12%, transparent);
    transition: transform .2s ease, box-shadow .2s ease, border-color .2s ease, background .2s ease;
  }
  .bslib-value-box.kpi::before {
    content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 6px;
    background: linear-gradient(180deg, color-mix(in srgb, var(--kpi-accent, var(--brand)) 85%, transparent), transparent);
    border-top-left-radius: 20px; border-bottom-left-radius: 20px;
  }
  .bslib-value-box.kpi::after {
    content: ''; position: absolute; right: 14px; top: 14px; width: 120px; height: 120px;
    background: radial-gradient(45% 45% at 50% 50%, rgba(255,255,255,.7) 0%, rgba(255,255,255,0) 70%);
    pointer-events: none;
  }
  .bslib-value-box.kpi:hover {
    transform: translateY(-3px); box-shadow: 0 14px 28px rgba(2, 8, 20, .12);
    border-color: color-mix(in srgb, var(--kpi-accent, var(--brand)) 28%, var(--border));
  }
  .bslib-value-box.kpi .value-box-showcase {
    display: flex; align-items: center; justify-content: center;
    width: 72px; height: 72px; min-width: 72px; border-radius: 16px;
    background: color-mix(in srgb, var(--kpi-accent, var(--brand)) 10%, #ffffff);
    border: 1px solid color-mix(in srgb, var(--kpi-accent, var(--brand)) 40%, var(--border));
    box-shadow: inset 0 0 0 6px rgba(255,255,255,.6), 0 10px 20px rgba(0,0,0,.10);
  }
  .bslib-value-box.kpi .value-box-showcase .bi { font-size: 1.9rem; color: var(--kpi-accent, var(--brand)); }
  .bslib-value-box.kpi .value-box-title { font-weight: 700; letter-spacing: .2px; margin-bottom: 6px; }
  .bslib-value-box.kpi .value-box-value { font-size: 2.2rem; line-height: 1.08; }

  .kpi-primary { --kpi-accent: var(--kpi-primary); }
  .kpi-success { --kpi-accent: var(--kpi-success); }
  .kpi-warning { --kpi-accent: var(--kpi-warning); }
  .kpi-danger  { --kpi-accent: var(--kpi-danger); }
  .kpi-info    { --kpi-accent: var(--kpi-info); }

  @media (max-width: 768px) {
    .bslib-value-box.kpi { height: auto; min-height: 220px; }
    .bslib-value-box.kpi .value-box-value { font-size: 2rem; }
  }

  /* ---------------- Sidebar ---------------- */
  .bslib-sidebar-layout .sidebar, .bslib-sidebar {
    background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
    border: 1px solid var(--border); border-radius: 16px;
    box-shadow: 0 10px 24px rgba(0,0,0,.06);
    padding: 14px 14px 16px 14px; position: sticky;
    top: calc(var(--nav-height, 56px) + 16px);
    max-height: calc(100vh - (var(--nav-height, 56px) + 32px));
    overflow: auto;
  }
  .sb-title-row { display: flex; align-items: center; gap: 10px; margin: 2px 2px 10px 2px; }
  .sb-title-row .bi { font-size: 1.1rem; color: var(--brand); }
  .sb-title-row span { font-weight: 800; letter-spacing: .2px; color: #0f172a; }

  .sb-card { background: #fff; border: 1px solid var(--border); border-radius: 14px;
    padding: 12px 14px; margin-bottom: 12px; box-shadow: 0 2px 10px rgba(0,0,0,.03); }
  .sb-card-title{ font-size: .9rem; font-weight: 700; color: #0f172a; margin-bottom: 8px;
    display: flex; align-items: center; gap: 8px; }
  .sb-card-title::before{ content: ''; width: 8px; height: 8px; border-radius: 999px; background: var(--brand); }

  .bslib-sidebar .form-label, .bslib-sidebar .control-label,
  .sidebar .form-label, .sidebar .control-label{ font-weight: 600; color: #0f172a; }

  /* Borda degradê/hover da sidebar */
  .bslib-sidebar-layout .sidebar,
  .bslib-sidebar{
    border: 2px solid transparent;
    background:
      linear-gradient(180deg, #ffffff, #f8fafc) padding-box,
      linear-gradient(135deg,
        color-mix(in srgb, var(--brand) 60%, transparent),
        color-mix(in srgb, var(--nav-pill-active) 50%, transparent)
      ) border-box;
    border-radius: 18px;
    box-shadow:
      inset 0 0 0 1px rgba(255,255,255,.7),
      0 8px 26px rgba(2,8,20,.06);
    transition: box-shadow .2s ease, transform .2s ease;
  }
  .bslib-sidebar-layout .sidebar:hover,
  .bslib-sidebar:hover{
    transform: translateY(-2px);
    box-shadow:
      inset 0 0 0 1px rgba(255,255,255,.8),
      0 14px 32px rgba(2,8,20,.12);
  }

  /* ---------------- Rodapé + widgets ---------------- */
  footer { text-align: center; margin-top: 40px; color: var(--muted); font-size: 0.9rem;
    padding: 20px; border-top: 1px solid var(--border); }

  /* Gráficos ocupam 100% da largura do card */
  .card .html-widget, .card .html-widget-container, .card .dygraph-container, .card .dygraphs-g {
    width: 100% !important; max-width: 100% !important;
  }

  /* ---------------- Downloads (aba) ---------------- */
  .dl-grid {
    display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 16px;
  }
  .dl-card {
    background: #fff; border: 1px solid var(--border); border-radius: 16px;
    padding: 16px; box-shadow: 0 8px 18px rgba(0,0,0,.05);
  }
  .dl-card .dl-title {
    display: flex; align-items: center; gap: 10px; font-weight: 800; margin-bottom: 6px;
  }
  .dl-card .dl-desc { color: var(--muted); margin-bottom: 12px; }
  .dl-card .btn {
    width: 100%; padding: 14px 16px; font-weight: 700; border-radius: 12px;
  }
")

# ---------------------- UI ---------------------------------------------
ui <- page_navbar(
  title = div(
    style = "display:flex; align-items:center; gap:10px;",
    img(src = "logo3.png", height = "80px"),
    span("📈 Painel de Meningite")
  ),
  theme = bs_theme(bootswatch = "flatly", version = 5,
                   primary = "#0d6efd", base_font = font_google("Inter")),
  header = tags$head(tags$style(app_css)),
  
  sidebar = sidebar(
    width = 320,
    div(class = "sb-title-row", bs_icon("cloud-upload"), span("Atualização de dados")),
    div(class = "sb-card",
        div(class = "sb-card-title", "Enviar Meningite25 (DBF)"),
        fileInput("dbf2", "Selecione o arquivo MENINNET2025.DBF",
                  accept = c(".dbf"), buttonLabel = "Procurar...", placeholder = "Nenhum arquivo escolhido")
    ),
    div(class = "sb-title-row", bs_icon("sliders"), span("Filtros")),
    div(class = "sb-card",
        div(class = "sb-card-title", "Filtros"),
        uiOutput("anos_ui"),
        uiOutput("tipos_ui"),
        radioButtons(
          "filtro123", "Escopo:",
          choices = c("Meningite Geral" = "geral", "Meningite 1,2,3" = "t123"),
          selected = "geral"
        )
    ),
    div(class = "sb-card",
        div(class = "sb-card-title", "Parâmetros"),
        numericInput("pop", "População para incidência (denominador):",
                     value = 1350000, min = 1, step = 1000)
    ),
    div(class = "sb-footer", bs_icon("lightbulb"),
        span("Dica: ajuste os filtros para refinar KPIs e gráficos."))
  ),
  
  # ------------------ Visão Geral ------------------
  nav_panel(
    "Visão Geral",
    div(
      class = "pane-scroll",
      div(
        class = "kpi-grid",
        layout_columns(
          col_widths = c(3,3,3,3),
          value_box(title = "Casos Notificados(filtro atual)", value = textOutput("vb_casos"),
                    showcase = bs_icon("bar-chart-line"), theme = "primary", class = "kpi kpi-primary"),
          value_box(title = "Casos Confirmados", value = textOutput("vb_conf"),
                    showcase = bs_icon("check2-circle"), theme = "success", class = "kpi kpi-success"),
          value_box(title = "Óbitos por Meningite", value = textOutput("vb_obitos"),
                    showcase = bs_icon("x-octagon"), theme = "danger", class = "kpi kpi-danger"),
          value_box(title = "Letalidade (confirmados)", value = textOutput("vb_letalidade"),
                    showcase = bs_icon("heartbreak"), theme = "danger", class = "kpi kpi-danger")
        ),
        layout_columns(
          col_widths = c(3,3,3,3),
          value_box(title = "% confirmação", value = textOutput("vb_taxa_conf"),
                    showcase = bs_icon("percent"), theme = "info", class = "kpi kpi-info"),
          value_box(title = "Incidência (por 10 mil)", value = textOutput("vb_incidencia"),
                    showcase = bs_icon("graph-up-arrow"), theme = "warning", class = "kpi kpi-warning"),
          value_box(title = "% Meningocócica (entre casos)", value = textOutput("vb_pct_meningococica"),
                    showcase = bs_icon("pie-chart-fill"), theme = "info", class = "kpi kpi-info"),
          value_box(title = "Investigações abertas", value = textOutput("vb_invest_abertas"),
                    showcase = bs_icon("folder2-open"), theme = "primary", class = "kpi kpi-primary")
        )
      ),
      card(
        height = "560px", full_screen = TRUE, card_body_fill = TRUE,
        title = "Séries temporais",
        tabsetPanel(
          tabPanel("Mensal (linha)",  dygraphOutput("dy_mensal",  height = "400px")),
          tabPanel("Semanal (linha)", dygraphOutput("dy_semana",  height = "400px"))
        )
      ),
      layout_columns(
        col_widths = c(6,6),
        card(height = "520px", full_screen = TRUE, title = "Distribuição Mensal por Ano (boxplot)",
             card_body_fill = TRUE, plotOutput("plot_box_ano", height = "100%")),
        card(height = "520px", full_screen = TRUE, title = "Casos por Ano (barras)",
             card_body_fill = TRUE, plotOutput("plot_barras_ano", height = "100%"))
      ),
      layout_columns(
        col_widths = c(6,6),
        card(height = "520px", full_screen = TRUE, title = "Casos por Raça/Cor",
             card_body_fill = TRUE, plotOutput("plot_raca", height = "100%")),
        card(height = "520px", full_screen = TRUE, title = "Casos por Sexo",
             card_body_fill = TRUE, plotOutput("plot_sexo", height = "100%"))
      ),
      layout_columns(
        col_widths = c(6,6),
        card(height = "520px", full_screen = TRUE, title = "Casos por Escolaridade",
             card_body_fill = TRUE, plotOutput("plot_escolaridade", height = "100%")),
        card(height = "520px", full_screen = TRUE, title = "Casos por Tipo de Meningite",
             card_body_fill = TRUE, plotOutput("plot_tipo", height = "100%"))
      ),
      layout_columns(
        col_widths = c(6,6),
        card(height = "520px", full_screen = TRUE, title = "Casos por Faixa Etária",
             card_body_fill = TRUE, plotOutput("plot_faixa", height = "100%")),
        card(height = "520px", full_screen = TRUE, title = "Casos com Investigação Aberta",
             card_body_fill = TRUE, plotOutput("plot_investigacao", height = "100%"))
      )
    )
  ),
  
  # ------------------ Variações (3 linhas; cada uma com gráfico + tabela) ------------------
  nav_panel(
    "Variações",
    div(
      class = "pane-scroll",
      
      # Linha 1 — YoY
      card(
        title = tagList(bs_icon("thermometer-half"), " Variação % vs. mesmo mês do ano anterior (YoY)"),
        height = "600px", full_screen = TRUE, card_body_fill = TRUE,
        tabsetPanel(
          tabPanel("Gráfico", plotOutput("plot_var_yoy_heat", height = "480px")),
          tabPanel("Tabela",  htmlOutput("tab1"))
        )
      ),
      
      # Linha 2 — MoM (barras facetadas)
      card(
        title = tagList(bs_icon("bar-chart-line"), " Variação % mês a mês (MoM)"),
        height = "600px", full_screen = TRUE, card_body_fill = TRUE,
        tabsetPanel(
          tabPanel("Gráfico", plotOutput("plot_var_mom_facets", height = "480px")),
          tabPanel("Tabela",  htmlOutput("tab2"))
        )
      ),
      
      # Linha 3 — Acumulada YoY
      card(
        title = tagList(bs_icon("calendar-range"), " Variação % acumulada vs. mesmo período do ano anterior (YoY)"),
        height = "600px", full_screen = TRUE, card_body_fill = TRUE,
        tabsetPanel(
          tabPanel("Gráfico", plotOutput("plot_var_cum_yoy_heat", height = "480px")),
          tabPanel("Tabela",  htmlOutput("tab3"))
        )
      )
    )
  ),
  
  # ------------------ Dados ------------------
  nav_panel(
    "Dados",
    card(title = "Série Mensal (tabela)", full_screen = TRUE, DTOutput("dt_notifica")),
    card(title = "Base tratada (amostra)", full_screen = TRUE, DTOutput("dt_base"))
  ),
  
  # ------------------ Downloads ------------------
  nav_panel(
    "Downloads",
    card(
      title = "Arquivos para Download",
      card_body_fill = TRUE,
      div(class = "dl-grid",
          div(class = "dl-card",
              div(class = "dl-title", bs_icon("calendar3"), span("Série Mensal (CSV) – com filtro atual")),
              div(class = "dl-desc", "Casos, confirmados, óbitos e incidência por mês."),
              downloadButton("dl_notifica", "Baixar Série Mensal", class = "btn btn-primary")
          ),
          div(class = "dl-card",
              div(class = "dl-title", bs_icon("calendar-week"), span("Série Semanal (CSV) – com filtro atual")),
              div(class = "dl-desc", "Casos agregados por semana epidemiológica (ISO)."),
              downloadButton("dl_semana", "Baixar Série Semanal", class = "btn btn-primary")
          ),
          div(class = "dl-card",
              div(class = "dl-title", bs_icon("pie-chart-fill"), span("Meningocócica (CSV) – com filtro atual")),
              div(class = "dl-desc", "Recorte de Meningite Meningocócica/Meningococcemia por mês."),
              downloadButton("dl_meningococica", "Baixar Meningocócica", class = "btn btn-primary")
          ),
          div(class = "dl-card",
              div(class = "dl-title", bs_icon("table"), span("Base tratada (CSV) – com filtro atual")),
              div(class = "dl-desc", "Dados tratados, com variáveis categorizadas e faixas etárias."),
              downloadButton("dl_base", "Baixar Base Tratada", class = "btn btn-primary")
          )
      )
    )
  )
)

# ---------------------- Server ------------------------------------------
server <- function(input, output, session){
  
  # =============== Utilidades e tratamento de dados (dentro do server) ===============
  meses_pt <- c("janeiro","fevereiro","março","abril","maio","junho",
                "julho","agosto","setembro","outubro","novembro","dezembro")
  
  ler_dbf <- function(path = NULL, upload = NULL) {
    if (!is.null(upload) && !is.na(upload$datapath)) {
      rio::import(upload$datapath)
    } else if (!is.null(path) && file.exists(path)) {
      rio::import(path)
    } else {
      NULL
    }
  }
  
  .calc_idade_anos <- function(df){
    cn <- names(df)
    idade_num <- NA_real_
    if (all(c("NU_IDADE_N","TP_IDADE") %in% cn)) {
      val  <- suppressWarnings(as.numeric(df$NU_IDADE_N))
      tipo <- suppressWarnings(as.integer(df$TP_IDADE))
      fator <- dplyr::case_when(
        tipo == 1L ~ 1,           # anos
        tipo == 2L ~ 1/12,        # meses -> anos
        tipo == 3L ~ 1/365.25,    # dias  -> anos
        tipo == 4L ~ 1/8760,      # horas -> anos
        TRUE ~ NA_real_
      )
      idade_num <- val * fator
    } else if ("NU_IDADE_N" %in% cn) {
      idade_num <- suppressWarnings(as.numeric(df$NU_IDADE_N))
    } else if ("NU_IDADE" %in% cn) {
      idade_num <- suppressWarnings(as.numeric(df$NU_IDADE))
    } else {
      poss <- cn[grepl("IDADE", cn)]
      if (length(poss) > 0) idade_num <- suppressWarnings(as.numeric(df[[poss[1]]]))
    }
    ifelse(!is.na(idade_num) & idade_num >= 0 & idade_num <= 120, idade_num, NA_real_)
  }
  
  faixa_etaria_cuts <- function(idade){
    cut(idade,
        breaks = c(-Inf, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, Inf),
        labels = c("0–1", "1–4", "5–9", "10–14", "15–19", "20–29",
                   "30–39", "40–49", "50–59", "60–69", "70–79", "80+"),
        right = FALSE, ordered_result = TRUE)
  }
  
  .parse_dt <- function(x){
    if (inherits(x, "Date")) return(x)
    if (is.numeric(x)) {
      guess <- as.Date("1899-12-30") + as.integer(x)
      return(guess)
    }
    y <- suppressWarnings(lubridate::parse_date_time(
      x, orders = c("Ymd","Y-m-d","dmY","d/m/Y","d-m-Y","m/d/Y","mdY"),
      tz = "UTC", exact = FALSE
    ))
    as.Date(y)
  }
  
  prepara_dados <- function(d){
    req(nrow(d) > 0)
    nu_ano_num <- suppressWarnings(as.numeric(as.character(d$NU_ANO)))
    d <- d |>
      mutate(
        DT_NOTIFIC = .parse_dt(DT_NOTIFIC),
        semana_epi = ISOweek::ISOweek(DT_NOTIFIC),
        ano_data   = lubridate::year(DT_NOTIFIC),
        ano_sinan  = nu_ano_num,
        ano        = dplyr::coalesce(ano_data, ano_sinan),
        mes_num    = lubridate::month(DT_NOTIFIC),
        mes_nome   = factor(meses_pt[mes_num], levels = meses_pt, ordered = TRUE),
        SEXO = case_when(
          CS_SEXO == "M" ~ "Masculino",
          CS_SEXO == "F" ~ "Feminino",
          TRUE ~ NA_character_
        ),
        GESTANTE = case_when(
          CS_GESTANT == 1 ~ "1° Trimestre",
          CS_GESTANT == 2 ~ "2° Trimestre",
          CS_GESTANT == 3 ~ "3° Trimestre",
          CS_GESTANT == 4 ~ "Idade gestacional ignorada",
          CS_GESTANT == 5 ~ "Não",
          CS_GESTANT == 6 ~ "Não se aplica",
          CS_GESTANT == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        RACA = case_when(
          CS_RACA == 1 ~ "Branca",
          CS_RACA == 2 ~ "Preta",
          CS_RACA == 3 ~ "Amarela",
          CS_RACA == 4 ~ "Parda",
          CS_RACA == 5 ~ "Indígena",
          CS_RACA == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        ESCOLARIDADE = case_when(
          CS_ESCOL_N == '01' ~ "1ª a 4ª série incompleta do EF",
          CS_ESCOL_N == '02' ~ "4ª série completa do EF",
          CS_ESCOL_N == '03' ~ "5ª à 8ª série incompleta do EF",
          CS_ESCOL_N == '05' ~ "Ensino médio incompleto",
          CS_ESCOL_N == '04' ~ "Ensino fundamental completo",
          CS_ESCOL_N == '06' ~ "Ensino médio completo",
          CS_ESCOL_N == '07' ~ "Educação superior incompleta",
          CS_ESCOL_N == '08' ~ "Educação superior completa",
          CS_ESCOL_N == '09' ~ "Ignorado",
          CS_ESCOL_N == '10' ~ "Não se aplica",
          TRUE ~ NA_character_
        ),
        CEFALEIA = case_when(
          CLI_CEFALE == 1 ~ "Sim",
          CLI_CEFALE == 2 ~ "Não",
          CLI_CEFALE == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        FEBRE = case_when(
          CLI_FEBRE == 1 ~ "Sim",
          CLI_FEBRE == 2 ~ "Não",
          CLI_FEBRE == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        VOMITO = case_when(
          CLI_VOMITO == 1 ~ "Sim",
          CLI_VOMITO == 2 ~ "Não",
          CLI_VOMITO == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        OCORR_HOSP = case_when(
          ATE_HOSPIT == 1 ~ "Sim",
          ATE_HOSPIT == 2 ~ "Não",
          ATE_HOSPIT == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        CASO_CONFIRMADO = case_when(
          CLASSI_FIN == 1 ~ "Confirmado",
          CLASSI_FIN == 2 ~ "Descartado",
          TRUE ~ NA_character_
        ),
        TIPO_MENINGITE = case_when(
          CON_DIAGES == '01' ~ "Meningococcemia",
          CON_DIAGES == '02' ~ "Meningite Meningocócica",
          CON_DIAGES == '03' ~ "Meningite Meningocócica com Meningococcemia",
          CON_DIAGES == '04' ~ "Meningite Tuberculosa",
          CON_DIAGES == '05' ~ "Meningite por outras bactérias",
          CON_DIAGES == '06' ~ "Meningite não especificada",
          CON_DIAGES == '07' ~ "Meningite Asséptica",
          CON_DIAGES == '08' ~ "Meningite por outra etiologia",
          CON_DIAGES == '09' ~ "Meningite por Hemófilo",
          CON_DIAGES == '10' ~ "Meningite por Pneumococo",
          TRUE ~ NA_character_
        ),
        EVOLUCAO_CASO = case_when(
          EVOLUCAO == 1 ~ "Alta",
          EVOLUCAO == 2 ~ "Óbito por Meningite",
          EVOLUCAO == 3 ~ "Óbito por outra causa",
          EVOLUCAO == 9 ~ "Ignorado",
          TRUE ~ NA_character_
        ),
        investigacao_aberta = case_when(
          !is.na(DT_INVEST) &  is.na(DT_ENCERRA) ~ "Sim",
          !is.na(DT_INVEST) & !is.na(DT_ENCERRA) ~ "Não",
          is.na(DT_INVEST) ~ "Não",
          TRUE ~ NA_character_
        )
      )
    d$idade_anos   <- .calc_idade_anos(d)
    d$FAIXA_ETARIA <- faixa_etaria_cuts(d$idade_anos)
    d
  }
  # =============================================================================
  
  # ---------- Tema/cores p/ ggplot ----------
  brand  <- "#0ea5e9"
  brand2 <- "#22d3ee"
  theme_title_center <- theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.title.position = "plot"
  )
  theme_var <- theme_minimal(base_size = 16) + theme_title_center
  
  # ---------- Formatadores para dygraphs ----------
  pt_date <- htmlwidgets::JS("
    function(d){
      var dt = new Date(d);
      return dt.toLocaleDateString('pt-BR',{day:'2-digit',month:'short',year:'numeric'});
    }
  ")
  iso_week_fmt <- htmlwidgets::JS("
    function(d){
      var date = new Date(d);
      var target = new Date(Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), date.getUTCDate()));
      var dayNr = (target.getUTCDay() + 6) % 7;
      target.setUTCDate(target.getUTCDate() - dayNr + 3);
      var firstThursday = new Date(Date.UTC(target.getUTCFullYear(),0,4));
      dayNr = (firstThursday.getUTCDay() + 6) % 7;
      firstThursday.setUTCDate(firstThursday.getUTCDate() - dayNr + 3);
      var week = 1 + Math.floor((target - firstThursday) / (7*24*3600*1000));
      var year = target.getUTCFullYear();
      return year + '-W' + ('0' + week).slice(-2);
    }
  ")
  
  # ===================== Carga e JUNÇÃO dos dados ======================
  base10 <- reactive({
    d1 <- ler_dbf(path = "Dados/MENINNET10.DBF")
    validate(need(!is.null(d1), "Arquivo base 'MENINNET10.DBF' não encontrado em 'Dados/'."))
    d1
  })
  base25 <- reactiveVal(NULL)
  observeEvent(input$dbf2, {
    req(input$dbf2$datapath)
    
    d2 <- ler_dbf(upload = input$dbf2)
    validate(need(!is.null(d2) && nrow(d2) > 0, 
                  "Falha ao ler o MENINNET2025.DBF enviado ou arquivo vazio."))
    
    # --- helper: tenta obter ano a partir de NU_ANO ou de colunas de data ---
    get_year_vec <- function(df) {
      nms <- names(df)
      
      # 1) Tenta NU_ANO (num, char ou factor)
      if ("NU_ANO" %in% nms) {
        y <- suppressWarnings(as.numeric(as.character(df$NU_ANO)))
        return(y)
      }
      
      # 2) Tenta colunas de data comuns do MENINNET
      date_cols <- intersect(c("DT_NOTIFIC","DT_SIN_PRI","DT_DIGITA","DT_ENCERRA"), nms)
      if (length(date_cols) > 0) {
        to_date <- function(x) {
          if (inherits(x, "Date")) return(x)
          x_chr <- as.character(x)
          # remove não-dígitos e tenta ymd (ex.: 20250131) e dmy (ex.: 31/01/2025)
          x_num <- gsub("[^0-9]", "", x_chr)
          d <- suppressWarnings(lubridate::ymd(x_num, quiet = TRUE))
          if (all(is.na(d))) d <- suppressWarnings(lubridate::dmy(x_chr, quiet = TRUE))
          d
        }
        # usa a primeira coluna de data que produzir algum ano válido
        for (col in date_cols) {
          d <- to_date(df[[col]])
          y <- suppressWarnings(lubridate::year(d))
          if (any(!is.na(y))) return(y)
        }
      }
      
      # 3) Falhou: retorna NA
      rep(NA_real_, nrow(df))
    }
    
    anos <- get_year_vec(d2)
    validate(need(any(!is.na(anos)), 
                  "Não foi possível identificar o ano (NU_ANO ou datas inválidas/ausentes)."))
    
    d2$.__ano__ <- anos
    d2 <- d2[!is.na(d2$.__ano__) & d2$.__ano__ >= 2025, , drop = FALSE]  # troque para > 2025 se quiser estritamente acima
    
    validate(need(nrow(d2) > 0, "Nenhum registro com ano ≥ 2025 após o filtro."))
    
    # envia para o reativo/base do app
    base25(d2)
    
    showNotification(
      sprintf("MENINNET2025.DBF: %s registros com ano ≥ 2025.",
              format(nrow(d2), big.mark = ".", decimal.mark = ",")),
      type = "message"
    )
  })
  
  
  dados_raw <- reactive({
    d1 <- base10(); d2 <- base25()
    if (!is.null(d2)) dplyr::bind_rows(d1, d2) else d1
  })
  dados <- reactive(prepara_dados(dados_raw()))
  
  # ---------- UI dinâmico ----------
  output$anos_ui <- renderUI({
    anos <- sort(unique(dados()$ano))
    if (length(anos) == 0) return(NULL)
    selectizeInput(
      "anos", "Anos:", choices = anos, selected = anos, multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Selecione um ou mais anos")
    )
  })
  output$tipos_ui <- renderUI({
    tipos <- sort(na.omit(unique(dados()$TIPO_MENINGITE)))
    selectizeInput("tipos", "Tipo de meningite (opcional):",
                   choices = tipos, multiple = TRUE,
                   options = list(placeholder = "Todos"))
  })
  
  # ---------- Filtro aplicado (inclui NOVO filtro 1,2,3) ----------
  dados_filtro <- reactive({
    d <- dados()
    if (!is.null(input$anos)) d <- d |> dplyr::filter(ano %in% input$anos)
    if (!is.null(input$tipos) && length(input$tipos) > 0) {
      d <- d |> dplyr::filter(TIPO_MENINGITE %in% input$tipos)
    }
    if (!is.null(input$filtro123) && input$filtro123 == "t123") {
      alvo <- c(
        "Meningite Meningocócica",
        "Meningococcemia",
        "Meningite Meningocócica com Meningococcemia"
      )
      d <- d |> dplyr::filter(TIPO_MENINGITE %in% alvo)
    }
    d
  })
  
  # ---------- Séries agregadas ----------
  notifica_meningite <- reactive({
    d <- dados_filtro() |>
      mutate(data_mes = lubridate::floor_date(DT_NOTIFIC, unit = "month"),
             ano      = lubridate::year(data_mes),
             mes_num  = lubridate::month(data_mes),
             mes_nome = factor(meses_pt[mes_num], levels = meses_pt, ordered = TRUE)) |>
      group_by(ano, mes_num, mes_nome, data = data_mes) |>
      summarise(
        Casos = n(),
        Casos_Confirmados = sum(CASO_CONFIRMADO == "Confirmado", na.rm = TRUE),
        Obitos_Meningite  = sum(EVOLUCAO_CASO == "Óbito por Meningite", na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(ano, mes_num) |>
      mutate(incidencia = (Casos_Confirmados / input$pop) * 100000)
    d
  })
  
  semanal <- reactive({
    dados_filtro() |>
      mutate(
        data       = as.Date(DT_NOTIFIC),
        iso_year   = lubridate::isoyear(data),
        iso_week   = lubridate::isoweek(data),
        iso_label  = sprintf("%04d-W%02d-1", iso_year, iso_week),
        data_semana = ISOweek::ISOweek2date(iso_label)
      ) |>
      group_by(iso_year, iso_week, data_semana) |>
      summarise(
        Casos              = n(),
        Casos_Confirmados  = sum(CASO_CONFIRMADO == "Confirmado", na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(data_semana)
  })
  
  meningococica <- reactive({
    dados_filtro() |>
      dplyr::filter(TIPO_MENINGITE %in% c(
        "Meningite Meningocócica",
        "Meningococcemia",
        "Meningite Meningocócica com Meningococcemia"
      )) |>
      mutate(data_mes = lubridate::floor_date(DT_NOTIFIC, "month"),
             ano = lubridate::year(data_mes), mes_num = lubridate::month(data_mes),
             mes_nome = factor(meses_pt[mes_num], levels = meses_pt, ordered = TRUE)) |>
      group_by(ano, mes_num, mes_nome, TIPO_MENINGITE, data = data_mes) |>
      summarise(soma = n(), .groups = "drop") |>
      arrange(ano, mes_num)
  })
  
  # ---------- KPIs ----------
  output$vb_casos <- renderText({
    format(sum(notifica_meningite()$Casos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  output$vb_conf <- renderText({
    format(sum(notifica_meningite()$Casos_Confirmados, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  output$vb_incidencia <- renderText({
    d <- notifica_meningite()
    inc <- sum(d$Casos_Confirmados, na.rm = TRUE) / input$pop * 10000
    if (is.nan(inc)) return("—")
    scales::number(inc, accuracy = 0.1, decimal.mark = ",")
  })
  output$vb_obitos <- renderText({
    format(sum(notifica_meningite()$Obitos_Meningite, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  output$vb_taxa_conf <- renderText({
    d <- notifica_meningite()
    tx <- sum(d$Casos_Confirmados, na.rm = TRUE) / sum(d$Casos, na.rm = TRUE)
    if (is.nan(tx)) return("—")
    scales::percent(tx, accuracy = 0.1)
  })
  output$vb_letalidade <- renderText({
    d <- notifica_meningite()
    let <- sum(d$Obitos_Meningite, na.rm = TRUE) / sum(d$Casos_Confirmados, na.rm = TRUE)
    if (is.nan(let)) return("—")
    scales::percent(let, accuracy = 0.1)
  })
  output$vb_invest_abertas <- renderText({
    d <- dados_filtro()
    total <- sum(d$investigacao_aberta == "Sim", na.rm = TRUE)
    format(total, big.mark = ".", decimal.mark = ",")
  })
  output$vb_pct_meningococica <- renderText({
    d <- dados_filtro()
    if (nrow(d) == 0) return("—")
    alvo <- c(
      "Meningite Meningocócica",
      "Meningococcemia",
      "Meningite Meningocócica com Meningococcemia"
    )
    num <- sum(d$TIPO_MENINGITE %in% alvo, na.rm = TRUE)
    den <- nrow(d)
    tx <- ifelse(den > 0, num / den, NA_real_)
    if (is.na(tx)) "—" else scales::percent(tx, accuracy = 0.1)
  })
  
  # ---------- Séries (dygraph) ----------
  output$dy_mensal <- renderDygraph({
    d <- notifica_meningite(); req(nrow(d) > 0)
    serie <- xts(d[, c("Casos", "Casos_Confirmados")], order.by = d$data)
    colnames(serie) <- c("Casos", "Confirmados")
    
    dygraph(serie, main = "Casos e Confirmados (Mensal)") |>
      dyOptions(
        drawPoints = TRUE, pointSize = 2,
        colors = c("#1f77b4", "#e63946"),
        fillGraph = TRUE, fillAlpha = 0.1
      ) |>
      dyLegend(show = "always", labelsSeparateLines = TRUE, hideOnMouseOut = FALSE) |>
      dyAxis("x", axisLabelFormatter = pt_date, valueFormatter = pt_date) |>
      dyAxis("y", label = "Casos")
  })
  
  output$dy_semana <- renderDygraph({
    s <- semanal(); req(nrow(s) > 0)
    serie <- xts(s |> dplyr::select(Casos, Casos_Confirmados), order.by = s$data_semana)
    colnames(serie) <- c("Notificados", "Confirmados")
    
    dygraph(serie, main = "Casos por Semana Epidemiológica (ISO)") |>
      dyLegend(show = "always", labelsSeparateLines = TRUE, hideOnMouseOut = FALSE, width = 180) |>
      dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE, fillAlpha = 0.10) |>
      dySeries("Notificados",  label = "Notificados",  color = "#2563eb", strokeWidth = 2, fillGraph = TRUE) |>
      dySeries("Confirmados", label = "Confirmados", color = "#ef4444", strokeWidth = 2, fillGraph = TRUE) |>
      dyAxis("x", axisLabelFormatter = iso_week_fmt, valueFormatter = iso_week_fmt) |>
      dyAxis("y", label = "Casos")
  })
  
  # ---------- Gráficos (ggplot) ----------
  output$plot_box_ano <- renderPlot({
    d <- notifica_meningite(); req(nrow(d) > 0)
    ggplot(d, aes(x = factor(ano), y = Casos)) +
      geom_boxplot(fill = "#b3e5fc", color = "black", outlier.colour = "#ef4444") +
      geom_point(position = position_jitter(width = .15), alpha = .5, color = brand) +
      labs(title = "Distribuição Mensal por Ano (boxplot)", x = "Ano", y = "Casos (mensal)") +
      theme_classic(base_size = 14) + theme_title_center
  })
  
  output$plot_barras_ano <- renderPlot({
    d <- notifica_meningite() |> group_by(ano) |> summarise(Casos = sum(Casos), .groups = "drop")
    req(nrow(d) > 0)
    ggplot(d, aes(x = factor(ano), y = Casos)) +
      geom_col(fill = brand, color = "black") +
      geom_label(aes(label = Casos), vjust = -0.2, size = 3.8, label.size = 0, fill = "white") +
      labs(title = "Casos por Ano", x = "Ano", y = "Casos (total no ano)") +
      scale_y_continuous(expand = expansion(mult = c(0, .10))) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank()) + theme_title_center
  })
  
  output$plot_raca <- renderPlot({
    d <- dados_filtro() |> filter(!is.na(RACA)) |> count(RACA, name = "Casos") |> arrange(desc(Casos))
    req(nrow(d) > 0)
    ggplot(d, aes(x = reorder(RACA, Casos), y = Casos, fill = RACA)) +
      geom_col(color = "black") + coord_flip() +
      geom_label(aes(label = Casos), hjust = 1.02, size = 5, label.size = 0, fill = "white") +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(title = "Casos por Raça/Cor", x = "Raça/Cor", y = "Casos") +
      theme_classic(base_size = 15) +
      theme(panel.grid.minor = element_blank()) + theme_title_center
  })
  
  output$plot_sexo <- renderPlot({
    d <- dados_filtro() |> filter(!is.na(SEXO)) |> count(SEXO, name = "Casos")
    req(nrow(d) > 0)
    ggplot(d, aes(x = SEXO, y = Casos, fill = SEXO)) +
      geom_col(color = "black") +
      geom_label(aes(label = Casos), vjust = -0.2, size = 5, label.size = 0, fill = "white") +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(title = "Casos por Sexo", x = "Sexo", y = "Casos") +
      scale_y_continuous(expand = expansion(mult = c(0, .10))) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank()) + theme_title_center
  })
  
  output$plot_escolaridade <- renderPlot({
    d <- dados_filtro() |> filter(!is.na(ESCOLARIDADE)) |> count(ESCOLARIDADE, name = "Casos") |> arrange(desc(Casos))
    req(nrow(d) > 0)
    ggplot(d, aes(x = reorder(ESCOLARIDADE, Casos), y = Casos)) +
      geom_col(fill = brand, color = "black") + coord_flip() +
      geom_label(aes(label = Casos), hjust = 1.02, size = 5, label.size = 0, fill = "white") +
      labs(title = "Casos por Escolaridade", x = "Escolaridade", y = "Casos") +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank()) + theme_title_center
  })
  
  output$plot_tipo <- renderPlot({
    d <- dados_filtro() |> filter(!is.na(TIPO_MENINGITE)) |> count(TIPO_MENINGITE, name = "Casos") |> arrange(desc(Casos))
    req(nrow(d) > 0)
    ggplot(d, aes(x = reorder(TIPO_MENINGITE, Casos), y = Casos)) +
      geom_col(fill = brand2, color = "black") + coord_flip() +
      geom_label(aes(label = Casos), hjust = 1.02, size = 3.8, label.size = 0, fill = "white") +
      labs(title = "Casos por Tipo de Meningite", x = "Tipo de Meningite", y = "Casos") +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank()) + theme_title_center
  })
  
  output$plot_faixa <- renderPlot({
    d <- dados_filtro() |>
      filter(!is.na(FAIXA_ETARIA)) |>
      count(FAIXA_ETARIA, name = "Casos") |>
      arrange(as.integer(FAIXA_ETARIA))
    req(nrow(d) > 0)
    ggplot(d, aes(x = FAIXA_ETARIA, y = Casos)) +
      geom_col(fill = brand, color = "black") +
      geom_label(aes(label = Casos), vjust = -0.2, size = 3.8, label.size = 0, fill = "white") +
      labs(title = "Casos por Faixa Etária", x = "Faixa etária", y = "Casos") +
      scale_y_continuous(expand = expansion(mult = c(0, .10))) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank()) + theme_title_center
  })
  
  # ---------- Nova aba Variações: GRÁFICOS ----------
  output$plot_var_yoy_heat <- renderPlot({
    d <- notifica_meningite(); req(nrow(d) > 0)
    yoy <- d |>
      arrange(ano, mes_num) |>
      group_by(mes_num) |>
      mutate(variacao = (Casos - lag(Casos)) / lag(Casos) * 100) |>
      ungroup() |>
      mutate(lbl = ifelse(is.finite(variacao), paste0(sprintf('%.1f', variacao), '%'), ''))
    
    ggplot(yoy, aes(x = mes_nome, y = factor(ano), fill = variacao)) +
      geom_tile(color = "white") +
      geom_text(aes(label = lbl), size = 5.0) +
      scale_fill_gradient2(
        low = "#16a34a", mid = "#f8fafc", high = "#ef4444",
        midpoint = 0, na.value = "#e5e7eb", name = "Variação (%)"
      ) +
      labs(
        title   = "Variação % YoY por mês",
        x = "Mês", y = "Ano",
        caption = paste(
          "Como ler: cada célula compara o mês com o MESMO mês do ano anterior.",
          "Ex.: +15% em 'mar/2024' significa que mar/2024 teve 15% mais casos que mar/2023.",
          "Cores: verde = queda; vermelho = aumento. Branco = sem base comparável."
        )
      ) +
      theme_var +
      theme(
        panel.grid   = element_blank(),
        axis.text.x  = element_text(size = 13),
        axis.text.y  = element_text(size = 13),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 13),
        plot.title   = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 12, color = "#64748b", hjust = 0, margin = margin(t = 8))
      )
  })
  
  # MoM (barras facetadas por ano)
  output$plot_var_mom_facets <- renderPlot({
    d <- notifica_meningite(); req(nrow(d) > 0)
    
    mom <- d |>
      arrange(ano, mes_num) |>
      group_by(ano) |>
      mutate(variacao = (Casos - lag(Casos)) / lag(Casos) * 100) |>
      ungroup() |>
      mutate(
        lbl       = ifelse(is.finite(variacao), paste0(sprintf('%.1f', variacao), '%'), NA_character_),
        vjust_lbl = ifelse(!is.finite(variacao), 0.8, ifelse(variacao >= 0, -0.2, 1.2))
      )
    
    ggplot(mom, aes(x = mes_nome, y = variacao, fill = variacao >= 0)) +
      geom_hline(yintercept = 0, linewidth = 0.5) +
      geom_col(color = "black") +
      geom_text(
        aes(label = lbl, vjust = vjust_lbl),
        size = 4.6, fontface = "bold", na.rm = TRUE
      ) +
      facet_wrap(~ ano, scales = "free_x") +
      scale_fill_manual(
        values = c("TRUE" = "#ef4444", "FALSE" = "#2563eb"),
        labels = c("TRUE" = "Aumento (≥ 0%)", "FALSE" = "Queda (< 0%)"),
        name   = "Direção",
        na.translate = FALSE
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0.05, 0.12))) +
      labs(
        title = "Variação % MoM (por ano)",
        x = "Mês", y = "Variação (%)",
        caption = paste(
          "Como ler: cada barra compara o mês com o MÊS anterior do mesmo ano.",
          "Ex.: −8% em 'set/2024' = set/2024 teve 8% menos casos que ago/2024.",
          "Cores: vermelho = aumento; azul = queda."
        )
      ) +
      theme_var +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x  = element_text(size = 13),
        axis.text.y  = element_text(size = 13),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 13),
        plot.title   = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 12, color = "#64748b", hjust = 0, margin = margin(t = 8))
      )
  })
  
  # ---------- Tabelas de variação ----------
  format_pct <- function(x) {
    ifelse(is.na(x), "-", paste0(formatC(x, format = "f", digits = 2, decimal.mark = ","), "%"))
  }
  color_neg <- function(x) {
    ifelse(x == "-", "-", ifelse(grepl('^\\s*-', x), sprintf('<span style=\"color:red;\">%s</span>', x), x))
  }
  
  output$tab1 <- renderUI({
    d <- notifica_meningite() |>
      arrange(ano, mes_num) |>
      group_by(mes_nome) |>
      mutate(variacao = (Casos - lag(Casos)) / lag(Casos) * 100,
             variacao = ifelse(is.infinite(variacao), NA_real_, variacao)) |>
      ungroup() |>
      select(ano, mes_nome, variacao) |>
      pivot_wider(names_from = mes_nome, values_from = variacao) |>
      mutate(across(-ano, format_pct)) |>
      mutate(across(-ano, color_neg))
    html <- knitr::kable(d, "html", caption = "Variação % vs. mesmo mês do ano anterior", escape = FALSE) |>
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped","hover","condensed","responsive")) |>
      kableExtra::row_spec(0, bold = TRUE, color = "black", background = "#4CAF50") |>
      kableExtra::column_spec(1, bold = TRUE) |>
      kableExtra::kable_classic_2()
    HTML(as.character(html))
  })
  
  output$tab2 <- renderUI({
    d <- notifica_meningite() |>
      arrange(ano, mes_num) |>
      mutate(variacao = (Casos - lag(Casos)) / lag(Casos) * 100,
             variacao = ifelse(is.infinite(variacao), NA_real_, variacao)) |>
      select(ano, mes_nome, variacao) |>
      pivot_wider(names_from = mes_nome, values_from = variacao) |>
      mutate(across(-ano, format_pct)) |>
      mutate(across(-ano, color_neg))
    html <- knitr::kable(d, "html", caption = "Variação % vs. mês anterior", escape = FALSE) |>
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped","hover","condensed","responsive")) |>
      kableExtra::row_spec(0, bold = TRUE, color = "black", background = "#4CAF50") |>
      kableExtra::column_spec(1, bold = TRUE) |>
      kableExtra::kable_classic_2()
    HTML(as.character(html))
  })
  
  output$tab3 <- renderUI({
    d <- notifica_meningite() |>
      arrange(ano, mes_num) |>
      group_by(ano) |>
      mutate(casos_acum = cumsum(Casos)) |>
      ungroup() |>
      group_by(mes_nome) ||
      mutate(variacao = (casos_acum - lag(casos_acum)) / lag(casos_acum) * 100,
             variacao = ifelse(is.infinite(variacao), NA_real_, variacao)) |>
      ungroup() |>
      select(ano, mes_nome, variacao) |>
      pivot_wider(names_from = mes_nome, values_from = variacao) |>
      mutate(across(-ano, format_pct)) |>
      mutate(across(-ano, color_neg))
    html <- knitr::kable(d, "html", caption = "Variação % acumulada vs. mesmo período do ano anterior", escape = FALSE) |>
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped","hover","condensed","responsive")) |>
      kableExtra::row_spec(0, bold = TRUE, color = "black", background = "steelblue") |>
      kableExtra::column_spec(1, bold = TRUE) |>
      kableExtra::kable_classic_2()
    HTML(as.character(html))
  })
  
  # ---------- Tabelas (DT) ----------
  output$dt_notifica <- renderDT({
    notifica_meningite() |>
      select(ano, mes = mes_nome, Casos, Casos_Confirmados, incidencia, data) |>
      datatable(
        extensions = "Buttons",
        options = list(dom = "Bfrtip", buttons = c("copy","csv","excel"), pageLength = 12),
        rownames = FALSE
      )
  })
  output$dt_base <- renderDT({
    head(dados_filtro(), 500) |>
      datatable(options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # ---------- Downloads ----------
  output$dl_notifica <- downloadHandler(
    filename = function() sprintf("serie_mensal_meningite_%s.csv", Sys.Date()),
    content  = function(file) readr::write_csv2(notifica_meningite(), file)
  )
  output$dl_semana <- downloadHandler(
    filename = function() sprintf("serie_semanal_meningite_%s.csv", Sys.Date()),
    content  = function(file) {
      s <- semanal() |> select(iso_year, iso_week, data_semana, Casos)
      readr::write_csv2(s, file)
    }
  )
  output$dl_meningococica <- downloadHandler(
    filename = function() sprintf("meningite_meningococica_%s.csv", Sys.Date()),
    content  = function(file) readr::write_csv2(meningococica(), file)
  )
  output$dl_base <- downloadHandler(
    filename = function() sprintf("base_tratada_meningite_%s.csv", Sys.Date()),
    content  = function(file) readr::write_csv2(dados_filtro(), file)
  )
}

# ---------------------- Run App ----------------------------------------
shinyApp(ui, server)
