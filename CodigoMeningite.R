

# Carregando pacotes ------------------------------------------------------

pacman::p_load(tidyverse, rio, dygraphs, xts, ggiraph, forcats, patchwork, kableExtra, ISOweek, prophet,
               xgboost, googlesheets4)


# Carregando dados --------------------------------------------------------

Dados1 <- rio::import("Dados/MENINNET10.DBF")
Dados2 <- rio::import("Dados/MENINNET2025.DBF")






Dados_Meningite <- rbind(Dados1, Dados2)

# Atualização do dashboard Looker Studio ----------------------------------
# 
# Dados_Meningite_Atualizado <- dados_2025 |>
#   select(-TP_NOT, -NM_CONTATO, -NU_TELEFON, -TEL_CONTAT,
#          -NU_DDD_TEL, -SOUNDEX, -ID_AGRAVO, -NM_PACIENT,
#          -FONETICA_N, -END_CONTAT)
# 
# write.csv2(Dados_Meningite_Atualizado, file = "Meningite Atualizado.csv")
# 

attach(Dados_Meningite)


Dados_Meningite <- Dados_Meningite |>
  mutate(
    semana_epi = epiweek(DT_NOTIFIC),
    SEXO = case_when(
      CS_SEXO == "M" ~ "Masculino",
      CS_SEXO == "F" ~ "Feminino",
      TRUE ~ NA_character_),
    GESTANTE = case_when(
      CS_GESTANT == 1 ~ "1° Trimestre",
      CS_GESTANT == 2 ~ "2° Trimestre",
      CS_GESTANT == 3 ~ "3° Trimestre",
      CS_GESTANT == 4 ~ "Idade gestacional ignorada",
      CS_GESTANT == 5 ~ "Não",
      CS_GESTANT == 6 ~ "Não se aplica",
      CS_GESTANT == 9 ~ "Ignorado",
      TRUE ~ NA_character_),
    RACA = case_when(
      CS_RACA == 1 ~ "Branca",
      CS_RACA == 2 ~ "Preta",
      CS_RACA == 3 ~ "Amarela",
      CS_RACA == 4 ~ "Parda",
      CS_RACA == 5 ~ "Indígena",
      CS_RACA == 9 ~ "Ignorado",
      TRUE ~ NA_character_),
    ESCOLARIDADE = case_when(
      CS_ESCOL_N == "01" ~ "1ª a 4ª série incompleta do EF",
      CS_ESCOL_N == "02" ~ "4ª série completa do EF",
      CS_ESCOL_N == "03" ~ "5ª à 8ª série incompleta do EF",
      CS_ESCOL_N == "05" ~ "Ensino médio incompleto",
      CS_ESCOL_N == "04" ~ "Ensino fundamental completo",
      CS_ESCOL_N == "06" ~ "Ensino médio completo",
      CS_ESCOL_N == "07" ~ "Educação superior incompleta",
      CS_ESCOL_N == "08" ~ "Educação superior completa",
      CS_ESCOL_N == "09" ~ "Ignorado",
      CS_ESCOL_N == "10" ~ "Não se aplica",
      TRUE ~ NA_character_),
    CEFALEIA = case_when(CLI_CEFALE == 1 ~ "Sim",
                         CLI_CEFALE == 2 ~ "Não",
                         CLI_CEFALE == 9 ~ "Ignorado",
                         TRUE ~ NA_character_),
    FEBRE = case_when(CLI_FEBRE == 1 ~ "Sim",
                      CLI_FEBRE == 2 ~ "Não",
                      CLI_FEBRE == 9 ~ "Ignorado",
                      TRUE ~ NA_character_),
    VOMITO = case_when(CLI_VOMITO == 1 ~ "Sim",
                      CLI_VOMITO == 2 ~ "Não",
                      CLI_VOMITO == 9 ~ "Ignorado",
                      TRUE ~ NA_character_),
    OCORR_HOSP  = case_when(ATE_HOSPIT == 1 ~ "Sim",
                            ATE_HOSPIT == 2 ~ "Não",
                            ATE_HOSPIT == 9 ~ "Ignorado",
                            TRUE ~ NA_character_),
    CASO_CONFIRMADO = case_when(CLASSI_FIN == 1 ~ "Confirmado",
                                CLASSI_FIN == 2 ~ "Descartado",
                                TRUE ~ NA_character_),
    TIPO_MENINGITE = case_when(
      CON_DIAGES == "01"  ~ "Meningococcemia",
      CON_DIAGES == "02"  ~ "Meningite Meningocócica",
      CON_DIAGES == "03"  ~ "Meningite Meningocócica com Meningococcemia",
      CON_DIAGES == "04"  ~ "Meningite Tuberculosa",
      CON_DIAGES == "05"  ~ "Meningite por outras bactérias",
      CON_DIAGES == "06"  ~ "Meningite não especificada",
      CON_DIAGES == "07"  ~ "Meningite Asséptica",
      CON_DIAGES == "08"  ~ "Meningite por outra etiologia",
      CON_DIAGES == "09"  ~ "Meningite por Hemófilo",
      CON_DIAGES == "10" ~ "Meningite por Pneumococo",
      TRUE ~ NA_character_
  ),
    EVOLUCAO_CASO  = case_when(EVOLUCAO == 1 ~ "Alta",
                               EVOLUCAO == 2 ~ "Óbito por Meningite",
                               EVOLUCAO == 3 ~ "Óbito por outra causa",
                               EVOLUCAO == 9 ~ "Ignorado",
                               TRUE ~ NA_character_),
  investigacao_aberta = case_when(
    !is.na(DT_INVEST) & is.na(DT_ENCERRA) ~ "Sim", 
    !is.na(DT_INVEST) & !is.na(DT_ENCERRA) ~ "Não",
    is.na(DT_INVEST) ~ "Não",                             
    TRUE ~ NA_character_                
  ),
  ano = year(DT_NOTIFIC),
  mes = month(DT_NOTIFIC, label = TRUE, abbr = FALSE)
  )




# Gráficos ----------------------------------------------------------------



notifica_meningite <- Dados_Meningite |> 
  group_by(ano, mes) |> 
  summarise(Casos = n(), 
            Casos_Confirmados = sum(CASO_CONFIRMADO == "Confirmado", na.rm = TRUE),
            .groups = "drop") |> 
  mutate(data = make_date(ano, mes, 1),
         incidencia = (Casos_Confirmados/1350000)*100000)


write.csv2(notifica_meningite, file = "Meningococica.csv", fileEncoding = "latin1")
 
# semana epidemiologicayj vxxxx

casos_semana_epi <- Dados_Meningite |> 
  group_by(ano, semana_epi) |> 
  summarise(Casos = n(), 
            Casos_Confirmados = sum(CASO_CONFIRMADO == "Confirmado", na.rm = TRUE),
            .groups = "drop") |> 
  mutate(incidencia = (Casos_Confirmados/1350000)*100000,
         data = as.Date("2020-01-01") + (semana_epi - 1) * 7)



casos_meningite_xts <- xts(
  notifica_meningite[, c("Casos", "Casos_Confirmados")], # Selecionar colunas de interesse
  order.by = notifica_meningite$data
)


incidencia_xts <- xts(
  notifica_meningite[,c("incidencia", "Casos_Confirmados")], # Selecionar colunas de interesse
  order.by = notifica_meningite$data)


# 
# # Criar uma coluna de data para o eixo X (ano-mês)
# notifica_meningite$data <- as.Date(paste(notifica_meningite$ano, 
#                                          notifica_meningite$mes, "01", sep = "/"), 
#                                    format = "%Y/%m/%d")






####################################


# # Previsão de casos -------------------------------------------------------
# attach(Dados_Meningite)
# 
# meningite <- Dados_Meningite |>
#   filter(
#     ano >= 2020) |> 
#   mutate(ano_mes = as.Date(paste0(year(DT_NOTIFIC), "-", 
#                                   month(DT_NOTIFIC), "-01"))) |> 
#   group_by(ano_mes) |> 
#   summarise(casos = n())
# 
#  
# 
# TS_Meningite <- ts(meningite$casos, start = c(2020,1), frequency = 12)
# 
# TS_Meningite |> plot()
# 
# modelo_meningite <- forecast::auto.arima(TS_Meningite, trace = TRUE)
# 
# previsao_meningite <- forecast::forecast(modelo_meningite, h = 6)
# previsao_meningite |> plot(main = "Previsão de Casos de Meningite")
# 
# 
# ajuste_meningite <- seasonal::seas(TS_Meningite)
# ajuste_meningite |> plot()
# 
# meningite_df <- data.frame(ajuste_meningite$data)
# 
# TS_Meningite_Ajustado <- ts(meningite_df$final, start = c(2020, 1), frequency = 12)
# 
# prev <- forecast::forecast(forecast::auto.arima(TS_Meningite_Ajustado), h = 12)
# 
# prev |> plot()



# Tabela de Variação ------------------------------------------------------

tabela1 <- notifica_meningite |> 
  arrange(ano, match(mes, c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
                            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"))) |> 
  group_by(mes) |> 
  mutate(variacao_mes_ano_anterior = (Casos - lag(Casos)) / lag(Casos) * 100) |> 
  ungroup() |> 
  select(ano, mes, variacao_mes_ano_anterior) |> 
  pivot_wider(names_from = mes, values_from = variacao_mes_ano_anterior) |> 
  mutate(across(-ano, ~ ifelse(is.na(.), "-", sprintf("%.2f%%", .)))) |>
  mutate(across(-ano, ~ ifelse(. == "-", "-", 
                               ifelse(grepl("-", .), 
                                      sprintf('<span style="color:red;">%s</span>', .), .)))) |> 
  kable("html", caption = "Variação Percentual de Casos Comparada ao Mesmo Mês do Ano Anterior", escape = FALSE) |> 
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) |> 
  row_spec(0, bold = TRUE, color = "black", background = "#4CAF50") |> 
  column_spec(1, bold = TRUE) |> 
  kable_classic_2()





tabela2 <- notifica_meningite |> 
  arrange(ano, match(mes, c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
                            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"))) |> 
  mutate(variacao_mes_anterior = (Casos - lag(Casos)) / lag(Casos) * 100) |> 
  ungroup() |> 
  select(ano, mes, variacao_mes_anterior) |> 
  pivot_wider(names_from = mes, values_from = variacao_mes_anterior) |> 
  mutate(across(-ano, ~ ifelse(is.na(.), "-", sprintf("%.2f%%", .)))) |>
  mutate(across(-ano, ~ ifelse(. == "-", "-", 
                               ifelse(grepl("-", .), 
                                      sprintf('<span style="color:red;">%s</span>', .), .)))) |> 
  kable("html", caption = "Variação Percentual de Casos Comparada ao Mês Anterior", escape = FALSE) |> 
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) |> 
  row_spec(0, bold = TRUE, color = "black", background = "#4CAF50") |> 
  column_spec(1, bold = TRUE) |> 
  kable_classic_2()




tabela3 <- notifica_meningite |> 
  arrange(ano, match(mes, c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
                            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"))) |> 
  group_by(ano) |> 
  mutate(casos_acumulados = cumsum(Casos)) |>  # Soma acumulada dos casos no ano
  ungroup() |> 
  group_by(mes) |> 
  mutate(variacao_periodo_ano_anterior = (casos_acumulados - lag(casos_acumulados)) / lag(casos_acumulados) * 100) |> 
  ungroup() |> 
  select(ano, mes, variacao_periodo_ano_anterior) |> 
  pivot_wider(names_from = mes, values_from = variacao_periodo_ano_anterior) |> 
  mutate(across(-ano, ~ ifelse(is.na(.), "-", sprintf("%.2f%%", .)))) |>
  mutate(across(-ano, ~ ifelse(. == "-", "-", 
                               ifelse(grepl("-", .), 
                                      sprintf('<span style="color:red;">%s</span>', .), .)))) |> 
  kable("html", caption = "Variação Percentual do Período Acumulado em Relação ao Mesmo Período do Ano Anterior", escape = FALSE) |> 
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) |> 
  row_spec(0, bold = TRUE, color = "black", background = "steelblue") |> 
  column_spec(1, bold = TRUE) |> 
  kable_classic_2()







# # Semana Epidemiologica ---------------------------------------------------
# 
# meningite_epi <- Dados_Meningite |> 
#   filter(ano >= 2020) |> 
#   group_by(semana_epi, ano) |> 
#   summarise(soma = n(), .groups = "drop") |> 
#   mutate(data_inicio_semana = ISOweek2date(paste0(ano, "-W", sprintf("%02d", semana_epi), "-1"))) |> 
#   arrange(data_inicio_semana)
# 
# 
# TS_Meningite_EPI <- ts(meningite_epi$soma, 
#                        start = c(min(meningite_epi$ano), min(meningite_epi$semana_epi)), 
#                        frequency = 52)
# TS_Meningite_EPI |> plot()
# 
# # Modelo
# pacf(TS_Meningite_EPI)
# acf(TS_Meningite_EPI)
# 
# modelo_menin_epi <- forecast::tbats(menin_adj_ts)
# modelo_menin_epi |> plot()
# 
# prev_menin_epi <- forecast::forecast(modelo_menin_epi, h = 10)
# prev_menin_epi |> plot()


# Teste com prophet -------------------------------------------------------

# 
# menin_prophet <- data.frame(ds = meningite_epi$data_inicio_semana, y = meningite_epi$soma)
# modelo_menin_prophet <- prophet(menin_prophet)
# 
# 
# futuro_menin <- make_future_dataframe(modelo_menin_prophet, periods = 52, freq = "week")
# 
# previsa_menin_prophet <- predict(modelo_menin_prophet, futuro_menin)
# 
# plot(modelo_menin_prophet, previsa_menin_prophet)
# prophet_plot_components(modelo_menin_prophet, previsa_menin_prophet)
# 
# 
# 
# ggplot() +
#   geom_line(data = menin_prophet, aes(x = ds, y = y), color = "blue", size = 1) +  # menin_prophet históricos em azul
#   geom_line(data = previsa_menin_prophet, aes(x = as.Date(ds), y = yhat), color = "red", 
#             size = 1, linetype = "dashed") +  # Previsões futuras em vermelho
#   geom_ribbon(data = previsa_menin_prophet, aes(x = as.Date(ds),
#                                                 ymin = yhat_lower, ymax = yhat_upper), 
#               alpha = 0.2, fill = "red") +  # Intervalo de confiança
#   geom_vline(xintercept = as.numeric(max(menin_prophet$ds)), 
#              linetype = "dotted", color = "black", size = 1) +  # Linha vertical separando passado e futuro
#   labs(title = "Previsão com Prophet", x = "Data", y = "Valores") +
#   theme_minimal()
# 





# QCC ---------------------------------------------------------------------



# Criar boxplot dos casos de meningite por mês
ggplot(notifica_meningite, aes(x = as.factor(ano), y = Casos)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.shape = 16) + 
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Adiciona pontos individuais
  labs(title = "Distribuição dos Casos de Meningite por Ano",
       x = "Mês",
       y = "Número de Casos") +
  theme_classic()


# 
# notifica_meningite |> 
#   tidyr::drop_na(TIPO_MENINGITE) |> 
#   streamgraph("TIPO_MENINGITE", "Casos", "data", width=800, height=500) |> 
#   sg_axis_x(tick_interval=1, tick_format="%Y-%m") |> 
#   sg_fill_tableau() |> 
#   sg_legend(show=TRUE, label = "names: ")
# 

# Graficos ----------------------------------------------------------------



Meningococica <- Dados_Meningite |> 
  filter(TIPO_MENINGITE %in% c("Meningite Meningocócica", 
                               "Meningococcemia", 
                               "Meningite Meningocócica com Meningococcemia"))



dados_plot <- Meningococica %>% 
  filter(ano >= 2020) |> 
  group_by(ano, mes, TIPO_MENINGITE) %>% 
  summarise(soma = n(), .groups = "drop") %>% 
  mutate(
    mes = str_to_title(as.character(mes)),
    data = dmy(paste("01", mes, ano))
  )

# Calcula média e desvio padrão globais
stats <- dados_plot %>% 
  summarise(
    media = mean(soma),
    desvio = sd(soma)
  )

# Adiciona limites ao dataframe
dados_plot <- dados_plot %>%
  mutate(
    limite_superior = stats$media + stats$desvio,
    limite_inferior = stats$media - stats$desvio,
    media_geral = stats$media
  )


# Gráfico de barras com valores e linhas de controle
barra_meningococica <- ggplot(dados_plot, aes(x = data, y = soma, fill = TIPO_MENINGITE)) +
  geom_col(position = "stack") +
  geom_text(aes(label = soma), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  geom_hline(aes(yintercept = media_geral), 
             linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = limite_superior), 
             linetype = "dotted", color = "red") +
  geom_hline(aes(yintercept = limite_inferior), 
             linetype = "dotted", color = "blue") +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "6 month") +
  labs(
    title = "Gráfico de Controle de Meningite por Tipo (Barras Empilhadas)",
    x = "Data",
    y = "Número de Casos",
    fill = "Tipo de Meningite"
  ) +
  theme_minimal() +
  theme(legend.position = "top")













