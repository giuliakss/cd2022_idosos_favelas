# ========================================
# Funções auxiliares interdependentes (cada função depende da anterior)
# ========================================



# --------------------------------------------------
# Função: tratar_colunas
# Descrição: padroniza colunas selecionadas, convertendo
# valores para numérico e tratando casos específicos.
#
# Entrada:
#   - df: data.frame
#   - colunas: vetor com nomes das colunas a tratar
#
# Saída:
#   - data.frame com as colunas especificadas tratadas
# --------------------------------------------------

tratar_colunas <- function(df, colunas) {
  colunas_existentes <- colunas[colunas %in% names(df)]
  colunas_validas <- colunas_existentes[sapply(df[colunas_existentes], is.character)]
  
  df %>%
    mutate(across(
      all_of(colunas_validas),
      ~ as.numeric(
        ifelse(
          is.na(.), 0,
          ifelse(. == "X", 3, .)
        )
      )
    ))
}


# --------------------------------------------------
# Função: tratar_base_taxa
# Descrição: prepara base para cálculo de taxa,
# somando variáveis do numerador e denominador por linha.
#
# Entrada:
#   - df: data.frame
#   - vars_num: vetor de variáveis do numerador
#   - vars_denom: vetor de variáveis do denominador
#
# Saída:
#   - data.frame com as colunas especificadas somadas
# --------------------------------------------------
tratar_base_taxa <- function(df, vars_num, vars_denom) {
  
  df <- tratar_colunas(df, c(vars_num, vars_denom))
  
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      numerador = sum(dplyr::c_across(dplyr::all_of(vars_num)), na.rm = TRUE),
      denominador = sum(dplyr::c_across(dplyr::all_of(vars_denom)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}


# --------------------------------------------------
# Função: calcular_taxa
# Descrição: calcula a taxa agregada como a soma do
# numerador dividida pela soma do denominador.
#
# Entrada:
#   - df: data.frame
#   - vars_num: vetor de variáveis do numerador
#   - vars_denom: vetor de variáveis do denominador
#
# Saída:
#   - valor numérico da taxa
# --------------------------------------------------
calcular_taxa <- function(df, vars_num, vars_denom) {
  
  df_tratado <- tratar_base_taxa(df, vars_num, vars_denom)
  print("numerador é:")
  print(sum(df_tratado$numerador))
  print("denominador é:")
  print(sum(df_tratado$denominador))
  sum(df_tratado$numerador) / sum(df_tratado$denominador)
}


# --------------------------------------------------
# Função: calcular_taxas_dentro_fora
# Descrição: calcula taxas para três grupos:
# favela, urbano sem favela e urbano total.
#
# Entrada:
#   - df: data.frame com variáveis territoriais
#   - func: função de cálculo de taxa
#   - vars_num: vetor de variáveis do numerador
#   - vars_denom: vetor de variáveis do denominador
#
# Saída:
#   - vetor nomeado com taxas para cada grupo
# --------------------------------------------------
calcular_taxas_dentro_fora <- function(df, func, vars_num, vars_denom) {
  
  df_favela <- df %>% dplyr::filter(CD_TIPO == "1")
  df_urbano <- df %>% dplyr::filter(CD_SIT %in% c("1", "2", "3"))
  df_urbano_sem_fav <- df_urbano %>% 
    dplyr::filter(!is.na(CD_TIPO) & CD_TIPO != "1")
  
  taxas <- c(
    func(df_favela, vars_num, vars_denom),
    func(df_urbano_sem_fav, vars_num, vars_denom),
    func(df_urbano, vars_num, vars_denom)
  )
  
  names(taxas) <- c("favela", "urbano_sem_fav", "urbano")
  
  return(taxas)
}

# Melhorar: otimizar filtros (não precisaria recalcular toda chamada)

# --------------------------------------------------
# Função: adicionar_indicador
# Descrição: adiciona uma nova linha de indicador
# (em percentual) à tabela de indicadores.
#
# Entrada:
#   - descricao: nome do indicador
#   - df_indicadores: tabela existente (ou NULL)
#   - df: base de dados
#   - func: função de cálculo de taxa
#   - vars_num: vetor de variáveis do numerador
#   - vars_denom: vetor de variáveis do denominador
#
# Saída:
#   - data.frame de indicadores atualizado com novo indicador
# --------------------------------------------------
adicionar_indicador <- function(descricao, df_indicadores, df, func, vars_num, vars_denom) {
  
  # remove geometria se existir
  df <- sf::st_drop_geometry(df)

  taxas <- calcular_taxas_dentro_fora(df, func, vars_num, vars_denom)
  print(descricao)
  print(taxas)
  nova_linha <- data.frame(
    descricao = descricao,
    favela = round(taxas["favela"] * 100, 1),
    urbano_sem_fav = round(taxas["urbano_sem_fav"] * 100, 1),
    urbano = round(taxas["urbano"] * 100, 1),
    stringsAsFactors = FALSE
  )
  
  if (is.null(df_indicadores)) {
    return(nova_linha)
  } else {
    return(dplyr::bind_rows(df_indicadores, nova_linha))
  }
}

