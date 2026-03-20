# ========================================
# Construção de indicadores municipais
# ========================================

# ------------------------
# Pacotes
# ------------------------
library(dplyr)

# ------------------------
# Funções auxiliares
# ------------------------
source("scripts/00_variaveis.R")
source("scripts/01_funcoes_auxiliares.R")

# ------------------------
# 1. Carregar base
# ------------------------

setores_rj <- readRDS("dados/derivados/base_setores_2022_rj_com_infos.rds")

# ------------------------
# 2. Inicializar objeto que será preenchendo com indicadores
# ------------------------

indicadores_municipais <- NULL

# ------------------- INDICADORES ----------------------

# ------------------------
# -----% Idosos
# ------------------------

vars_num <- vars_total_pop_60_ou_mais
vars_den <- vars_total_pop

indicadores_municipais <- adicionar_indicador(
  "% Idosos dentre população total",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)


# ------------------------
# % Pretos ou Pardos
# ------------------------

vars_num <- vars_pop_negra
vars_den <- vars_pop_total_cor_ou_raca

indicadores_municipais <- adicionar_indicador(
  "% Pretas ou Pardos dentre população total",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização geral
# ------------------------

vars_num <-  vars_total_pop_15_ou_mais_alfa
vars_den <- vars_tot_pop_15_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização população total",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Idosos alfabetizados (dentre alfabetizados)
# ------------------------

vars_num <- vars_total_pop_60_ou_mais_alfa
vars_den <- vars_total_pop_15_ou_mais_alfa

indicadores_municipais <- adicionar_indicador(
  "% de idosos alfabetizados dentre alfabetizados",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização entre idosos
# ------------------------

vars_num <- vars_total_pop_60_ou_mais_alfa
vars_den <- vars_total_pop_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização dentre idosos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização entre idosos negros
# ------------------------

vars_num <- vars_negros_pop_60_ou_mais_alfa
vars_den <- vars_negros_pop_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização dentre idosos pretos ou pardos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# % idosos negros
# ------------------------

vars_num <- vars_negros_pop_60_ou_mais
vars_den <- vars_total_pop_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% de idosos pretos ou pardos sobre total de idosos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# % 70+ entre idosos
# ------------------------

vars_num <- vars_tot_pop_70_ou_mais
vars_den <- vars_total_pop_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% de idosos com 70 anos ou mais sobre total de idosos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# % 80+ entre idosos
# ------------------------

vars_num <- vars_tot_pop_80_ou_mais
vars_den <- vars_total_pop_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% de idosos com 80 anos ou mais sobre total de idosos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# % 70+ população total
# ------------------------

vars_num <- vars_tot_pop_70_ou_mais
vars_den <- vars_total_pop

indicadores_municipais <- adicionar_indicador(
  "% de idosos com 70 anos ou mais sobre total da população",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# % 80+ população total
# ------------------------

vars_num <- vars_tot_pop_80_ou_mais
vars_den <- vars_total_pop

indicadores_municipais <- adicionar_indicador(
  "% de idosos com 80 anos ou mais sobre total da população",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Mulheres entre idosos
# ------------------------

vars_num <- tot_mulheres_60_ou_mais
vars_den <- vars_total_pop_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% sexo feminino dentre idosos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Mulheres 70+
# ------------------------

vars_num <- tot_mulheres_70_ou_mais
vars_den <- vars_tot_pop_70_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% sexo feminino dentre pessoas com 70 anos ou mais",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Mulheres 80+
# ------------------------

vars_num <- tot_mulheres_80_ou_mais
vars_den <- vars_tot_pop_80_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% sexo feminino dentre pessoas com 80 anos ou mais",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Mulheres 15+
# ------------------------

vars_num <- tot_mulheres_15_ou_mais
vars_den <- vars_tot_pop_15_ou_mais

indicadores_municipais <- adicionar_indicador(
  "% sexo feminino dentre pessoas com 15 anos ou mais",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização feminina
# ------------------------

vars_num <- tot_alfa_mulheres_15_ou_mais
vars_den <- tot_mulheres_15_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização feminina geral",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização masculina
# ------------------------

vars_num <- tot_alfa_homens_15_ou_mais
vars_den <- tot_homens_15_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização masculina geral",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização feminina idosa
# ------------------------

vars_num <- tot_alfa_mulheres_60_ou_mais
vars_den <- tot_mulheres_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização feminina entre idosas",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Alfabetização masculina idoso
# ------------------------

vars_num <- tot_alfa_homens_60_ou_mais
vars_den <- tot_homens_60_ou_mais

indicadores_municipais <- adicionar_indicador(
  "Taxa de alfabetização masculina idosos",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)


# ------------------------
# Razão de dependência
# ------------------------

vars_num <- vars_total_pop_60_ou_mais
vars_den <- vars_tot_pop_15_a_59

indicadores_municipais <- adicionar_indicador(
  "Razão de dependência",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)

# ------------------------
# Índice de envelhecimento
# ------------------------

vars_num <- vars_total_pop_60_ou_mais
vars_den <- vars_tot_pop_0_a_14

indicadores_municipais <- adicionar_indicador(
  "Índice envelhecimento",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)


# ------------------------
# Índice de envelhecimento 65+
# ------------------------

vars_num <- vars_total_pop_65_ou_mais
vars_den <- vars_tot_pop_0_a_14

indicadores_municipais <- adicionar_indicador(
  "Índice envelhecimento 65+",
  indicadores_municipais,
  setores_rj,
  calcular_taxa,
  vars_num,
  vars_den
)


# ------------------------
# 3. Salvar resultados
# ------------------------

dir.create("dados/derivados", showWarnings = FALSE)

write.csv(
  indicadores_municipais,
  "resultados/indicadores_municipais.csv",
  fileEncoding = "latin1",
  row.names = FALSE
)

# ------------------------
# Vendo tabela de resultados 
# ------------------------

View(indicadores_municipais)

# ------------------------
# Exportando base para uso do QGIS
# ------------------------

sf::st_write(base_mapa2, "base_mapa2.gpkg")

# ------------------------
# Exportando base para uso do QGIS
# ------------------------


base_mapa2 %>% 
    dplyr::filter(tipo =="favela") %>% 
    dplyr::arrange(desc(perc_idosos)) %>%
    dplyr::select(CD_BAIRRO, nome, perc_idosos)



base_mapa2 %>% 
    dplyr::filter(tipo !="favela") %>% 
    dplyr::arrange(desc(perc_idosos)) %>%
    dplyr::select(CD_BAIRRO, nome, perc_idosos)


setores_rj %>% 
  dplyr::filter(grepl("Vila Abrolhos", NM_FCU)) %>% 
  dplyr::select(NM_FCU, NM_BAIRRO)

