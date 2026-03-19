# Condições de vida dos idosos em FCUs no Rio de Janeiro (CD 2022)

## Objetivo
Esse projeto dá suporte para a comparação entre o perfil demográfico de envelhecimento entre favelas e áreas urbanas de não favela do município do Rio de Janeiro, com dados do censo de 2022 (IBGE).

## Estrutura

- `dados/brutos`: bases originais
- `dados/derivados`: bases geradas pelo próprio script utilizadas no cálculo de indicadores
- `scripts`: scripts do projeto
- `resultados`: Imagens e tabelas gerados pelo script

## Pipeline

1. Construção da base 
2. Geração de mapas
3. Geração de tabelas

## Dados

Os dados utilizados neste projeto foram obtidos a partir do Censo Demográfico 2022 do IBGE. 
Foram utilizados os seguintes arquivos:

- Agregados por setores censitários – cor ou raça  
- Agregados por setores censitários – alfabetização  

- Fonte: IBGE (2022)  
- Página de download:  
https://www.ibge.gov.br/estatisticas/sociais/saude/22827-censo-demografico-2022.html?=&t=downloads

Os arquivos foram utilizados em sua forma original, sem modificações prévias. No script foram filtrados os dados referentes ao município do Rio de Janeiro e tratados os dados faltantes ('X')

Além disso, os arquivos dos contornos dos bairros e dos setores censitários foram adquiridos através do endereço web:
https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html

Os arquivos de contorno territorial e os arquivos originais do censo não estão presentes no repositório por restrições de tamanho.

Explicitamente, o endereço para a malha territorial de setores censitárias foi encontrada neste endereço web:
[link para malha territorial setores censitários](
https://www.ibge.gov.br/geociencias/downloads-geociencias.html?caminho=organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2022/setores/shp/UF) 
e os arquivos SHAPE File de bairros por UF foi encontrado neste endereço web:
[link para malha territorial bairros](
https://www.ibge.gov.br/geociencias/downloads-geociencias.html?caminho=organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2022/bairros/shp/UF)





