# PACOTES ----

library(tidyverse)
library(bit64)
library(ggmap)
library(basedosdados)
library(tmap)
library(sf)
library(maptools)
library(rgdal)
library(sp)
library(gpclib)
library(rgeos)
library(mapproj)

# INSTALANDO OS DADOS ----

# obter dados do base dos dados (não é necessário, mas pode ser interessante para fazer análises adicionais)
# precisa colocar billing_id usando esse site https://console.cloud.google.com/bigquery?p=basedosdados&page=project&pli=1&project=temporal-field-323402

basedosdados::set_billing_id(" EXEMPLO   ")


# precisa colocar o seu cartão de crédito para fazer o geocode (função que obtém latitude e longitude dos pontos)
register_google(key = " EXEMPLOOO ")

# obtendo os dados do base dos dados (https://basedosdados.org/)
tibble(
  query = c(
    "SELECT escola.ano, escola.sigla_uf, 
    escola.id_municipio, escola.rede, escola.id_escola, escola.tipo_situacao_funcionamento,
    escola.internet,
    escola.banda_larga,
    FROM `basedosdados.br_inep_censo_escolar.escola` as escola 
    WHERE escola.id_municipio = '3302270'",
    
    "SELECT geobr.id_municipio, geobr.geometria,
    FROM `basedosdados.br_geobr_mapas.municipio` as geobr 
    WHERE geobr.id_municipio = '3302270'")) %>% 
  
  mutate(resultados = map(query, read_sql)) -> queries

queries %>% pull(resultados) %>% 
  reduce(left_join) -> japeri

japeri_atual <- japeri %>% filter(ano == 2020)


# dados desse site: https://inepdata.inep.gov.br/analytics/saw.dll?dashboard
# necessário para obter o endereço das escolas
escolas_japeri <- read.csv("Tabela da lista das escolas.csv", sep = ";")


# dados desse site: http://downloads.ibge.gov.br/downloads_geociencias.htm
# essa base contém o desenho do mapa dos municípios
# clicar em organizacao_do_territorio > malhas_territoriais > malhas_municipais > municipio_2021 > Brasil > BR > BR_MUNICIPIOS_2021.zip
shpmun <- readOGR("BR_Municipios_2021/BR_Municipios_2021.shp", stringsAsFactors=FALSE, encoding="UTF-8")

# transformando em um formato legível para o R
mapaMun <- fortify(shpmun, region = 'CD_MUN')

# filtrando pelo código do IBGE do município de Japeri
mapaMun_japeri <- mapaMun %>% filter(id == "3302270")




# JUNTANDO AS INFORMAÇÕES ----

# como temos algumas colunas em que latitude e longitude são NA, preferi fazer o geocode dos endereços
geocode_teste_escolas_japeri <- cbind(escolas_japeri, geocode(escolas_japeri$Endereço))

# renomeando código.inep para id_escola para facilitar a junção das tabelas
colnames(geocode_teste_escolas_japeri)[3] <- "id_escola"

#transformar id_escola em character ao invés de integer pelo mesmo motivo:
geocode_teste_escolas_japeri$id_escola <- as.character(geocode_teste_escolas_japeri$id_escola)

geocode_escolas_japeri <- full_join(japeri_atual,geocode_teste_escolas_japeri)




# GRÁFICOS ----

# Desenhando o mapa de Japeri com as escolas coloridas de acordo com a rede (estadual, municipal, privada ou federal)
ggplot(mapaMun_japeri) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank()) +
  geom_point(data = geocode_escolas_japeri, mapping = aes(x=lon, y = lat, color = rede))