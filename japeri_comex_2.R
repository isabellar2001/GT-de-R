library(basedosdados)
library(bit64)
library(tidyverse)

basedosdados::set_billing_id("EXEMPLO ")

#exportações
tibble(
  query = c(
    "SELECT * FROM `basedosdados.br_me_comex_stat.municipio_exportacao` WHERE id_municipio = '3302270'")) %>% 
  
  mutate(resultados = map(query, read_sql)) -> queries

queries %>% pull(resultados) %>% 
  reduce(left_join) -> export_municipios

#importações
tibble(
  query = c(
    "SELECT * FROM `basedosdados.br_me_comex_stat.municipio_importacao` WHERE id_municipio = '3302270'")) %>% 
  
  mutate(resultados = map(query, read_sql)) -> queries

queries %>% pull(resultados) %>% 
  reduce(left_join) -> import_municipios


# fazer com índice dólar
# corrigindo os tipos
export_municipios$ano <- as.integer(export_municipios$ano)
export_municipios$valor_fob_dolar <- as.integer(export_municipios$valor_fob_dolar)
export_municipios$peso_liquido_kg <- as.integer(export_municipios$peso_liquido_kg)
import_municipios$ano <- as.integer(import_municipios$ano)
import_municipios$valor_fob_dolar <- as.integer(import_municipios$valor_fob_dolar)
import_municipios$peso_liquido_kg <- as.integer(import_municipios$peso_liquido_kg)

#filtrando para o ano mais recente
import_municipios_2021 <- import_municipios %>% filter(ano == 2021)
export_municipios_2021 <- export_municipios %>% filter(ano == 2021)


# gráfico da evolução do volume em peso das exportações
export_municipios %>% 
  group_by(ano) %>% 
  summarise(sum_peso = sum(peso_liquido_kg)) %>% 
  ggplot(aes(x= ano, y = sum_peso)) +
  geom_line()

# gráfico da evolução do volume em peso das importações
import_municipios %>% 
  group_by(ano) %>% 
  summarise(sum_peso = sum(peso_liquido_kg)) %>% 
  ggplot(aes(x= ano, y = sum_peso)) +
  geom_line()

# vendo o volume de exportações em 2021 por classificação do bem exportado
id_ano_valor_fob_export <- export_municipios_2021 %>% 
  group_by(id_sh4) %>% 
  summarise(sum_valor_fob_id = sum(valor_fob_dolar))

# item com maior valor exportado de Japeri em 2021 foi produto de beleza

id_ano_valor_fob_import <- import_municipios_2021 %>% 
  group_by(id_sh4) %>% 
  summarise(sum_valor_fob_id = sum(valor_fob_dolar))

# item com maior valor importado de Japeri em 2021 Ácidos graxos (gordos) monocarboxílicos industriais; óleos ácidos de refinação;


#gráfico da balança comercial de japeri ao longo dos anos
# série não deflacionada
tabela_export_japeri <- export_municipios %>% 
  group_by(ano) %>% 
  summarise(sum_valor_fob_export = sum(valor_fob_dolar),
            sum_peso_export = sum(peso_liquido_kg))

tabela_import_japeri <- import_municipios %>% 
  group_by(ano) %>% 
  summarise(sum_valor_fob_import = sum(valor_fob_dolar),
            sum_peso_import = sum(peso_liquido_kg))

tabela_unificada <- left_join(tabela_export_japeri, tabela_import_japeri, by = "ano") %>% 
  mutate(balanca_comercial = sum_valor_fob_export - sum_valor_fob_import)

ggplot(data = tabela_unificada, aes(x= ano, y = balanca_comercial)) +
  geom_line()