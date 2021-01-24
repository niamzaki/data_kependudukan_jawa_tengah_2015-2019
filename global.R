library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(GGally)
library(glue)
library(DT)
library(rsconnect)
library(data.table)

options(shiny.maxRequestSize=200*1024^2)
options(scipen = 999)

grb_mod <- readRDS("grb_mod.rds")

data_jateng <- readRDS("data_jateng_RDS.RDS")

data_jumlah <- data_jateng %>% 
  group_by(tahun) %>% 
  summarise(jumlah=sum(jumlah_penduduk),
            jumlah_mskn=sum(kemiskinan),
            jumlah_snts=sum(angka_sanitasi))

kstp_prov <- data_jateng %>% 
  group_by(tahun) %>% 
  summarise(angka_akses_sanitasi=median(akses_sanitasi),
            tingkat_pengangguran_terbuka=median(tpt),
            angka_kemiskinan=median(persentase_kemiskinan))

kstp_prov_long <- pivot_longer(kstp_prov,cols = c("angka_akses_sanitasi","tingkat_pengangguran_terbuka","angka_kemiskinan")) %>% 
  mutate(angka=str_to_title(str_replace_all(name,"_"," "))) %>% 
  select(-c(name)) %>% 
  mutate(name=angka)

apm_prov <- data_jateng %>% 
  group_by(tahun) %>% 
  summarise(apm_sd=median(apm_sd),
            apm_smp=median(apm_smp),
            apm_sma=median(apm_sma))

apm_prov_long <- pivot_longer(apm_prov,cols = c("apm_sd","apm_smp","apm_sma")) %>%
  mutate(angka=str_to_upper(str_replace_all(name,"_"," "))) %>% 
  select(-c(name)) %>% 
  mutate(name=angka)

kstp_kab <- data_jateng %>% 
  group_by(tahun,kabupaten) %>% 
  summarise(angka_akses_sanitasi=median(akses_sanitasi),
            tingkat_pengangguran_terbuka=median(tpt),
            angka_kemiskinan=median(persentase_kemiskinan))

kstp_kab_long <- pivot_longer(kstp_kab,cols = c("angka_akses_sanitasi","tingkat_pengangguran_terbuka","angka_kemiskinan")) %>% 
  mutate(angka=str_to_title(str_replace_all(name,"_"," "))) %>% 
  select(-c(name)) %>% 
  mutate(name=angka)

apm_kab <- data_jateng %>% 
  group_by(tahun,kabupaten) %>% 
  summarise(apm_sd=median(apm_sd),
            apm_smp=median(apm_smp),
            apm_sma=median(apm_sma))

apm_kab_long <- pivot_longer(apm_kab,cols = c("apm_sd","apm_smp","apm_sma")) %>%
  mutate(angka=str_to_upper(str_replace_all(name,"_"," "))) %>% 
  select(-c(name)) %>% 
  mutate(name=angka)

data_scatter <- data_jateng %>%
  mutate(Tingkat_Pengangguran_Terbuka=angka_tpt,
         Kepadatan=kepadatan,
         Jumlah_Penduduk=jumlah_penduduk,
         Tahun=tahun,
         Kemiskinan=kemiskinan,
         Angka_Akses_Sanitasi=angka_sanitasi,
         APM_SD=apm_sd,
         APM_SMP=apm_smp,
         APM_SMA=apm_sma) %>%
  select(c("kabupaten","luas","Tingkat_Pengangguran_Terbuka","Kepadatan","Jumlah_Penduduk","Tahun","Kemiskinan",
           "Angka_Akses_Sanitasi","APM_SD","APM_SMP","APM_SMA"))
data_scatter_copy <- copy(data_scatter)
colnames(data_scatter_copy) <- gsub(pattern = "[_]", replacement = " ", colnames(data_scatter))

for_choose <- data_scatter %>% 
  select(-c("kabupaten","luas","Tahun"))


