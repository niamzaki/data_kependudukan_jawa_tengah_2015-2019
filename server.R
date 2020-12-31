function(input,output){
    #Tab 1
    #Peta Kepadatan
    output$density_map <- renderLeaflet({
        ss2 <- data_jateng %>% 
            filter(tahun==input$tahun)
        
        dt_gr <- ss2 %>% 
            left_join(grb_mod,by=c("kabupaten"="NAME_2"))
        dt_gr_sf <- dt_gr %>% sf::st_as_sf()
        
        mybins <- c( 0,500,1250,2500,4000,6000,7500,8500, Inf)
        mypalette <- colorBin(palette = "YlOrRd", domain = dt_gr_sf$kepadatan, 
                              na.color = "transparent", bins = mybins)
        
        mytext <- paste(dt_gr_sf$kabupaten) %>% lapply(htmltools::HTML)
        
        popup_shape <- paste("<h3><b>", dt_gr_sf$kabupaten, "</b></h3>", 
                             "Kepadatan Penduduk: ", round(dt_gr_sf$kepadatan,2), " jiwa/km\u00B2 <br>", 
                             "Persentase Akses Sanitasi Layak: ", dt_gr_sf$akses_sanitasi," % <br>",
                             "Persentase Penduduk Miskin:", dt_gr_sf$persentase_kemiskinan," %<br>",
                             "Persentase Pengangguran Terbuka: ", dt_gr_sf$tpt," % <br>",
                             "Angka Partisipasi Murni SD/MI: ", dt_gr_sf$apm_sd," % <br>",
                             "Angka Partisipasi Murni SMP/MTs: ", dt_gr_sf$apm_smp," % <br>",
                             "Angka Partisipasi Murni SMA/MA: ", dt_gr_sf$apm_sma," % <br>",
                             sep = "")
        m <- leaflet(dt_gr_sf) %>% addProviderTiles("Esri.WorldTopoMap") %>% 
            setView(lat = -7,lng = 110,zoom = 8) %>% addPolygons(fillColor = ~mypalette(kepadatan),
                                                                 color = "green", dashArray = "3", fillOpacity = 0.6,
                                                                 weight = 1, label = mytext, labelOptions = labelOptions(style = list(`font-weight` = "normal", 
                                                                                                                                      padding = "3px 8px"), textsize = "13px", direction = "auto"), 
                                                                 popup = popup_shape) %>% addLegend(pal = mypalette, 
                                                                                                    values = ~diff, opacity = 0.9, title = paste("Kepadatan", 
                                                                                                                                                 "<br>", "Penduduk (orang/Km\u00B2)"), position = "topleft")
        
        m
    })
    #Tab 1
    #Peta kemiskinan
    output$kemiskinan <- renderLeaflet({
        ss2 <- data_jateng %>% 
            filter(tahun==input$tahun)
        
        dt_gr <- ss2 %>% 
            left_join(grb_mod,by=c("kabupaten"="NAME_2"))
        dt_gr_sf <- dt_gr %>% sf::st_as_sf()
        
        mybins <- c( 0,5,10,15,20,25,100)
        mypalette <- colorBin(palette = "YlOrRd", domain = dt_gr_sf$persentase_kemiskinan, 
                              na.color = "transparent", bins = mybins)
        
        mytext <- paste(dt_gr_sf$kabupaten) %>% lapply(htmltools::HTML)
        
        popup_shape <- paste("<h3><b>", dt_gr_sf$kabupaten, "</b></h3>", 
                             "Persentase Penduduk Miskin:", dt_gr_sf$persentase_kemiskinan," %<br>",
                             "Kepadatan Penduduk: ", round(dt_gr_sf$kepadatan,2), " jiwa/km\u00B2 <br>", 
                             "Persentase Akses Sanitasi Layak: ", dt_gr_sf$akses_sanitasi," % <br>",
                             "Persentase Pengangguran Terbuka: ", dt_gr_sf$tpt," % <br>",
                             "Angka Partisipasi Murni SD/MI: ", dt_gr_sf$apm_sd," % <br>",
                             "Angka Partisipasi Murni SMP/MTs: ", dt_gr_sf$apm_smp," % <br>",
                             "Angka Partisipasi Murni SMA/MA: ", dt_gr_sf$apm_sma," % <br>",
                             sep = "")
        m <- leaflet(dt_gr_sf) %>% addProviderTiles("Esri.WorldTopoMap") %>% 
            setView(lat = -7,lng = 110,zoom = 8) %>% addPolygons(fillColor = ~mypalette(persentase_kemiskinan), 
                                                                 color = "green", dashArray = "3", fillOpacity = 0.6, 
                                                                 weight = 1, label = mytext, labelOptions = labelOptions(style = list(`font-weight` = "normal", 
                                                                                                                                      padding = "3px 8px"), textsize = "13px", direction = "auto"), 
                                                                 popup = popup_shape) %>% addLegend(pal = mypalette, 
                                                                                                    values = ~diff, opacity = 0.9, title = paste("Persentase", 
                                                                                                                                                 "<br>", "Kemiskinan (%)"), position = "topleft")
        
        m
    })
    #Tab 1
    #Value Box
    output$jumlah_penduduk <- renderValueBox({
        jml <- data_jumlah %>% 
            filter(tahun==input$tahun)
        
        valueBox(value = formatC(jml$jumlah,format = "f",big.mark = ",",digits = 0), subtitle = "Jumlah Penduduk", 
                 icon = icon("users"))
    })
    output$jumlah_penduduk_miskin <- renderValueBox({
        jml_mskn <- data_jumlah %>% 
            filter(tahun==input$tahun)
        
        valueBox(value = formatC(jml_mskn$jumlah_mskn,format = "f",big.mark = ",",digits = 0), subtitle = "Jumlah Penduduk Miskin", 
                 icon = icon("frown"))
    })
    output$akses_sanitasi_layak <- renderValueBox({
        jml_sn <- data_jumlah %>% 
            filter(tahun==input$tahun)
        
        valueBox(value = formatC(jml_sn$jumlah_snts,format = "f",big.mark = ",",digits = 0), subtitle = "Jumlah Rumah Tangga yang Memiliki Akses Terhadap Sanitasi Layak", 
                 icon = icon("water"))
    })
    #Tab 1 peringkat data per kabupaten
    output$ranking <- renderPlotly({
        ss2 <- data_jateng %>%
            select(kepadatan,kemiskinan,angka_tpt,angka_sanitasi,tahun,kabupaten) %>% 
            filter(tahun==input$tahun_r)
        
        kpdt <- ggplot(data=ss2,aes(x = reorder(kabupaten,desc(kepadatan)),y = kepadatan,fill=kepadatan,
                                    text=glue("{kabupaten}
                                            Kepadatan Penduduk: {round(kepadatan,2)} jiwa/km\u00B2")))+
            geom_col(show.legend = F)+
            theme(axis.text.x = element_text(angle = 30))+
            labs(x="Nama Kabupaten/Kota",y="Kepadatan Penduduk",caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        kms <- ggplot(data=ss2,aes(x = reorder(kabupaten,desc(kemiskinan)),y = kemiskinan,fill=kemiskinan,
                                   text=glue("{kabupaten}
                                            Jumlah Penduduk Miskin: {kemiskinan} jiwa")))+
            geom_col(show.legend = F)+
            theme(axis.text.x = element_text(angle = 30))+
            labs(x="Nama Kabupaten/Kota",y="Jumlah Penduduk Miskin",caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        tpt <- ggplot(data=ss2,aes(x = reorder(kabupaten,desc(angka_tpt)),y = angka_tpt,fill=angka_tpt,
                                   text=glue("{kabupaten}
                                            Jumlah Pengangguran Terbuka: {angka_tpt} jiwa")))+
            geom_col(show.legend = F)+
            theme(axis.text.x = element_text(angle = 30))+
            labs(x="Nama Kabupaten/Kota",y="Jumlah Pengangguran Terbuka",caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        snt <- ggplot(data=ss2,aes(x = reorder(kabupaten,desc(angka_sanitasi)),y = angka_sanitasi,fill=angka_sanitasi,
                                   text=glue("{kabupaten}
                                            Jumlah Rumah Tangga Mendapat Akses Sanitasi Layak: {angka_sanitasi} jiwa")))+
            geom_col(show.legend = F)+
            theme(axis.text.x = element_text(angle = 30))+
            labs(x="Nama Kabupaten/Kota",y="Angka Akses Sanitasi Layak",caption = "Sumber: BPS Provinsi Jawa Tengah")
        
    if(input$kategori=="Kepadatan Penduduk"){
        rnk=kpdt
    }else if(input$kategori=="Angka Kemiskinan"){
        rnk=kms
    }else if(input$kategori=="Tingkat Pengangguran Terbuka"){
        rnk=tpt
    }else if(input$kategori=="Akses Sanitasi Layak"){
        rnk=snt
        
    }
       
        ggplotly(rnk, tooltip = "text")
    })
    #TAb2
    #tren keseluruhan jateng
    output$trn_prov <- renderPlotly({
        trn_kms_prov <- ggplot(data = kstp_prov_long,aes(x = tahun,y = value,color=name,group=name,
                                                         text=glue("Tahun: {tahun} 
                                                                   {str_to_title(name)}: {value} %")))+
            geom_line(aes(fill=name),show.legend = F)+geom_point(aes(fill=name),show.legend = F)+
            labs(x="Tahun",y="Persentase",caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        apm_prv <- ggplot(data = apm_prov_long,aes(x = tahun,y = value,color=name,group=name,
                                                   text=glue("Tahun: {tahun} 
                                                             {str_to_upper(name)}: {value} %")))+
            geom_line(aes(fill=name),show.legend = F)+geom_point(aes(fill=name),show.legend = F)+
            labs(x="Tahun",y="Persentase",caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        if(input$kategori_a=="Data Sosial Kependudukan"){
            data_apm=trn_kms_prov
        }else{
            data_apm=apm_prv
        }
        
        ggplotly(data_apm,tooltip = "text")
    })
    #Tab 2
    #tren data kependudukan per kabupaten
    output$trn_kab <- renderPlotly({
        kstp <- kstp_kab_long %>% 
            filter(kabupaten==input$kategori_kbptn)
        apm_kab_fltr <- apm_kab_long %>% 
            filter(kabupaten==input$kategori_kbptn)
        
        trn_kms_kab <- ggplot(data = kstp,aes(x = tahun,y = value,color=name,group=name,
                                                       text=glue("{str_to_title(name)}
                                                                 Kabupaten/Kota: {kabupaten} 
                                                                 Tahun: {tahun} 
                                                                 Persentase: {value} %")))+
            geom_line(aes(fill=name),show.legend = F)+geom_point(aes(fill=name),show.legend = F)+
            labs(x="Tahun",y="Persentase",caption = "Sumber: BPS Provinsi Jawa Tengah")
        apm_kab_jt <- ggplot(data = apm_kab_fltr,aes(x = tahun,y = value,color=name,group=name,
                                                     text=glue("{str_to_upper(name)}
                                                               Kabupaten/Kota: {kabupaten}
                                                               Tahun: {tahun}
                                                               {value} %")))+
            geom_line(aes(fill=name),show.legend = F)+geom_point(aes(fill=name),show.legend = F)+
            labs(x="Tahun",y="Persentase",caption = "Sumber: BPS Provinsi Jawa Tengah")
        if(input$kategori_kab=="Data Sosial Kependudukan"){
            kab=trn_kms_kab
        }else{
            kab=apm_kab_jt
        }
        
        ggplotly(kab,tooltip = "text")
    })
    #value box tab 2A no.1
    output$tren_sanitasi_prov <- renderValueBox({
        sanitasi_prov <- (tail(kstp_prov$angka_akses_sanitasi,1)-head(kstp_prov$angka_akses_sanitasi,1))/head(kstp_prov$angka_akses_sanitasi,1)*100
        apm_sd_prov <- (tail(apm_prov$apm_sd,1)-head(apm_prov$apm_sd,1))/head(apm_prov$apm_sd,1)*100
        
        valueBox(value = paste0(round(ifelse(input$kategori_a=="Data Sosial Kependudukan",sanitasi_prov,apm_sd_prov),2),"%"), 
                 subtitle = ifelse(input$kategori_a=="Data Sosial Kependudukan",
                                   "Pertumbuhan Angka Akses Sanitasi Layak","Pertumbuhan APM SD"),
                 icon = icon(ifelse(input$kategori_a=="Data Sosial Kependudukan","water","child")),
                 color = "green")
        
    })
    #value box tab 2A no.2
    output$tren_kemiskinan_prov <- renderValueBox({
        kemiskinan_prov <- (tail(kstp_prov$angka_kemiskinan,1)-head(kstp_prov$angka_kemiskinan,1))/head(kstp_prov$angka_kemiskinan,1)*100
        apm_smp_prov <- (tail(apm_prov$apm_smp,1)-head(apm_prov$apm_smp,1))/head(apm_prov$apm_smp,1)*100
        
        valueBox(value = paste0(round(ifelse(input$kategori_a=="Data Sosial Kependudukan",kemiskinan_prov,apm_smp_prov),2),"%"), 
                 subtitle = ifelse(input$kategori_a=="Data Sosial Kependudukan",
                                   "Pertumbuhan Angka Kemiskinan","Pertumbuhan APM SMP"), 
                 icon = icon(ifelse(input$kategori_a=="Data Sosial Kependudukan","sad-tear","school")),
                 color = "green")
        
    })
    #value box tab 2A no.3
    output$tren_tpt_prov <- renderValueBox({
        tpt_prov <- (tail(kstp_prov$tingkat_pengangguran_terbuka,1)-head(kstp_prov$tingkat_pengangguran_terbuka,1))/head(kstp_prov$tingkat_pengangguran_terbuka,1)*100
        apm_sma_prov <- (tail(apm_prov$apm_sma,1)-head(apm_prov$apm_sma,1))/head(apm_prov$apm_sma,1)*100
        
        valueBox(value = paste0(round(ifelse(input$kategori_a=="Data Sosial Kependudukan",tpt_prov,apm_sma_prov),2),"%"), 
                 subtitle = ifelse(input$kategori_a=="Data Sosial Kependudukan",
                                   "Pertumbuhan Angka Pengangguran Terbuka","Pertumbuhan APM SMA"), 
                 icon = icon(ifelse(input$kategori_a=="Data Sosial Kependudukan","frown","graduation-cap")),
                 color = ifelse(input$kategori_a=="Data Sosial Kependudukan","green","maroon")
                 )
    })
    #value box tab 2B no.1
    output$tren_sanitasi_kab <- renderValueBox({
        kstp_per_kab <- kstp_kab %>% 
            filter(kabupaten==input$kategori_kbptn)
        sanitasi_kab <- (tail(kstp_per_kab$angka_akses_sanitasi,1)-head(kstp_per_kab$angka_akses_sanitasi,1))/head(kstp_per_kab$angka_akses_sanitasi,1)*100
        
        apm_per_kab <- apm_kab %>% 
            filter(kabupaten==input$kategori_kbptn)
        apm_sd_kab <- (tail(apm_per_kab$apm_sd,1)-head(apm_per_kab$apm_sd,1))/head(apm_per_kab$apm_sd,1)*100
        
        valueBox(value = paste0(round(ifelse(input$kategori_kab=="Data Sosial Kependudukan",sanitasi_kab,apm_sd_kab),2),"%"), 
                 subtitle = ifelse(input$kategori_kab=="Data Sosial Kependudukan",
                                   "Pertumbuhan Angka Akses Sanitasi Layak","Pertumbuhan APM SD"),
                 icon = icon(ifelse(input$kategori_kab=="Data Sosial Kependudukan","water","child")),
                 color = ifelse(input$kategori_kab=="Data Sosial Kependudukan",ifelse(sanitasi_kab>0,"green","maroon"),ifelse(apm_sd_kab>0,"green","maroon"))
                 )
    })
    #value box tab 2B no.2
    output$tren_kemiskinan_kab <- renderValueBox({
        kstp_per_kab <- kstp_kab %>% 
            filter(kabupaten==input$kategori_kbptn)
        kemiskinan_kab <- (tail(kstp_per_kab$angka_kemiskinan,1)-head(kstp_per_kab$angka_kemiskinan,1))/head(kstp_per_kab$angka_kemiskinan,1)*100
        
        apm_per_kab <- apm_kab %>% 
            filter(kabupaten==input$kategori_kbptn)
        apm_smp_kab <- (tail(apm_per_kab$apm_smp,1)-head(apm_per_kab$apm_smp,1))/head(apm_per_kab$apm_smp,1)*100
        
        valueBox(value = paste0(round(ifelse(input$kategori_kab=="Data Sosial Kependudukan",kemiskinan_kab,apm_smp_kab),2),"%"), 
                 subtitle = ifelse(input$kategori_kab=="Data Sosial Kependudukan",
                                   "Pertumbuhan Angka Kemiskinan","Pertumbuhan APM SMP"),
                 icon = icon(ifelse(input$kategori_kab=="Data Sosial Kependudukan","sad-tear","school")),
                 color = ifelse(input$kategori_kab=="Data Sosial Kependudukan",ifelse(kemiskinan_kab<0,"green","maroon"),ifelse(apm_smp_kab>0,"green","maroon"))
        )
    })
    #value box tab 2B no.3
    output$tren_tpt_kab <- renderValueBox({
        kstp_per_kab <- kstp_kab %>% 
            filter(kabupaten==input$kategori_kbptn)
        tpt_kab <- (tail(kstp_per_kab$tingkat_pengangguran_terbuka,1)-head(kstp_per_kab$tingkat_pengangguran_terbuka,1))/head(kstp_per_kab$tingkat_pengangguran_terbuka,1)*100
        
        apm_per_kab <- apm_kab %>% 
            filter(kabupaten==input$kategori_kbptn)
        apm_sma_kab <- (tail(apm_per_kab$apm_sma,1)-head(apm_per_kab$apm_sma,1))/head(apm_per_kab$apm_sma,1)*100
        
        valueBox(value = paste0(round(ifelse(input$kategori_kab=="Data Sosial Kependudukan",tpt_kab,apm_sma_kab),2),"%"), 
                 subtitle = ifelse(input$kategori_kab=="Data Sosial Kependudukan",
                                   "Pertumbuhan Angka Pengangguran Terbuka","Pertumbuhan APM SMA"),
                 icon = icon(ifelse(input$kategori_kab=="Data Sosial Kependudukan","frown","graduation-cap")),
                 color = ifelse(input$kategori_kab=="Data Sosial Kependudukan",ifelse(tpt_kab<0,"green","maroon"),ifelse(apm_sma_kab>0,"green","maroon"))
        )
    })
    #Tab 3
    #Korelasi provinsi
    output$cor_prov <- renderPlotly({
        scatter_satu_prov <- ggplot(data = data_scatter,aes(x = eval(parse(text = str_replace_all(input$sumbu_x_prov," ","_")))
                                                            ,y = eval(parse(text = str_replace_all(input$sumbu_y_prov," ","_")))
                                                            ,size=Kepadatan,color=kabupaten,
                                                            text=paste("<b>",kabupaten,"</b><br>",
                                                                       "Tahun: ",Tahun,"<br>",
                                                                       "Kepadatan Penduduk: ",Kepadatan," jiwa/km\u00B2 <br>",
                                                                       input$sumbu_x_prov,": ",eval(parse(text = str_replace_all(input$sumbu_x_prov," ","_"))), "<br>",
                                                                       input$sumbu_y_prov,": ",eval(parse(text = str_replace_all(input$sumbu_y_prov," ","_"))), "<br>",
                                                                       sep= "")
                                                                
                                                            ))+
            geom_jitter(show.legend = F)+
            labs(x=input$sumbu_x_prov,
                 y=input$sumbu_y_prov,caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        ggplotly(scatter_satu_prov,
                 tooltip = "text")
        
    })
    #Valuebox korelasi provinsi
    output$korelasi_prov <- renderValueBox({
        
        uy <- data_scatter %>%
            summarize(hasil=cor(eval(parse(text = str_replace_all(input$sumbu_x_prov," ","_"))),
                                eval(parse(text = str_replace_all(input$sumbu_y_prov," ","_")))))
        
        valueBox(value = paste0(round(uy$hasil,2)) ,
                 subtitle ="Nilai Korelasi" ,
                 icon = icon("link"),
                 color = "aqua")
    })
    #Plot korelasi per kabupaten
    output$cor_kab <- renderPlotly({
        filter_per_kab <- data_scatter %>% 
            filter(kabupaten==input$nama_kab)
        
        
        scatter_satu_kab <- ggplot(data = filter_per_kab,aes(x = eval(parse(text = str_replace_all(input$sumbu_x_kab," ","_")))
                                                            ,y = eval(parse(text = str_replace_all(input$sumbu_y_kab," ","_")))
                                                            ,size=Kepadatan,color=kabupaten,
                                                            text=paste("<b>",kabupaten,"</b><br>",
                                                                       "Tahun: ",Tahun,"<br>",
                                                                       "Kepadatan Penduduk: ",Kepadatan," jiwa/km\u00B2 <br>",
                                                                       input$sumbu_x_kab,": ",eval(parse(text = str_replace_all(input$sumbu_x_kab," ","_"))), "<br>",
                                                                       input$sumbu_y_kab,": ",eval(parse(text = str_replace_all(input$sumbu_y_kab," ","_"))), "<br>",
                                                                       sep= "")
                                                            
        ))+
            geom_jitter(show.legend = F)+
            labs(x=input$sumbu_x_kab,
                 y=input$sumbu_y_kab,caption = "Sumber: BPS Provinsi Jawa Tengah")
        
        ggplotly(scatter_satu_kab,
                 tooltip = "text")
        
    })
    #Value box korelasi per kabupaten
    output$korelasi_kab <- renderValueBox({
        filter_per_kab <- data_scatter %>% 
            filter(kabupaten==input$nama_kab)
        
        ux <- filter_per_kab %>%
            summarize(hasil=cor(eval(parse(text = str_replace_all(input$sumbu_x_kab," ","_"))),
                                eval(parse(text = str_replace_all(input$sumbu_y_kab," ","_")))))
        
        valueBox(value = paste0(round(ux$hasil,2)) ,
                 subtitle ="Nilai Korelasi" ,
                 icon = icon("link"),
                 color = "aqua")
    })
    #Tab 4 Dataset
    output$data_jateng_bro <- renderDataTable({
        DT::datatable(data = data_scatter_copy ,options = list(scrollX=T))
    })
}

