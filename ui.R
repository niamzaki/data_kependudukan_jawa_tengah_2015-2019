header <- dashboardHeader(
    title = "Jawa Tengah"
)
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Gambaran Besar",
                 tabName = "ovr",
                 icon = icon("map")),
        menuItem(text = "Tren Data",
                 tabName = "trndt",
                 icon = icon("chart-line")),
        menuItem(text = "Korelasi",
                 tabName = "kor",
                 icon = icon("link")),
        menuItem(text = "Data",icon = icon("database"),
        menuSubItem("Tabel Data",tabName = "dt_jt",
                    icon = icon("table")),
        menuSubItem(text = "Pranala",
                    icon = icon("file-download"),
                    href = "https://jateng.bps.go.id/"))
    ) )
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "ovr",
                fluidPage(img(src="jateng.png",height=70,width=70),
                    h2(tags$b("Data Kependudukan Provinsi Jawa Tengah")),
                    br(),
                    div(style = "text-align:justify", 
                        p("Data berikut merupakan data sosial dan kependudukan Provinsi Jawa Tengah 
                          yang diantaranya berisi informasi jumlah penduduk, kepadatan penduduk, angka kemiskinan, 
                          tingkat penangguran terbuka, angka akses sanitasi layak dan angka partisipasi murni dari 
                          tahun 2015 hingga 2019 dari laman BPS Provinsi Jawa Tengah.", 
                          "Harapannya dari data yang telah divisualkan di bawah bisa diambil kesimpulannya
                          atau digunakan sebagai sumber masukan untuk pembagunan Provinsi Jawa Tengah kedepannya dan
                          juga sebagai bahan evaluasi selama lima tahun kebelakang."
                          ),
                        p(h5(tags$b("Sumber: BPS Provinsi Jawa Tengah"))),
                        br()
                    )
                ),
                fluidPage(
                    tabBox(width = 9,
                           title = tags$b("Peta Kependudukan Provinsi Jawa Tengah"),
                           id = "tabset1",
                           side = "right",
                            tabPanel(tags$b("Kepadatan Penduduk"), 
                                     leafletOutput("density_map")
                            ),
                            tabPanel(tags$b("Peta Sebaran Angka Kemiskinan"), 
                                     leafletOutput("kemiskinan")
                            )
                    ),
                    box(width = 3,
                        background = "blue",
                        height = 100,
                        selectInput(inputId = "tahun",
                                    label = h4(tags$b("Pilih Tahun:")),
                                    choices = unique(data_jateng$tahun))),
                    
                    valueBoxOutput(width = 3,
                                   "jumlah_penduduk"),
                    valueBoxOutput(width = 3,
                                   "jumlah_penduduk_miskin"),
                    valueBoxOutput(width = 3,
                                   "akses_sanitasi_layak")
                ),
                fluidPage(
                    box(width = 9,
                        title = tags$b("Distribusi Data Setiap Kabupaten/Kota di Jawa Tengah"),
                        plotlyOutput("ranking")
                    ),
                    box(width = 3,
                        background = "blue",
                        height = 100,
                        selectInput(inputId = "tahun_r",
                                    label = h4(tags$b("Pilih Tahun:")),
                                    choices = unique(data_jateng$tahun))),
                    box(width = 3,
                        background = "blue",
                        height = 180,
                        radioButtons(inputId = "kategori",
                                    label = h4(tags$b("Pilih Kategori:")),
                                    choices = str_replace_all(list("kepadatan","kemiskinan","angka_tpt","angka_sanitasi"),
                                                              c("kepadatan","kemiskinan","angka_tpt","angka_sanitasi"),
                                                              c("Kepadatan Penduduk","Angka Kemiskinan","Tingkat Pengangguran Terbuka","Akses Sanitasi Layak"))))
                    ),
                
                fluidPage(
                    h4(tags$b("Glosarium")),
                    div(style = "text-align:justify", 
                        p("Tingkat Pengangguran Terbuka (TPT) adalah persentase jumlah pengangguran terhadap jumlah angkatan kerja."),
                        p("Angka kepadatan penduduk adalah angka yang menunjukan rata-rata  jumlah  penduduk  tiap  1 kilometer  persegi."),
                        p("Angka Partisipasi Murni (APM) adalah proporsi anak sekolah pada satu kelompok usia tertentu yang bersekolah pada jenjang yang sesuai dengan kelompok usianya."),
                        
                        br()
                    )
                ),
                
                ),
        #TAb 2
        tabItem(tabName = "trndt",
                fluidPage(box(width = 9,
                              title = tags$b("Data Kependudukan dan APM Provinsi Jawa Tengah"),
                              plotlyOutput("trn_prov")
                ),
                box(width = 3,
                    background = "blue",
                    height = 120,
                    radioButtons(inputId = "kategori_a",
                                 label = h4(tags$b("Pilih Kategori:")),
                                 choices = c("Data Sosial Kependudukan","Angka Partisipasi Murni"))),
                valueBoxOutput(width = 3,
                               "tren_sanitasi_prov"),
                valueBoxOutput(width = 3,
                               "tren_kemiskinan_prov"),
                valueBoxOutput(width = 3,
                               "tren_tpt_prov")
                ),
                #Per kabupaten
                fluidPage(
                    box(width = 9,
                        title = tags$b("Data Kependudukan dan APM Per Kabupaten/Kota"),
                        plotlyOutput("trn_kab")),
                    
                    box(width = 3,
                        background = "blue",
                        height = 120,
                        radioButtons(inputId = "kategori_kab",
                                     label = h4(tags$b("Pilih Kategori:")),
                                     choices = c("Data Sosial Kependudukan","Angka Partisipasi Murni"))),
                    box(width = 3,
                        background = "blue",
                        height = 120,
                        selectInput(inputId = "kategori_kbptn",
                                     label = h4(tags$b("Pilih Kabupaten/Kota:")),
                                     choices = unique(kstp_kab$kabupaten)))
                    
                ),
                fluidPage(
                    valueBoxOutput(width = 3,
                                   "tren_sanitasi_kab"),
                    valueBoxOutput(width = 3,
                                   "tren_kemiskinan_kab"),
                    valueBoxOutput(width = 3, 
                                   "tren_tpt_kab")
                )
                ),
        #PAge 3
        #A
        tabItem(tabName = "kor",
                fluidPage(box(width = 12,
                              title = tags$b("Korelasi Antar Parameter di Provinsi Jawa Tengah"),
                              side="right",
                              plotOutput("cor_kab")
                ),
                fluidPage(box(width = 9,
                              title = tags$b("Korelasi Antar Parameter Provinsi Jawa Tengah"),
                              plotlyOutput("cor_prov")
                ),
                box(width = 3,
                    background = "blue",
                    height = 120,
                    selectInput(inputId = "sumbu_x_prov",
                                 label = h4(tags$b("Pilih Parameter Sumbu X:")),
                                 choices = str_replace_all(colnames(for_choose),"_"," "))),
                box(width = 3,
                    background = "blue",
                    height = 120,
                    selectInput(inputId = "sumbu_y_prov",
                                label = h4(tags$b("Pilih Parameter Sumbu Y:")),
                                choices = str_replace_all(colnames(for_choose),"_"," "))),
                
                valueBoxOutput(width = 3,
                               "korelasi_prov")
                ))),
        #Tab 4
        tabItem(tabName = "dt_jt",
                h2(tags$b("Data Kependudukan Jawa Tengah")),
                dataTableOutput(outputId = "data_jateng_bro"))
                
    ))
dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar )
