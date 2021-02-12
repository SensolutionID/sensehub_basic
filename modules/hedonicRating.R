hedonicRatingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Hedonic Rating"),
        wellPanel(
          h4(tagList(icon("upload"), "Unggah Data")),
          radioButtons(
            inputId = ns("sumber_data"),
            label = "Pilih data:",
            choices = c(
              "Gunakan contoh data" = "contoh",
              "Unggah data" = "unggah"
            )
          ),
          conditionalPanel(
            condition = "input.sumber_data == 'contoh'",
            ns = ns,
            helpText(tags$small("Contoh menggunakan data kesukaan untuk produk parfum. Pengaturan parameter adalah sebagai berikut: Panelis = 'Consumer', Sampel = 'Perfume', Nilai kesukaan = 'Liking'."))
          ),
          conditionalPanel(
            condition = "input.sumber_data == 'unggah'",
            ns = ns,
            tipify(
              fileInput(
                inputId = ns("data"),
                label = "",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                ),
                buttonLabel = "Pilih",
                placeholder = "Tidak ada dokumen"
              ),
              title = "format file *.csv",
              placement = "top"
            )
          ),
          actionButton(
            inputId = ns("unggah"),
            label = "Gunakan data"
          ),
          conditionalPanel(
            condition = "input.unggah",
            ns = ns,
            br(),
            h4(tagList(icon("sliders"), "Pengaturan Parameter")),
            pickerInput(
              inputId = ns("panelis"),
              label = "Panelis",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("sampel"),
              label = "Sampel",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("kesukaan"),
              label = "Nilai kesukaan",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            actionButton(
              inputId = ns("terapkan"),
              label = "Terapkan"
            )
          )
        )
      ),
      column(
        8,
        panel(
          heading = h4(tagList(icon("database"), "Data")),
          status = "primary",
          tabsetPanel(
            tabPanel(
              "Tabel",
              icon = icon("table"),
              br(),
              withSpinner(DT::dataTableOutput(ns("tabel_data")))
            ),
            tabPanel(
              "Ringkasan",
              icon = icon("briefcase"),
              br(),
              withSpinner(tableOutput(ns("ringkasan")))
            )
          )
          ),
          panel(
            heading = h4(tagList(icon("line-chart"), "Analisis Lokal")),
            status = "primary",
            tabsetPanel(
              tabPanel(
                "Ringkasan Statistik",
                icon = icon("bookmark"),
                h4("Atribut Kesukaan"),
                withSpinner(DT::dataTableOutput(ns("statistik_atributUtama")))
              ),
              tabPanel(
                "Tabel",
                icon = icon("table"),
                h4("Atribut Kesukaan"),
                withSpinner(DT::dataTableOutput(ns("nilai_atributUtama")))
              ),
              tabPanel(
                "Grafik",
                icon = icon("image"),
                hr(),
                h4("Atribut Kesukaan"),
                withSpinner(plotOutput(ns("grafik_atributUtama")))
              )
            )
          ),
          panel(
            heading = h4(tagList(icon("globe"), "Analisis Global")),
            status = "primary",
            tabsetPanel(
              tabPanel(
                "Dimensi",
                icon = icon("plus"),
                h4("Analisis Komponen Utama"),
                fluidRow(
                  column(5,
                    align = "center",
                    withSpinner(plotOutput(ns("global_screeplot")))
                  ),
                  column(7,
                    align = "center",
                    withSpinner(tableOutput(ns("global_ringkasan")))
                  )
                )
              ),
              tabPanel(
                "Deskripsi Dimensi",
                icon = icon("file"),
                withSpinner(verbatimTextOutput(ns("global_dimensi")))
              ),
              tabPanel(
                "Peta Persepsi",
                icon = icon("image"),
                fluidRow(
                  column(
                    1,
                    dropdownButton(
                      circle = TRUE,
                      status = "primary",
                      icon = icon("gear"),
                      width = "300px",
                      tooltip = tooltipOptions(title = "Pengaturan"),
                      selectInput(
                        inputId = ns("grafikGlobal_opsi"),
                        label = "Pilihan Grafik",
                        choices = c("Panelis", "Preferensi"),
                        selected = "Preferensi"
                      ),
                      numericInput(
                        inputId = ns("grafikGlobal_sumbu_x"),
                        label = "Dimensi Sumbu X",
                        min = 1,
                        max = 4,
                        value = 1
                      ),
                      numericInput(
                        inputId = ns("grafikGlobal_sumbu_y"),
                        label = "Dimensi Sumbu Y",
                        min = 2,
                        max = 5,
                        value = 2
                      ),
                      textInput(
                        inputId = ns("grafikGlobal_judul"),
                        label = "Judul Grafik",
                        value = ""
                      ),
                      sliderInput(
                        inputId = ns("grafikGlobal_cex"),
                        label = "Ukuran Label",
                        min = 2,
                        max = 8,
                        value = 4,
                        step = 1
                      )
                    )
                  ),
                  column(11,
                    align = "center",
                    withSpinner(plotOutput(ns("grafikGlobal"))),
                    uiOutput(ns("opsi_unduh_grafikGlobal"))
                  )
                )
              )
            )
          )
      )
    )
  )
}

hedonicRating <- function(input, output, session) {
  ns <- session$ns
  
  # Data ----
  observe({
    if (input$sumber_data == "contoh") {
      enable("unggah")
    } else if (input$sumber_data == "unggah" & !is.null(input$data)) {
      enable("unggah")
    } else {
      disable("unggah")
    }
  })
  ## Tabel ----
  mentah <- eventReactive(input$unggah, {
    if (input$sumber_data == "contoh") {
      read_csv("data/perfumes_liking.csv")
    } else if (input$sumber_data == "unggah") {
      read_csv(input$data$datapath)
    }
  })
  
  output$tabel_data <- DT::renderDataTable({
    mentah() %>% 
      datatable(
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy",
                text = "Salin"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Unduh Tabel"
              )
            )
          ,
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )
  
  ## Ringkasan ----
  observeEvent(input$unggah, {
    updatePickerInput(
      session = session,
      inputId = "panelis",
      choices = colnames(mentah())
    )
    
    updatePickerInput(
      session = session,
      inputId = "sampel",
      choices = colnames(mentah())
    )
    
    updatePickerInput(
      session = session,
      inputId = "kesukaan",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel) & !is.null(input$kesukaan)
    )
  )
  
  observeEvent(input$terapkan, {
    showModal(
      modalDialog("Silakan periksa tab 'Ringkasan' untuk melihat ringkasan parameter penelitian. Hasil analisa data disajikan pada tab 'Analisis Lokal' dan 'Analisis Global' (diperlukan waktu beberapa detik untuk proses kalkulasi).",
                  title = strong("Parameter berhasil diterapkan!"),
                  footer = modalButton("Oke"),
                  easyClose = FALSE
      )
    )
  })
  
  ringkasan <- eventReactive(input$terapkan, {
    mentah() %>%
    {
      tibble(
        "Metode" = "Hedonic Rating",
        "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
        "Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% sort() %>% paste(collapse = ", "),
        "Jumlah Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% length()
      )
    } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })
  
  output$ringkasan <- renderTable({
    ringkasan()
  }, colnames = FALSE)
  
  # Analisis Lokal ----
  
  ## Atribut Utama ----
  lokal_atributUtama <- eventReactive(input$terapkan, {
    req(mentah())
    mentah() %>%
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Kesukaan = !!input$kesukaan) %>%
      mutate(Panelis = as.factor(Panelis),
             Sampel = as.factor(Sampel)) %>% 
      lmerTest::lmer(Kesukaan ~ Sampel + (1 | Panelis), data = .) %>% 
      anova() %>% 
      tidy() %>% {
        data_frame(Atribut = "Kesukaan",
                   F_stat = .$statistic,
                   P_value = .$p.value)
      }
  })
  
  ### Ringkasan Statistik ----
  statistik_atributUtama <- reactive({
    req(lokal_atributUtama())
    lokal_atributUtama() %>%
      select(Atribut, F_stat, P_value) %>%
      mutate(
        F_stat = round(F_stat, 3),
        P_value = signif(P_value, 3),
        " " = if_else(P_value < 0.01, "**",
                      if_else(P_value > 0.05, "t.s.", "*")
        )
      ) %>%
      arrange(P_value) %>% 
      rename("F statistik" = F_stat, "P value" = P_value)
  })
  
  output$statistik_atributUtama <- DT::renderDataTable({
    statistik_atributUtama() %>%
      datatable(
        rownames = FALSE,
        caption = "Ringkasan parameter statistik dari Mixed-effect Analysis of Variance (ANOVA) dengan sampel sebagai fixed-effect factor dan panelis sebagai random-effect factor. Tanda (**) menunjukan signifikansi pada tingkat kepercayaan 99%, tanda (*) pada tingkat kepercayaan 95%, sedangkan t.s. menunjukan tidak beda signifikan.",
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brt",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy",
                text = "Salin"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Unduh Tabel"
              )
            )
        )
      )
  }, server = FALSE
  )
  
  ### Tabel Nilai ----
  nilai_atributUtama <- reactive({
    req(lokal_atributUtama())
    mentah() %>% 
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Kesukaan = !!input$kesukaan) %>%
      mutate(Panelis = as.factor(Panelis),
             Sampel = as.factor(Sampel)) %>% 
      group_by(Sampel) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE)
  })
  
  output$nilai_atributUtama <- DT::renderDataTable({
    nilai_atributUtama() %>% 
      datatable(
        rownames = FALSE,
        caption = "Tabel Nilai Rerata",
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy",
                text = "Salin"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Unduh Tabel"
              )
            )
          ,
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )
  
  
  ### Grafik ----
  
  grafik_atributUtama <- reactive({
    req(nilai_atributUtama())
    nilai_atributUtama() %>% 
      ggplot(aes_string(x = "Sampel", y = "Kesukaan")) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Rerata", title = input$opsi_grafik_atributUtama) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid = element_blank(),
        panel.background = element_blank()
      )
  })
  
  output$grafik_atributUtama <- renderPlot({
    grafik_atributUtama()
  })
  
  # Analisis Global ----
  global <- eventReactive(input$terapkan, {
    req(mentah())
    res <- mentah() %>% 
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Kesukaan = !!input$kesukaan) %>%
      mutate(Panelis = as.factor(Panelis),
             Sampel = as.factor(Sampel)) %>% 
      group_by(Panelis) %>% 
      mutate(Kesukaan = scale(Kesukaan, scale = FALSE)) %>% 
      spread(key = "Sampel", value = "Kesukaan") %>% 
      ungroup() %>% 
      select(-Panelis) %>% 
      PCA(graph = FALSE)
    return(res)
  })
  
  ## Ringkasan Dimensi ----
  output$global_screeplot <- renderPlot({
    plot_eigen(global())
  })
  
  output$global_ringkasan <- renderTable({
    req(global())
    global()$eig %>%
      signif(3) %>%
      as_tibble(rownames = "Dimensi") %>%
      rename(
        Eigenvalue = eigenvalue,
        "Persentase Varian" = `percentage of variance`,
        "Persentase Kumulatif Varian" = `cumulative percentage of variance`
      ) %>%
      mutate(Dimensi = str_replace_all(Dimensi, "comp", "Dimensi")) %>%
      as.data.frame()
  })
  ## Deskripsi Dimensi ----
  global_dimensi <- reactive({
    if (is.null(input$atribut_tambahan)) {
      list(
        Sampel = list(Koordinat = global()$ind$coord %>%
                        signif(2) %>%
                        `colnames<-`(paste("Dim", 1:ncol(.)))),
        Atribut = list(
          Koordinat = global()$var$coord %>%
            signif(2) %>%
            `colnames<-`(paste("Dim", 1:ncol(.))),
          Korelasi = global()$var$cor %>%
            signif(2) %>%
            `colnames<-`(paste("Dim", 1:ncol(.)))
        )
      )
    } else {
      list(
        Sampel = list(Koordinat = global()$ind$coord %>%
                        signif(2) %>%
                        `colnames<-`(paste("Dim", 1:ncol(.)))),
        Atribut = list(
          Koordinat = global()$var$coord %>%
            signif(2) %>%
            `colnames<-`(paste("Dim", 1:ncol(.))),
          Korelasi = global()$var$cor %>%
            signif(2) %>%
            `colnames<-`(paste("Dim", 1:ncol(.)))
        )
      )
    }
  })
  
  output$global_dimensi <- renderPrint({
    global_dimensi()
  })
  
  ## Peta Persepsi ----
  grafikGlobal <- reactive({
    req(global())
    if (input$grafikGlobal_opsi == "Panelis") {
      plot_sample(
        res = global(),
        axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y),
        main = input$grafikGlobal_judul,
        lab.size = input$grafikGlobal_cex
      )
    } else if (input$grafikGlobal_opsi == "Preferensi") {
      plot_attribute(
        res = global(),
        axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y),
        main = input$grafikGlobal_judul,
        lab.size = input$grafikGlobal_cex
      )
    }
  })

  output$grafikGlobal <- renderPlot({
    plot(grafikGlobal())
  })

  observeEvent(input$terapkan, {
    output$opsi_unduh_grafikGlobal <- renderUI({
      downloadButton(ns("unduh_grafikGlobal"), label = "Simpan Grafik")
    })
  })

  output$unduh_grafikGlobal <- downloadHandler(
    filename = "Grafik - SenseHub.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
          width = width, height = height,
          res = 72, units = "in"
        )
      }
      ggsave(file, plot = grafikGlobal(), device = device)
    }
  )
}
