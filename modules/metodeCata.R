metodeCataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Check-all-that-Apply"),
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
            helpText(tags$small("Contoh menggunakan data dari metode Check-all-that-Apply (CATA). Pengaturan parameter adalah sebagai berikut: Panelis = 'Consumer', Sampel = 'Product', Atribut Utama= 'Firm' - 'Astringent', dan Atribut Tambahan = 'Liking'."))
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
              inputId = ns("atribut"),
              label = "Atribut Sensoris",
              choices = NULL,
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE,
                title = "Pilih kolom",
                "deselect-all-text" = "Hapus semua",
                "select-all-text" = "Pilih semua",
                size = 8
              )
            ),
            tipify(
              pickerInput(
                inputId = ns("atribut_tambahan"),
                label = "Atribut Tambahan",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  "actions-box" = TRUE,
                  title = "Pilih kolom",
                  "deselect-all-text" = "Hapus semua",
                  "select-all-text" = "Pilih semua",
                  size = 8
                )
              ),
              title = "Harus berupa data kontinyu! Contoh: kesukaan.", 
              placement = "top"
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
              fluidRow(
                column(
                  6,
                  h4("Atribut Utama"),
                  withSpinner(DT::dataTableOutput(ns("statistik_atributUtama")))
                ),
                column(
                  6,
                  h4("Atribut Tambahan"),
                  withSpinner(DT::dataTableOutput(ns("statistik_atributTambahan")))
                )
              )
            ),
            tabPanel(
              "Tabel",
              icon = icon("table"),
              h4("Atribut Utama"),
              withSpinner(DT::dataTableOutput(ns("nilai_atributUtama"))),
              hr(),
              h4("Atribut Tambahan"),
              withSpinner(DT::dataTableOutput(ns("nilai_atributTambahan")))
            ),
            tabPanel(
              "Grafik",
              icon = icon("image"),
              h4("Atribut Utama"),
              pickerInput(
                inputId = ns("opsi_grafik_atributUtama"),
                label = "Pilih atribut untuk ditampilkan",
                choices = NULL,
                options = list(
                  "live-search" = TRUE,
                  title = "Pilih kolom"
                )
              ),
              withSpinner(plotOutput(ns("grafik_atributUtama"))),
              hr(),
              h4("Atribut Tambahan"),
              pickerInput(
                inputId = ns("opsi_grafik_atributTambahan"),
                label = "Pilih atribut untuk ditampilkan",
                choices = NULL,
                options = list(
                  "live-search" = TRUE,
                  title = "Pilih kolom"
                )
              ),
              withSpinner(plotOutput(ns("grafik_atributTambahan")))
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
              h4("Analisis Korenspondensi"),
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
              "Frekuensi Atribut",
              icon = icon("bar-chart"),
              h4("Frekuensi Per Sampel"),
              pickerInput(
                inputId = ns("opsi_sampel"),
                label = "Sampel untuk ditampilkan:",
                choices = NULL,
                options = list(
                  title = "Pilih sampel",
                  size = 8
                )
              ),
              withSpinner(DT::dataTableOutput(ns("global_frekuensi")))
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
                      choices = c("Sampel", "Atribut", "Atribut Tambahan"),
                      selected = "Sampel"
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

metodeCata <- function(input, output, session) {
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
      read_csv("data/data_cata.csv")
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
      inputId = "atribut",
      choices = colnames(mentah())
    )
    
    updatePickerInput(
      session = session,
      inputId = "atribut_tambahan",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel)  & !is.null(input$atribut)
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
        "Metode" = "Check-all-that-Apply",
        "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
        "Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% paste(collapse = ", "),
        "Jumlah Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% length(),
        "Atribut" = paste(input$atribut, collapse = ", "),
        "Jumlah Atribut" = length(input$atribut),
        "Atribut Tambahan" = paste(input$atribut_tambahan, collapse = ", "),
        "Jumlah Atribut Tambahan" = length(input$atribut_tambahan)
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
      select(input$panelis, input$sampel, one_of(input$atribut)) %>% 
      rename(Panelis = !!input$panelis, 
             Sampel = !!input$sampel) %>%
      mutate(Panelis = as.factor(Panelis), 
             Sampel = as.factor(Sampel)) %>% 
      gather(key = "Atribut", value = "Nilai", -c(Panelis, Sampel)) %>%
      nest(-Atribut, .key = "Data") %>%
      mutate(Model = map(Data, ~ cochran.qtest(Nilai ~ Sampel | Panelis, data = .))) %>%
      mutate(Proba = map(Model, "estimate")) %>%
      mutate(Q_stat = map_dbl(Model, "statistic")) %>%
      mutate(P_value = map_dbl(Model, "p.value")) %>% 
      select(Atribut, Proba, Q_stat, P_value)
  })
  
  ### Ringkasan Statistik ----
  statistik_atributUtama <- reactive({
    req(lokal_atributUtama())
    lokal_atributUtama() %>%
      select(Atribut, Q_stat, P_value) %>%
      mutate(Q_stat = round(Q_stat, 3), 
             P_value = signif(P_value, 3), 
             " " = if_else(P_value < 0.01, "**",
                                  if_else(P_value > 0.05, "t.s.", "*"))) %>%
      arrange(P_value) %>%
      rename("Q statistik" = Q_stat, "P value" = P_value)
  })
  
  output$statistik_atributUtama <- DT::renderDataTable({
    statistik_atributUtama() %>%
      datatable(
        rownames = FALSE,
        caption = "Ringkasan parameter statistik dari uji Cochran Q. Tanda (**) menunjukan signifikansi pada tingkat kepercayaan 99%, tanda (*) pada tingkat kepercayaan 95%, sedangkan t.s. menunjukan tidak beda signifikan.",
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
    lokal_atributUtama() %>%
      select(Atribut, Proba) %>%
      mutate(Proba = map(Proba, as.data.frame)) %>%
      mutate(Proba = map(Proba, `colnames<-`, "Probabilitas")) %>%
      mutate(Proba = map(Proba, as_tibble, rownames = "Sampel")) %>%
      unnest() %>%
      mutate(Sampel = str_remove_all(Sampel, "proba in group ")) %>%
      mutate_if(is.numeric,funs(round(., 3)))
  })
  
  output$nilai_atributUtama <- DT::renderDataTable({
    nilai_atributUtama() %>% 
      datatable(
        rownames = FALSE,
        caption = "Tabel Nilai Probabilitas Jawaban 'Ya'/'Ada'",
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
  observeEvent(input$terapkan, {
    updatePickerInput(
      session = session,
      inputId = "opsi_grafik_atributUtama",
      choices = input$atribut
    )
  }, ignoreInit = TRUE)
  
  grafik_atributUtama <- reactive({
    req(nilai_atributUtama())
    req(input$opsi_grafik_atributUtama)
    nilai_atributUtama() %>% 
      filter(Atribut == input$opsi_grafik_atributUtama) %>% 
      ggplot(aes(x = Sampel, y = Probabilitas, fill = Sampel)) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Probabilitas", fill = "", title = input$opsi_grafik_atributUtama) +
      guides(fill = FALSE) +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
      )
  })
  
  output$grafik_atributUtama <- renderPlot({
    grafik_atributUtama()
  })
  
  ## Atribut Tambahan ----
  lokal_atributTambahan <- eventReactive(input$terapkan, {
    req(mentah())
    if (is.null(input$atribut_tambahan)) {
      NULL
    } else {
      mentah() %>%
        select(input$panelis, input$sampel, one_of(input$atribut_tambahan)) %>% 
        rename(Panelis = !!input$panelis, 
               Sampel = !!input$sampel) %>%
        mutate(Panelis = as.factor(Panelis), 
               Sampel = as.factor(Sampel)) %>% 
        gather(key = "Atribut", value = "Nilai", -c(Panelis, Sampel)) %>%
        nest(-Atribut, .key = "Data") %>%
        mutate(
          Model = map(Data, ~ lmer(Nilai ~ Sampel + (1 | Panelis), data = .)),
          ANOVA = map(Model, anova),
          Tidy_ANOVA = map(ANOVA, tidy),
          LS_means = map(Model, ls_means, which = "Sampel"),
          F_stat = map_dbl(Tidy_ANOVA, ~ .$statistic[1]),
          P_value = map_dbl(Tidy_ANOVA, ~ .$p.value[1])
        ) %>%
        select(Atribut, LS_means, F_stat, P_value)
    }
  })
  
  ### Ringkasan Statistik ----
  statistik_atributTambahan <- reactive({
    req(lokal_atributTambahan())
    lokal_atributTambahan() %>%
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
  
  output$statistik_atributTambahan <- DT::renderDataTable({
    statistik_atributTambahan() %>%
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
  nilai_atributTambahan <- reactive({
    req(lokal_atributTambahan())
    lokal_atributTambahan() %>%
      select(Atribut, LS_means) %>%
      mutate(LS_means = map(LS_means, as_tibble, rownames = "Sampel")) %>% 
      mutate(LS_means = map(LS_means, select, Sampel, Estimate, 'Std. Error')) %>%
      unnest() %>% 
      mutate(Sampel = str_remove(Sampel, "Sampel")) %>% 
      rename(Rerata = Estimate, 
             Std.Error = 'Std. Error') %>% 
      mutate_if(is.numeric,funs(round(., 3)))
  })
  
  output$nilai_atributTambahan <- DT::renderDataTable({
    nilai_atributTambahan() %>% 
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
  observeEvent(input$terapkan, {
    updatePickerInput(
      session = session,
      inputId = "opsi_grafik_atributTambahan",
      choices = input$atribut_tambahan
    )
  }, ignoreInit = TRUE)
  
  grafik_atributTambahan <- reactive({
    req(nilai_atributTambahan())
    req(input$opsi_grafik_atributTambahan)
    nilai_atributTambahan() %>% 
      filter(Atribut == input$opsi_grafik_atributTambahan) %>% 
      ggplot(aes(x = Sampel, y = Rerata, fill = Sampel)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymax = Rerata + Std.Error, ymin = Rerata - Std.Error),  width = 0.5) +
      labs(x = "", y = "Rerata", fill = "", title = input$opsi_grafik_atributTambahan) +
      guides(fill = FALSE) +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
      )
  })
  
  output$grafik_atributTambahan <- renderPlot({
    grafik_atributTambahan()
  })
  
  # Analisis Global ----
  global <- eventReactive(input$terapkan, {
    req(mentah())
    if (is.null(input$atribut_tambahan)) {
      df <- mentah() %>%
        rename(Panelis = !!input$panelis, Sampel = !!input$sampel) %>%
        mutate(Panelis = as.factor(Panelis), Sampel = as.factor(Sampel)) %>%
        group_by(Sampel) %>%
        select(one_of(input$atribut)) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        as.data.frame() %>%
        `rownames<-`(.[, "Sampel"]) %>%
        select_if(is.numeric)
      kuanti_tambahan <- NULL
    } else {
      df <- left_join(
        mentah() %>%
          rename(Panelis = !!input$panelis, Sampel = !!input$sampel) %>%
          mutate(Panelis = as.factor(Panelis), Sampel = as.factor(Sampel)) %>%
          group_by(Sampel) %>%
          select(one_of(input$atribut)) %>%
          summarise_if(is.numeric, sum, na.rm = TRUE),
        mentah() %>%
          rename(Panelis = !!input$panelis, Sampel = !!input$sampel) %>%
          mutate(Panelis = as.factor(Panelis), Sampel = as.factor(Sampel)) %>%
          group_by(Sampel) %>%
          select(one_of(input$atribut_tambahan)) %>%
          summarise_if(is.numeric, mean, na.rm = TRUE),
        by = "Sampel"
      ) %>%
        as.data.frame() %>%
        `rownames<-`(.[, "Sampel"]) %>%
        select_if(is.numeric)
      kuanti_tambahan <- which(names(df) %in% input$atribut_tambahan)
    }
    res <- CA(df, quanti.sup = kuanti_tambahan, graph = FALSE)
    return(res)
  })
  
  ## Ringkasan Dimensi ----
  output$global_screeplot <- renderPlot({
    plot_eigen(global(), threshold = FALSE)
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
      mutate(Dimensi = str_replace_all(Dimensi, "dim", "Dimensi")) %>%
      as.data.frame()
  })
  
  ## Deskripsi Dimensi ----
  global_dimensi <- reactive({
    if (is.null(input$atribut_tambahan)) {
      list(
        Sampel = list(Koordinat = global()$row$coord %>%
                        signif(2) %>%
                        `colnames<-`(paste("Dim", 1:ncol(.)))),
        Atribut = list(Koordinat = global()$col$coord %>%
                         signif(2) %>%
                         `colnames<-`(paste("Dim", 1:ncol(.))))
      )
    } else {
      list(
        Sampel = list(Koordinat = global()$row$coord %>%
                        signif(2) %>%
                        `colnames<-`(paste("Dim", 1:ncol(.)))),
        Atribut = list(Koordinat = global()$col$coord %>%
                         signif(2) %>%
                         `colnames<-`(paste("Dim", 1:ncol(.)))),
        "Atribut Tambahan" = list(Koordinat = global()$quanti.sup$coord %>%
                                    signif(2) %>%
                                    `colnames<-`(paste("Dim", 1:ncol(.))))
      )
    }
  })
  
  output$global_dimensi <- renderPrint({
    global_dimensi()
  })
  
  ## Frekuensi Atribut
  observeEvent(input$terapkan, {
    updatePickerInput(
      session = session,
      inputId = "opsi_sampel",
      choices = pull(unique(mentah()[, input$sampel]))
    )
  })
  
  global_frekuensi <- eventReactive(input$opsi_sampel, {
    mentah() %>%
      rename(Panelis = !!input$panelis, Sampel = !!input$sampel) %>%
      mutate(Panelis = as.factor(Panelis), Sampel = as.factor(Sampel)) %>%
      group_by(Sampel) %>%
      select(one_of(input$atribut)) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      as.data.frame() %>%
      `rownames<-`(.[, "Sampel"]) %>%
      select_if(is.numeric) %>% 
      descfreq(proba = 1) %>% 
      map(`colnames<-`, c("% Internal", "% Global", "Frekuensi Internal", "Frekuensi Global", "P value", "V test")) %>% 
      map(signif, 2) %>% 
      map(as_tibble, rownames = "Atribut") %>% 
      `[[`(input$opsi_sampel)
  })
  
  output$global_frekuensi <- DT::renderDataTable({
    global_frekuensi() %>% 
      datatable(
        rownames = FALSE,
        caption = "Ringkasan analisa frekuensi atribut per sampel. Nilai V test positif menunjukan atribut tersebut sering digunakan untuk mendeskripsikan sampel, sedangkan negatif menunjukan atribut tersebut jarang digunakan untuk mendeskripsikan sampel.",
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
  
  ## Peta Persepsi ----
  grafikGlobal <- reactive({
    req(global())
    if (input$grafikGlobal_opsi == "Sampel") {
      plot_sample(global(), 
                  axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y), 
                  main = input$grafikGlobal_judul,
                  lab.size = input$grafikGlobal_cex)
    } else if (input$grafikGlobal_opsi == "Atribut") {
      plot_attribute(res = global(), 
                     axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y), 
                     main = input$grafikGlobal_judul,
                     lab.size = input$grafikGlobal_cex)
    } else if (input$grafikGlobal_opsi == "Atribut Tambahan") {
      if (is.null(input$atribut_tambahan)) {
        data_frame() %>% 
          ggplot()
      } else {
        plot_attribute_sup_ca(global(), 
                              axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y), 
                              main = input$grafikGlobal_judul,
                              lab.size = input$grafikGlobal_cex)
      }
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
