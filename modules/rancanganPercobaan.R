rancanganPercobaanUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Rancangan Percobaan"),
        wellPanel(
          h4(tagList(icon("sliders"), "Pengaturan Parameter")),
          numericInput(
            inputId = ns("jumlah_sampel"),
            label = "Jumlah Sampel",
            value = 3
          ),
          uiOutput(ns("penamaan_sampel")),
          numericInput(
            inputId = ns("jumlah_panelis"),
            label = "Jumlah Panelis",
            value = 10
          ),
          numericInput(
            inputId = ns("jumlah_atribut"),
            label = "Jumlah Atribut",
            value = 5
          ),
          actionButton(
            inputId = ns("rancang"),
            label = "Rancang"
          )
        )
      ),
      column(
        8,
        tabsetPanel(
          tabPanel(
            "Tabel Rancangan",
            icon = icon("table"),
            verbatimTextOutput(ns("test")),
            br(),
            DT::dataTableOutput(ns("tabel_rancangan"))
          ),
          tabPanel(
            "Format Tabulasi",
            icon = icon("table"),
            br(),
            DT::dataTableOutput(ns("format_tabulasi"))
          )
        )
      )
    )
  )
}

rancanganPercobaan <- function(input, output, session) {
  ns <- session$ns
  
  output$penamaan_sampel <- renderUI({
    req(input$jumlah_sampel)
    map(1:input$jumlah_sampel,
        ~ textInput(
          inputId = ns(paste0("sampel_", .x)),
          label = paste("Nama Sampel", .x)
        )
    )
  })
  
  rancangan <- eventReactive(input$rancang, {
    set.seed(input$jumlah_sampel + input$jumlah_panelis)
    nama <- map_chr(seq_along(rnorm(input$jumlah_sampel)), ~ input[[paste0("sampel_", .x)]])
    kode <- sample(100:999, input$jumlah_sampel, replace = FALSE)
    res <- williams(input$jumlah_sampel) %>%
      apply(2, rep, input$jumlah_panelis) %>%
      `colnames<-`(paste("Urutan", 1:input$jumlah_sampel)) %>%
      as_tibble() %>%
      mutate_all(funs(plyr::mapvalues(., from = 1:input$jumlah_sampel, to = paste0(nama," (", kode, ")")))) %>% 
      mutate(Panelis = paste("Panelis", 1:nrow(.))) %>% 
      select(Panelis, everything()) %>% 
      `[`(1:input$jumlah_panelis,)
    return(res)
  })
  
  output$tabel_rancangan <- DT::renderDataTable({
    rancangan() %>%
      datatable(
        rownames = FALSE,
        caption = "Tabel rancangan dengan menggunakan William's Latin Square Design. Angka 3-digit pada tiap sel merupakan kode acak untuk sampel.",
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 400,
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


  tabulasi <- eventReactive(input$rancang, {
    req(input$jumlah_atribut)
    rancangan() %>% 
      gather(key = "Urutan", value = "Sampel", -Panelis) %>%
      mutate(Panelis = factor(Panelis, levels = unique(Panelis))) %>% 
      arrange(Panelis) %>% 
      cbind(., matrix(rep(NA_integer_, nrow(.) * input$jumlah_atribut),
                      ncol = input$jumlah_atribut,
                      dimnames = list(NULL, paste("Atribut", rep(1:input$jumlah_atribut)))
      )) %>% 
      as_tibble()
  })

  output$format_tabulasi <- DT::renderDataTable({
    tabulasi() %>%
      datatable(
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 400,
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
}
