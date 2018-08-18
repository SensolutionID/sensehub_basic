pairedPreferenceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Paired Preference"),
        wellPanel(
          textInput(
            inputId = ns("sampelA"),
            label = "Nama sampel A",
            value = "Produk baru"
          ),
          numericInput(
            inputId = ns("n_sampelA"),
            label = "Jumlah preferensi untuk sampel A",
            value = 30
          ),
          textInput(
            inputId = ns("sampelB"),
            label = "Nama sampel B",
            value = "Produk lama"
          ),
          numericInput(
            inputId = ns("n_sampelB"),
            label = "Jumlah preferensi untuk sampel B",
            value = 50
          ),
          actionButton(
            inputId = ns("analisa"),
            label = "Analisis"
          )
        )
      ),
      column(
        8,
        withSpinner(verbatimTextOutput(ns("hasil")))
      )
    )
  )
}

pairedPreference <- function(input, output, session) {
  ns <- session$ns

  hasil <- eventReactive(input$analisa, {
    binom.test(
      input$n_sampelA,
      (input$n_sampelA + input$n_sampelB),
      alternative = "greater"
    )
  })

  output$hasil <- renderPrint({
    cat(
      "Uji Paired Preference dengan", hasil()$statistic, "preferensi untuk", input$sampelA, "dan", hasil()$parameter, "preferesi untuk", input$sampelB,
      "\n",
      "\n",
      "Proporsi preferensi untuk", paste0(input$sampelA, ":"),
      "\n",
      "Estimat:", hasil()$estimate,
      "\n",
      "95%CI bawah:", hasil()$conf.int[1],
      "\n",
      "95%CI atas:", hasil()$conf.int[2],
      "\n",
      "\n",
      "Hasil uji paired preference:",
      "\n",
      "'exact' binomial test: p-value = ", hasil()$p.value,
      "\n",
      "Hipotesis alternatif:", paste("preferensi terhadap", input$sampelA, "lebih besar dari", input$sampelB)
    )
  })
}
