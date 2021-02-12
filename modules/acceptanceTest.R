acceptanceTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        helper(
          h1("Acceptance Test"),
          type = "markdown",
          content = "acceptanceTest",
          size = "l"
        ),
        wellPanel(
          numericInput(
            inputId = ns("n_sampel"),
            label = "Jumlah panelis yang menerima produk",
            value = 30
          ),
          numericInput(
            inputId = ns("n_total"),
            label = "Total jumlah panelis",
            value = 70
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

acceptanceTest <- function(input, output, session) {
  ns <- session$ns
  
  hasil <- eventReactive(input$analisa, {
    binom.test(
      input$n_sampel,
      input$n_total,
      alternative = "greater"
    )
  })
  
  output$hasil <- renderPrint({
    cat(
      "Uji Acceptance Test dengan", isolate(input$n_sampel), "jumlah penerimaan dari total", isolate(input$n_total), "panelis",
      "\n",
      "\n",
      "Proporsi penerimaan :",
      "\n",
      "Estimat:", hasil()$estimate,
      "\n",
      "95%CI bawah:", hasil()$conf.int[1],
      "\n",
      "95%CI atas:", hasil()$conf.int[2],
      "\n",
      "\n",
      "Hasil uji acceptance test:",
      "\n",
      "'exact' binomial test: p-value = ", hasil()$p.value,
      "\n",
      "Hipotesis alternatif: sampel dapat diterima lebih dari 50% total jumlah panelis"
    )
  })
}
