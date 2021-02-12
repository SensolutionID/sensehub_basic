ujiDiskriminasiUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Uji Diskriminasi"),
        wellPanel(
          numericInput(
            inputId = ns("n_benar"),
            label = "Jumlah Jawaban Benar",
            value = 10,
            min = 1
          ),
          numericInput(
            inputId = ns("n_percobaan"),
            label = "Jumlah Percobaan",
            value = 15,
            min = 2
          ),
          radioButtons(
            inputId = ns("metode"),
            label = "Metode",
            choices = c(
              "2-AFC" = "twoAFC",
              "3-AFC" = "threeAFC",
              "Duo-Trio" = "duotrio",
              "Triangle" = "triangle",
              "Tetrad" = "tetrad",
              "Hexad" = "hexad",
              "Two-out-of-Five" = "twofive"
            ),
            selected = "duotrio"
          ),
          actionButton(
            inputId = ns("analisa"),
            label = "Analisis"
          )
        )
      ),
      column(
        8,
        withSpinner(verbatimTextOutput(ns("hasil"))),
        withSpinner(plotOutput(ns("magnitude")))
      )
    )
  )
}

ujiDiskriminasi <- function(input, output, session) {
  ns <- session$ns

  hasil <- eventReactive(input$analisa, {
    discrim(input$n_benar, input$n_percobaan, method = input$metode)
  })

  output$hasil <- renderPrint({
    cat(
      "Estimat untuk uji", 
      switch(isolate(input$metode),
        "twoAFC" = "2-AFC",
        "threeAFC" = "3-AFC",
        "duotrio" = "Duo-Trio",
        "triangle" = "Triangle",
        "tetrad" = "Tetrad",
        "hexad" = "Hexad",
        "twofive" = "Two-out-of-Five"
      ), 
      "dengan", isolate(input$n_benar), "jawaban benar dari total", isolate(input$n_percobaan), "percobaan",
      "\n",
      "\n"
    )

    hasil()$coefficients %>%
      round(2) %>%
      `rownames<-`(c("Probabilitas Jawaban Benar (pc)", "Proporsi Diskriminator (pd)", "d-prime")) %>%
      `colnames<-`(c("Estimat", "Std. Error", "95%CI bawah", "95%CI atas")) %>%
      print()

    cat(
      "\n",
      "Hasil uji diskriminasi:", "\n", "'exact' binomial test: p-value = ", hasil()$p.value, "\n", "Hipotesis alternatif: nilai d-prime lebih besar dari 0"
    )
  })

  output$magnitude <- renderPlot({
    z <- seq(-5, 5, length.out = 1000)
    y <- dnorm(z)
    y2 <- dnorm(z, mean = coef(hasil())[3, 1])
    plot(z, y,
      type = "l", xlab = "Sensory Magnitude", ylab = "",
      main = "Distribusi Intensitas Sensoris", las = 1, lty = 2
    )
    lines(z, y2, col = "red", lty = 1)
  })
}
