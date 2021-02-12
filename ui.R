ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = "shortcut icon", href = "logo.png"),
    includeScript("google-analytics.js"),
    tags$style(type = "text/css", "body {padding-top: 70px;}")
  ),
  navbarPage(
    title = "SenseHub - Basic",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("flatly"),
    windowTitle = "SenseHub: Aplikasi Web Terintegrasi untuk Analisis Sensoris",
    tabPanel(
      "Rancangan Percobaan",
      icon = icon("cogs"),
      rancanganPercobaanUI("rancangan")
    ),
    tabPanel(
      "Performa Panelis",
      icon = icon("address-card"),
      h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
    ),
    tabPanel(
      "Uji Diskriminatif",
      icon = icon("search-plus"),
      ujiDiskriminasiUI("diskriminasi")
    ),
    navbarMenu(
      "Uji Deskriptif",
      icon = icon("leanpub"),
      "Kuantitatif",
      tabPanel(
        "Quantitative Descriptive Analysis (QDA)",
        metodeQdaUI("qda")
      ),
      tabPanel(
        "Flash Profiling (FP)",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      tabPanel(
        "Free Choice Profiling (FCP)",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      tabPanel(
        "Rate-all-that-Apply (RATA)",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      "----",
      "Kualitatif",
      tabPanel(
        "Check-all-that-Apply (CATA)",
        metodeCataUI("cata")
      ),
      tabPanel(
        "Rate-all-that-Apply (RATA as CATA)",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      "----",
      "Sorting",
      tabPanel(
        "Sorting Task",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      tabPanel(
        "Hierarchical Sorting Task",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      ),
      "----",
      "Projective Mapping",
      tabPanel(
        "Napping",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")      ),
      tabPanel(
        "Sorted Napping",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      )
    ),
    navbarMenu(
      "Uji Afektif",
      icon = icon("heart"),
      "Penerimaan",
      tabPanel(
        "Acceptance Test",
        acceptanceTestUI("acceptance")
      ),
      "----",
      "Preferensi",
      tabPanel(
        "Paired Preference",
        pairedPreferenceUI("pairedpref")
        ),
      tabPanel(
        "Multiple Paired Preference",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      tabPanel(
        "Preference Ranking",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      "----",
      "Hedonik",
      tabPanel(
        "Hedonic Rating",
        hedonicRatingUI("hedonic")
      ),
      "----",
      "Drivers of liking",
      tabPanel(
        "Internal Preference Mapping (MDPref)",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      ),
      tabPanel(
        "External Preference Mapping (PrefMap)",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      )
    ),
    navbarMenu(
      "Optimasi",
      icon = icon("arrow-up"),
      tabPanel(
        "Just-about-Right (JAR)",
        h1("Maaf, fitur ini hanya tersedia di SenseHub - Pro. Silakan kontak info@sensolution.id untuk mendapatkan SenseHub - Pro.")
      ),
      tabPanel(
        "Ideal Profile Method (IPM)",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      )
    ),
    tabPanel(
      "Ihwal",
      icon = icon("support"),
      wellPanel(
        includeMarkdown("README.md")
      )
    )
  )
)