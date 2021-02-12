server <- function(input, output, session) {
  observe_helpers()
  callModule(rancanganPercobaan, "rancangan")
  callModule(metodeQda, "qda")
  callModule(metodeCata, "cata")
  callModule(ujiDiskriminasi, "diskriminasi")
  callModule(acceptanceTest, "acceptance")
  callModule(pairedPreference, "pairedpref")
  callModule(hedonicRating, "hedonic")
}
