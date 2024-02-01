# Display footer

about_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('aboutit'))
  )
  
}

about_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$aboutit <- renderUI({
      fluidRow(
        column(
          12,
          style = 'padding-left:25px; padding-right:25px;',
          'The Puget Sound Regional Council(PSRC) provides data to help local planners and decision-makers better understand the region and visualize its future. Flip through the tabs at the top to learn more about the latest trends.',
          h2('Related Puget Sound Regional Council Data Resources'),
          tags$div(
            tags$img(
              src = "trends-image.png",
              width = "25%",
              height = "25%",
              style = "padding-top: 10px; border-radius:30px 0 30px 0;",
              alt = "trend logo"
            ),
            tags$a(href = "https://www.psrc.org/puget-sound-trends", "More Puget Sound Trends at psrc.org")
          ),
          tags$div(
            tags$img(
              src = "hackathon.jpg",
              width = "25%",
              height = "25%",
              style = "padding-top: 10px; border-radius:30px 0 30px 0;",
              alt = "people programming and eating pizza"
            ),
            tags$a(href = "https://github.com/psrc/data-trends-app", "Code for this app on Github!")
          ),
          tags$div(
            tags$img(
              src = "portal.jpg",
              width = "25%",
              height = "25%",
              style = "padding-top: 10px; border-radius:30px 0 30px 0;",
              alt = "portal picture"
            ),
            tags$a(href = "https://psrc-psregcncl.hub.arcgis.com/", "PSRC Open Data Portal")
          ),
          tags$div(
            tags$img(
              src = "community-profiles.jpg",
              width = "25%",
              height = "25%",
              style = "padding-top: 10px; border-radius:30px 0 30px 0;",
              alt = "portal picture"
            ),
            tags$a(href = "https://psrcwa.shinyapps.io/community-profiles", "Community Profiles")
          )
        )
      )
      
    })
    
  }) # end moduleServer
  
}