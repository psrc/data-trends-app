shinyUI(
  
  navbarPage(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "Mode to Work",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = tags$a(div(tags$img(src='footer-logo.png',
                                style="margin-top: -30px; padding-left: 40px;",
                                height = "80")
    ), href="https://www.psrc.org", target="_blank"),
    tags$head(
      tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:25px !important; 
                            padding-bottom:0 !important;
                            height: 75px;
                            }
                           .navbar {min-height:25px !important;}'))
    ),
    
    windowTitle = "Data Trends in the Puget Sound", 
    theme = "styles.css",
    position = "fixed-top",
    tabPanel(
    title="Overview", h1("Data Trends by Puget Sound Regional Council"),
             tags$img(src="banner.png",  width = "100%", height = "100%",style = "padding-top: 10px; padding-bottom: 10px; border-radius:30px 0 30px 0; position:center", 
                      alt = "colorful data bars"),
             fluidRow(column(12, style='padding-left:25px; padding-right:25px;padding-bottom: 10px','The Puget Sound Regional Council(PSRC) provides data to help local planners and decision-makers better understand the region and visualize its future.Flip through the tabs at the top to learn more about the latest trends.',
                             h2('Related Puget Sound Regional Council Data Resources'),
                             tags$div(
                                      tags$img(src="trends-image.png", width = "25%", height = "25%", style = "padding-top: 10px; border-radius:30px 0 30px 0;", 
                                                alt = "trend logo"),
                                      tags$a(href="https://www.psrc.org/puget-sound-trends", "More Puget Sound Trends at psrc.org")
                                     ),
                             tags$div(
                               tags$img(src="portal.jpg", width = "25%", height = "25%", style = "padding-top: 10px; border-radius:30px 0 30px 0;", 
                                        alt = "portal picture"),
                               tags$a(href="https://psrc-psregcncl.hub.arcgis.com/", "PSRC Open Data Portal")
                                     ),
                             tags$div(
                               tags$img(src="community-profiles.jpg", width = "25%", height = "25%", style = "padding-top: 10px; border-radius:30px 0 30px 0;", 
                                        alt = "portal picture"),
                               tags$a(href="https://psrcwa.shinyapps.io/community-profiles", "Community Profiles")
                                      )
                             
                            )# end column
                   )#end fluidRow
             ), #end mainPanel for Overview Page,
             tabPanel(     
             title="Commute Mode",
             value="Mode-Page",
             banner_ui('modeBanner'),
             fluidRow(column(12, style='padding-left:25px; padding-right:25px;',
                             mode_overview_ui('modeOverview'),
                             hr(style = "border-top: 1px solid #000000;"),
                             commute_modes_ui('CommuteMode')
                                )
                             ), # End of Main Panel Modes
                      ), # End of Main Panel Fluid Row for Modes Tab
    
            tabPanel(title=icon("info-circle"),
             value="Data-Source-Page",
             hr(style = "border-top: 1px solid #000000;"),
             source_ui('dataSource')),
             hr(style = "border-top: 1px solid #000000;"),
    
    tags$footer(footer_ui('psrcfooter'))
    
    )
)

  