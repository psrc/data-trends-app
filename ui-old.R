shinyUI(fluidPage(
  
  titlePanel(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "landing",
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
    title="Overview", h1("Data Trends by Puget Sound Regional Council"),
             tags$img(src="banner.png",  width = "100%", height = "100%",style = "padding-top: 10px; padding-bottom: 10px; border-radius:30px 0 30px 0; position:center", 
                      alt = "colorful data bars")
             ), #end titlePanel for Overview Page,
      sidebarLayout("Data Trend Topics",
             Panel(     
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
)
  
