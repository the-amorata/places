library(shinyjs)
source('~/apps/places/util.r')
 
ids = c('23659736197', '24373273029', '24373350533')
display_cols = c('#1b87e0','#ed635f','#6abf90','#ff814a','#8369a8','#f4de5b', '#FFFFFF', '#000000')
garments  = c('<img src="garment-type/tee.jpg">' = 'tee',
              '<img src="garment-type/hoodie.jpg">' = 'hoodie', 
              '<img src="garment-type/long-sleeve.jpg">' = 'long-sleeve')
image_sizes  = c('<img src="graphic-size/across-the-chest.jpg">' = 'across chest 7"x7"', 
                 '<img src="graphic-size/left-pocket.jpg">' = 'left pocket 2.5"x2.5"')
no_zoom = "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0\"/>"

jsCode <- "shinyjs.order = function(params) {window.location.href = params[0];}"

fluidPage(
  
  title = 'P L A C E S',
  theme = 'amorata.css',
  
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c('order')),
  
  fluidRow(
    column(
      8, 
      offset = 2, 
      align = 'center',
      
      tags$head(HTML(no_zoom)),
      h1('P L A C E S'),
      
      bsCollapse(
        id = "main", 
        open = "intro",
        
        intro_card('banner.jpg'),
        
        bsCollapsePanel(
          'create',
          p('1. set address/location. 2. pick state or county map view. 3. select graphic color.'),
          imageOutput('state', height = 'auto'),
          fluidRow(
            br(),
            column(6, textInput('ll', NULL, placeholder = "address / location name")),
            column(2, selectInput('lvl', NULL, c('state', 'county'))),
            column(
              2,
              colourpicker::colourInput('hex',
                                        NULL,
                                        palette = 'limited',
                                        allowedCols = display_cols,
                                        showColour = 'background',
                                        value = display_cols[2])
            ),
            column(2, actionButton('search', 'CREATE'))
          )
        ),
        
        garment_card('garment', garments),
        image_size_card('image_size', image_sizes),
        bsCollapsePanel('details', uiOutput('details_card')),
        bsCollapsePanel('review', uiOutput('checkout'))
      ), # end of bsCollapse
      helpText(
        'comments/questions?', 
        a('contact amorata', href = 'mailto:info@amoratadesigns.com')
      )
    ) # end of centering column
  ) # end of main fluidRow
) # end of everything
