# sudo yum install libpng-devel
# sudo yum install libjpeg-turbo-devel
lapply(c('shiny', 'shinyBS'), library, character.only = TRUE)
lapply(list.files('~/apps/places/src/', full.names = TRUE), source)

### Utility Functions

mk_filename <- function(data) {
  sprintf("%s_%s", as.integer(Sys.time()), digest::digest(data))
}

mk_url <- function(img_file, order_details, id, q, gs) {
  url_base = paste0('http://amorata.myshopify.com/cart/', id, ':', q, '?')
  file_base = 'http://amorata-apps.com:8787/files/apps/places/plots/'
  
  img_attr = paste0('attributes[img-file]=', file_base, img_file)
  gs_attr  = paste0('attributes[graphic-size]=', gs)
  od_attr  = paste0('attributes[order-details]=', paste(order_details,  collapse = ' '))
  all_attr = paste(img_attr, gs_attr, od_attr, sep = '&')
  
  paste0(url_base, all_attr)
}

selInput <- function(inputId, choices, placeholder) {
  selectizeInput(inputId = inputId, 
                 label = NULL, 
                 choices = choices,
                 options = list(
                   placeholder = placeholder,
                   onInitialize = I('function() { this.setValue(""); }')))
}

render_detail_row <- function(i, garment, hex) {
  shirt_sizes  = switch(garment,
                        tee = c('xs', 's', 'm', 'l', 'xl', 'xxl', 'xxxl'),
                        hoodie = c('xs', 's', 'm', 'l', 'xl', 'xxl'),
                        'long-sleeve' = c('s', 'm', 'l', 'xl', 'xxl'))
  shirt_colors = switch(garment,
                        tee = c('white', 'ash', 'heather grey', 'black'),
                        hoodie = c('white', 'heather grey', 'black'),
                        'long-sleeve' = c('white', 'black'))
  
  if (hex == '#FFFFFF') {shirt_colors = shirt_colors[!(shirt_colors %in% c('white',  'ash'))]}
  if (hex == '#000000') {shirt_colors = shirt_colors[shirt_colors != 'black']}
  if (hex == '#F4DE5B') {shirt_colors = shirt_colors[shirt_colors != 'white']}
  
  renderUI({
    fluidRow(
      column(4, selInput(paste0('shirt_color', i), shirt_colors, 'garmet color')),
      column(4, selInput(paste0('shirt_size', i), shirt_sizes, 'garment size')),
      column(4, numericInput(paste0('q',i), NULL, 1, 1, step = 1))
    )
  })
}

intro_card <- function(img_file) {
  bsCollapsePanel(
    'intro',
    img(src = img_file, 
        align = 'center', 
        width = '100%', 
        height = 'auto')
  )
}

garment_card <- function(inputId, choices) {
  bsCollapsePanel(
    'garment type',
    p('select garment type (tee, hoodie, long sleeve)'),
    radioButtons_withHTML(inputId, 
                          NULL, 
                          choices = choices, 
                          inline = FALSE)
  )
}

image_size_card <- function(inputId, choices) {
  bsCollapsePanel(
    'graphic size',
    p('across the chest (11"x11") or left pocket (4.5"x4.5")'),
    radioButtons_withHTML(inputId, 
                          NULL, 
                          choices = choices, 
                          inline = FALSE)
  )
}

sample_ligtbox <- function() {
  tags$div(
    fluidRow(
      lapply(list.files('~/apps/places/www/samples/'), function(x) {
        column(4, align = 'center', img(src = paste0('samples/', x), height = '260px'))
      })
    )
  )
}
