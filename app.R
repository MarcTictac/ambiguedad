library(shiny)
library(stringr)

# Función para detectar indicios de ambigüedad intencionada
detect_ambiguity <- function(texto) {
  # Convertir el texto a minúsculas para facilitar la búsqueda
  texto <- tolower(texto)
  
  # Características textuales indicativas de ambigüedad intencionada
  palabras_vagas <- sum(str_count(texto, regex("\\b(algo|alguien|cosa|tal vez|posiblemente|quizás|aproximadamente|más o menos|casi|un poco|un tanto|alguno|bastante|unos|demasiado|poco|mucho|indeterminado|incierto|dudoso|ambiguo|difuso|equívoco|irregular|inexacto|impreciso|vago|inconcreto|subjetivo|general|común|ordinario|genérico|usual|habitual|similar|semejante|razonable|medio|regular|normal|estimación|cálculo|aproximación|promedio|borrador|posibilidad|conjetura|posible|puede|podría|algunos|varios|ciertos|cierto|cierta|ciertas|a menudo|frecuentemente|potencialmente|dependiendo|aproximadamente|muchos|posible|puede|podría|algunos|varios|ciertos|a menudo|frecuentemente|potencialmente|dependiendo|aproximadamente|muchas)\\b", ignore_case = TRUE)))
  palabras_con_multiples_significados <- sum(str_count(texto, regex("\\b(llamada|planta|carta|gato|mano|sierra|canal|copa|bola|clave|carrera|cola|batería|cámara|fila|caña|pasillo|orden|blanco|conductor|tapa|volante|regla|nota|disco|botón|papel|clavo|tarjeta|cuadro|muñeca|llave|red|torre|rueda|tecla|tronco|juego|curso|clima|cámara|banco|cálculo|ratón|mango|rama|pico|vela|cura|tasa|banda)\\b", ignore_case = TRUE)))
  frases_ambiguas <- sum(str_count(texto, regex("\\b(de cierta manera|en cierto sentido|hasta cierto punto|en algunos casos|de una forma u otra|es decir|o sea|quiere decir)\\b", ignore_case = TRUE)))
  omision_informacion <- sum(str_count(texto, regex("\\b(etc|y demás|y así sucesivamente|entre otros|como sea|en fin|etc.|etcétera|y cosas por el estilo|y otros tantos)\\b", ignore_case = TRUE)))
  
  # Evaluar la presencia de ambigüedad intencionada
  resultados <- list(
    palabras_vagas = ifelse(palabras_vagas > 0, "Uso de palabras vagas detectado", "No se detectó uso de palabras vagas"),
    palabras_con_multiples_significados = ifelse(palabras_con_multiples_significados > 0, "Uso de palabras con múltiples significados detectado", "No se detectó uso de palabras con múltiples significados"),
    frases_ambiguas = ifelse(frases_ambiguas > 0, "Uso de frases ambiguas detectado", "No se detectó uso de frases ambiguas"),
    omision_informacion = ifelse(omision_informacion > 0, "Omisión de información detectada", "No se detectó omisión de información")
  )
  
  return(resultados)
}

# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Detección de Ambigüedad Intencionada en Texto"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("texto", "Ingresa el texto a analizar:", rows = 10, width = '100%'),
      actionButton("analizar", "Analizar Texto")
    ),
    
    mainPanel(
      h3("Resultados del Análisis"),
      verbatimTextOutput("resultados")
    )
  )
)

# Definir el servidor
server <- function(input, output) {
  observeEvent(input$analizar, {
    texto <- input$texto
    resultados <- detect_ambiguity(texto)
    output$resultados <- renderPrint({
      print(resultados)
    })
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
