library(shiny)
library(cookies)

extract_cookie_value <- function(session, cookie_name) {
  cookie_string <- session$request$HTTP_COOKIE
  if (is.null(cookie_string)) {
    return(NULL)
  }
  cookies <- unlist(strsplit(cookie_string, "; "))
  user_cookie <- grep(paste0("^", cookie_name, "="), cookies, value = TRUE)
  if (length(user_cookie) > 0) {
    return(unlist(strsplit(user_cookie, "="))[2])
  } else {
    return(NULL)
  }
}

ui_page1 <- fluidPage(
  tags$head(
    tags$script(
      HTML("
        Shiny.addCustomMessageHandler('redirect', function(message) {
          var xhr = new XMLHttpRequest();
          xhr.open('GET', '/cookie', true);
          xhr.setRequestHeader('Header-User-Cookie', message);
          xhr.onreadystatechange = function() {
            if (xhr.readyState == 4 && xhr.status == 200)
            window.location = 'close';
          };
          xhr.send();
        });
        Shiny.addCustomMessageHandler('redirect_remove', function(message) {
          var xhr = new XMLHttpRequest();
          xhr.open('GET', '/cookie_remove', true);
          xhr.onreadystatechange = function() {
            if (xhr.readyState == 4 && xhr.status == 200)
            window.location = 'close';
          };
          xhr.send();
        });
        Shiny.addCustomMessageHandler('refreshApp', function(message) {
          window.location.reload();
        });
      ")
    )
  ),
  titlePanel("shinyCookiesToyApp"),
  sidebarLayout(
    sidebarPanel(
      actionButton("action_btn", "Set httpOnly cookie!"),
      actionButton("action_btn2", "Remove httpOnly cookie!")
    ),
    mainPanel(
      textOutput("cookie_output")
    )
  )
)

ui_page2 <- set_cookie_response(
  cookie_name = "newCookie",
  cookie_value = "newCookieValue",
  http_only = TRUE,
  secure_only = TRUE,
  redirect = "/close",
  same_site = "Strict"
)

ui_page3 <- set_cookie_response(
  cookie_name = "newCookie",
  cookie_value = "",
  http_only = TRUE,
  secure_only = TRUE,
  expiration = -1,
  redirect = "/close",
  same_site = "Strict"
)

custom_ui <- function(req) {
   if (req$PATH_INFO == '/') {
     return(ui_page1)
   } else if (req$PATH_INFO == "/cookie") {
     return(ui_page2)
   } else if (req$PATH_INFO == "/cookie_remove") {
     return(ui_page3)
   } else if (req$PATH_INFO == "/close") {} else {
     return("404: Page not found")
   }
}

server <- function(input, output, session) {
  observeEvent(input$action_btn, {
    session$sendCustomMessage(type = "redirect", message = "aaaa")
    session$sendCustomMessage(type = "refreshApp", message = "")
  })

  observeEvent(input$action_btn2, {
    session$sendCustomMessage(type = "redirect_remove", message = "")
    session$sendCustomMessage(type = "refreshApp", message = "")
  })

  output$cookie_output <- renderText({
    extract_cookie_value(session, "newCookie")
  })
}

# Run the application
shinyApp(ui = custom_ui, server = server, uiPattern = ".*")
