## Twillio Texting

#' Send a SMS text message from Twillio
#'
#' This function can send a text message to one or multiple numbers
#'
#' @param accountSID Your account ID
#'
#' @param auth_token Your secret token
#'
#' @param message the message to send in  a text.
#'
#' @param From The phonenumber that calls. Check Your Twilio account for the number.
#' Make sure to include the international number.
#'
#' @param To The phone number to call. \emph{You can include multple numbers as a quoted charecter
#' vector to sent the same message to multiple phones at the same time}.
#' Make sure to include the international number.
#'
#' @export
#'
twilio_stateless_SMS <- function(accountSID, auth_token, message=NULL, From, To) {
  ## Initial Error Checking
  if(is.null(message)) stop("a charecter message must be set!")

  if(is.null(From)) stop("A Twillio registered From number must be set")
  if(is.null(To)) stop("A To number must be set")


  ## Base Auth URL for POST
  url <- paste0('https://api.twilio.com/2010-04-01/Accounts/', accountSID, '/Messages.json')

  ## Implicit Multi Call
  if(length(To) > 1) {
    sapply(To, function(z) {
      .post(url, list(Body=message, From=From, Method="GET", To=z), accountSID, auth_token)
      ## Sleep 1 second before sending SMS:
      ### Requiered becuase of Call+SMS rate-limiting from Twillio!
      Sys.sleep(1)
    })

  } else {
    ## Single Call
    .post(url, list(Body=message, From=From, Method="GET", To=To), accountSID, auth_token)
      ## Sleep 1 second before sending SMS:
      ### Requiered becuase of Call+SMS rate-limiting from Twillio!
      Sys.sleep(1)
  }
}
