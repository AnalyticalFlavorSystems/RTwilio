#' Call someone with a TwiML Voice
#'
#' You can use this to call someone and use a pre-uploaded twilio XML (TwiML) 
#' file with what is to be said.
#'
#' @param accountSID Your account ID
#'
#' @param auth_token Your secret token
#'
#' @param xmlURL The URL where the TwiML file is located.
#'
#' @param from The phonenumber that calls.  Check Your Twilio account for the number.  
#' Make sure to include the international number.
#'
#' @param to The phone number to call.  Make sure to include the international number.
#'
#' @export
#'
twilio_call <- function(accountSID, auth_token, xmlURL, from, to) {
   url <- paste0('https://api.twilio.com/2010-04-01/Accounts/', accountSID, '/Calls.json')
   body <- list(Url=xmlURL, From=from, Method="GET", To=to)
   .post(url, body, accountSID, auth_token)
} 


#' Send an outbound call without writing the file to disk
#'
#' this function sends an outbound call using a twillio twimlet,
#' allowing stateless application calling functionality
#'
#' @param accountSID Your account ID
#'
#' @param auth_token Your secret token
#'
#' @param message the message to transcribe to speech. By default this will be spoken by a female
#' in british english and repeated 3 times. \emph{Either message or Twxml must be null!
#' you cannot send a call with both parameters set}
#'
#' @param TwXML Any properly formated XML acceptable by Twillio
#'
#' @param from The phonenumber that calls. Check Your Twilio account for the number.
#' Make sure to include the international number.
#'
#' @param to The phone number to call. Make sure to include the international number.
#'
#' @export
#'
twilio_stateless_call <- function(accountSID, auth_token, message=NULL, TwXML=NULL, From, To) {
  ## Initial Error Checking
  if(is.null(message) & is.null(TwXML)) stop("Either message or TwXML must be set!")
  if(all(!is.null(message), !is.null(TwXML))) stop("Only message or TwXML can be set at one time!")

  if(is.null(From)) stop("A Twillio registered From number must be set")
  if(is.null(To)) stop("A To number must be set")


  ## Base Auth URL for POST
  url <- paste0('https://api.twilio.com/2010-04-01/Accounts/', accountSID, '/Calls.json')
  ## Base URL for stateless Call
  callURL <- "http://twimlets.com/echo?Twiml="

  if(is.null(TwXML)) {
    TwXML <- URLencode(paste0("<Response><Say voice='alice' language='en-gb' loop='3'>", message,
                              "</Say></Response>"))
  } else {
    ## Encode XML structure as a url charecter vector
    TwXML <- URLencode(as(TwXML, "character"))
  }

  ## Implicit Multi Call
  if(length(To) > 1) {
    sapply(To, function(z) {
      .post(url, list(Url=paste0(callURL, TwXML), From=From, Method="GET", To=z), accountSID, auth_token)
      ### Don't worry about Call+SMS rate-limiting from Twillio here!
      ### Twillio keeps its own message queue
      })
  } else {
    
    ## Single Call
    .post(url, list(Url=paste0(callURL, TwXML), From=From, Method="GET", To=To), accountSID, auth_token)
      ### Don't worry about Call+SMS rate-limiting from Twillio here!
      ### Twillio keeps its own message queue
  }
}


# Internal Helper function - do not export
.post <- function(url, body, accountSID, auth_token) {
  res <- POST(url = url, user_agent="RTwilio", 
              config=authenticate(accountSID, auth_token, "basic"), body=body)
  content(res)
}
