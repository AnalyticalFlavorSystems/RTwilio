require(jsonlite)
require(httr)


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
#' @param from The phonenumber to call.  Check Your Twilio account for the number.  
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

.post <- function(url, body, accountSID, auth_token) {
  res <- POST(url = url, user_agent="RTwilio", 
              config=authenticate(accountSID, auth_token, "basic"), body=body)
  content(res)
}
