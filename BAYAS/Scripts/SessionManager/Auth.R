mailAuth <- function(){
  #Env vars
  MAIL_AUTH <- ""
  MAIL_AUTH_PW <- ""
  SESSION_CRYPT_KEY <- "aa"
  tryCatch({
    MAIL_AUTH <- Sys.getenv("BAYAS_SMTP_USER")
    MAIL_AUTH_PW <- Sys.getenv("BAYAS_SMTP_PASS")
    SESSION_CRYPT_KEY <- Sys.getenv("BAYAS_CYRPTKEY")
  })
  
  list(
    MAIL_AUTH=MAIL_AUTH,
    MAIL_AUTH_PW=MAIL_AUTH_PW,
    SESSION_CRYPT_KEY=SESSION_CRYPT_KEY
  )
}
