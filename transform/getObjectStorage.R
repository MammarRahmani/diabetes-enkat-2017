getObjectStorageFileWithCredentials_2b960019ae7a47dd9601026211e617af <- function(container, filename) {
  # This functions returns a textConnection object for a file
  # from Bluemix Object Storage.
  
  if(!require(httr)) install.packages('httr')
  if(!require(RCurl)) install.packages('RCurl')
  library(httr, RCurl)
  auth_url <- paste("https://identity.open.softlayer.com",'/v3/auth/tokens', sep= '')
  auth_args <- paste('{"auth": {"identity": {"password": {"user": {"domain": {"id": ', "a2710b40b6be4d6daa778694c1070db6",'},"password": ',
                     "jXusW7A{k4[bW6Bb",',"name": ', "member_6426f285b9adb2ff8fbd8dbbce62f14a7dfa3230",'}},"methods": ["password"]}}}', sep='"')
  auth_response <- httr::POST(url = auth_url, body = auth_args)
  x_subject_token <-  headers(auth_response)[['x-subject-token']]
  auth_body <-  content(auth_response)
  access_url <-  unlist(lapply(auth_body[['token']][['catalog']], function(catalog){
    if((catalog[['type']] == 'object-store')){
      lapply(catalog[['endpoints']], function(endpoints){
        if(endpoints[['interface']] == 'public' && endpoints[['region_id']] == 'dallas') {
          paste(endpoints[['url']], container, filename, sep='/')}
      })
    }
  }))
  data <- content(httr::GET(url = access_url, add_headers ("Content-Type" = "application/json", "X-Auth-Token" = x_subject_token)), as="text")
  textConnection(data)
}