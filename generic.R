library("httr2")
library("jsonlite")
library("getopt")

cat("start generic wrapper service \n")

getParameters <- function(){
    con <- file("inputs.json", "r")
    line <- readLines(con, n = 1)
    json <- fromJSON(line)
    close(con)
    return(json)
}

parseResponseBody <- function(body) {
  hex <- c(body)
  intValues <- as.integer(hex)
  rawVector <- as.raw(intValues)
  readableOutput <- rawToChar(rawVector)
  jsonObject <- jsonlite::fromJSON(readableOutput)
  return(jsonObject)
}

getOutputs <- function(inputs, output, server, process) {
    url <- paste(paste(server, "processes/", sep = ""), process, sep = "")
    request <- request(url)
    response <- req_perform(request)
    responseBody <- parseResponseBody(response$body)
    outputs <- list()

    for (x in 1:length(responseBody$outputs)) {
        outputformatName <- paste(names(responseBody$outputs[x]), "_outformat", sep="")
        output_item <- list()

        for (p in names(inputs)) {
            if(p == outputformatName){
                format <- list("mediaType" = inputs[[outputformatName]])
                output_item$format <- format
            }
        }
        output_item$transmissionMode <- "reference"
        outputs[[x]] <- output_item
    }

    names(outputs) <- names(responseBody$outputs)
    return(outputs)
}

executeProcess <- function(url, process, requestBodyData, output) {
    url <- paste(paste(paste(url, "processes/", sep = ""), process, sep = ""), "/execution", sep = "")
    cookie <- requestBodyData$inputs$cookie
    requestBodyData$inputs$cookie <- NULL
    print("hallo1")
    print(requestBodyData)
    print(cookie)
    print("hallo2")
    response <- request(url) %>%
    req_headers(
      #"User-Agent" = "python-requests/2.31.0", 
      #"Accept-Encoding" = "gzip, deflate, br", 
      #"Accept" = "*/*", 
      #"Connection" = "keep-alive", 
      "Prefer" = "respond-async",
      "Authorization" = cookie
      #"Content-Length" = "196",
      #"Content-Type" = "application/json"
    ) %>%
    req_body_json(requestBodyData) %>%
    req_perform()

    cat("\n Process executed")
    cat("\n status: ", response$status_code)
    cat("\n jobID: ", parseResponseBody(response$body), "\n")
    
    job_location <- response |> resp_headers("location")
    jobID <- sub(".*\\/([a-z0-9\\-]+)$", "\\1", job_location$location)
    #jobID <- parseResponseBody(response$body)$jobID

    return(jobID)
}

checkJobStatus <- function(server, jobID) {
  response <- request(paste0(server, "jobs/", jobID)) %>%
    req_headers(
        'accept' = 'application/json'
    ) %>%
    req_perform()
  jobResponse <- parseResponseBody(response$body) 
  jobStatus <- jobResponse$status
  jobProgress <- jobResponse$progress
  cat(paste0("\n status: ", jobStatus, ", progress: ", jobProgress))
  return(jobResponse)
}

getStatusCode <- function(server, jobID) {
  url <- paste0(server, "jobs/", jobID)
  response <- request(url) %>%
      req_headers(
        'accept' = 'application/json'
      ) %>%
      req_perform()
  return(response$status_code)
}

getResult <- function (server, jobID) {
  print("start getting result")
  print(paste0(server, "jobs/", jobID, "/results"))
  response <- request(paste0(server, "jobs/", jobID, "/results")) %>%
    req_headers(
      'accept' = 'application/json'
    ) %>%
    req_perform()
  return(response)
}

retrieveResults <- function(server, jobID, outputData) {
    status_code <- getStatusCode(server, jobID)
    print(status_code)
    if(status_code == 200){
        status <- "running"
        cat(status)
        while(status == "running"){
            jobRes <- checkJobStatus(server, jobID)
            jobStatus <- jobRes$status
            if (jobStatus == "successful") {
                status <- jobStatus
                #result <- getResult(server, jobID)
                #if (result$status_code == 200) {
                #  resultBody <- parseResponseBody(result$body)
                #  urls <- unname(unlist(lapply(resultBody, function(x) x$href)))
                #  urls_with_newline <- paste(urls, collapse = "\n")
                con <- file(outputData, "w")
                print("----------------")
                print(jobRes)
                print("----------------")
                print(jobRes$links)
                print("----------------")
                print(jobRes$links[3])
                print("----------------")
                print(jobRes$links[3]$href)
                print("----------------")
                print(jobRes$links$href)
                print("----------------")
                print(jobRes$links$href[3])
                print("----------------")
                writeLines(jobRes$links$href[3], con = con)
                close(con)
                #}
            } else if (jobStatus == "failed") {
              status <- jobStatus
            }
        Sys.sleep(10)
        }
        cat("\n done \n")
    } else if (status_code1 == 400) {
    print("A query parameter has an invalid value.")
  } else if (status_code1 == 404) {
    print("The requested URI was not found.")
  } else if (status_code1 == 500) {
    print("The requested URI was not found.")
  } else {
    print(paste("HTTP", status_code1, "Error:", resp1$status_message))
  }
}

is_url <- function(x) {
  grepl("^https?://", x)
}

print("--> Retrieve parameters")
inputParameters <- getParameters()
print("--> Parameters retrieved")

args <- commandArgs(trailingOnly = TRUE)

server <- args[2]

outputLocation <- args[4]

print("--> Retrieve outputs")
process <- "water-quality-eo-app-pkg"
outputs <- getOutputs(inputParameters, outputLocation, server, process)
print("--> Outputs retrieved")

print("--> Parse inputs")
convertedKeys <- c()
for (key in names(inputParameters)) {
  if (is.character(inputParameters[[key]]) && (endsWith(inputParameters[[key]], ".dat") || endsWith(inputParameters[[key]], ".txt"))) { 
    con <- file(inputParameters[[key]], "r")
    url_list <- list()
    while (length(line <- readLines(con, n = 1)) > 0) {
      if (is_url(line)) {
        url_list <- c(url_list, list(list(href = trimws(line))))
      }
    }
    close(con)
    inputParameters[[key]] <- url_list
    convertedKeys <- append(convertedKeys, key)
  }
  else if (grepl("_Array_", key)) {
    keyParts <- strsplit(key, split = "_")[[1]]
    type <- keyParts[length(keyParts)]
    values <- inputParameters[[key]]
    value_list <- strsplit(values, split = ",")

    convertedValues <- c()

    for (value in value_list) {
      if(type == "integer") {
        value <- as.integer(value)
      } else if (type == "numeric") {
        value <- as.numeric(balue)
      } else if (type == "character") {
        value <- as.character(value)
      }
    convertedValues <- append(convertedValues, value)

    convertedKey <- ""
    for (part in keyParts) {
      if(part == "Array") {
        break
      }
      convertedKey <- paste(convertedKey, paste(part, "_", sep=""), sep="")
    }
    convertedKey <- substr(convertedKey, 1, nchar(convertedKey)-1)
}

    inputParameters[[key]] <- convertedValues
    convertedKeys <- append(convertedKeys, convertedKey)
  } else {
    convertedKeys <- append(convertedKeys, key)
  }
}
print(convertedKeys)
names(inputParameters) <- convertedKeys
print("--> Inputs parsed")

print("--> Prepare process execution")
jsonData <- list(
  "inputs" = inputParameters,
  "outputs" = outputs
)

print("--> Execute process")
jobID <- executeProcess(server, process, jsonData, outputLocation)
print("--> Process executed")

print("--> Retrieve results")
retrieveResults(server, jobID, outputLocation)
print("--> Results retrieved")