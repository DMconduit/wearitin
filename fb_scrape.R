
# These are functions taken from the fitbitScraper package by Cory Nissen
# via GitHub
# https://github.com/corynissen/fitbitScraper/blob/master/R/get_sleep_data.R

#########################
#		NOTE			#
#########################
# using this rather than the CRAN package because
# the "as" function in get_intraday_day was thrwing an error
# I used a different approach in the version below

login <- function(email, password, rememberMe=FALSE){
	#' Login to fitbit.com and get cookie
	#'
	#' Get the login cookie after login at www.fitbit.com
	#' @param email Email address used to login to fitbit.com
	#' @param password Password used to login to fitbit.com
	#' @param rememberMe Value for rememberMe during login, default is FALSE, but changing to TRUE may help with login issues
	#' @keywords login
	#' @export
	#' @return A string containing the cookie that is returned after login at www.fitbit.com
	#' @examples
	#' \dontrun{
	#' cookie <- login(email="corynissen<at>gmail.com", password="mypasswordhere")
	#' }
	#' login
  if(!is.character(email)){stop("email must be a character string")}
  if(!is.character(password)){stop("password must be a character string")}

  rememberMe <- ifelse(rememberMe, "true", "false")

  url <- "https://www.fitbit.com/login"
  headers <- list("Host" = "www.fitbit.com",
                  "Connection" = "keep-alive",
                  "Content-Length" = "278",
                  "Cache-Control" = "max-age=0",
                  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                  "Origin" =  "https://www.fitbit.com",
                  "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.99 Safari/537.36",
                  "Content-Type" =  "application/x-www-form-urlencoded",
                  "Referer" = "https://www.fitbit.com/login",
                  "Accept-Encoding" = "gzip, deflate",
                  "Accept-Language" = "en-US,en;q=0.8")
  body <- list("email"=email, "password"=password, "rememberMe"=rememberMe,
               "login"="Log In")

  a <- httr::POST(url, headers=headers, body=body)
  cookie <- a$cookies$u
  if(is.null(cookie)){
    all_cookies <- a$cookies
    cookie <- all_cookies[grep("^u$", all_cookies$name, ignore.case=F),c("name", "value")]$value
    if(is.null(cookie)){
      stop("login failed")
    }
  }

  return(cookie)
}

get_intraday_data <- function(cookie, what="steps", date){
	#' Get intraday data from fitbit.com
	#'
	#' Get intraday data from fitbit using cookie returned from login function
	#' @param cookie Cookie returned after login, specifically the "u" cookie
	#' @param what What data you wish to be returned. Options include "steps", "distance", "floors", "active-minutes", "calories-burned", "heart-rate"
	#' @param date Date in YYYY-MM-DD format
	#' @keywords data
	#' @export
	#' @return A dataframe with two columns:
	#'  \item{time}{A POSIXct time value}
	#'  \item{data}{The data column corresponding to the choice of "what"}
	#' @examples
	#' \dontrun{
	#' get_intraday_data(cookie, what="steps", date="2015-01-20")
	#' }
	#' get_intraday_data
  if(!is.character(cookie)){stop("cookie must be a character string")}
  if(!is.character(what)){stop("what must be a character string")}
  if(!is.character(date)){stop("date must be a character string")}
  if(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", date)){stop('date must have format "YYYY-MM-DD"')}
  if(!what %in% c("steps", "distance", "floors", "active-minutes", "calories-burned",
                  "heart-rate")){
    stop('what must be one of "steps", "distance", "floors", "active-minutes", "calories-burned",
         "heart-rate"')
  }
  url <- "https://www.fitbit.com/ajaxapi"
  request <- paste0('{"template":"/ajaxTemplate.jsp","serviceCalls":[{"name":"activityTileData","args":{"date":"',
                    date,
                    '","dataTypes":"',
                    what,
                    '"},"method":"getIntradayData"}]}'
  )
  csrfToken <- stringr::str_extract(cookie,
                                    "[A-Z0-9]{8}\\-[A-Z0-9]{4}\\-[A-Z0-9]{4}\\-[A-Z0-9]{4}\\-[0-9A-Z]{12}")					
  body <- list(request=request, csrfToken = csrfToken)
  response <- httr::POST(url, body=body, httr::config(cookie=cookie))
  dat_string <- as.character(response)
  dat_list <- RJSONIO::fromJSON(dat_string, asText=TRUE)
  dat_list <- dat_list[[1]]$dataSets$activity$dataPoints
  if(what=="heart-rate"){
    df <- data.frame(time=as.character(unlist(sapply(dat_list, "[", "dateTime"))),
                     data=as.numeric(unlist(sapply(dat_list, "[", "bpm"))),
                     stringsAsFactors=F)
  }else{
    df <- data.frame(time=as.character(unlist(sapply(dat_list, "[", "dateTime"))),
                     data=as.numeric(unlist(sapply(dat_list, "[", 2))),
                     stringsAsFactors=F)
  }
  names(df) <- c("time", what)
  tz <- Sys.timezone()
  if(is.null(tz)){tz <- format(Sys.time(),"%Z")}
  df$time <- as.POSIXct(df$time, "%Y-%m-%d %H:%M:%S", tz=tz)
  return(df)
}

get_sleep_data <- function(cookie, start_date="2015-01-13", end_date="2015-01-20"){
  	#' Get sleep data from fitbit.com
	#'
	#' Get sleep data from fitbit using cookie returned from login function
	#' @param cookie Cookie returned after login, specifically the "u" cookie
	#' @param start_date Date in YYYY-MM-DD format
	#' @param end_date Date in YYYY-MM-DD format
	#' @keywords data
	#' @export
	#' @return A list with two things
	#'  \item{summary}{A list of sleep summary values}
	#'  \item{df}{A data frame containing various sleep values over time}
	#' @examples
	#' \dontrun{
	#' get_sleep_data(cookie, start_date="2015-01-13", end_date="2015-01-20")
	#' }
	#' get_sleep_data
  if(!is.character(cookie)){stop("cookie must be a character string")}
  if(!is.character(start_date)){stop("start_date must be a character string")}
  if(!is.character(end_date)){stop("end_date must be a character string")}
  if(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", start_date)){stop('start_date must have format "YYYY-MM-DD"')}
  if(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", end_date)){stop('end_date must have format "YYYY-MM-DD"')}

  url <- "https://www.fitbit.com/ajaxapi"
  request <- paste0('{"template":"/ajaxTemplate.jsp","serviceCalls":[{"name":"activityTileData","args":{"dateFrom":"',
                    start_date,
                    '","dateTo":"',
                    end_date,
                    '"},"method":"getSleepTileData"}]}'
                    )

  csrfToken <- stringr::str_extract(cookie,
                                    "[A-Z0-9]{8}\\-[A-Z0-9]{4}\\-[A-Z0-9]{4}\\-[A-Z0-9]{4}\\-[0-9A-Z]{12}")
  body <- list(request=request, csrfToken = csrfToken)
  response <- httr::POST(url, body=body, httr::config(cookie=cookie))

  dat_string <- as.character(response)
  dat_list <- RJSONIO::fromJSON(dat_string, asText=TRUE)
  if("hasLoggedSleep" %in% names(dat_list)){
    summary <- list(avgSleepDuration = dat_list$avgSleepDuration,
                    avgSleepTime = dat_list$avgSleepTime,
                    avgSleepScore = dat_list$avgSleepScore,
                    avgGraphicPercent = dat_list$avgGraphicPercent)
    # get individual day data
    df <- data.frame(cbind(date = sapply(dat_list$entries, "[[", "date"),
                           startTime = sapply(dat_list$entries, "[[", "startTime"),
                           endTime = sapply(dat_list$entries, "[[", "endTime"),
                           sleepDuration = sapply(dat_list$entries, "[[", "sleepDuration"),
                           awokenCount = sapply(dat_list$entries, "[[", "awokenCount"),
                           restlessCount = sapply(dat_list$entries, "[[", "restlessCount"),
                           awakeTime = sapply(dat_list$entries, "[[", "awakeTime"),
                           restlessTime = sapply(dat_list$entries, "[[", "restlessTime"),
                           minAsleep = sapply(dat_list$entries, "[[", "minAsleep"),
                           sleepQualityScoreB = sapply(dat_list$entries, "[[", "sleepQualityScoreB"),
                           sleepQualityScoreA = sapply(dat_list$entries, "[[", "sleepQualityScoreA")),
                     stringsAsFactors=F)
  }else{
    stop("No sleep data available")
  }
  return(list(summary=summary, df=df))
}
