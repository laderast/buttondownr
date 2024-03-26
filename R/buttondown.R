#' Get all subscribers as a data.frame
#'
#' @return data.frame of all subscribers
#' @export
#'
#' @examples
#'
#' if(interactive()){
#'   #make sure your api key is set using set_api_key()
#'   df <- get_subscribers()
#'   head(df)
#' }
get_subscribers <- function(){

  endpoint_url <- "https://api.buttondown.email/v1/subscribers"

  api_key <- get_api_key()

  api_format <- paste("Token", api_key)

  subscriber_list <- NULL

  while(!is.null(endpoint_url)) {

    request <- httr2::request(endpoint_url) |>
      httr2::req_headers(Authorization=api_format)

    response <- httr2::req_perform(request)

    out_json <- response |>
      httr2::resp_body_json()

    subscriber_list <- append(subscriber_list, out_json$results)
    endpoint_url <- out_json$`next`

  }

  subscriber_frame <- as.data.frame(purrr::reduce(subscriber_list, rbind))

  return(subscriber_frame)

}


#'  List all emails and their status
#'
#' @return data.frame of emails with the following columns:
#'
#' @export
#'
#' @examples
#'
#' if(interactive()){
#'   # make sure you've set your api key with set_api_key()
#'   df <- list_emails()
#'   head(df)
#' }
list_emails <- function(){

  endpoint_url <- "https://api.buttondown.email/v1/emails"

  api_key <- get_api_key()
  api_format <- paste("Token", api_key)

  req <- httr2::request(endpoint_url) |>
    httr2::req_headers(Authorization=api_format)

  response <- httr2::req_perform(req)

  resp_json <- response |>
    httr2::resp_body_json()

  result_list <- resp_json$results

  email_frame <- as.data.frame(purrr::reduce(result_list, rbind))

  return(email_frame)
}


#' Sends a draft email from a Rmarkdown file
#'
#' This function will send a rendered blastula HTML from a markdown file
#' as a httr2::request() to the buttondown API.
#'
#' This function differs in that it will send the entire HTML rendered by
#' blastula, including encoded plots and graphics.
#'
#' The disadvantage of using this function is that it renders larger HTML files,
#' and most email clients will cut these off. If that's the case, then use
#' send_email_with_images() to upload images to the buttondown API and send
#' a smaller markdown file instead.
#'
#' @param input_rmd - path to an rmarkdown document
#' @param draft - status of email. By default, set to `draft`
#'
#' @return response
#' @export
#'
#' @examples
#' if(interactive()){
#'
#'
#' }
send_email <- function(input_rmd, draft=FALSE){

    endpoint_url <- "https://api.buttondown.email/v1/emails"
    api_key <- get_api_key()
    api_format <- paste("Token", api_key)

    subject <- rmarkdown::yaml_front_matter(input_rmd)$title

    blast_message <- blastula::render_email(input_rmd)
    html_rendered <- blast_message$html_html

    req_list <- list(
      body=html_rendered, status="draft", subject=subject
    )

    if(draft){
      req_list$status <- "draft"
    }

    resp <- httr2::request(endpoint_url) |>
      httr2::req_body_json(req_list) |>
      httr2::req_headers(Authorization=api_format) |>
      httr2::req_perform()

    return(resp)

}


send_email_md <- function(md_text, title){
  endpoint_url <- "https://api.buttondown.email/v1/emails"
  api_key <- get_api_key()
  api_format <- paste("Token", api_key)

  req_list <- list(
    body=md_text, status="draft", subject=title
  )

  resp <- httr2::request(endpoint_url) |>
    httr2::req_body_json(req_list) |>
    httr2::req_headers(Authorization=api_format) |>
    httr2::req_perform()

  return(resp)

}

#' Render email with images
#'
#' Given a path to a markdown file, renders plots to graphics and text to
#' github flavored markdown and uploads images to Buttondown API, substituting
#' the image paths with buttondown URLs.
#'
#' This is helpful because you hit size limits with email clients.
#'
#' @param markdown_path
#'
#' @return htte2 response if successful.
#' @export
#'
#' @examples
send_email_with_images <- function(markdown_path){

  file_name <- tools::file_path_sans_ext(markdown_path)

  image_dir_relative <- file_name |>
    fs::path_file() |> paste0("_files") |> paste0("/figure-gfm/")

  file_md <- fs::path_ext_remove(markdown_path) |>
    paste0(".md")

  image_dir <- file_name |> paste0("_files") |> paste0("/figure-gfm/")

  rmarkdown::render(markdown_path, params = NULL)

  if(!fs::file_exists(file_md)){
    cli::cli_abort("Markdown file not created")
  }

  paths <- fs::dir_ls(image_dir)

  files_from_image_dir <- paths |> fs::path_file()

  rel_paths <- paste0(image_dir_relative, files_from_image_dir)

  url_list <- NULL

  for(p in paths){
    resp <- send_image(p)

    url <- resp |>
      httr2::resp_body_json() |>
      purrr::pluck("image")
    url_list <- c(url_list, url)
  }

  names(url_list) <- rel_paths

  md_char <- readr::read_file(file_md)

  #for(i in length(url_list)){
  #  md_char <- stringr::str_replace(md_char, rel_paths[i], url_list[i])
  #}

  md_char <- stringr::str_replace_all(md_char, url_list)

  title <- rmarkdown::yaml_front_matter(markdown_path)$title
  readr::write_lines(md_char, file = file_md,append = FALSE)
  resp <- send_email_md(md_char, title)

  return(resp)
}

#' Sends an image to the buttondown API
#'
#' @param filepath - path to an image file
#'
#' @return resp - httr2 response from buttondown server
#' @export
#'
#' @examples
send_image <- function(filepath){

  endpoint_url <- "https://api.buttondown.email/v1/images"
  api_key <- get_api_key()
  api_format <- paste("Token", api_key)

  #req_list <- list(image=filepath)

  resp <- httr2::request(endpoint_url) |>
    httr2::req_body_multipart(image=curl::form_file(filepath)) |>
    httr2::req_headers(Authorization=api_format) |>
    httr2::req_perform()

  url <- NULL

  status_code <- resp$status_code

  if(status_code != "201") {
    cli::cli_abort(paste("Bad response from server:", status_code))
  }

  return(resp)
}

#' Stores an API key to the keyring
#'
#' This function will set your api key in your keyring
#' You can always check whether it's set by running get_api_key()
#'.
#' When run, it will open a keyring dialog box prompting you to enter your API
#' key. This is better because your API key isn't stored in your history and is
#' secure.
#'
#' @return
#' @export
#'
#' @examples
set_api_key <- function() {
  keyring::key_set("BUTTON_API", prompt = "Enter your Buttondown API key: ")
}


#' gets a buttondown API key as plaintext from the keychain
#'
#' Note that you may have to give rstudio permission to access your keychain
#' the first time you run it.
#'
#' @return api_key
#' @export
#'
#' @examples
get_api_key <- function() {
  api_key <- keyring::key_get("BUTTON_API")

  if(is.null(api_key)){cli::cli_abort("You need a Buttondown API key. \nSet it with set_api_key()")}

  return(api_key)
}


convert_to_jpg <- function(path){
       out_path <- paste0(tools::file_path_sans_ext(path), ".jpg")
       image <- magick::image_read(path)
       magick::image_write(image,path=out_path, quality=50)
   }
