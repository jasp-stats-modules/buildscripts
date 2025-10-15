#!/usr/bin/env Rscript
options(repos='https://cran.r-project.org')

workdir <- file.path('build')
if(dir.exists(workdir))
    unlink(workdir, recursive = TRUE, force = FALSE)

toolLibpath <- file.path(workdir, 'toolLibpath')
dir.create(toolLibpath, recursive=TRUE)
.libPaths(toolLibpath)

install.packages("remotes")
install.packages("optparse")
install.packages("httr2")
remotes::install_github("jasp-stats/jaspModuleTools")
library(jaspModuleTools)


#github upload stuff
############################################################################################################################################################
library("optparse")
library("fs")
library("httr2")

getOS <- function() {
  os <- Sys.info()[['sysname']]
  if(os == 'Darwin')
    os <- 'MacOS'
  if(Sys.getenv('FLATPAK_ID') != "")
    os <- 'Flatpak'
  return(os)
}

getAssetName <- function(path, add_post = c()) {
  pkgVersion <- read.dcf(fs::path(fs::path_dir(fs::path_dir(path)), 'DESCRIPTION'))[4]
  RVersion <- paste0('R-', paste(R.Version()$major, gsub('\\.', '-', R.Version()$minor), sep = '-'))
  name <- paste(fs::path_ext_remove(fs::path_file(path)), pkgVersion, getOS(), Sys.info()['machine'], RVersion, sep = '_')
  if(length(add_post)) name <- paste(name, paste(add_post, collapse ='_'), sep = '_')
  name <- paste(name, fs::path_ext(path), sep='.')
  name
}

getReleaseName <- function(path, commit, add_post = c()) {
  pkgVersion <- read.dcf(fs::path(fs::path_dir(fs::path_dir(path)), 'DESCRIPTION'))[4]
  RVersion <- paste0('R-', paste(R.Version()$major, gsub('\\.', '-', R.Version()$minor), sep = '-'))
  name <- paste(pkgVersion, substr(commit, 1, 8), RVersion, sep='_') 
  if(length(add_post)) name <- paste(name, paste(add_post, collapse ='_'), sep = '_')
  name
}

create_release <- function(owner, repo, tag_name, token, release_description="") {
  print(owner)
  print(repo)
  url <- sprintf('https://api.github.com/repos/%s/%s/releases', owner, repo)
  req <- httr2::request(url)
  req <- req |> 
    httr2::req_method('POST') |>
    httr2::req_error(is_error = function(x) {FALSE}) |>
    httr2::req_headers(Accept = 'application/vnd.github+json') |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(tag_name = tag_name, name = tag_name, body = release_description, prerelease = Sys.getenv("BETA_BUILD", FALSE)))
  
  print(req)  
  resp <- req |> httr2::req_perform()
  if(httr2::resp_status(resp) != 201) {
    resp |> httr2::resp_raw()
    stop("failed to create new release!")
  }
  resp
}

update_release <- function(url, token, release_description) {
  req <- httr2::request(url)
  req <- req |> 
    httr2::req_method('PATCH') |>
    httr2::req_error(is_error = function(x) {FALSE}) |>
    httr2::req_headers(Accept = 'application/vnd.github+json') |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(body = release_description))
  
  resp <- req |> httr2::req_perform()
}

get_release <- function(owner, repo, tag_name, token, release_description = "") {
  url <- sprintf('https://api.github.com/repos/%s/%s/releases/tags/%s', owner, repo, tag_name)
  req <- httr2::request(url)
  req <- req |> 
    httr2::req_method('GET') |>
    httr2::req_error(is_error = function(x) {FALSE}) |>
    httr2::req_headers(Accept = 'application/vnd.github+json') |>
    httr2::req_auth_bearer_token(token) 
  
  resp <- req |> httr2::req_perform()
  if(httr2::resp_status(resp) == 200) { #already exist update description
    url <- (resp |> httr2::resp_body_json())$url
    update_release(url, token, release_description)
  }
  else if(httr2::resp_status(resp) == 404)
    create_release(owner, repo, tag_name, token, release_description = release_description)
  else {
    resp |> httr2::resp_raw()
    stop("failed to query release!")
  }
}


upload_asset <- function(owner, repo, tag_name, asset_path, asset_name = "", token = "", release_description="", overwrite = TRUE) {
  if(asset_name == "") asset_name <- basename(asset_path)
  if(token == "") token = Sys.getenv("BUNDLE_PAT")
  
  release <- get_release(owner, repo, tag_name, token, release_description=release_description) |> httr2::resp_body_json()
  present_names <- sapply(release$assets, function(x) {x$name})
  index <- which(present_names == asset_name)
  if(length(index)) { #already exists
    if(!overwrite)
      stop("the asset already exists")
    else { #delete existing asset to replace it later
      url <- release$assets[[index[[1]]]]$url
      req <- httr2::request(url)
      req <- req |> 
        httr2::req_method('DELETE') |>
        httr2::req_error(is_error = function(x) {FALSE}) |>
        httr2::req_headers(Accept = 'application/vnd.github+json') |>
        httr2::req_auth_bearer_token(token) |> httr2::req_perform()
    }
  }
  
  url <- gsub('\\{\\?name,label\\}', paste0('?name=', asset_name), release$upload_url)
  req <- httr2::request(url)
  req <- req |> 
    httr2::req_method('POST') |>
    httr2::req_error(is_error = function(x) {FALSE}) |>
    httr2::req_headers(Accept = 'application/vnd.github+json') |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_file(path = asset_path, type = 'application/octet-stream') 
  
  resp <- (req |> httr2::req_perform() |> httr2::resp_status()) == 201 
}



uploadSubmoduleScript <- function(dir, overwrite = FALSE, clean = TRUE, release_description = "") {
  build <- path(dir, 'build')
  if(dir.exists(build)) {
    bundle <- fs::dir_ls(build, regexp = '*.JASPModule')[[1]]
    oldwd <- getwd(); setwd(dir)
    commit <- system('git rev-parse HEAD', intern = TRUE)
    url <- system('git remote get-url --push origin', intern = TRUE)
    repo <- gsub('\\.git', '', basename(url))
    owner <- basename(dirname(url))
    setwd(oldwd)
    if(upload_asset(owner, repo, getReleaseName(bundle, commit), bundle, asset_name = getAssetName(bundle), overwrite = overwrite, release_description=release_description))
      if(clean) unlink(build, recursive = TRUE)
  }
  else
    print('nothing to be done here!')
}

#building stuff
############################################################################################################################################################

options(jaspRemoteCellarRedownload=FALSE)

modules <- commandArgs(trailingOnly=TRUE)
currentJASPVersion <- readLines(url("https://raw.githubusercontent.com/jasp-stats/jasp-desktop/refs/heads/development/version.txt"))[[1]]
release_description <- paste0("---\n","jasp: \'>=", "0.95.1\'", "\n---\n") #temp

print(modules)
f <- function(mod) {
  tryCatch({
    bundlesDir <- file.path(mod, 'build')
    dir.create(bundlesDir, recursive=TRUE)
    jaspModuleTools::compile(mod, workdir=workdir, resultdir=bundlesDir, bundleAll=FALSE, buildforJaspVersion=currentJASPVersion)
    uploadSubmoduleScript(mod, overwrite=TRUE, clean=TRUE, release_description)
  }, error = function(e) { cat("Could not build:", conditionMessage(e), "\n") })
}
sapply(modules, f)
warnings()

