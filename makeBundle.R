#!/usr/bin/env Rscript
options(repos='https://cran.r-project.org')
if (nzchar(Sys.getenv("BETA_BUILD"))) print("BETA_BUILD")

workdir <- file.path('build')
if(dir.exists(workdir))
    unlink(workdir, recursive = TRUE, force = FALSE)

toolLibpath <- file.path(workdir, 'toolLibpath')
dir.create(toolLibpath, recursive=TRUE)
.libPaths(toolLibpath)

install.packages("remotes")
install.packages("optparse")
install.packages("httr2")
install.packages("stringr")
install.packages("readr")
remotes::install_github("jasp-stats/jaspModuleTools")
library(jaspModuleTools)


#github upload stuff
############################################################################################################################################################
library("optparse")
library("fs")
library("httr2")
library("stringr")
library("readr")


getOS <- function() {
  os <- Sys.info()[['sysname']]
  if(os == 'Darwin')
    os <- 'MacOS'
  if(Sys.getenv('FLATPAK_ID') != "")
    os <- 'Flatpak'
  return(os)
}

getPlatform <- function() {
    paste(getOS(), Sys.info()['machine'], sep = '_')
}

getAssetName <- function(path, pkgVersion, add_post = c()) {
  RVersion <- paste0('R-', paste(R.Version()$major, gsub('\\.', '-', R.Version()$minor), sep = '-'))
  name <- paste(fs::path_ext_remove(fs::path_file(path)), pkgVersion, getPlatform(), RVersion, sep = '_')
  if(length(add_post)) name <- paste(name, paste(add_post, collapse ='_'), sep = '_')
  name <- paste(name, fs::path_ext(path), sep='.')
  name
}

getReleaseName <- function(path, commit, pkgVersion, add_post = c()) {
  RVersion <- paste0('R-', paste(R.Version()$major, gsub('\\.', '-', R.Version()$minor), sep = '-'))
  name <- paste(pkgVersion, substr(commit, 1, 8), RVersion, sep='_')
  if(length(add_post)) name <- paste(name, paste(add_post, collapse ='_'), sep = '_')
  name
}

getReleaseDescription <- function(path) {
  name <- getQmlPkgName(path)
  desc <- getQmlDescription(path)
  release_description <- paste0("---\njasp: \'>=", "0.95.1\'", "\nname: \'", name, "\'\ndescription: \'", desc, "\'\n---\n")
}

getQmlPkgName <- function(path) {
  qml_text <- read_file(file.path(path, "inst", "Description.qml"))
  match <- str_match(qml_text, 'title\\s*:\\s*qsTr\\("(.*?)"\\)')
  title <- match[2]
}

getQmlDescription <- function(path) {
  qml_text <- read_file(file.path(path, "inst", "Description.qml"))
  match <- str_match(qml_text, 'description\\s*:\\s*qsTr\\("(.*?)"\\)')
  description <- match[2]
}

get_new_release_version <- function(pkg_path, owner, repo, token) {
  pkg_version_str <- read.dcf(fs::path(pkg_path, 'DESCRIPTION'))[1, "Version"]
  pkg_version <- as.package_version(pkg_version_str)

  if (!nzchar(Sys.getenv("BETA_BUILD"))) { # release
    return(unname(pkg_version_str))
  }
    
  versions_match_base <- function(base, target) { #func to compare if target version starts with base version
    v_base <- unclass(base)[[1]]
    v_target <- unclass(target)[[1]]
    if (length(v_target) < length(v_base)) return(FALSE)
    return(all(v_base == v_target[1:length(v_base)]))
  }
    
  url <- sprintf('https://api.github.com/repos/%s/%s/releases?per_page=100', owner, repo)
  req <- httr2::request(url)
  req <- req |>
    httr2::req_method('GET') |>
    httr2::req_error(is_error = function(x) {FALSE}) |>
    httr2::req_headers(Accept = 'application/vnd.github+json') |>
    httr2::req_auth_bearer_token(token)

  resp <- req |> httr2::req_perform()

  if(httr2::resp_status(resp) == 200) {
    releases_data <- resp |> httr2::resp_body_json()
    
    if (length(releases_data) == 0) {
      version <- as.package_version("0.0.0")
    } 
    else {
      version <- as.package_version("0.0.0")
      release_dates <- vapply(releases_data, function(x) {
        if (is.null(x$published_at)) "" else x$published_at
      }, character(1))
      sorted_indices <- order(release_dates, decreasing = TRUE)
      for (i in sorted_indices) {
        rel <- releases_data[[i]]
        rel_version <- as.package_version(sub("_.*", "", rel$name))
        print(rel_version)
        if(versions_match_base(pkg_version, rel_version)) {
          asset_names <- if (length(rel$assets) > 0) {
            vapply(rel$assets, function(a) a$name, character(1))
          } else { character(0) }
          has_os_asset <- any(grepl(getPlatform(), asset_names, fixed = TRUE))
          if (has_os_asset) {
            version <- rel_version
            break
          }
        }
      }
    }
  }
  else if(httr2::resp_status(resp) == 404) {
    version <- as.package_version("0.0.0")
  }
  else {
    stop(sprintf("Failed to query release! Status: %s", httr2::resp_status(resp)))
  }
  
  if(lengths(unclass(pkg_version)) < lengths(unclass(version))) { # hidden version postfix exist, new one is needed, so we need to add 1
    paste0(pkg_version_str, '.', as.character(tail(unclass(version)[[1]], 1) + 1))
  }
  else { # no postfix is present yet so the number 1 seems fine as a start
    paste0(pkg_version_str, '.', as.character(1))
  }
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
    httr2::req_body_json(list(tag_name = tag_name, name = tag_name, body = release_description, prerelease = nzchar(Sys.getenv("BETA_BUILD"))))

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



uploadSubmoduleScript <- function(dir, owner, repo, commit, newVersionNum, overwrite = FALSE, clean = TRUE, release_description = "") {
  build <- path(dir, 'build')
  if(dir.exists(build)) {
    bundle <- fs::dir_ls(build, regexp = '*.JASPModule')[[1]]
    if(upload_asset(owner, repo, getReleaseName(bundle, commit, newVersionNum, if (nzchar(Sys.getenv("BETA_BUILD"))) c("Beta") else c("Release")), bundle, asset_name = getAssetName(bundle, newVersionNum), overwrite = overwrite, release_description=release_description))
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
token = Sys.getenv("BUNDLE_PAT")
                                                
print(modules)
f <- function(mod) {
  tryCatch({
    #gather data
    oldwd <- getwd(); setwd(mod)
    commit <- system('git rev-parse HEAD', intern = TRUE)
    url <- system('git remote get-url --push origin', intern = TRUE)
    repo <- gsub('\\.git', '', basename(url))
    owner <- basename(dirname(url))
    setwd(oldwd)
    newVersionNum = get_new_release_version(mod, owner, repo, token)
      
    bundlesDir <- file.path(mod, 'build')
    dir.create(bundlesDir, recursive=TRUE)
    jaspModuleTools::compile(mod, workdir=workdir, resultdir=bundlesDir, bundleAll=TRUE, buildforJaspVersion=currentJASPVersion, includeInManifest = list(version=newVersionNum), deleteLibrary = TRUE)
    uploadSubmoduleScript(mod, owner, repo, commit, newVersionNum, overwrite=TRUE, clean=TRUE, getReleaseDescription(mod))
  }, error = function(e) { cat("Could not build:", conditionMessage(e), "\n") })
}
sapply(modules, f)
warnings()

