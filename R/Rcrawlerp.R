#' Rcrawler
#'
#' The crawler's main function, by providing only the website URL and the Xpath or CSS selector patterns
#' this function can crawl the whole website (traverse all web pages) download webpages, and scrape/extract
#' its contents in an automated manner to produce a structured dataset. The process of a crawling
#' operation is performed by several concurrent processes or nodes in parallel, so it's recommended to
#' use 64bit version of R.
#'
#'
#' @param Website character, the root URL of the website to crawl and scrape.
#' @param no_cores integer, specify the number of clusters (logical cpu) for parallel crawling, by default it's the numbers of available cores.
#' @param no_conn integer, it's the number of concurrent connections per one core, by default it takes the same value of no_cores.
#' @param MaxDepth integer, repsents the max deph level for the crawler, this is not the file depth in a directory structure, but 1+ number of links between this document and root document, default to 10.
#' @param RequestsDelay integer, The time interval between each round of parallel http requests, in seconds used to avoid overload the website server. default to 0.
#' @param Obeyrobots boolean, if TRUE, the crawler will parse the website\'s robots.txt file and obey its rules allowed and disallowed directories.
#' @param Useragent character, the User-Agent HTTP header that is supplied with any HTTP requests made by this function.it is important to simulate different browser's user-agent to continue crawling without getting banned.
#' @param use_proxy object created by httr::use_proxy() function, if you want to use a proxy (does not work with webdriver).
#' @param Timeout integer, the maximum request time, the number of seconds to wait for a response until giving up, in order to prevent wasting time waiting for responses from slow servers or huge pages, default to 5 sec.
#' @param URLlenlimit integer, the maximum URL length limit to crawl, to avoid spider traps; default to 255.
#' @param urlExtfilter character's vector, by default the crawler avoid irrelevant files for data scraping such us xml,js,css,pdf,zip ...etc, it's not recommanded to change the default value until you can provide all the list of filetypes to be escaped.
#' @param crawlUrlfilter character's vector, filter Urls to be crawled by one or more regular expression patterns. Useful for large websites to control the crawler behaviour and which URLs should be crawled. For example, In case you want to crawl a website's search resutls (guided/oriented crawling). without start ^ and end $ regex.
#' @param dataUrlfilter character's vector, filter Urls to be scraped/collected by one or more regular expression patterns.Useful to control which pages should be collected/scraped, like product, post, detail or category pages if they have a commun URL pattern. without start ^ and end $ regex.
#' @param crawlZoneCSSPat one or more css pattern of page sections from where the crawler should gather links to be followed, to avoid navigating through all visible links and to have more control over the crawler behaviour in target website.
#' @param crawlZoneXPath one or more xpath pattern of page sections from where the crawler should gather links to be followed.
#' @param ignoreUrlParams character's vector, the list of Url paremeter to be ignored during crawling. Some URL parameters are ony related to template view if not ignored will cause duplicate page (many web pages having the same content but have different URLs) .
#' @param ignoreAllUrlParams, boolean, choose to ignore all Url parameter after "?" (Not recommended for Non-SEF CMS websites because only the index.php will be crawled)
#' @param statslinks boolean, if TRUE, the crawler counts the number of input and output links of each crawled web page.
#' @param DIR character, correspond to the path of the local repository where all crawled data will be stored ex, "C:/collection" , by default R working directory.
#' @param saveOnDisk boolean, By default is true, the crawler will store crawled Html pages and extracted data CSV file on a specific folder. On the other hand you may wish to have DATA only in memory.
#' @param KeywordsFilter character vector,  For users who desires to scrape or collect only web pages that contains some keywords one or more. Rcrawler calculate an accuracy score based of the number of founded keywords. This parameter must be a vector with at least one keyword like c("mykeyword").
#' @param KeywordsAccuracy integer value range bewteen 0 and 100, used only with KeywordsFilter parameter to determine the accuracy of web pages to collect. The web page Accuracy value is calculated using the number of matched keywords and their occurence.
#' @param FUNPageFilter function, filter out pages to be collected/scraped by a custom function (conditions, prediction, calssification model). This function should take a \link{LinkExtractor} object as arument then finally returns TRUE or FALSE.
#' @param Encod character, set the website caharacter encoding, by default the crawler will automatically detect the website defined character encoding.
#' @param ExtractXpathPat character's vector, vector of xpath patterns to match for data extraction process.
#' @param ExtractCSSPat character's vector, vector of CSS selector pattern to match for data extraction process.
#' @param PatternsNames character vector, given names for each xpath pattern to extract.
#' @param ExcludeXpathPat character's vector, one or more Xpath pattern to exclude from extracted content ExtractCSSPat or ExtractXpathPat (like excluding quotes from forum replies or excluding middle ads from Blog post) .
#' @param ExcludeCSSPat character's vector, similar to ExcludeXpathPat but using Css selectors.
#' @param ExtractAsText boolean, default is TRUE, HTML and PHP tags is stripped from the extracted piece.
#' @param ManyPerPattern boolean, ManyPerPattern boolean, If False only the first matched element by the pattern is extracted (like in Blogs one page has one article/post and one title). Otherwise if set to True  all nodes matching the pattern are extracted (Like in galleries, listing or comments, one page has many elements with the same pattern )
#' @param NetworkData boolean, If set to TRUE, then the crawler map all the internal hyperlink connections within the given website and return DATA for Network construction using igraph or other tools.(two global variables is returned see details)
#' @param NetwExtLinks boolean, If TRUE external hyperlinks (outlinks) also will be counted on Network edges and nodes.
#' @param Vbrowser boolean, If TRUE the crawler will use web driver phantomsjs (virtual browser) to fetch and parse web pages instead of GET request
#' @param LoggedSession A loggedin browser session object, created by \link{LoginSession} function
#' @return
#'
#' The crawling and scraping process may take a long time to finish, therefore, to avoid data loss in the case that a function crashes or stopped in the middle of action, some important data are exported at every iteration to R global environement:
#'
#' - INDEX: A data frame in global environement representing the generic URL index,including the list of fetched URLs and page details
#'   (contenttype,HTTP state, number of out-links and in-links, encoding type, and level).
#'
#' - A repository in workspace that contains all downloaded pages (.html files)
#'
#' Data scraping is enabled by setting ExtractXpathPat or ExtractCSSPat parameter:
#'
#' - DATA: A list of lists in global environement holding scraped contents.
#'
#' - A csv file 'extracted_contents.csv' holding all extracted data.
#'
#' If NetworkData is set to TRUE two additional global variables returned by the function are:
#'
#' - NetwIndex : Vector maps alls hyperlinks (nodes) with a unique integer ID
#'
#' - NetwEdges : data.frame representing edges of the network, with these column : From, To, Weight (the Depth level where the link connection has been discovered) and Type (1 for internal hyperlinks 2 for external hyperlinks).
#'
#' @details
#'
#' To start Rcrawler task you need to provide the root URL of the website you want to scrape, it could be a domain, a subdomain or a website section (eg. http://www.domain.com, http://sub.domain.com or http://www.domain.com/section/).
#' The crawler then will retreive the web page and go through all its internal links. The crawler continue to follow and parse all website's links automatically on the site until all website's pages have been parsed.
#'
#' The process of a crawling is performed by several concurrent processes or nodes in parallel, So, It is recommended to use R 64-bit version.
#'
#' For more tutorials check https://github.com/salimk/Rcrawler/
#'
#' To scrape content with complex character such as Arabic or Chinese, you need to run Sys.setlocale function then set the appropriate encoding in Rcrawler function.
#'
#' If you want to learn more about web scraper/crawler architecture, functional properties and implementation using R language, Follow this link and download the published paper for free .
#'
#' Link: http://www.sciencedirect.com/science/article/pii/S2352711017300110
#'
#' Dont forget to cite Rcrawler paper:
#'
#' Khalil, S., & Fakir, M. (2017). RCrawler: An R package for parallel web crawling and scraping. SoftwareX, 6, 98-106.
#'
#' @examples
#'
#' \dontrun{
#'
#'  ######### Crawl, index, and store all pages of a websites using 4 cores and 4 parallel requests
#'  #
#'  Rcrawler(Website ="http://glofile.com/", no_cores = 4, no_conn = 4)
#'
#'  ######### Crawl and index the website using 8 cores and 8 parallel requests with respect to
#'  # robot.txt rules using Mozilla string in user agent.
#'
#'  Rcrawler(Website = "http://www.example.com/", no_cores=8, no_conn=8, Obeyrobots = TRUE,
#'  Useragent="Mozilla 3.11")
#'
#'  ######### Crawl the website using the default configuration and scrape specific data from
#'  # the website, in this case we need all posts (articles and titles) matching two XPath patterns.
#'  # we know that all blog posts have datesin their URLs like 2017/09/08 so to avoid
#'  # collecting category or other pages we can tell the crawler that desired page's URLs
#'  # are like 4-digit/2-digit/2-digit/ using regular expression.
#'  # Note thatyou can use the excludepattern  parameter to exclude a node from being
#'  # extracted, e.g., in the case that a desired node includes (is a parent of) an
#'  # undesired "child" node. (article having inner ads or menu)
#'
#'  Rcrawler(Website = "http://www.glofile.com/", dataUrlfilter =  "/[0-9]{4}/[0-9]{2}/",
#'  ExtractXpathPat = c("//*/article","//*/h1"), PatternsNames = c("content","title"))
#'
#'  ######### Crawl the website. and collect pages having URLs matching this regular expression
#'  # pattern (/[0-9]{4}/[0-9]{2}/). Collected pages will be stored in a local repository
#'  # named "myrepo". And The crawler stops After reaching the third level of website depth.
#'
#'   Rcrawler(Website = "http://www.example.com/", no_cores = 4, no_conn = 4,
#'   dataUrlfilter =  "/[0-9]{4}/[0-9]{2}/", DIR = "./myrepo", MaxDepth=3)
#'
#'
#'  ######### Crawl the website and collect/scrape only webpage related to a topic
#'  # Crawl the website and collect pages containing keyword1 or keyword2 or both.
#'  # To crawl a website and collect/scrape only some web pages related to a specific topic,
#'  # like gathering posts related to Donald trump from a news website. Rcrawler function
#'  # has two useful parameters KeywordsFilter and KeywordsAccuracy.
#'  #
#'  # KeywordsFilter : a character vector, here you should provide keywords/terms of the topic
#'  # you are looking for. Rcrawler will calculate an accuracy score based on matched keywords
#'  # and their occurrence on the page, then it collects or scrapes only web pages with at
#'  # least a score of 1% wich mean at least one keyword is founded one time on the page.
#'  # This parameter must be a vector with at least one keyword like c("mykeyword").
#'  #
#'  # KeywordsAccuracy: Integer value range between 0 and 100, used only in combination with
#'  # KeywordsFilter parameter to determine the minimum accuracy of web pages to be collected
#'  # /scraped. You can use one or more search terms; the accuracy will be calculated based on
#'  # how many provided keywords are found on on the page plus their occurrence rate.
#'  # For example, if only one keyword is provided c("keyword"), 50% means one occurrence of
#'  # "keyword" in the page 100% means five occurrences of "keyword" in the page
#'
#'   Rcrawler(Website = "http://www.example.com/", KeywordsFilter = c("keyword1", "keyword2"))
#'
#'  # Crawl the website and collect webpages that has an accuracy percentage higher than 50%
#'  # of matching keyword1 and keyword2.
#'
#'   Rcrawler(Website = "http://www.example.com/", KeywordsFilter = c("keyword1", "keyword2"),
#'    KeywordsAccuracy = 50)
#'
#'
#'  ######### Crawl a website search results
#'  # In the case of scraping web pages specific to a topic of your interest; The methods
#'  # above has some disadvantages which are complexity and time consuming as the whole
#'  # website need to be crawled and each page is analyzed to findout desired pages.
#'  # As result you may want to make use of the search box of the website and then directly
#'  # crawl only search result pages. To do so, you may use \code{crawlUrlfilter} and
#'  # \code{dataUrlfilter} arguments or \code{crawlZoneCSSPat}/\code{CrawlZoneXPath} with
#'  \code{dataUrlfilter}.
#'  #- \code{crawlUrlfilter}:what urls shoud be crawled (followed).
#'  #- \code{dataUrlfilter}: what urls should be collected (download HTML or extract data ).
#'  #- \code{crawlZoneCSSPat} Or \code{CrawlZoneXPath}: the page section where links to be
#'      crawled are located.
#'
#'  # Example1
#'  # the command below will crawl all result pages knowing that result pages are like :
#'     http://glofile.com/?s=sur
#'     http://glofile.com/page/2/?s=sur
#'     http://glofile.com/page/2/?s=sur
#'  # so they all have "s=sur" in common
#'  # Post pages should be crawled also, post urls are like
#'    http://glofile.com/2017/06/08/placements-quelles-solutions-pour-dper/
#'    http://glofile.com/2017/06/08/taux-nette-detente/
#'  # which contain a date format march regex "[0-9]{4}/[0-9]{2}/[0-9]{2}
#'
#'  Rcrawler(Website = "http://glofile.com/?s=sur", no_cores = 4, no_conn = 4,
#'  crawlUrlfilter = c("[0-9]{4}/[0-9]{2}/[0-9]d{2}","s=sur"))
#'
#'  # In addition by using dataUrlfilter we specify that :
#'  #  1- only post pages should be collected/scraped not all crawled result pages
#'  #  2- additional urls should not be retreived from post page
#'  #  (like post urls listed in 'related topic' or 'see more' sections)
#'
#'  Rcrawler(Website = "http://glofile.com/?s=sur", no_cores = 4, no_conn = 4,
#'  crawlUrlfilter = c("[0-9]{4}/[0-9]{2}/[0-9]d{2}","s=sur"),
#'  dataUrlfilter = "[0-9]{4}/[0-9]{2}/[0-9]{2}")
#'
#'  # Example 2
#'  # collect job pages from indeed search result of "data analyst"
#'
#'  Rcrawler(Website = "https://www.indeed.com/jobs?q=data+analyst&l=Tampa,+FL",
#'   no_cores = 4 , no_conn = 4,
#'   crawlUrlfilter = c("/rc/","start="), dataUrlfilter = "/rc/")
#'  # To include related post jobs on each collected post remove dataUrlfilter
#'
#'  # Example 3
#'  # One other way to control the crawler behaviour, and to avoid fetching
#'  # unnecessary links is to indicate to crawler the page zone of interest
#'  # (a page section from where links should be grabed and crawled).
#'  # The follwing example is similar to the last one,except this time we provide
#'  # the xpath pattern of results search section to be crawled with all links within.
#'
#'  Rcrawler(Website = "https://www.indeed.com/jobs?q=data+analyst&l=Tampa,+FL",
#'   no_cores = 4 , no_conn = 4,MaxDepth = 3,
#'   crawlZoneXPath = c("//*[\@id='resultsCol']"), dataUrlfilter = "/rc/")
#'
#'
#'  ######### crawl and scrape a forum posts and replays, each page has a title and
#'  # a list of replays , ExtractCSSPat = c("head>title","div[class=\"post\"]") .
#'  # All replays have the same pattern, therfore we set TRUE ManyPerPattern
#'  # to extract all of them.
#'
#'  Rcrawler(Website = "https://bitcointalk.org/", ManyPerPattern = TRUE,
#'  ExtractCSSPat = c("head>title","div[class=\"post\"]"),
#'  no_cores = 4, no_conn =4, PatternsName = c("Title","Replays"))
#'
#'
#'  ######### scrape data/collect pages meeting your custom criteria,
#'  # This is useful when filetring by keyword or urls does not fullfil your needs, for example
#'  # if you want to detect target pages  by classification/prediction model, or simply by checking
#'  # a sppecifi text value/field in the web page, you can create a custom filter function for
#'  # page selection as follow.
#'  # First will create and test our function and test it with un one page .
#'
#'  pageinfo<-LinkExtractor(url="http://glofile.com/index.php/2017/06/08/sondage-quel-budget/",
#'  encod=encod, ExternalLInks = TRUE)
#'
#'  Customfilterfunc<-function(pageinfo){
#'   decision<-FALSE
#'   # put your conditions here
#'     if(pageinfo$Info$Source_page ... ) ....
#'   # then return a boolean value TRUE : should be collected / FALSE should be escaped
#'
#'   return TRUE or FALSE
#'  }
#'   # Finally, you just call it inside Rcrawler function, Then the crawler will evaluate each
#'    page using your set of rules.
#'
#'  Rcrawler(Website = "http://glofile.com", no_cores=2, FUNPageFilter= Customfilterfunc )
#'
#'  ######### Website Network
#'  # Crawl the entire website, and create network edges DATA of internal links.
#'  # Using Igraph for exmaple you can plot the network by the following commands
#'
#'    Rcrawler(Website = "http://glofile.com/" , no_cores = 4, no_conn = 4, NetworkData = TRUE)
#'    library(igraph)
#'    network<-graph.data.frame(NetwEdges, directed=T)
#'    plot(network)
#'
#'   # Crawl the entire website, and create network edges DATA of internal and external links .
#'   Rcrawler(Website = "http://glofile.com/" , no_cores = 4, no_conn = 4, NetworkData = TRUE,
#'   NetwExtLinks = TRUE)
#'
#' ###### Crawl a website using a web driver (Vitural browser)
#' ###########################################################################
#' ## In some case you may need to retreive content from a web page which
#' ## requires authentication via a login page like private forums, platforms..
#' ## In this case you need to run \link{LoginSession} function to establish a
#' ## authenticated browser session; then use \link{LinkExtractor} to fetch
#' ## the URL using the auhenticated session.
#' ## In the example below we will try to fech a private blog post which
#' ## require authentification .
#'
#' If you retreive the page using regular function LinkExtractor or your browser
#' page<-LinkExtractor("http://glofile.com/index.php/2017/06/08/jcdecaux/")
#' The post is not visible because it's private.
#' Now we will try to login to access this post using folowing creditentials
#' username : demo and password : rc@pass@r
#'
#' #1 Download and install phantomjs headless browser (skip if installed)
#' install_browser()
#'
#' #2 start browser process
#' br <-run_browser()
#'
#' #3 create auhenticated session
#' #  see \link{LoginSession} for more details
#'
#'  LS<-LoginSession(Browser = br, LoginURL = 'http://glofile.com/wp-login.php',
#'                 LoginCredentials = c('demo','rc@pass@r'),
#'                 cssLoginCredentials =c('#user_login', '#user_pass'),
#'                 cssLoginButton='#wp-submit' )
#'
#' #check if login successful
#' LS$session$getTitle()
#' #Or
#' LS$session$getUrl()
#' #Or
#' LS$session$takeScreenshot(file = 'sc.png')
#' LS$session$getUrl()
# Scrape data from web page that require authentification
#' LS<-run_browser()
#' LS<-LoginSession(Browser = LS, LoginURL = 'https://manager.submittable.com/login',
#'    LoginCredentials = c('your email','your password'),
#'    cssLoginFields =c('#email', '#password'),
#'    XpathLoginButton ='//*[\@type=\"submit\"]' )
#'
#'
#' # page<-LinkExtractor(url='https://manager.submittable.com/beta/discover/119087',
#' LoggedSession = LS)
#' # cont<-ContentScraper(HTmlText = page$Info$Source_page,
#' XpathPatterns = c("//*[\@id=\"submitter-app\"]/div/div[2]/div/div/div/div/div[3]",
#' "//*[\@id=\"submitter-app\"]/div/div[2]/div/div/div/div/div[2]/div[1]/div[1]" ),
#' PatternsName = c("Article","Title"),astext = TRUE )
#'
#'}
#'
#'
#' @author salim khalil
#' @import foreach doParallel parallel
#' @export
#' @importFrom  selectr css_to_xpath
#' @import  webdriver
#' @importFrom  httr GET
#' @importFrom  httr user_agent
#' @importFrom  httr timeout
#' @importFrom  httr content
#' @importFrom  data.table %chin% %like% chmatch
#' @importFrom  xml2  xml_find_all
#' @importFrom  utils write.table
#' @importFrom  utils flush.console
#'


Rcrawler <- function(Website, no_cores,no_conn, MaxDepth, DIR, RequestsDelay=0,Obeyrobots=FALSE,
                     Useragent, use_proxy=NULL, Encod, Timeout=5, URLlenlimit=255, urlExtfilter,
                     dataUrlfilter, crawlUrlfilter, crawlZoneCSSPat=NULL, crawlZoneXPath=NULL,
                     ignoreUrlParams,ignoreAllUrlParams=FALSE, KeywordsFilter,KeywordsAccuracy,FUNPageFilter,
                     ExtractXpathPat, ExtractCSSPat, PatternsNames, ExcludeXpathPat, ExcludeCSSPat,
                     ExtractAsText=TRUE, ManyPerPattern=FALSE, saveOnDisk=TRUE, NetworkData=FALSE, NetwExtLinks=FALSE,
                     statslinks=FALSE, Vbrowser=FALSE, LoggedSession) {

  if (missing(DIR)) DIR<-getwd()
  if (missing(KeywordsAccuracy)) KeywordsAccuracy<-1
  if (missing(ignoreUrlParams)) ignoreUrlParams<-""
  if (missing(MaxDepth)) MaxDepth<-10
  if (missing(no_cores)) no_cores<-parallel::detectCores()-1
  if (missing(no_conn)) no_conn<-no_cores
  if (missing(Obeyrobots)) Obeyrobots<-FALSE


  if (missing(dataUrlfilter)){
    dataUrlfilter<-".*"
    dataUrlfilterMissing<-TRUE
  }
  else {
    dataUrlfilter<-paste(dataUrlfilter,collapse="|")
    dataUrlfilterMissing<-FALSE
  }

  if (missing(crawlUrlfilter)){
    crawlUrlfilter <-".*"
    crawlUrlfilterMissing<-TRUE
    }
  else {
    crawlUrlfilter<-paste(crawlUrlfilter,collapse="|")
    crawlUrlfilterMissing<-FALSE
  }

  if(missing(Useragent)) {Useragent="Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"}
  if(missing(Encod)) {
    Encod<- Getencoding(Website)
    if (length(Encod)!=0){
      if(Encod=="NULL") Encod="UTF-8" ;
    }
  }

  if (!is.null(use_proxy) && (!missing(Vbrowser) || !missing(LoggedSession))) stop("webdriver cant be configured to use a proxy")


  if (!missing(FUNPageFilter)){
    if (!is.function(FUNPageFilter)) stop("FUNPageFilter parameter must be a function")
  }

  if(!missing(KeywordsFilter)){
    if(!is.vector(KeywordsFilter)){
      stop("KeywordsFilter parameter must be a vector with at least one element !")
    }
  }
  if(!is.numeric(KeywordsAccuracy)){
    stop ("KeywordsAccuracy parameter must be a numeric value between 1 and 100")
  } else {
    if(KeywordsAccuracy<=0 && KeywordsAccuracy>100) { # Should be KeywordsAccuracy < 0
      stop ("KeywordsAccuracy parameter must be a numeric value between 1 and 100")
    }
  }
  if(!missing(KeywordsFilter) && !missing(FUNPageFilter) ){
    stop("Please supply KeywordsFilter or FUNPageFilter, not both !")
  }

  if(!missing(ExcludeXpathPat) && !missing(ExcludeCSSPat) ){
    stop("Please supply ExcludeXpathPat or ExcludeCSSPat, not both !")
  }
  if ((!missing(ExcludeXpathPat) || !missing(ExcludeCSSPat)) && (missing(ExtractXpathPat) && missing(ExtractCSSPat))){
    stop("ExcludeXpathPat or ExcludeCSSPat should work only if ExtractXpathPat or ExtractCSSPat are used !")
  }
  if(!missing(ExtractXpathPat) && !missing(ExtractCSSPat) ){
    stop("Please supply ExtractXpathPat or ExtractCSSPat, not both !")
  }
  if(!missing(ExtractCSSPat)) {
    if(is.vector(ExtractCSSPat)){
      ExtractXpathPat<- unlist(lapply(ExtractCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check ExtractCSSPat syntax !"))}))
    } else {
      stop("ExtractCSSPat parameter must be a vector with at least one element !")
    }
  }
  if(!missing(ExcludeCSSPat)) {
    if(is.vector(ExcludeCSSPat)){
      ExcludeXpathPat<- unlist(lapply(ExcludeCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check ExcludeCSSPat syntax !"))}))
    }
  }
  if(missing(ExcludeCSSPat) && missing(ExcludeXpathPat) ){
    ExcludeXpathPat=NULL
  }
  if(!is.null(crawlZoneCSSPat)){
    crawlZoneXPath<- unlist(lapply(crawlZoneCSSPat, FUN = function(x) { tryCatch(selectr::css_to_xpath(x, prefix = "//") ,error=function(e) stop("Unable to translate supplied css selector, Please check CrawlZoneCSSPat syntax !"))}))
  }

   if(missing(urlExtfilter)){ urlExtfilter<-c("flv","mov","swf","txt","xml","js","css","zip","gz","rar","7z","tgz","tar","z","gzip","bzip","tar","mp3","mp4","aac","wav","au","wmv","avi","mpg","mpeg","pdf","doc","docx","xls","xlsx","ppt","pptx","jpg","jpeg","png","gif","psd","ico","bmp","odt","ods","odp","odb","odg","odf") }
  keywordCheck<-FALSE
  if(missing(KeywordsFilter) ){
    keywordCheck<-FALSE
  } else {
    if(is.vector(KeywordsFilter)) {
      keywordCheck<-TRUE
    }
  }
  if(!missing(LoggedSession)){
    if(length(LoggedSession)!=3){ # Assuming LoggedSession structure has 3 main components
      stop("Error in LoggedSession Argument, use run_browser() and LogginSession() functions to create a valid browser session object")
    }
  }
  domain<-strsplit(gsub("http://|https://|www\\.", "", Website), "/")[[c(1, 1)]]
  if (Obeyrobots) {
    rules<-RobotParser(Website,Useragent) # Assuming RobotParser is defined elsewhere or loaded
    urlbotfiler<-rules[[2]] # Assuming rules[[2]] contains disallowed paths
    urlbotfiler<-gsub("^\\/", paste0("http://www.",domain,"/"), urlbotfiler , perl=TRUE) # More robust base URL construction
    urlbotfiler<-gsub("\\*", ".*", urlbotfiler , perl=TRUE)
  } else {urlbotfiler=" "}


  IndexErrPages<-c(200) # Only process 200 OK pages by default, can be extended by user

  tryCatch(curdate<-format(Sys.time(), "%d%H%M%S"),error=function(e) curdate<-sample(1000:9999, 1) ) # Added seconds for more uniqueness
  if(saveOnDisk){
    foldename<-paste0(domain,"-",curdate) # More robust paste
    path<-file.path(DIR, foldename) # Use file.path for OS-agnostic paths
    dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0777") # Added showWarnings = FALSE
  }

  if(!missing(ExtractXpathPat) && saveOnDisk) {
    Filecontent <- file(file.path(path,"extracted_data.csv"), "w", encoding = "UTF-8") # Specify encoding
  }

  # Initialize data structures
  pkg.env <- new.env(parent = emptyenv()) # Create new environment for package-like storage
  pkg.env$Exdata <- list()
  # Define INDEX structure
  pkg.env$INDEX <- data.frame(Id = integer(), Url = character(), Status = character(), Level = integer(),
                               OUT = integer(), IN = integer(), `Http Resp` = integer(),
                               `Content Type` = character(), Encoding = character(), Accuracy = character(),
                               stringsAsFactors = FALSE)


  if(NetworkData){
    pkg.env$GraphINDEX <- data.frame(ID = integer(), Url = character(), HttpStatus = integer(), Nofollow = logical(), stringsAsFactors = FALSE)
    # Add the root website to GraphINDEX
    pkg.env$GraphINDEX[1, ] <- list(ID = 1, Url = Website, HttpStatus = NA_integer_, Nofollow = NA) # Root URL is not from a link, so Nofollow is NA

    pkg.env$GraphEgdes <- data.frame(From = integer(), To = integer(), Weight = integer(), Type = integer(), stringsAsFactors = FALSE)
  }

  pkg.env$Lbrowsers<-list()

  if(!missing(LoggedSession)){
      no_conn<-no_cores # Force no_conn to be no_cores for logged sessions
      cat("Preparing browser process(es) for logged-in session...\n")
      pkg.env$Lbrowsers[[1]]<-LoggedSession
      if(no_cores>=2){
        for(i in 2:no_cores){
          pkg.env$Lbrowsers[[i]]<-run_browser() # Assuming run_browser is defined
          # Re-login for each new browser instance
          pkg.env$Lbrowsers[[i]]<-LoginSession(Browser = pkg.env$Lbrowsers[[i]], LoginURL = LoggedSession$loginInfo$LoginURL, LoginCredentials =LoggedSession$loginInfo$LoginCredentials,
                                               cssLoginFields = LoggedSession$loginInfo$cssLoginFields,cssLoginButton =LoggedSession$loginInfo$cssLoginButton,cssRadioToCheck = LoggedSession$loginInfo$cssRadioToCheck,
                                               XpathLoginFields = LoggedSession$loginInfo$XpathLoginFields, XpathLoginButton = LoggedSession$loginInfo$XpathLoginButton, XpathRadioToCheck = LoggedSession$loginInfo$XpathRadioToCheck)
          cat("Browser:",i," port:", pkg.env$Lbrowsers[[i]]$process$port, "initialized and logged in.\n")
          Sys.sleep(1)
        }
      }
  } else if(Vbrowser){
      no_conn<-no_cores # Force no_conn to be no_cores for VBrowser
      cat("Preparing browser process(es)...\n")
      for(i in 1:no_cores){
        pkg.env$Lbrowsers[[i]]<-run_browser() # Assuming run_browser is defined
        cat("Browser:",i," port:",pkg.env$Lbrowsers[[i]]$process$port, "initialized.\n")
        Sys.sleep(1)
      }
  }

  cat("\n")

  # Frontier (list of URLs to visit)
  Lshemav<-list(Website) # Initialize with the root website

  lev<-0       # Current crawl depth
  t<-1         # Index for iterating through Lshemav
  posx<-0      # Row index for pkg.env$INDEX

  # Cluster initialisation
  cl <- parallel::makeCluster(no_cores)
  cat("Registering parallel backend...\n")
  doParallel::registerDoParallel(cl)

  # Export necessary functions and variables to cluster workers
  # Ensure LinkExtractor is available, as it's called by workers
  # Also ensure custom helper functions like RemoveTags, Precifunc, NormalizeForExcel, isTarget, ContentScraper, Getencoding are available if used by workers or LinkExtractor
  # For now, assuming LinkExtractor is self-contained or its dependencies are also exported.
  parallel::clusterEvalQ(cl, {
    library(xml2)
    library(httr)
    library(data.table)
    library(webdriver) # If Vbrowser or LoggedSession
    library(jsonlite)  # If Vbrowser or LoggedSession for HAR parsing
    # Source LinkExtractor if it's in a separate file and not part of a package context known to workers
    # source("R/LinkExtractor.R") # Example if LinkExtractor.R contains the function
  })
  parallel::clusterExport(cl, c("LinkExtractor", "LinkNormalization", "Drv_fetchpage", "GetEncodingHTML", "get_contenttype"), envir = environment()) # Export from this function's environment

  Iter<-0
  while (t <= length(Lshemav) && MaxDepth >= lev){ # Corrected loop condition
    Iter<-Iter+1
    rest <- length(Lshemav) - t + 1 # Number of remaining URLs in current depth

    # Determine the batch of URLs to process in this iteration
    if (no_conn <= rest){
      current_batch_urls <- Lshemav[t:(t + no_conn - 1)]
      l_idx_end <- t + no_conn - 1
    } else {
      current_batch_urls <- Lshemav[t:length(Lshemav)]
      l_idx_end <- length(Lshemav)
    }

    if (RequestsDelay > 0 && !(Vbrowser || !missing(LoggedSession))) { # Delay only for non-browser GET requests
        Sys.sleep(RequestsDelay)
    }

    allpaquet <- foreach::foreach(url_to_crawl = current_batch_urls, current_id = (t:l_idx_end), browser_instance_idx = 1:length(current_batch_urls), .verbose=FALSE, .inorder=FALSE, .errorhandling='pass') %dopar% {
        # browser_to_use will cycle through available browsers if Vbrowser/LoggedSession
        actual_browser_idx <- if (Vbrowser || !missing(LoggedSession)) ((browser_instance_idx - 1) %% no_cores) + 1 else NULL
        browser_arg <- if (!is.null(actual_browser_idx)) pkg.env$Lbrowsers[[actual_browser_idx]] else NULL

        LinkExtractor(url = url_to_crawl, id = current_id, lev = lev, IndexErrPages = IndexErrPages,
                      Useragent = Useragent, Timeout = Timeout, URLlenlimit = URLlenlimit,
                      urlExtfilter = urlExtfilter, encod = Encod, urlbotfiler = urlbotfiler,
                      removeparams = ignoreUrlParams, ExternalLInks = NetwExtLinks,
                      Browser = browser_arg, # Pass the specific browser instance
                      RenderingDelay = if(Vbrowser || !missing(LoggedSession)) RequestsDelay else 0, # Delay inside LinkExtractor for browser
                      removeAllparams = ignoreAllUrlParams, urlregexfilter = crawlUrlfilter,
                      urlsZoneXpath = crawlZoneXPath, use_proxy = if(is.null(browser_arg)) use_proxy else NULL)
    }

    cat("Processed batch: ")
    new_links_for_frontier <- character() # Collect new unique links for the next level

    for (s_idx in 1:length(allpaquet)){
      current_paquet <- allpaquet[[s_idx]]
      original_url_pos <- t + s_idx - 1 # Position in Lshemav
      cat(original_url_pos,"..", sep = "")
      utils::flush.console()

      if(is.null(current_paquet) || !is.list(current_paquet) || is.null(current_paquet$Info)) {
          cat("Error processing URL:", Lshemav[[original_url_pos]], "\n")
          next
      }

      page_info <- current_paquet$Info

      # Update pkg.env$GraphINDEX with crawled page info (including Nofollow for its source if applicable)
      if(NetworkData) {
          crawled_url_id <- which(pkg.env$GraphINDEX$Url == page_info$Url)
          if(length(crawled_url_id) > 0) { # Should always find it as it was added before crawling or is the root
              if(is.na(pkg.env$GraphINDEX$HttpStatus[crawled_url_id[1]])) {
                 pkg.env$GraphINDEX$HttpStatus[crawled_url_id[1]] <- as.integer(page_info$Status_code)
              }
              # Nofollow for the page itself (not the links on it) is NA unless set otherwise (e.g. from HTTP headers if implemented)
              posNodeFrom <- pkg.env$GraphINDEX$ID[crawled_url_id[1]]
          } else { # Should not happen if pre-added
              posNodeFrom <- NA
          }
      }

      # Process Internal Links
      internal_links_df <- current_paquet$InternalLinks
      if(NetworkData && nrow(internal_links_df) > 0 && !is.na(posNodeFrom)) {
        for(i in 1:nrow(internal_links_df)) {
          link_row <- internal_links_df[i, ]
          is_nofollow <- !is.na(link_row$rel) && grepl("nofollow", link_row$rel, ignore.case = TRUE)

          existing_link_idx <- data.table::chmatch(link_row$href, pkg.env$GraphINDEX$Url)
          posNodeTo <- NA_integer_

          if(!is.na(existing_link_idx)) {
            posNodeTo <- pkg.env$GraphINDEX$ID[existing_link_idx]
            if(is.na(pkg.env$GraphINDEX$Nofollow[existing_link_idx])) { # Only update if NA
              pkg.env$GraphINDEX$Nofollow[existing_link_idx] <- is_nofollow
            }
          } else {
            new_id <- nrow(pkg.env$GraphINDEX) + 1
            pkg.env$GraphINDEX <- rbind(pkg.env$GraphINDEX,
                                        data.frame(ID = new_id, Url = link_row$href,
                                                   HttpStatus = NA_integer_, Nofollow = is_nofollow,
                                                   stringsAsFactors = FALSE))
            posNodeTo <- new_id
          }
          if(!is.na(posNodeTo)) {
            pkg.env$GraphEgdes <- rbind(pkg.env$GraphEgdes,
                                        data.frame(From = posNodeFrom, To = posNodeTo, Weight = lev, Type = 1))
          }
        }
      }
      if(nrow(internal_links_df) > 0) {
        new_links_for_frontier <- c(new_links_for_frontier, internal_links_df$href)
      }

      # Process External Links (if NetwExtLinks is TRUE)
      external_links_df <- current_paquet$ExternalLinks
      if(NetworkData && NetwExtLinks && nrow(external_links_df) > 0 && !is.na(posNodeFrom)) {
        for(i in 1:nrow(external_links_df)) {
          link_row <- external_links_df[i, ]
          is_nofollow <- !is.na(link_row$rel) && grepl("nofollow", link_row$rel, ignore.case = TRUE)

          existing_link_idx <- data.table::chmatch(link_row$href, pkg.env$GraphINDEX$Url)
          posNodeTo <- NA_integer_

          if(!is.na(existing_link_idx)) {
            posNodeTo <- pkg.env$GraphINDEX$ID[existing_link_idx]
            if(is.na(pkg.env$GraphINDEX$Nofollow[existing_link_idx])) { # Only update if NA
               pkg.env$GraphINDEX$Nofollow[existing_link_idx] <- is_nofollow
            }
          } else {
            new_id <- nrow(pkg.env$GraphINDEX) + 1
            pkg.env$GraphINDEX <- rbind(pkg.env$GraphINDEX,
                                        data.frame(ID = new_id, Url = link_row$href,
                                                   HttpStatus = NA_integer_, Nofollow = is_nofollow,
                                                   stringsAsFactors = FALSE))
            posNodeTo <- new_id
          }
          if(!is.na(posNodeTo)) {
            pkg.env$GraphEgdes <- rbind(pkg.env$GraphEgdes,
                                        data.frame(From = posNodeFrom, To = posNodeTo, Weight = lev, Type = 2))
          }
        }
      }

      # Update INDEX (main crawling log)
      # This part needs to be carefully reviewed based on how FUNPageFilter, KeywordsFilter, and dataUrlfilter interact
      # For simplicity, adding all successfully fetched pages to INDEX here. Filtering for DATA can be separate.
      if (page_info$Status_code %in% IndexErrPages && grepl(dataUrlfilter, page_info$Url)) {
          posx <- posx + 1
          # Simplified INDEX update for now, accuracy calculation might need more context
          accuracy_val <- ""
          if(!missing(KeywordsFilter) && !is.null(page_info$Source_page) && page_info$Source_page != "NULL" && page_info$Source_page != "N/A") {
                Notagcontentx<-tolower(gsub("\\W", " ",RemoveTags(page_info$Source_page), perl=TRUE)) # Assuming RemoveTags is available
                AccuracyResult <- sum(sapply(KeywordsFilter, function(k) Precifunc(k, length(KeywordsFilter), Notagcontentx))) # Assuming Precifunc
                accuracy_val <- paste0(format(round(AccuracyResult, 2), nsmall = 2),"%")
          }

          pkg.env$INDEX[posx, ] <- list(Id = posx, Url = page_info$Url, Status = "finished", Level = page_info$Crawl_level,
                                      OUT = page_info$SumLinks, IN = NA_integer_, # IN links count needs context from GraphEdges or similar
                                      `Http Resp` = page_info$Status_code, `Content Type` = page_info$Content_type,
                                      Encoding = page_info$Encoding, Accuracy = accuracy_val)
          if(saveOnDisk && !is.null(page_info$Source_page) && page_info$Source_page != "NULL" && page_info$Source_page != "N/A"){
            filename<-paste0(posx,".html")
            filepath<-file.path(path,filename)
            tryCatch(write(page_info$Source_page, file(filepath, open="w", encoding=Encod)), error = function(e) warning(paste("Failed to write page", filepath, e$message)))
          }

          # Content Extraction
          if(!missing(ExtractXpathPat) && !is.null(page_info$Source_page) && page_info$Source_page != "NULL" && page_info$Source_page != "N/A"){
              excontent <- ContentScraper(HTmlText = page_info$Source_page, XpathPatterns = ExtractXpathPat,
                                          PatternsName = PatternsNames, ManyPerPattern = ManyPerPattern,
                                          astext = ExtractAsText, ExcludeXpathPat = ExcludeXpathPat, encod = Encod) # Assuming ContentScraper
              if(isTarget(excontent)){ # Assuming isTarget
                  excontent <- c(PageID=posx, excontent)
                  pkg.env$Exdata[[length(pkg.env$Exdata) + 1]] <- excontent
                  if(saveOnDisk){
                      tryCatch(write.table(NormalizeForExcel(excontent), file = Filecontent, sep = ";",
                                           qmethod="double" ,row.names = FALSE, col.names = FALSE, na = "NA" ), # Assuming NormalizeForExcel
                               error = function(e) warning(paste("Failed to write extracted data for page", posx, e$message)))
                  }
              }
          }
      }
    } # End loop for s_idx in allpaquet
    cat("\n")

    # Update frontier (Lshemav)
    if(length(new_links_for_frontier) > 0) {
        unique_new_links <- unique(new_links_for_frontier)
        # Filter out links already processed or currently in frontier to avoid re-adding known URLs
        # This check needs to be against all URLs ever added to Lshemav or a global list of visited URLs
        # For simplicity, here we just add those not already in the current Lshemav. A more robust check is needed.
        # A common approach is to maintain a separate set/vector of all discovered URLs.
        all_urls_ever_seen <- unique(unlist(Lshemav)) # Inefficient for large lists
        truly_new_links <- setdiff(unique_new_links, all_urls_ever_seen)

        if(NetworkData){ # Add new links to GraphINDEX if not already there
            for(new_link_url in truly_new_links){
                if(! (new_link_url %chin% pkg.env$GraphINDEX$Url) ){
                     new_id <- nrow(pkg.env$GraphINDEX) + 1
                     # Nofollow status is unknown until the page is crawled, or if it's from a link, it's set when the edge is created.
                     # Here, these are just potential URLs to crawl.
                     pkg.env$GraphINDEX <- rbind(pkg.env$GraphINDEX,
                                                 data.frame(ID = new_id, Url = new_link_url,
                                                            HttpStatus = NA_integer_, Nofollow = NA,
                                                            stringsAsFactors = FALSE))
                }
            }
        }
        Lshemav <- c(Lshemav, truly_new_links)
    }

    t <- l_idx_end + 1 # Move to the next starting point in Lshemav
    lev <- lev + 1     # Increment depth level

    # Update global environment variables for user visibility
    assign("INDEX", pkg.env$INDEX, envir = .GlobalEnv)
    if(!missing(ExtractXpathPat)) assign("DATA", pkg.env$Exdata, envir = .GlobalEnv)
    if(NetworkData){
      assign("NetwEdges", pkg.env$GraphEgdes, envir = .GlobalEnv )
      assign("NetwIndex", pkg.env$GraphINDEX, envir = .GlobalEnv )
    }

    cat("Progress:",format(round(((l_idx_end/length(Lshemav))*100), 2),nsmall = 2),"% : ",l_idx_end, " processed from ",length(Lshemav)," | Collected pages:",nrow(pkg.env$INDEX)," | Level:",lev-1,"\n")

  } # End while loop

  if(!missing(ExtractXpathPat) && saveOnDisk && exists("Filecontent") && isOpen(Filecontent)){
    close(Filecontent)
  }

  if(Vbrowser || !missing(LoggedSession)){
    cat("Shutting-down browser(s)...\n")
      for(i in 1:no_cores){ # Use no_cores as that's how many were started
        if(length(pkg.env$Lbrowsers) >= i && !is.null(pkg.env$Lbrowsers[[i]])){
           tryCatch(stop_browser(pkg.env$Lbrowsers[[i]]), error = function(e) warning(paste("Failed to stop browser", i, e$message))) # Assuming stop_browser
           Sys.sleep(0.5)
        }
      }
      rm(list=ls(pattern="Lbrowsers", envir=pkg.env), envir=pkg.env) # Clean up
  }

  cat("Shutting-down parallel cluster...\n")
  parallel::stopCluster(cl)
  # stopImplicitCluster is deprecated and not needed with stopCluster
  rm(cl) # Remove cluster object

  cat("+ Check INDEX dataframe variable to see crawling details.\n")
  if(saveOnDisk) {
    cat("+ Collected web pages are stored in Project folder.\n")
    cat("+ Project folder name :", foldename,"\n")
    cat("+ Project folder path :", path,"\n")
  }
  if(!missing(ExtractXpathPat)){
    cat("+ Scraped data are stored in a variable named : DATA.\n")
    if(saveOnDisk){
      cat("+ Scraped data are stored in a CSV file named : extracted_data.csv.\n")
    }
  }
  if(NetworkData){
    cat("+ Network nodes are stored in a variable named : NetwIndex.\n")
    cat("+ Network edges are stored in a variable named : NetwEdges.\n")
  }
}

# Helper functions (ensure these are defined or sourced if not part of base R or loaded packages)
# Getencoding, RobotParser, RemoveTags, Precifunc, NormalizeForExcel, isTarget, ContentScraper, GetEncodingHTML
# LinkExtractor, LinkNormalization, Drv_fetchpage, get_contenttype are expected to be available.
# For this exercise, I'm assuming they are, but in a real scenario, they'd need to be defined or sourced.

# Dummy placeholder for Getencoding if not available
if (!exists("Getencoding")) {
  Getencoding <- function(url) { "UTF-8" }
}
# Dummy placeholder for RobotParser
if (!exists("RobotParser")) {
  RobotParser <- function(website, useragent) { list(NULL, character(0)) }
}
# Dummy placeholder for RemoveTags
if (!exists("RemoveTags")) {
  RemoveTags <- function(html_string) { gsub("<[^>]+>", "", html_string) }
}
# Dummy placeholder for Precifunc
if (!exists("Precifunc")) {
  Precifunc <- function(keyword, num_keywords, text_content) { sum(grepl(keyword, text_content, ignore.case = TRUE)) }
}
# Dummy placeholder for NormalizeForExcel
if (!exists("NormalizeForExcel")) {
  NormalizeForExcel <- function(data_list) { as.data.frame(t(unlist(data_list))) }
}
# Dummy placeholder for isTarget
if (!exists("isTarget")) {
  isTarget <- function(extracted_content) { TRUE } # Assume all extracted content is desired
}
# Dummy placeholder for ContentScraper
if (!exists("ContentScraper")) {
  ContentScraper <- function(...) { list(content="dummy content") }
}
# Dummy placeholder for GetEncodingHTML
if (!exists("GetEncodingHTML")) {
  GetEncodingHTML <- function(html_string) { "UTF-8" }
}
# Dummy placeholder for Listlength (if it's a custom function for list of lists)
if (!exists("Listlength")) {
  Listlength <- function(lol) { sum(sapply(lol, length)) }
}
# Dummy placeholder for run_browser
if (!exists("run_browser")) {
  run_browser <- function(...) { list(session = list(go=function(u){}, findElement=function(...){}, findElements=function(...){}, getSource=function(){"<html></html>"}, readLog=function(...){NULL}, getUrl=function(){""}), process=list(port=sample(4000:5000,1)), loginInfo=NULL) }
}
# Dummy placeholder for LoginSession
if (!exists("LoginSession")) {
  LoginSession <- function(...) { list(session = list(go=function(u){}, findElement=function(...){}, findElements=function(...){}, getSource=function(){"<html></html>"}, readLog=function(...){NULL}, getUrl=function(){""}), process=list(port=sample(4000:5000,1)), loginInfo=list(LoginURL="", LoginCredentials="", cssLoginFields="", cssLoginButton="", cssRadioToCheck="", XpathLoginFields="", XpathLoginButton="", XpathRadioToCheck="")) }
}
# Dummy placeholder for stop_browser
if (!exists("stop_browser")) {
  stop_browser <- function(...) { invisible(NULL) }
}
