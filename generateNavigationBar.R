# generate before_body.html

local({
  before_body <- "include/before_body.html"

  cat('
<nav class="navbar navbar-custom navbar-default" role="navigation">
  <div class="container-fluid">
    <div class="navbar-header">
        <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar">
					<span class="sr-only">Toggle navigation</span>
					<span class="icon-bar"></span>
					<span class="icon-bar"></span>
					<span class="icon-bar"></span>
				</button>

        <a class="navbar-brand" href="./index.html">François Guilhaumon</a>
    </div>
    <div id="navbar" class="navbar-collapse collapse">
      <ul class="nav navbar-nav">
        <li class="dropdown">
          <a href="" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Menu <span class="caret"></span></a>
          <ul class="dropdown-menu" role="menu">', file = before_body)

  index <- jsonlite::fromJSON("index.json")
  page_list  <- sort(sprintf('          <li><a href="%s">%s</a></li>', index$url, index$title))
  cat(paste0(page_list, collapse = "\n"),
      file = before_body, append = TRUE)

  cat('
          </ul>
        </li>
      </ul>
      <ul class="nav navbar-nav navbar-right">
          <li><a href="https://github.com/fguilhaumon"><i class="fa fa-github fa-lg"></i></a></li>
          <li><a href="https://scholar.google.com/citations?user=jJwtZT0AAAAJ"><i class="fa fa-google fa-lg"></i></a></li>
        </ul>
    </div><!--/.nav-collapse -->
  </div><!--/.container -->
</nav><!--/.navbar -->
', file = before_body, append = TRUE)
})
