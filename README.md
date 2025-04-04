# How to deploy the github-page

1. Make your changes on README.md file or vignettes/articles directory
   
2. Build site locally:  
Run this command ```pkgdown::build_site()```

to only build articles : ```pkgdown::build_article()```

3. Push to github:  
Copy generated files that are in "docs" directory and commit and push them in gh-pages branch  
(or maybe try directly usethis::use_pkgdown_github_pages())