# How to deploy the github-page

1. Make your changes on README.md file or vignettes/articles directory  
To add a new article to the menu, you have to modify _pkgdown.yml
   
2. Build site locally:  
Run this command ```pkgdown::build_site()```

to only build articles : ```pkgdown::build_article()```

3. Push to github:  
Copy generated files that are in "docs" directory to the project root and commit and push them in gh-pages branch  
(or maybe try directly usethis::use_pkgdown_github_pages())