# How to deploy the github-page

1. Make your changes on README.md file or vignettes/articles directory in dev branch 
To add a new article to the menu, you have to modify _pkgdown.yml
   
2. Build site locally: 

- Run this command to build all site : ```pkgdown::build_site()```
- only build articles : ```pkgdown::build_articles()```

3. Push to github:  

- try directly ```usethis::use_pkgdown_github_pages()```
- or push manually : 
  
  - Change branch to gh-pages : ```git checkout gh-pages```  
  - Copy generated files that are in "docs" directory to the project root ```cp -r docs/* .```  
  - Commit and push them in gh-pages branch