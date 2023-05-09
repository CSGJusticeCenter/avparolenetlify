# AV Parole Project

### File types  
An extremely brief (and mostly accurate) overview of the types of files needed to build the site:  

<dl>
  <dt>_quarto.yml</dt>  
  <dd>Sets the structure and theme of the site, and the structure of the <i>_site</i> folder. The layout of the YAML file should mirror the folder stucture of the repo and vice-versa (i.e., repo folders align with the menus/submenus of your site). New pages or sections for the site need to be added here along with their corresponding html files.</dd>  

  <dt>_site folder</dt>    
  <dd>The actual contents of your site, created by the rendering process. This folder's contents should mirror the main repo, but file extentions will be ".html" instead of ".qmd".</dd>  

  <dt>img folder</dt>
  <dd>Any images (logos, gifs, etc.) for the site. Images for individual pages should be saved in an img subfolder in the state folder. The <i>img</i> folders will be copied into the <i>_site</i> folder during the render process - you need <b>both</b> sets for the site to function.</dd>  

  <dt>styles.css</dt>  
  <dd>The css settings for the site. This file can be empty, but it must exist in the repo for everything to render. This file will also get copied into <i>_site</i> during the render process, and you need <b>both</b> copies.</dd>    

  <dt>&#60;file name&#62;.qmd</dt>  
  <dd>The Quarto files for each page of the site. These get re-created as html files during the render process.</dd>
</dl>  
