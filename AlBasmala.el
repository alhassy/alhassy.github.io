(setq org-static-blog-publish-title "Life & Computing Science")
(setq org-static-blog-publish-url "https://alhassy.github.io/")
(setq org-static-blog-publish-directory "~/blog/")
(setq org-static-blog-posts-directory "~/blog/posts/")
(setq org-static-blog-drafts-directory "~/blog/drafts/")

;; Use “#+filetags: τ₁ τ₂ … τₙ”
(setq org-static-blog-enable-tags t)

;; I'd like to have tocs and numbered headings
(setq org-export-with-toc t)
(setq org-export-with-section-numbers t)

 (setq org-static-blog-page-header
  (concat
   org-html-head-extra  ;; Alterd by ‘org-special-block-extras’
   (concat
   "<meta name=\"author\" content=\"Musa Al-hassy ??? \">
    <meta name=\"referrer\" content=\"no-referrer\">"
   "<link href=\"usual-org-front-matter.css\" rel=\"stylesheet\" type=\"text/css\" />" (ref:usualCSS)
   "<link href=\"org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />" (ref:orgNotesCSS)
   "<link href=\"floating-toc.css\" rel=\"stylesheet\" type=\"text/css\" />" (ref:tocCSS)
   "<link href=\"blog-banner.css\" rel=\"stylesheet\" type=\"text/css\" />" (ref:bannerCSS)
   "<link rel=\"icon\" href=\"images/favicon.png\">")
   "<script type=\"text/javascript\">
   /*
   @licstart  The following is the entire license notice for the
   JavaScript code in this tag.
   
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
   
   The JavaScript code in this tag is free software: you can
   redistribute it and/or modify it under the terms of the GNU
   General Public License (GNU GPL) as published by the Free Software
   Foundation, either version 3 of the License, or (at your option)
   any later version.  The code is distributed WITHOUT ANY WARRANTY;
   without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
   
   As additional permission under GNU GPL version 3 section 7, you
   may distribute non-source (e.g., minimized or compacted) forms of
   that code without the copy of the GNU GPL normally required by
   section 4, provided you include this license notice and a URL
   through which recipients can access the Corresponding Source.
   
   
   @licend  The above is the entire license notice
   for the JavaScript code in this tag.
   */
   <!--/*--><![CDATA[/*><!--*/
    function CodeHighlightOn(elem, id)
    {
      var target = document.getElementById(id);
      if(null != target) {
        elem.cacheClassElem = elem.className;
        elem.cacheClassTarget = target.className;
        target.className = \"code-highlighted\";
        elem.className   = \"code-highlighted\";
      }
    }
    function CodeHighlightOff(elem, id)
    {
      var target = document.getElementById(id);
      if(elem.cacheClassElem)
        elem.className = elem.cacheClassElem;
      if(elem.cacheClassTarget)
        target.className = elem.cacheClassTarget;
    }
   /*]]>*///-->
   </script>"
   "<script type=\"text/x-mathjax-config\">
       MathJax.Hub.Config({
           displayAlign: \"center\",
           displayIndent: \"0em\",
   
           \"HTML-CSS\": { scale: 100,
                           linebreaks: { automatic: \"false\" },
                           webFont: \"TeX\"
                          },
           SVG: {scale: 100,
                 linebreaks: { automatic: \"false\" },
                 font: \"TeX\"},
           NativeMML: {scale: 100},
           TeX: { equationNumbers: {autoNumber: \"AMS\"},
                  MultLineWidth: \"85%\",
                  TagSide: \"right\",
                  TagIndent: \".8em\"
                }
   });
   </script>
   <script type=\"text/javascript\"
           src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML\"></script>
   "
   ))

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"https://alhassy.github.io/\" class=\"logo\">Life & Computing Science</a>
  <br>
    <a href=\"https://alhassy.github.io/AlBasmala\">AlBasmala</a>
    <a href=\"https://alhassy.github.io/archive\">Archive</a>
    <a href=\"https://alhassy.github.io/tags\">Tags</a>
    <a href=\"https://alhassy.github.io/rss.xml\">RSS</a>
    <a href=\"https://alhassy.github.io/about\">About</a>
</div>")

(advice-add 'org-html--translate :before-until 'display-toc-as-Ξ)

;; (advice-remove 'org-html--translate 'display-toc-as-Ξ)

(defun display-toc-as-Ξ (phrase info)
  (when (equal phrase "Table of Contents")
    (s-collapse-whitespace
    "<a href=\"javascript:window.scrollTo(0,0)\"
        style=\"color: black !important; border-bottom: none !important;\"
        class=\"tooltip\"
        title=\"Go to the top of the page\">
      Ξ
    </a>")))

(setq org-static-blog-page-postamble
(s-collapse-whitespace (s-replace "\n" ""
"
<center>
  <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">
     <img alt=\"Creative Commons License\" style=\"border-width:0\"
          src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\"/>
  </a>

  <br/>
  <span xmlns:dct=\"https://purl.org/dc/terms/\"
        href=\"https://purl.org/dc/dcmitype/Text\"
        property=\"dct:title\" rel=\"dct:type\">
     <em>Life & Computing Science</em>
  </span>

  by
  <a xmlns:cc=\"https://creativecommons.org/ns#\"
  href=\"https://alhassy.github.io/\"
  property=\"cc:attributionName\" rel=\"cc:attributionURL\">
    Musa Al-hassy
  </a>

  is licensed under a
  <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">
    Creative Commons Attribution-ShareAlike 3.0 Unported License.
  </a>
</center>

<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
/* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
var disqus_shortname = 'life-and-computing-science';
/* * * DON'T EDIT BELOW THIS LINE * * */
(function() {
  var dsq = document.createElement('script');
  dsq.type = 'text/javascript';
  dsq.async = true;
  dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
  (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
</script>
<noscript>Please enable JavaScript to view the
    <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>")))

(cl-defun my/org-static-blog-assemble-image (file)
"Assemble the value of ‘#+fileimage: image width height border?’ as an HTML form."
(with-temp-buffer
  (insert-file-contents file)
  (goto-char 0)
  (search-forward-regexp "^\\#\\+fileimage: \\(.*\\)" nil t)
  (-let [(image width height no-border?)
         (s-split " " (substring-no-properties
                       (or (match-string 1)
                           "emacs-birthday-present.png")))]
    (setq width (or width 350))
    (setq height (or height 350))
    (setq no-border? (if no-border? "" "style=\"border: 2px solid black;\""))
    (format "<center> <img src=\"images/%s\" alt=\"Article image\"
            %s width=\"%s\" height=\"%s\" align=\"top\" /> </center>"
            image no-border? width height))))

(defun org-static-blog-post-preamble (post-filename)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   ;; The title
   "<h1 class=\"post-title\">"
   "<div class=\"title\" style=\"margin: 0 0 0 0 !important;\">"  
   "<a href=\"" (org-static-blog-get-post-url post-filename) "\">"
   (org-static-blog-get-title post-filename)
   "</a>"
   "</h1></div>"
   ;; Move to the footer? Near the ‘Tags’ of the article?
   ;; The date
   "<div style=\"text-align: center;\">"
   (format-time-string (org-static-blog-gettext 'date-format)
                       (org-static-blog-get-date post-filename))
   "</div>"
   ;; The article's image
   (my/org-static-blog-assemble-image post-filename)
   "<br><center><strong>Abstract</strong></center>"))

(defun org-static-blog-post-postamble (post-filename)
  "Returns the tag list and comment box at the end of a post.
This function is called for every post and the returned string is
appended to the post body, and includes the tag list generated by
followed by the HTML code for comments."
  (concat
   ;; Tags
   "<div class=\"taglist\">"
   (org-static-blog-post-taglist post-filename)
   "</div>"
   ;;
   "<center><strong> Generated by Emacs and Org-mode (•̀ᴗ•́)و </strong></center>"
   ;; Link to source and history
   "<center>"
   "<a href=\"https://raw.githubusercontent.com/alhassy/alhassy.github.io/master/"
   "posts/" (f-base post-filename) ".org\"><img
   src=\"https://img.shields.io/badge/-Source-informational?logo=read-the-docs\"></a>"
   "&emsp;"
   "<a href=\"https://github.com/alhassy/alhassy.github.io/commits/master/"
   "posts/" (f-base post-filename) ".org\"><img
   src=\"https://img.shields.io/badge/-History-informational?logo=github\"></a>"
   "</center>"
   ;; Comments
   (if (string= org-static-blog-post-comments "")
       ""
     (concat "\n<div id=\"comments\">"
             org-static-blog-post-comments
             "</div>"))))

(setq index-content-header
  (concat
   "Here are some of my latest thoughts..."
   " badge:Made_with|Lisp such as doc:thread-first and doc:loop (•̀ᴗ•́)و"
   " tweet:https://alhassy.github.io/"))

(setq show-reading-time nil)

(defun org-static-blog-assemble-multipost-page
    (pub-filename post-filenames &optional front-matter)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (setq post-filenames
        (sort post-filenames (lambda (x y)
                               (time-less-p (org-static-blog-get-date y)
                                            (org-static-blog-get-date x)))))
  (with-temp-buffer
    (insert
     (concat
      "#+EXPORT_FILE_NAME: " pub-filename
      "\n#+options: toc:nil title:nil html-postamble:nil"
      "\n#+title: " (if (equal "index" (f-base pub-filename))
                        org-static-blog-publish-title
                        (f-base pub-filename))
      "\n#+begin_export html\n "
        org-static-blog-page-preamble
        org-static-blog-page-header
        (if front-matter front-matter "")
      "\n#+end_export"

      "\n\n"
      (if (equal "index" (f-base pub-filename))
          (format "#+begin_export html\n%s\n#+end_export\n%s"
                  org-static-blog-page-header index-content-header)
        "")

      "\n\n" ;; abstracts of posts
      (thread-last post-filenames
        (--map
         (format
          (concat
           ;; ⟨0⟩ Title and link to article
           "#+HTML: <h2 class=\"title\"><a href=\"%s\"> %s</a></h2>"
           ;; ⟨1⟩ Tags and reading time
           "\n#+begin_center\n%s\n%s\n#+end_center"
           ;; ⟨2⟩ Article image
           "\n@@html:%s@@"
           ;; ⟨3⟩ Preview
           "\n#+INCLUDE: \"%s::*Abstract\" :only-contents t"
           ;; ⟨4⟩ “Read more” link
           "\n@@html:<p style=\"text-align:right\">@@"
           " badge:Read|more|green|%s|read-the-docs @@html:</p>@@")
          ;; ⟨0⟩ Title and link to article
          (concat org-static-blog-publish-url (f-base it))
          (org-static-blog-get-title it)
          ;; ⟨1⟩ Tags and reading time
          (concat octoicon:tag " "
                  (s-join " "
                          (--map (format "badge:|%s|grey|%stag-%s.html"
                                         (s-replace "-" "_" it)
                                         org-static-blog-publish-url it)
                                 (org-static-blog-get-tags it))))
          (if (not show-reading-time)
              ""
            (format "\n%s %s mins read"
                    octoicon:clock
                    (with-temp-buffer (insert-file-contents it)
                                      (org-ascii-export-as-ascii)
                                      (setq __x
                                            (count-words (point-min) (point-max)))
                                      (kill-buffer "*Org ASCII Export*")
                                      (delete-other-windows)
                                      (/ __x 200)))) ;; 200 words per minute reading
          ;; ⟨2⟩ Article image
          (my/org-static-blog-assemble-image it)
          ;; ⟨3⟩ Preview
          it
          ;; ⟨4⟩ “Read more” link
          (concat org-static-blog-publish-url (f-base it))))
        (s-join "\n\n"))

      ;; bottom matter
      "\n#+begin_export html:\n"
      "<hr><hr> <div id=\"archive\">"
      "<a href=\""
      (org-static-blog-get-absolute-url org-static-blog-archive-file)
      "\">" (org-static-blog-gettext 'other-posts) "</a>"
      "</div>"
      "</div>"
      "<div id=\"postamble\" class=\"status\">"
      org-static-blog-page-postamble
      "</div>"
      "\n#+end_export"))
    (org-mode)
    (org-html-export-to-html)))

;; MA: Relocate this to my init.
;; Table captions should be below the tables
(setq org-html-table-caption-above nil
      org-export-latex-table-caption-above nil)

(defvar my/blog/tags
  '(emacs faith category-theory order-theory
    lisp types packages haskell agda
    c frama-c program-proving)
  "Tags for my blog articles.")

;; Use C-SPC to select multiple items

(defun my/blog/new-article ()
  "Make a new article for my blog; prompting for the necessary ingredients.

If the filename entered already exists, we simply write to it.
The user notices this and picks a new name."
  (interactive)
  (let (file desc)

    (thread-last org-static-blog-posts-directory
      f-entries
      (mapcar #'f-filename)
      (completing-read "Filename (Above are existing): ")
      (concat org-static-blog-posts-directory)
      (setq file))

    ;; For some reason, ‘find-file’ in the thread above
    ;; wont let the completing-read display the possible completions.
    (find-file file)

    (insert "#+title: " (read-string "Title: ")
            "\n#+author: " user-full-name
            "\n#+email: "  user-mail-address
            "\n#+date: " (format-time-string "<%Y-%m-%d %H:%M>")
            "\n#+filetags: " (s-join " " (helm-comp-read "Tags: "
                                                         my/blog/tags
                                                         :marked-candidates t))
            "\n#+fileimage: " (completing-read
                               "Image: "
                               (mapcar #'f-filename (f-entries "~/blog/images/")))
            "\n#+description: "
               (setq desc (read-string "Article Purpose: "))
            "\n\n* Abstract :ignore: \n" desc
            "\n\n* ???")))

;; Override all minor modes that use this binding.
(bind-key* (kbd "C-c C-b")
  (lambda (&optional prefix)
"C-c C-b        ⇒ Publish current buffer
C-u C-c C-b     ⇒ Publish entire blog
C-u C-u C-c C-b ⇒ Publish entire blog; re-rendering all blog posts
                  (This will take time!)
"
     (interactive "P")
     (pcase (or (car prefix) 0)
       (0  (org-static-blog-publish-file (f-full (buffer-name))))
           ;; (browse-url-of-file (format "%s%s.html" org-static-blog-posts-directory
           ;;                            (f-base (buffer-name))))
       ;; Apparently I have to publish the current buffer before trying
       ;; to publish the blog; otherwise I got some errors.
       (4  (org-static-blog-publish-file (f-full (buffer-name)))
           (org-static-blog-publish))
       (16 ;; (org-static-blog-publish t) ⇒ Crashes.
           ;; Delete all .html files, except “about”
           (thread-last (f-entries "~/blog/")
             (--filter (and (equal (f-ext it) "html")
                            (not (member (f-base it) '("about")))))
             (--map (f-delete it))) 
           ;; Publish as usual
           (org-static-blog-publish-file (f-full (buffer-name)))
           (org-static-blog-publish)))))
