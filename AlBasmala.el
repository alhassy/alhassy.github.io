(cl-defun blog/git (cmd &rest args)
  "Execute git command CMD, which may have %s placeholders whose values are positional in ARGS."
  (shell-command (apply #'format (concat "cd ~/blog; git " cmd) args)))

(cl-defun blog/publish-current-article ()
  "Place HTML files in the right place, update arhives, index, rss, tags; git push!"
  (interactive)
    (blog/git "add %s" (buffer-file-name))
  ;; Placed article html into the published blog directory
  (blog/preview)
  (save-buffer)
  (-let [article (concat (f-base (buffer-file-name)) ".html")]
    (shell-command (concat "mv " article " ~/blog/"))
    (blog/git "add %s %s" (buffer-file-name) article))

  ;; Updated index.html, tags, archive, and rss to now include this new article

  ;; Need to disable my export-preprocessing hooks, when using org-static-blog utils.
  (my/blog/style-setup/disable)
  (view-echo-area-messages)

  (message "⇒ HTMLizing article...")
  (blog/htmlize-file (buffer-file-name))

  (message "⇒ Assembling tags...")
  (org-static-blog-assemble-tags)
  (blog/git "add tag*")

  (message "⇒ Assembling archive...")
  (org-static-blog-assemble-archive)
  (blog/git "add archive.html")

  (message "⇒ Assembling RSS feed...")
  (org-static-blog-assemble-rss)
  (blog/git "add rss.xml")

  (message "⇒ Assembling landing page...")
  (org-static-blog-assemble-index)
  (blog/git "add index.html")

  (shell-command (format "git commit -m \"Publish: Article %s.org\"; git push" (f-base (buffer-file-name))))
  (message "⇒ It may take up 20secs to 1minute for changes to be live at alhassy.com; congratulations!"))

(org-defblock abstract (main) nil
  "Render a block in a slightly narrowed blueish box, titled \"Abstract\".

   Supported backends: HTML. "
   (format (concat
            "<div class=\"abstract\" style=\"border: 1px solid black;"
            "padding: 10px; margin-top: 50px; margin-bottom: 50px;"
            "margin-right: 150px; margin-left: 80px; background-color: lightblue;\">"
            "<center> <strong class=\"tooltip\""
            "title=\"What's the goal of this article?\"> Abstract </strong> </center>"
            "%s </div>")
           contents))

 (setq org-static-blog-page-header
  (concat
   org-html-head-extra  ;; Altered by ‘org-special-block-extras’
   (concat
   "<meta name=\"author\" content=\"Musa Al-hassy\">
    <meta name=\"referrer\" content=\"no-referrer\">"
   "<link href=\"usual-org-front-matter.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link href=\"org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link href=\"floating-toc.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link href=\"blog-banner.css\" rel=\"stylesheet\" type=\"text/css\" />"
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

(org-deflink blog
  "Provide the styles for “www.alhassy.com”'s “header” and “footer”.

The use of “blog:footer” aims to provide a clickable list of tags, produce an HTMLized version of the Org source,
and provides a Disqus comments sections. For details, consult the `org-static-blog-post-postamble' function."
      (pcase o-label
        ("header" (concat
                   org-static-blog-page-preamble
                   org-static-blog-page-header
                   "<link href=\"https://alhassy.github.io/org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   "<link href=\"https://alhassy.github.io/floating-toc.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   "<link href=\"https://alhassy.github.io/blog-banner.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   ;; The use of the “post-title” class is so that the org-static-blog-assemble-rss method can work as intended.
                   (or (ignore-errors (format "<br><center><h1 class=\"post-title\">%s</h1></center>" (org-static-blog-get-title (buffer-file-name)))) "")))
        ("footer" (org-static-blog-post-postamble (buffer-file-name)))
        (_ "")))

(defun blog/style-setup (_backend)
  "Insert blog header (fancy title), tags, blog image (before “* Abstract”), and footer (links to tags)."
    (goto-char (point-min))
    (insert "\n blog:header \n"
            "\n* Tags, then Image :ignore:"
            "\n#+html: "
            "<center>"
            (blog/tags-of-file (buffer-file-name))
            "</center>"
            "\n#+html: "
            (s-collapse-whitespace (my/org-static-blog-assemble-image (buffer-file-name) (f-full "~/blog/images")))
            "\n")

    ;; It seems I have essentially 3 different ways to handle things: Preview, Publish, Index. Consider brining those closer
    ;; together. If I decided to switch to the org-static-blog-publish-file approach, then I would need to
    ;; explicitly write #+begin_abstract...#+end_abstract, due to how I've defined
    ;; org-static-blog-post-preamble. (See also org-static-blog-assemble-multipost-page for how I handle the abstract there.)
    ;;
    ;; Wrap contents of “* Abstract” section in the “abstract” Org-special-block
    ;; (In case we are narrowed, we only act when we can find the Abstract.)
    (when (re-search-forward "^\* Abstract" nil t)
      (re-search-forward "^ * :END:" nil t) ;; Ignore :PROPERTIES: drawer, if any.
      (forward-line)
      (insert "\n#+begin_abstract\n")
      (call-interactively #'org-forward-heading-same-level)
    (insert "\n#+end_abstract\n"))

    (goto-char (point-max))
    ;; The Org file's title is already shown via blog:header, above, so we disable it in the preview.
    (insert (format "\n* footer :ignore: \n blog:footer \n #+options: title:nil \n")))

(cl-defun blog/preview ()
  "Enable preview-on-save, and add blog/style-setup from Org's export hook."
  (interactive)
  ;; Let's ensure we have no xwidget buffer lying around, otherwise Emacs might hang.
  (-let [kill-buffer-query-functions nil]
    (mapcar #'kill-buffer (--filter (equal 'xwidget-webkit-mode (buffer-local-value 'major-mode it)) (buffer-list))))
  ;; Inserting doc:org-link/blog /seamlessly/ via the export process
  (add-hook 'org-export-before-processing-hook  #'blog/style-setup)
  ;; Preview with every save
  (org-preview-html-mode))

(cl-defun blog/preview/disable ()
  "Disable preview-on-save, and remove blog/style-setup from Org's export hook."
  (interactive)
  (remove-hook 'org-export-before-processing-hook #'blog/style-setup)
  (org-preview-html-mode -1))

(advice-add 'org-html--translate :before-until 'display-toc-as-Ξ)
;; (advice-remove 'org-html--translate 'display-toc-as-Ξ)

(defun display-toc-as-Ξ (phrase info)
  (when (equal phrase "Table of Contents")
    (s-collapse-whitespace
    " <a href=\"javascript:window.scrollTo(0,0)\"
        style=\"color: black !important; border-bottom: none !important;\"
        class=\"tooltip\"
        title=\"Go to the top of the page\">
      Ξ
    </a> ")))

;; Table captions should be below the tables
(setq org-html-table-caption-above nil
      org-export-latex-table-caption-above nil)

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

(cl-defun my/org-static-blog-assemble-image (file &optional explicit-image-path-prefix)
  "Assemble the value of ‘#+fileimage: image width height border?’ as an HTML form.

By default, the image should be located in the top-level `images/' directory.
If the image is located elsewhere, or is a URL, is dictated by the presence of a `/'
in the image path.

Here are 4 example uses:

#+fileimage: emacs-birthday-present.png
#+fileimage: ../images/emacs-birthday-present.png
#+fileimage: https://upload.wikimedia.org/wikipedia/en/6/64/Dora_and_Boots.jpg 350 300
#+fileimage: https://unsplash.com/photos/Vc2dD4l57og

+ Notice that the second indicates explicit width and height.
+ (To make the first approach work with local previews,
   we need the variable EXPLICIT-IMAGE-PATH-PREFIX which is used for local previews in `my/blog/style-setup'.)
+ The unsplash approach is specific: It shows the *main* image in the provided URL, and links to the provided URL.
"
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
     (cond
      ((s-contains? "/" image) t) ;; It's a URL, or explicit path, do nothing to it.
      (explicit-image-path-prefix (setq image (format "%s/%s"  explicit-image-path-prefix image)))
      ((not (s-contains? "/" image)) (setq image (format "images/%s" image)))
       )

     (-let [unsplash (cl-second (s-match ".*unsplash.com/photos/\\(.*\\)" image))]
       (setq href (if unsplash (concat "https://unsplash.com/photos/" unsplash) image))
       (setq title (format "Image credit “%s”" (if unsplash (concat "https://unsplash.com/photos/" unsplash) image)))
       (setq src (if unsplash (format "https://source.unsplash.com/%s/%sx%s" unsplash width height) image))
       (format "<center><a href=\"%s\" class=\"tooltip\" title=\"%s\"><img src=\"%s\" alt=\"Article image\"
             %s width=\"%s\" height=\"%s\" align=\"top\"/></a></center>"
             href title src no-border? width height)))))

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
   ;; The date
   "<div style=\"text-align: center;\">"
   (format-time-string (org-static-blog-gettext 'date-format)
                       (org-static-blog-get-date post-filename))
   "</div>"
   ;; The article's image
   (my/org-static-blog-assemble-image post-filename)
   "<br><center><strong>Abstract</strong></center>"))

(use-package org-static-blog)
(use-package lf) ;; So we can use `lf-string' for multi-line strings supporting interpolation:
;; (lf-string "100/2 is ${ (/ 100 2) }; neato!") ;; ⇒ "100/2 is 50; neato!"

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

(defun org-static-blog-post-postamble (post-file-name)
  "Returns the HTML rendering the htmlised source, version history, and comment box at the end of a post.

This function is called for every post and the returned string is appended to the post body."
  (concat
   "<hr>"
   "<center>"
   (blog/htmlize-file post-file-name)
   "&ensp;"
   (blog/history-of-file post-file-name)
   ;;
   "<br>"
   "<a href=\"https://www.buymeacoffee.com/alhassy\"><img src="
   "\"https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee\">"
   "</a>"
   ;;
   "<br><strong> Generated by Emacs and Org-mode (•̀ᴗ•́)و </strong>"
   ;; org-static-blog-page-postamble
   (blog/license)
   ;; (blog/comments) ;; TODO. Not working as intended; low priority.
   "</center>"
   (blog/read-remaining-js)))

(defun blog/tags-of-file (file-name)
  "Get an HTML listing of tags, as shields.io bages, associated with the given file."
          (concat
   ;; Straightforward implementation.
   ;; "<div class=\"taglist\">"
   ;; (org-static-blog-post-taglist file-name)
   ;; "</div>"

  ;; Badges implementation
   (if (not file-name)
       ""
     (concat
      (format "<a href=\"https://alhassy.github.io/tags.html\"> %s </a>"
              (org-link/octoicon "tag" nil 'html))
      (s-join " "
              (--map  (org-link/badge
                       (format "|%s|grey|%stag-%s.html"
                               (s-replace "-" "_" it)
                               org-static-blog-publish-url it)
                       nil 'html)
                      (org-static-blog-get-tags file-name)))))))

(defun blog/history-of-file (file-name)
  "Get an HTML badge that points to the Github history of a given file name, in my blog."
  (concat
     "<a class=\"tooltip\" title=\"See the various edits to this article over time\""
     "href=\"https://github.com/alhassy/alhassy.github.io/commits/master/"
   "posts/" (f-base file-name) ".org\"><img
   src=\"https://img.shields.io/badge/-History-informational?logo=github\"></a>"))

(defun blog/htmlize-file (file-name)
  "Generate an htmlized version of a given source file; return an HTML badge linking to the colourised file.

We do not take the extra time to produce a colourised file when we are previewing an article."
  (unless org-preview-html-mode
(let ((org-hide-block-startup nil))
  (with-temp-buffer
    (find-file file-name)
    ;; (insert "\n#+HTML_HEAD: <link href=\"../doom-solarized-light.css\" rel=\"stylesheet\">\n")
    (org-mode)
    (outline-show-all)
    (switch-to-buffer (htmlize-buffer))
    (write-file (concat "~/blog/" (f-base file-name) ".org.html"))
    (kill-buffer))))
(concat
"<a class=\"tooltip\" title=\"See the colourised Org source of this article; i.e., what I typed to get this nice webpage\" href=\""
   (f-base file-name) ".org.html\"><img
   src=\"https://img.shields.io/badge/-Source-informational?logo=read-the-docs\"></a>"))

(defun blog/license ()
  "Get HTML for Creative Commons Attribution-ShareAlike 3.0 Unported License."
(s-collapse-whitespace (s-replace "\n" ""
"
<center style=\"font-size: 12px\">
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
    Creative Commons Attribution-ShareAlike 3.0 Unported License
  </a>
</center>")))

(defun blog/comments ()
  "Embed Disqus Comments for my blog"
(s-collapse-whitespace (s-replace "\n" ""
"
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

(defun blog/read-remaining-js ()
  "Get the HTML required to make use of ReadRemaining.js"

  ;; [Maybe Not True] ReadReamining.js does not work well with xWidget browser within Emacs
  (if (equal org-preview-html-viewer 'xwidget)
      ""

   ;; ReadRemaining.js ∷ How much time is left to finish reading this article?
   ;;
  ;; jQuery already loaded by org-special-block-extras.
  ;; "<script
  ;; src=\
  ;; "https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js\"></script>"
 "<link rel=\"stylesheet\" href=\"readremaining.js-readremainingjs/css/rr_light.css\"
     type='text/css'/>
  <script
     src=\"readremaining.js-readremainingjs/src/readremaining.jquery.js\"></script>
  <script src='readremaining.js/src/readremaining.jquery.js'
     type='text/javascript'></script>
  <script type=\"text/javascript\"> $('body').readRemaining({showGaugeDelay : 10,
     showGaugeOnStart : true}); </script>"))

(setq index-content-header
  (concat
   "Here are some of my latest thoughts..."
   " badge:Made_with|Lisp such as doc:thread-first and doc:loop (•̀ᴗ•́)و"
   " tweet:https://alhassy.github.io/"))

(defun org-static-blog-assemble-multipost-page
    (pub-filename post-filenames &optional front-matter)
  "Assemble a page that contains multiple posts “previews” one after another.

- Each “preview” consists of a post's title, tags, image, and abstract.
- Previews are sorted in descending time.

You can view the generated ~/blog/index.html by invoking:
   (progn (my/blog/style-setup/disable) (org-static-blog-assemble-index))
"
  (setq show-reading-time nil) ;; Experimental.
  (setq post-filenames
        (sort post-filenames (lambda (x y)
                               (time-less-p (org-static-blog-get-date y)
                                            (org-static-blog-get-date x)))))

  (let ((org-header (concat "#+EXPORT_FILE_NAME: " pub-filename
                            "\n#+options: toc:nil title:nil html-postamble:nil"
                            "\n#+title: " (if (equal "index" (f-base pub-filename))
                                              org-static-blog-publish-title
                                            (f-base pub-filename))
                            "\n#+begin_export html\n "
                            org-static-blog-page-preamble
                            org-static-blog-page-header
                            (if front-matter front-matter "")
                            "\n#+end_export"
                            ;; Extra styling of abstracts.
                            ;; Works; but not needed.
                            ;; "\n#+HTML_HEAD_EXTRA: <style> div.abstract {background-color: pink !important;} </style>"
                            ))
        (index-header (if (equal "index" (f-base pub-filename))
                          (format "#+begin_export html\n%s\n#+end_export\n%s"
                                  org-static-blog-page-header index-content-header)
                        ""))
        (_ (view-echo-area-messages))
        (abstracts-of-posts (--map
                             (concat
                              (progn (message "Processing %s..." it) "") ;; Progress indicator

                              ;; ⟨0⟩ Title and link to article
                              (format "#+HTML: <h2 class=\"title\"><a href=\"%s\"> %s</a></h2>"
                                      (concat org-static-blog-publish-url (f-base it))
                                      (org-static-blog-get-title it))

                              ;; ⟨1⟩ Tags and reading time
                              (format "\n#+begin_export html\n<center>%s\n%s</center>\n#+end_export"
                                      (blog/tags-of-file it)
                                      ;; Experimenting.
                                      (if (not show-reading-time)
                                          ""
                                        (format "\n%s %s mins read"
                                                "octoicon:clock"
                                                (with-temp-buffer (insert-file-contents it)
                                                                  (org-ascii-export-as-ascii)
                                                                  (setq __x
                                                                        (count-words (point-min) (point-max)))
                                                                  (kill-buffer "*Org ASCII Export*")
                                                                  (delete-other-windows)
                                                                  (/ __x 200)))) ;; 200 words per minute reading
                                      )

                              ;; ⟨2⟩ Article image
                              (format "\n@@html:%s@@\n" (my/org-static-blog-assemble-image it))

                              ;; ⟨3⟩ Preview
                              (format "\n#+INCLUDE: \"%s::*Abstract\" :only-contents t" it)

                              ;; ⟨4⟩ “Read more” link
                              (format (concat "\n@@html:<p style=\"text-align:right\">@@"
                                              " badge:Read|more|green|%s|read-the-docs @@html:</p>@@")
                                      (concat org-static-blog-publish-url (f-base it))))
                             post-filenames))
        ;; This is the bottom-most matter in the index.html page
        (show-older-posts (concat  "\n#+begin_export html\n"
                                   "<hr> <div id=\"archive\">"
                                   "<a href=\""
                                   (org-static-blog-get-absolute-url org-static-blog-archive-file)
                                   "\">" (org-static-blog-gettext 'other-posts) "</a>"
                                   "</div>"
                                   "</div>"
                                   (blog/license)
                                   "\n#+end_export")))
    (with-temp-buffer
      (insert (s-join "\n\n" (list org-header
                                   index-header
                                   (s-join "\n\n" abstracts-of-posts)
                                   show-older-posts)))
      (org-mode)
      (org-html-export-to-html))))

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
            "\n#+include: ../MathJaxPreamble.org" ;; TODO. Is this someting I actually want here? If so, then consider tangling it from AlBasmala! (and add the whitespace-MathJax setup from above!)
            "\n#+description: "
               (setq desc (read-string "Article Purpose: "))
            "\n\n* Abstract :ignore: \n #+begin_abstract\n" desc
            "\n#+end_abstract"
            "\n\n* ???")))
