(org-deflink image
             "Provide a quick way to insert images along with credits via tooltips.

Example usage:

image:https://upload.wikimedia.org/wikipedia/commons/3/33/Heisokudachi.svg|100|100

image:URL|WIDTH|HEIGHT|CENTER?|CREDIT?
"
;;             (upcase (or o-description o-label))
  (-let [(image width height center? credit?) (s-split "|" o-label)]
    (-let [unsplash (cl-second (s-match ".*unsplash.com/photos/\\(.*\\)" image))]
      (let* ((href (if unsplash (concat "https://unsplash.com/photos/" unsplash) image))
            (title (format "Image credit “%s”" (or credit? (if unsplash (concat "https://unsplash.com/photos/" unsplash) image))))
            (src (if unsplash (format "https://source.unsplash.com/%s/%sx%s" unsplash width height) image))
            (it (format "<a href=\"%s\" class=\"tooltip\" title=\"%s\"><img src=\"%s\" alt=\"Article image\"
             width=\"%s\" height=\"%s\" align=\"top\"/></a>"
                        href title src width height)))
        (if center?
            (format "<center> %s </center>" it)
          it)))))

(defmacro org-deftag (name args docstring &rest body)
  "Re-render an Org section in any way you like, by tagging the section with NAME.

That is to say, we essentially treat tags as functions that act on Org headings:
We redefine Org sections for the same purposes as Org special blocks.

Anyhow:
ARGS are the sequence of items seperated by underscores after the NAME of the new tag.
BODY is a form that may anaphorically mention:
- O-BACKEND: The backend we are exporting to, such as `latex' or `html'.
- O-HEADING: The string denoting the title of the tagged section heading.

DOCSTRING is mandatory; everything should be documented for future maintainability.

The result of this anaphoric macro is a symbolic function name `org-deftag/NAME',
which is added to `org-export-before-parsing-hook'.

----------------------------------------------------------------------

Below is the motivating reason for inventing this macro. It is used:

     ** Interesting, but low-priority, content   :details_red:
     Blah blah blah blah blah blah blah blah blah blah blah.
     Blah blah blah blah blah blah blah blah blah blah blah.

Here is the actual implementation:

(org-deftag details (color)
   \"HTML export a heading as if it were a <details> block; COLOR is an optional
   argument indicating the background colour of the resulting block.\"
   (insert \"\n#+html:\"
           (format \"<details style=\\\"background-color: %s\\\">\" color)
           \"<summary>\" (s-replace-regexp \"^\** \" \"\" heading) \"</summary>\")
   (org-next-visible-heading 1)
   (insert \"#+html: </details>\"))

"
  (let ((func-name (intern (format "org-deftag/%s" name))))
    `(progn
       (cl-defun ,func-name (o-backend)
         ,docstring
         (outline-show-all)
         (org-map-entries
          (lambda ()
            (kill-line)
            (let ((o-heading (car kill-ring)))
              (if (not (s-contains? (format ":%s" (quote ,name)) o-heading 'ignoring-case))
                  (insert o-heading)
                (-let [,args (cdr (s-split "_" (car (s-match (format "%s[^:]*" (quote ,name)) o-heading))))]
                  (setq o-heading (s-replace-regexp (format ":%s[^:]*:" (quote ,name)) "" o-heading))
                  ,@body)
                ;; Otherwise we impede on the auto-inserted “* footer :ignore:”
                (insert "\n"))))))
       (add-hook 'org-export-before-parsing-hook (quote ,func-name))
       (quote ,func-name))))

(org-deftag details (anchor color)
   "HTML export a heading as if it were a <details> block; ANCHOR & COLOR are optional
   arguments indicating the anchor for this block as well as the background colour of the resulting block.

For example, in my blog, I would use :details_rememberthis_#F47174: to mark a section as
friendly-soft-red to denote it as an “advanced” content that could be ignored
on a first reading of my article.
Incidentally, `orange' and `#f2b195' are also nice ‘warning’ colours."
   (insert "\n#+html:"
           (format "<div>%s <details class=\"float-child\" style=\"background-color: %s\">"
                   (if anchor (format "<a style=\"width: 1%%;float: left; padding: 0px\" id=\"%s\" href=\"#%s\">🔗</a>" anchor anchor) "")
                   color)
           "<summary> <strong> <font face=\"Courier\" size=\"3\" color=\"green\">"
           (s-replace-regexp "^\** " "" o-heading)
           "</font> </strong> </summary>")
   (org-next-visible-heading 1)
   (insert "#+html: </details> </div>"))

(use-package org-static-blog)
(use-package lf) ;; So we can use `lf-string' for multi-line strings supporting interpolation:
;; (lf-string "100/2 is ${ (/ 100 2) }; neato!") ;; ⇒ "100/2 is 50; neato!"

(defvar blog/title "Life & Computing Science"
  "Title of the blog.")

(defvar blog/url "https://alhassy.com"
  "URL of the blog.")

(defvar blog/publish-directory "~/blog/"
  "Directory containing published HTML files.")

(defvar blog/posts-directory "~/blog/posts"
  "Directory containing source Org files.

When publishing, posts are rendered as HTML and included in the index and RSS feed.
See `blog/create-index' and `blog/publish-directory'.")

(defun blog/new-article ()
"Make a new article for my blog; prompting for the necessary ingredients.

If the filename entered already exists, we simply write to it.
The user notices this and picks a new name.

This sets up a new article based on existing tags and posts.
+ Use C-SPC to select multiple tag items

Moreover it also enables `org-preview-html-mode' so that on every alteration,
followed by a save, C-x C-s, will result in a live preview of the blog article,
nearly instantaneously."
  (interactive)
  (let (file desc)

    (thread-last blog/posts-directory
      f-entries
      (mapcar #'f-filename)
      (completing-read "Filename (Above are existing): ")
      (concat blog/posts-directory)
      (setq file))

    ;; For some reason, ‘find-file’ in the thread above
    ;; wont let the completing-read display the possible completions.
    (find-file file)

    (insert "#+title: " (read-string "Title: ")
            "\n#+author: " user-full-name
            "\n#+email: "  user-mail-address
            ;; "\n#+date: " (format-time-string "<%Y-%m-%d %H:%M>")
            "\n#+filetags: " (s-join " " (helm-comp-read "Tags: "
                                                         blog/tags
                                                         :marked-candidates t))
            "\n#+fileimage: emacs-birthday-present.png"
            ;; "\n#+fileimage: " (completing-read
            ;;                    "Image: "
            ;;                    (mapcar #'f-filename (f-entries "~/blog/images/")))
            ;; "\n#+include: ../MathJaxPreamble.org" ;; TODO. Is this someting I actually want here? If so, then consider tangling it from AlBasmala! (and add the whitespace-MathJax setup from above!)
            "\n#+description: "
               (setq desc (read-string "Article Purpose: "))
            "\n\n* Abstract :ignore: \n" desc
            "\n\n* ???")
    (save-buffer)
    (blog/preview)))

(defun blog/create-posts-json-file ()
  "Create cache info about posts."
  (interactive)
  (require 'json)
  (cl-loop for file in (f-files "~/blog/posts")
           when (s-ends-with? ".org" file)
           collect (blog/info file) into posts
           finally
           ;; Sorted in descending time; i.e., the latest article should be first
           (setq posts (sort posts (lambda (newer older) (time-less-p (date-to-time (@date older)) (date-to-time (@date newer))))))
           (f-write-text (json-encode posts)  'utf-8 (f-expand "~/blog/posts.json"))
           (find-file "~/blog/posts.json")
           (json-pretty-print-buffer)
           (write-file "~/blog/posts.json")))


(defvar blog/posts (with-temp-buffer (insert-file-contents "~/blog/posts.json") (json-parse-buffer))
  "Load cached info about posts")


(defvar blog/tags (sort (seq-uniq (-flatten (seq-map (lambda (it) (s-split " " (map-elt it "tags"))) blog/posts))) #'string<)
  "Tags for my blog articles.")

;; Convenient accessor methods: Given a JSON hashmap, get the specified key values.
;; Later, we redefine these, for example `@image' will actually produces the HTML for the image.
;; Example usage: (@title (seq-elt posts 0))  ⇒  "Java CheatSheet"

;; Extract the ‘#+title:‘ from POST-FILENAME.
(defun @title       (json) (map-elt json "title"))

;; TODO: Consider using: (format-time-string "%d %b %Y" ⋯) to have the same format across all articles.
(defun @date (json)
  "Extract the “#+date:” from JSON."
  (map-elt json "date"))

(defun @file        (json) (map-elt json "file"))
(defun @description (json) (map-elt json "description"))
(defun @abstract    (json) (map-elt json "abstract"))

;; Returns absolute URL to the published POST-FILENAME.
;;
;; This function concatenates publish URL and generated custom filepath to the
;; published HTML version of the post.
;;
(defun @url         (json) (map-elt json "url"))

(defun @history (json)
  "Get an HTML badge that points to the Github history of a given file name, in my blog."
  (concat
   "<a class=\"tooltip\" title=\"See the various edits to this article over time\" href=\""
   (map-elt json "history")
   "\"><img src=\"https://img.shields.io/badge/-History-informational?logo=github\"></a>"))

(defun @tags (json)
  "Get an HTML listing of tags, as shields.io bages, associated with the given file.

Example use:  (@tags (seq-elt blog/posts 0))
"
  (concat
   ;; Straightforward implementation.
   ;; "<div class=\"taglist\">"
   ;; (org-static-blog-post-taglist file-name)
   ;; "</div>"

  ;; Badges implementation
   (concat
    (format "<a href=\"https://alhassy.github.io/tags.html\"> %s </a>"
            (org-link/octoicon "tag" nil 'html))
    (s-join " "
            (--map  (org-link/badge
                     (format "|%s|grey|%stag-%s.html"
                             (s-replace "-" "_" it)
                             "https://alhassy.com" it)
                     nil 'html)
                    (s-split " " (map-elt json "tags")))))))

(cl-defun @image (json &optional explicit-image-path-prefix)
  "Assemble the value of ‘#+fileimage: image width height border?’ as an HTML form.

By default, the image should be located in the top-level `images/' directory.
If the image is located elsewhere, or is a URL, is dictated by the presence of a `/'
in the image path.

Example use:  (@image (seq-elt blog/posts 0))

Here are 4 example uses:

#+fileimage: emacs-birthday-present.png
#+fileimage: ../images/emacs-birthday-present.png
#+fileimage: https://upload.wikimedia.org/wikipedia/en/6/64/Dora_and_Boots.jpg 350 300
#+fileimage: https://unsplash.com/photos/Vc2dD4l57og

+ Notice that the second indicates explicit width and height.
+ (To make the first approach work with local previews,
   we need the variable EXPLICIT-IMAGE-PATH-PREFIX which is used for local previews in `my/blog/style-setup'. This requires a slash at the end.)
+ The unsplash approach is specific: It shows the *main* image in the provided URL, and links to the provided URL.
"
  (-let [(image width height no-border?) (s-split " " (map-elt json "image"))]
    (setq width (or width 350))
    (setq height (or height 350))
    (setq no-border? (if no-border? "" "style=\"border: 2px solid black;\""))
    (cond
     ((s-contains? "/" image) t) ;; It's a URL, or explicit path, do nothing to it.
     (explicit-image-path-prefix (setq image (format "%s%s"  explicit-image-path-prefix image)))
     ((not (s-contains? "/" image)) (setq image (format "images/%s" image))))
    (-let [unsplash (cl-second (s-match ".*unsplash.com/photos/\\(.*\\)" image))]
      (setq href (if unsplash (concat "https://unsplash.com/photos/" unsplash) image))
      (setq title (format "Image credit “%s”" (if unsplash (concat "https://unsplash.com/photos/" unsplash) image)))
      (setq src (if unsplash (format "https://source.unsplash.com/%s/%sx%s" unsplash width height) image))
      (s-collapse-whitespace
       (format "<center class=\"post-image\"><a href=\"%s\" class=\"tooltip\" title=\"%s\"><img src=\"%s\" alt=\"Article image\"
             %s width=\"%s\" height=\"%s\" align=\"top\"/></a></center>"
              href title src no-border? width height)))))

(defun blog/info (post-filename)
  "Extract the `#+BLOG_KEYWORD: VALUE` pairs from POST-FILENAME.

Example use: (blog/info \"~/blog/posts/HeytingAlgebra.org\")
"
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (-snoc
       (cons
       (cons "file" (f-base post-filename))
      (cl-loop for (prop.name prop.regex prop.default) on
            `("title" "^\\#\\+title:[ ]*\\(.+\\)$" ,post-filename
                     "date" "^\\#\\+date:[ ]*<\\([^]>]+\\)>$" ,(time-since 0)
                     "image" "^\\#\\+fileimage: \\(.*\\)" "emacs-birthday-present.png 350 350"
                     "description" "^\\#\\+description:[ ]*\\(.+\\)$" "I learned something neat, and wanted to share!"
                     "tags" "^\\#\\+filetags:[ ]*\\(.+\\)$" "" ;; String; Space separated sequence of tags
                     )
            by 'cdddr
            ;; See: https://stackoverflow.com/questions/19774603/convert-alist-to-from-regular-list-in-elisp
            do (goto-char (point-min))
            collect (cons prop.name
                          (if (search-forward-regexp prop.regex nil t)
                              (match-string 1)
                            prop.default))))
       (cons "url" (concat "https://alhassy.com/" (f-base post-filename)))
       (cons "history" (format "https://github.com/alhassy/alhassy.github.io/commits/master/posts/%s.org"
                              (f-base post-filename)))
       (cons "abstract" (progn
                  (goto-char (point-min))
                  (when (re-search-forward "^\* Abstract" nil t)
                    (beginning-of-line)
                    (-let [start (point)]
                      (org-narrow-to-subtree)
                      (org-fold-show-entry)
                      (re-search-forward "^ *:END:" nil t) ;; Ignore :PROPERTIES: drawer, if any.
                      (forward-line)
                      (buffer-substring-no-properties (point) (point-max))))))))))

(org-defblock abstract (main) nil
  "Render a block in a slightly narrowed blueish box, titled \"Abstract\".

   Supported backends: HTML. "
   (format (concat
            "<div class=\"abstract\" style=\"border: 1px solid black;"
            "padding: 1%%; margin-top: 1%%; margin-bottom: 1%%;"
            "margin-right: 10%%; margin-left: 10%%; background-color: lightblue;\">"
            "<center> <strong class=\"tooltip\""
            "title=\"What's the goal of this article?\"> Abstract </strong> </center>"
            "%s </div>")
           contents))

(setq index-content-header
  (concat
   "Here are some of my latest thoughts..."
   " badge:Made_with|Lisp such as doc:thread-first and doc:loop (•̀ᴗ•́)و"
   " tweet:https://alhassy.github.io/"))

(defun blog/make-index-page ()
  "Assemble the blog index page.

The index page contains blurbs of all of my articles.

Precondition: `blog/posts' refers to all posts, in reverse chronological order.

You can view the generated ~/blog/index.html by invoking:
  (org-static-blog-assemble-index)
"
  (interactive)
  (view-echo-area-messages)
  (with-temp-buffer
    (insert
     (s-join
      "\n"
      (list
       ;; TODO: Actually look at this concat result and notice that osbe is adding
       ;; way too much content to the header!
       ;; (progn (org-special-block-extras-mode -1) "")
       (setq org-html-head-extra "")
       ;; Org-mode header
       "#+EXPORT_FILE_NAME: ~/blog/index.html"
       "#+options: toc:nil title:nil html-postamble:nil"
       (concat "#+title: " blog/title)
       "#+begin_export html"
       ;; MA: Not ideal, the sizes I've set in the actual article are best.
       ;; "<style>"
       ;; ".post-image { margin: auto; width: 30em !important; height: 30em !important; }"
       ;; "</style>"
       org-static-blog-page-preamble
       org-static-blog-page-header
       "#+end_export"
       ;; TODO: Delete the following comment when things work and are done.
       ;; Extra styling of abstracts.
       ;; Works; but not needed.
       ;; "\n#+HTML_HEAD_EXTRA: <style> div.abstract {background-color: pink !important;} </style>"

       ;; Index landing page header
       ;; "\n#+begin_export html"
       ;; TODO: Rename `index-content-header' to have the `blog/' prefix
       ;; Also make the “Made with Lisp” tagline refer to my Lisp cheat sheet.
       ;; org-static-blog-page-header ;; TODO: This is used above, does it need to occur again?
       ;; "#+end_export"
       index-content-header

       ;; TODO: Add this loop body to the info of each post, for future use via AngularJS view-by-tags.
       ;; Blurbs of posts
       (s-join "\n" (--map
        (concat
         (progn (message "Processing %s..." it) "") ;; Progress indicator

         ;; ⟨0⟩ Title and link to article
         (format "#+HTML: <h2 class=\"title\"><a href=\"%s\"> %s</a></h2>" (@url it) (@title it))

         ;; ⟨1⟩ Tags and reading time
         (format "\n#+begin_export html\n<center>%s\n</center>\n#+end_export" (@tags it))

         ;; ⟨2⟩ Article image
         (format "\n@@html:%s@@\n" (@image it))

         ;; ⟨3⟩ Preview
         (@abstract it)

         ;; ⟨4⟩ “Read more” link
         (format (concat "\n@@html:<p style=\"text-align:right\">@@" " badge:Read|more|green|%s|read-the-docs @@html:</p>@@") (@url it))
         )
        (seq--into-list blog/posts)))

       ;; “Show older posts”
       ;; This is the bottom-most matter in the index.html page
       "#+begin_export html"
       "<hr> <center> <em> Thanks for reading everything! 😁 Bye! 👋 </em> </center> <br/>"
       (blog/license) ;; TODO: Add a “proudly created with Emacs’ Org-mode” tagline?
       "\n#+end_export"
       )))
    (org-mode)
    (org-html-export-to-html)))

(org-deflink blog
  "Provide the styles for “www.alhassy.com”'s “header” and “footer”.

The use of “blog:footer” aims to provide a clickable list of tags, produce an HTMLized version of the Org source,
and provides a Disqus comments sections. For details, consult the `blog/footer' function.

Finally, I want to avoid any `@@backend:...@@' from appearing in the browser frame's title.
We accomplish this with the help of some handy-dandy JavaScript: Just use “blog:sanitise-title”.
"
      (pcase o-label
        ("header" (concat
                   org-static-blog-page-preamble
                   org-static-blog-page-header
                   "<link href=\"https://alhassy.github.io/org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   "<link href=\"https://alhassy.github.io/floating-toc.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   "<link href=\"https://alhassy.github.io/blog-banner.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   ;; The use of the “post-title” class is so that the org-static-blog-assemble-rss method can work as intended.
                   (thread-last (org-static-blog-get-title (buffer-file-name))
                                (s-replace-regexp "@@html:" "")
                                (s-replace-regexp "@@" "")
                                (format "<br><center><h1 class=\"post-title\">%s</h1></center>"))))
        ("footer" (blog/footer (buffer-file-name)))
        ("sanitise-title" "<script> window.parent.document.title =  window.parent.document.title.replace(/@@.*@@/, \"\") </script>")
        (_ "")))

(defun blog/style-setup (_backend)
  "Insert blog header (fancy title), tags, blog image (before “* Abstract”), and footer (links to tags).

There are default options: TOC is at 2 levels, no classic Org HTML postamble nor drawers are shown.
Notice that if you explicitly provide options to change the toc, date, or show drawers, etc;
then your options will be honoured. (Since they will technically come /after/ the default options,
which I place below at the top of the page.)
"
  (goto-char (point-min))
  (let ((post (blog/info (buffer-file-name))))
    (insert "#+options: toc:2 html-postamble:nil d:nil"
            "\n#+date: " (format-time-string "%Y-%m-%d" (current-time))
            (if (buffer-narrowed-p) "\n#+options: broken-links:t" "")
            "\n blog:header blog:sanitise-title \n"
            "\n* Tags, then Image :ignore:"
            "\n#+html: "
            "<center>"
            (@tags post)
            "</center>"
            "\n#+html: "
            (@image post
                    ;; Need this conditional since AlBasmala lives in ~/blog whereas usually articles live in ~/blog/posts.
                    ;; TODO: Consider just making AlBasmala live in ~/blog/posts, I don't think there's any real reason for breaking consistency.
                    (if (equal (f-base (@file post)) "AlBasmala") "./images/" "../images/"))
            "\n")

    ;; Wrap contents of “* Abstract” section in the “abstract” Org-special-block
    ;; (In case we are narrowed, we only act when we can find the Abstract.)
    ;; TODO: Replace this with (@abstract (blog/info (buffer-file-name))), or: (@abstract post)
    (when (re-search-forward "^\* Abstract" nil t)
      (beginning-of-line)
      (-let [start (point)]
        (org-narrow-to-subtree)
        (org-show-entry)
        (re-search-forward "^ * :END:" nil t) ;; Ignore :PROPERTIES: drawer, if any.
        (forward-line)
        (insert "\n#+begin_abstract\n")
        (call-interactively #'org-forward-heading-same-level)
        ;; In case there is no next section, just go to end of file.
        (when (equal start (point)) (goto-char (point-max)))
        (insert "\n#+end_abstract\n")
        (widen)))

    (goto-char (point-max))
    ;; The Org file's title is already shown via blog:header, above, so we disable it in the preview.
    (insert (format "\n* footer :ignore: \n blog:footer \n #+options: title:nil \n"))))

(cl-defun blog/preview ()
  "Enable preview-on-save, and add blog/style-setup from Org's export hook."
  (interactive)
  ;; Let's ensure we have no xwidget buffer lying around, otherwise Emacs might hang.
  (-let [kill-buffer-query-functions nil]
    (mapcar #'kill-buffer (--filter (equal 'xwidget-webkit-mode (buffer-local-value 'major-mode it)) (buffer-list))))
  ;; Inserting org-link/blog /seamlessly/ via the export process
  (add-hook 'org-export-before-processing-hook  #'blog/style-setup)
  ;; Preview with every save
  (setq org-preview-html-viewer 'xwidget)
  (org-preview-html-mode))

(cl-defun blog/preview/disable ()
  "Disable preview-on-save, and remove blog/style-setup from Org's export hook."
  (interactive)
  (remove-hook 'org-export-before-processing-hook #'blog/style-setup)
  (org-preview-html-mode -1))

(defun blog/footer (post-file-name)
  "Returns the HTML rendering the htmlised source, version history, and comment box at the end of a post.

This function is called for every post and the returned string is appended to the post body, as a postamble."
  (let ((post (blog/info (buffer-file-name))))
    (concat
     "<hr>"
     "<center>"
     (blog/htmlize-file post-file-name)
     "&ensp;"
     (@history post)
     ;;
     ;; Consider only add this to posts tagged “arabic”?
     (blog/css/arabic-font-setup)
     ;;
     "<br>"
   "<a href=\"https://www.buymeacoffee.com/alhassy\"><img src="
   "\"https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee\">"
   "</a>"
   ;;
   "<br><strong> Generated by Emacs and Org-mode (•̀ᴗ•́)و </strong>"
   (blog/license)
   ;; (blog/comments) ;; TODO. Not working as intended; low priority.
   "</center>"
   ;; The next line is required to make the org-static-blog-assemble-rss method work.
   "<div hidden> <div id=\"postamble\" class=\"status\"> </div> </div>"
   (blog/read-remaining-js))))

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

 (setq org-static-blog-page-header
  (concat
   ;; NOPE: org-html-head-extra  ;; Altered by ‘org-special-block-extras’
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
   "
   <script type=\"text/x-mathjax-config\">
   MathJax.Hub.Config({
       \"HTML-CSS\": {
           styles: {
               \".MathJax nobr\": {
                   padding: \"0.5em 0.5em\"
               },
           }
       }
   });
   </script>
   "
   ))

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"https://alhassy.github.io/\" class=\"logo\">Life & Computing Science</a>
  <br/>
  <a href=\"https://alhassy.github.io/AlBasmala\">AlBasmala</a>
  <a href=\"https://alhassy.github.io/rss.xml\">RSS</a>
  <a href=\"https://alhassy.github.io/about\">About</a>
</div>")

;; Table captions should be below the tables
(setq org-html-table-caption-above nil
      org-export-latex-table-caption-above nil)

(advice-add 'org-html--translate :before-until 'blog/display-toc-as-Ξ)
;; (advice-remove 'org-html--translate 'display-toc-as-Ξ)

(defun blog/display-toc-as-Ξ (phrase info)
  (when (equal phrase "Table of Contents")
    (s-collapse-whitespace
    " <a href=\"javascript:window.scrollTo(0,0)\"
        style=\"color: black !important; border-bottom: none !important;\"
        class=\"tooltip\"
        title=\"Go to the top of the page\">
      Ξ
    </a> ")))

;; I'd like to have tocs and numbered headings
(setq org-export-with-toc t)
(setq org-export-with-section-numbers t)

(defun blog/ensure-useful-section-anchors (&rest _)
  "Org sections without an ID are given one based on its title.

All non-alphanumeric characters are cleverly replaced with ‘-’.

If multiple trees end-up with the same id property, issue a
message and undo any property insertion thus far.

E.g., ↯ We'll go on a ∀∃⇅ adventure
   ↦  We'll-go-on-a-adventure
"
  (interactive)
  (let ((ids))
    (org-map-entries
     (lambda ()
       (org-with-point-at (point)
         (let ((id (org-entry-get nil "CUSTOM_ID")))
           (unless id
             (thread-last (nth 4 (org-heading-components))
               (s-replace-regexp "[^[:alnum:]']" "-")
               (s-replace-regexp "-+" "-")
               (s-chop-prefix "-")
               (s-chop-suffix "-")
               (setq id))
             (if (not (member id ids))
                 (push id ids)
               (message-box "Oh no, a repeated id!\n\n\t%s" id)
               (undo)
               (setq quit-flag t))
             (org-entry-put nil "CUSTOM_ID" id))))))))

;; Whenever html & md export happens, ensure we have headline ids.
(advice-add 'org-html-export-to-html   :before 'blog/ensure-useful-section-anchors)
(advice-add 'org-md-export-to-markdown :before 'blog/ensure-useful-section-anchors)

;; Src: https://writepermission.com/org-blogging-clickable-headlines.html
(setq org-html-format-headline-function
      (lambda (todo todo-type priority text tags info)
        "Format a headline with a link to itself."
        (let* ((headline (get-text-property 0 :parent text))
               (id (or (org-element-property :CUSTOM_ID headline)
                       (ignore-errors (org-export-get-reference headline info))
                       (org-element-property :ID headline)))
               (link (if id
                         (format "<a href=\"#%s\">%s</a>" id text)
                       text)))
          (org-html-format-headline-default-function todo todo-type priority link tags info))))

(defun blog/css/arabic-font-setup ()
  "Setup the CSS/HTML required to make my Arabic writing look nice.

For a one-off use in an article, just place an “#+html:” in front of the result
of this function."

  "
  <link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Amiri'>
  <style>
     body {font-family: 'Amiri', sans-serif;}
     table {font-family:  'Scheherazade'; font-size: 105%; }
   </style>")
