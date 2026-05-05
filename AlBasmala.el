(require 'dash)
(require 'f)
(require 's)
(require 'org-static-blog)
(require 'htmlize)
(require 'org-special-block-extras)

(use-package org-special-block-extras)
(org-special-block-extras-mode t)


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
            (title (format "Image credit %s" (or credit? (if unsplash (concat "https://unsplash.com/photos/" unsplash) image))))
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
- O-BACKEND: The backend we are exporting to, such as latex or html.
- O-HEADING: The string denoting the title of the tagged section heading.

DOCSTRING is mandatory; everything should be documented for future maintainability.

The result of this anaphoric macro is a symbolic function name org-deftag/NAME,
which is added to org-export-before-parsing-hook.

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
                ;; Otherwise we impede on the auto-inserted "* footer :ignore:"
                (insert "\n"))))))
       (add-hook 'org-export-before-parsing-hook (quote ,func-name))
       (quote ,func-name))))

(org-deftag details (anchor color)
   "HTML export a heading as if it were a <details> block; ANCHOR & COLOR are optional
   arguments indicating the anchor for this block as well as the background colour of the resulting block.

For example, in my blog, I would use :details_rememberthis_#F47174: to mark a section as
friendly-soft-red to denote it as an 'advanced' content that could be ignored
on a first reading of my article.
Incidentally, orange and `#f2b195' are also nice 'warning' colours."
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

(defvar blog-title "Life & Computing Science"
  "Title of the blog.")

(defvar blog-url "https://alhassy.com"
  "URL of the blog.")

(defvar blog-publish-directory "~/blog/public/"
  "Directory containing published HTML files.

HTML is a build artefact; the .org sources in ~/blog/ are first-class.
On master we keep only sources; CI exports everything to public/ and deploys
public/'s contents to the gh-pages branch that GitHub Pages serves.
That gives flat URLs (alhassy.com/foo) while the master working tree stays clean.")

(defvar blog-posts-directory "~/blog"
  "Directory containing source Org files.

All article .org files — including AlBasmala.org itself — live here alongside
resources/.  blog--compute-posts-and-pages collects every .org file with #+date:
as a post and every file with #+site_nav: as a nav page; files with neither
(e.g. MathJaxPreamble.org) are silently skipped.
See blog-make-index-page and blog-publish-directory.")

(defun blog-new-article ()
"Make a new article for my blog; prompting for the necessary ingredients.

If the filename entered already exists, we simply write to it.
The user notices this and picks a new name.

This sets up a new article based on existing tags and posts.
+ Use C-SPC to select multiple tag items

Moreover it also enables org-preview-html-mode so that on every alteration,
followed by a save, C-x C-s, will result in a live preview of the blog article,
nearly instantaneously."
  (interactive)
  (let (file desc)

    (thread-last blog-posts-directory
      f-entries
      (mapcar #'f-filename)
      (completing-read "Filename (Above are existing): ")
      (concat blog-posts-directory)
      (setq file))

    ;; For some reason, 'find-file' in the thread above
    ;; wont let the completing-read display the possible completions.
    (find-file file)

    (insert "#+title: " (read-string "Title: ")
            "\n#+author: " user-full-name
            "\n#+email: "  user-mail-address
            ;; "\n#+date: " (format-time-string "<%Y-%m-%d %H:%M>")
            "\n#+filetags: " (s-join " " (helm-comp-read "Tags: "
                                                         blog-tags
                                                         :marked-candidates t))
            "\n#+fileimage: emacs-birthday-present.png"
            ;; "\n#+fileimage: " (completing-read
            ;;                    "Image: "
            ;;                    (mapcar #'f-filename (f-entries "~/blog/resources/")))
            ;; "\n#+include: ../MathJaxPreamble.org" ;; TODO. Is this someting I actually want here? If so, then consider tangling it from AlBasmala! (and add the whitespace-MathJax setup from above!)
            "\n#+description: "
               (setq desc (read-string "Article Purpose: "))
            "\n\n* Abstract :ignore: \n" desc
            "\n\n* ???")
    (save-buffer)
    (blog-preview)))

(defun blog-new-post ()
  "Insert a new article skeleton at point in a multiple-style container file.

Prompts for title, description, and (optionally) tags.  The image is
selected automatically from blog-tag-image-alist based on the tags entered
-- no image prompt required.  It can be overridden afterwards by editing the
:IMAGE: property in the drawer.

The :draft: heading tag is added automatically so the article is treated as
a draft until you remove it before publishing."
  (interactive)
  (unless (equal "multiple" (blog--article-style (buffer-file-name)))
    (user-error "Not a multiple-style file; use blog-new-article for standalone posts"))
  (let* ((title       (read-string "Article title: "))
         (description (read-string "One-line description: "))
         (tags-input  (read-string "Tags (space-separated, optional): "))
         (tags        (s-split " " tags-input t))
         (image       (blog--image-for-tags tags))
         (tag-suffix  (if tags (concat ":" (s-join ":" tags) ":") ""))
         (today       (format-time-string "%Y-%m-%d")))
    (unless (bolp) (newline))
    (insert
     "* " title " :draft:" tag-suffix "\n"
     ":PROPERTIES:\n"
     ":DATE:        <" today ">\n"
     ":DESCRIPTION: " description "\n"
     ":IMAGE:       " image "\n"
     ":END:\n"
     "\n"
     "** Abstract :ignore:\n"
     description "\n"
     "\n"
     "** ???\n")
    ;; Enable preview for this container if not already active.
    (unless (member #'blog-preview-subtree after-save-hook)
      (blog-preview))))

;; Convenient accessor methods: Given a JSON hashmap, get the specified key values.
;; Later, we redefine these, for example `@image' will actually produces the HTML for the image.
;; Example usage: (@title (seq-elt posts 0))  ⇒  "Java CheatSheet"

;; Extract the '#+title:' from POST-FILENAME.
(defun @title       (json) (map-elt json "title"))

;; TODO: Consider using: (format-time-string "%d %b %Y" ⋯) to have the same format across all articles.
(defun @date (json)
  "Extract the #+date: field from JSON."
  (map-elt json "date"))

(defun @file        (json) (map-elt json "file"))
(defun @slug        (json) (map-elt json "slug"))
(defun @description (json) (map-elt json "description"))
(defun @abstract    (json) (map-elt json "abstract"))

;; Returns absolute URL to the published POST-FILENAME.
;;
;; This function concatenates publish URL and generated custom filepath to the
;; published HTML version of the post.
;;
(defun @url                  (json) (map-elt json "url"))

;; For container sub-articles, the synthetic #+htmlized_source_url: keyword
;; carries the URL of the per-article colourised source view.
;; Returns nil for ordinary standalone articles (blog--footer falls back to blog--htmlize-file).
(defun @htmlized_source_url  (json) (map-elt json "htmlized_source_url"))

(defun @history (json)
  "Get an HTML badge that points to the Github history of a given file name, in my blog."
  (concat
   "<a class=\"tooltip\" title=\"See the various edits to this article over time\" href=\""
   (map-elt json "history")
   "\"><img src=\"https://img.shields.io/badge/-History-informational?logo=github\"></a>"))

(defun blog--tag-slug (tag)
  "Convert an internal TAG name (underscores, Org-compatible) to a kebab-case slug.

Org forbids dashes in tag names — we use underscores internally and replace them
with dashes for display and URLs so the reader sees kebab-case everywhere:
  programming_language  →  programming-language
  tag-programming_language.html  →  tag-programming-language.html"
  (s-replace "_" "-" (downcase tag)))

(defun @tags (json)
  "Get an HTML listing of tags, as shields.io badges, associated with the given file.

Tag names are stored with underscores (Org syntax requirement) but rendered with
dashes everywhere — display label, URL slug — so readers see kebab-case.

Example use:  (@tags (seq-elt blog-posts 0))
"
  (concat
  ;; Badges implementation
   (concat
    (format "<a href=\"https://alhassy.github.io/tags.html\"> %s </a>"
            (org-link/octoicon "tag" nil 'html))
    (s-join " "
            (--map  (let ((slug (blog--tag-slug it)))
                      (org-link/badge
                       (format "|%s|grey|%stag-%s.html"
                               slug "https://alhassy.com/" slug)
                       nil 'html))
                    (s-split " " (map-elt json "tags")))))))

(cl-defun @image (json &optional explicit-image-path-prefix)
  "Assemble the value of '#+fileimage: image width height border?' as an HTML form.

By default, the image should be located in the top-level resources/ directory.
If the image is located elsewhere, or is a URL, is dictated by the presence of a `/'
in the image path.

Example use:  (@image (seq-elt blog-posts 0))

Here are 4 example uses:

#+fileimage: emacs-birthday-present.png
#+fileimage: ../resources/emacs-birthday-present.png
#+fileimage: https://upload.wikimedia.org/wikipedia/en/6/64/Dora_and_Boots.jpg 350 300
#+fileimage: https://unsplash.com/photos/Vc2dD4l57og

+ Notice that the second indicates explicit width and height.
+ (To make the first approach work with local previews,
   we need the variable EXPLICIT-IMAGE-PATH-PREFIX which is used for local previews in my/blog--style-setup. This requires a slash at the end.)
+ The unsplash approach is specific: It shows the *main* image in the provided URL, and links to the provided URL.
"
  (-let [(image width height no-border?) (s-split " " (map-elt json "image"))]
    (setq width (or width 350))
    (setq height (or height 350))
    (setq no-border? (if no-border? "" "style=\"border: 2px solid black;\""))
    (cond
     ((s-contains? "/" image) t) ;; It's a URL, or explicit path, do nothing to it.
     (explicit-image-path-prefix (setq image (format "%s%s"  explicit-image-path-prefix image)))
     ((not (s-contains? "/" image)) (setq image (format "resources/%s" image))))
    (-let [unsplash (cl-second (s-match ".*unsplash.com/photos/\\(.*\\)" image))]
      (setq href (if unsplash (concat "https://unsplash.com/photos/" unsplash) image))
      (setq title (format "Image credit %s" (if unsplash (concat "https://unsplash.com/photos/" unsplash) image)))
      (setq src (if unsplash (format "https://source.unsplash.com/%s/%sx%s" unsplash width height) image))
      (s-collapse-whitespace
       (format "<center class=\"post-image\"><a href=\"%s\" class=\"tooltip\" title=\"%s\"><img src=\"%s\" alt=\"Article image\"
             %s width=\"%s\" height=\"%s\" align=\"top\"/></a></center>"
              href title src no-border? width height)))))

(defun blog--info (post-filename)
  "Extract the `#+BLOG_KEYWORD: VALUE` pairs from POST-FILENAME.

Example use: (blog--info \"~/blog/HeytingAlgebra.org\")

For container sub-articles, the temp file may carry synthetic keywords:
  #+history_url:         — overrides the auto-computed GitHub history URL
  #+htmlized_source_url: — URL for the per-article colourised source badge
These are ignored for ordinary standalone files (regex yields nil, fallback applies)."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (delay-mode-hooks (org-mode))
      (let* ((keyword-pairs
              (cl-loop for (prop.name prop.regex prop.default) on
                    `("title"                "^\\#\\+title:[ ]*\\(.+\\)$"                ,post-filename
                      "date"                 "^\\#\\+date:[ ]*<\\([^]>]+\\)>$"           ,(format-time-string "%Y-%m-%d %a")
                      "image"                "^\\#\\+fileimage: \\(.*\\)"                "emacs-birthday-present.png 350 350"
                      "description"          "^\\#\\+description:[ ]*\\(.+\\)$"          "I learned something neat, and wanted to share!"
                      "tags"                 "^\\#\\+filetags:[ ]*\\(.+\\)$"             "" ;; String; Space-separated
                      "history_url"          "^\\#\\+history_url:[ ]*\\(.+\\)$"          nil
                      "htmlized_source_url"  "^\\#\\+htmlized_source_url:[ ]*\\(.+\\)$"  nil
                      "site_nav"             "^\\#\\+site_nav:[ ]*\\(.+\\)$"             nil
                      )
                  by 'cdddr
                  ;; See: https://stackoverflow.com/questions/19774603/convert-alist-to-from-regular-list-in-elisp
                  do (goto-char (point-min))
                  collect (cons prop.name
                                (if (search-forward-regexp prop.regex nil t)
                                    (match-string 1)
                                  prop.default)))))
        (-snoc
         (cons
          (cons "file" (f-base post-filename))
          keyword-pairs)
         (cons "url" (concat "https://alhassy.com/" (f-base post-filename)))
         ;; Prefer an explicit #+history_url: (injected for container sub-articles)
         ;; over the auto-derived URL based on the file basename.
         (cons "history"
               (or (cdr (assoc "history_url" keyword-pairs))
                   (format "https://github.com/alhassy/alhassy.github.io/commits/master/%s.org"
                           (f-base post-filename))))
         (cons "abstract" (progn
                    (goto-char (point-min))
                    (when (re-search-forward "^\* Abstract" nil t)
                      (beginning-of-line)
                      (-let [start (point)]
                        (org-narrow-to-subtree)
                        (org-fold-show-entry)
                        (re-search-forward "^ *:END:" nil t) ;; Ignore :PROPERTIES: drawer, if any.
                        (forward-line)
                        (buffer-substring-no-properties (point) (point-max)))))))))))

(defvar blog-tag-image-alist
  '(("emacs"    . "./resources/emacs-birthday-present.png 350 350")
    ("lisp"     . "./resources/emacs-birthday-present.png 350 350")
    ("org"      . "./resources/org_logo.png 350 350")
    ("haskell"  . "./resources/haskell-logo.png 350 350")
    ("java"     . "./resources/modern-java.png 350 350")
    ("arabic"   . "./resources/arabic-irab.png 350 350")
    ("life"     . "./resources/musa_pink.jpg 350 350")
    ("family"   . "./resources/family-tree.png 350 350")
    ("karate"   . "./resources/fukyu-kata.png 350 350"))
  "Alist mapping Org heading tags to default image specs for blog posts.
First match wins.  The image spec is anything #+fileimage: accepts.
Used by blog--image-for-tags to avoid requiring an explicit :IMAGE: property
on every container sub-article.")

(defun blog--image-for-tags (tags)
  "Return a default image spec for the given list of TAGS (strings, lowercased).
Checks blog-tag-image-alist in order; returns the first match.
Falls back to the global default image when no tag matches."
  (or (cdr (seq-find (lambda (pair) (member (car pair) tags))
                     blog-tag-image-alist))
      "./resources/emacs-birthday-present.png 350 350"))

(defun blog--article-style (&optional filename)
  "Return the #+article_style keyword for FILENAME (default: current buffer file).
Returns \"multiple\" or \"standalone\" (the default when the keyword is absent)."
  (let ((case-fold-search t)
        (file (or filename (buffer-file-name))))
    (if (not file)
        "standalone"
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+article_style:[ ]*\\(.+\\)$" nil t)
            (s-trim (match-string 1))
          "standalone")))))

(defun blog--make-slug (title)
  "Convert TITLE to a URL-safe kebab-case slug.

Lowercases, strips non-alphanumeric characters (keeping spaces and existing
dashes), then collapses runs of spaces/dashes into a single dash."
  (thread-last title
    downcase
    (replace-regexp-in-string "[^[:alnum:][:space:]-]" "")
    (replace-regexp-in-string "[[:space:]]+" "-")
    (replace-regexp-in-string "-+" "-")
    (replace-regexp-in-string "^-\\|-$" "")))

(defun blog--make-slugs-for-headings (titles)
  "Return a list of unique slugs for TITLES in the same order.

Collisions within the list are resolved by appending -2, -3, ... to the
base slug.  This is a two-pass approach: first derive base slugs, then
detect and fix collisions."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (title)
              (let* ((base  (blog--make-slug title))
                     (count (gethash base seen 0))
                     (slug  (if (= count 0) base (format "%s-%d" base (1+ count)))))
                (puthash base (1+ count) seen)
                slug))
            titles)))

(defun blog--info-subtree-abstract (headline-node)
  "Return the abstract text for HEADLINE-NODE, or nil.

Priority:
1. :ABSTRACT: property on the headline.
2. A child heading whose title matches /abstract/i (e.g. ** Abstract :ignore:).
3. The first paragraph element in the heading body."
  (or
   ;; 1. Explicit property
   (org-element-property :ABSTRACT headline-node)
   ;; 2. Child heading named Abstract
   (org-element-map headline-node 'headline
     (lambda (child)
       (when (and (= (org-element-property :level child)
                    (1+ (org-element-property :level headline-node)))
                  (string-match-p "abstract"
                                  (downcase (or (org-element-property :raw-value child) ""))))
         (org-element-interpret-data (org-element-contents child))))
     nil t)
   ;; 3. First paragraph fallback
   (org-element-map headline-node 'paragraph
     (lambda (para) (org-element-interpret-data para))
     nil t)))

(defun blog--info-subtree (headline-node container-file slug)
  "Extract post metadata for HEADLINE-NODE from CONTAINER-FILE with SLUG.

Returns nil when the headline carries a :noexport: tag.
Returns an alist with the same keys as blog--info plus \"slug\" and \"container\"."
  (let* ((heading-tags (mapcar #'downcase
                               (org-element-property :tags headline-node))))
    (unless (member "noexport" heading-tags)
      (let* (;; draft?
             (draft? (or (member "draft" heading-tags)
                         (equal "t" (org-element-property :DRAFT headline-node))))
             ;; title: :TITLE: property overrides heading text
             (title (or (org-element-property :TITLE headline-node)
                        (org-element-property :raw-value headline-node)))
             ;; date
             (date-raw (org-element-property :DATE headline-node))
             (date (if date-raw
                       (replace-regexp-in-string "[<>]" "" date-raw)
                     (format-time-string "%Y-%m-%d")))
             ;; description
             (description (or (org-element-property :DESCRIPTION headline-node)
                              "I learned something neat, and wanted to share!"))
             ;; image: explicit :IMAGE: property wins; otherwise derive from tags
             (image (or (org-element-property :IMAGE headline-node)
                        (blog--image-for-tags heading-tags)))
             ;; tags: merge :TAGS: property with heading tags; strip structural tags
             (structural-tags '("noexport" "draft" "ignore" "details" "details_orange"
                                "details_green" "header" "reexport" "noreexport"))
             (prop-tags  (s-split " " (or (org-element-property :TAGS headline-node) "") t))
             (org-tags   (seq-remove (lambda (tag) (member tag structural-tags)) heading-tags))
             (all-tags   (seq-uniq (append prop-tags org-tags)))
             (tags-str   (s-join " " all-tags))
             ;; abstract
             (abstract (blog--info-subtree-abstract headline-node))
             ;; urls
             (container-base (f-base container-file))
             (url     (concat "https://alhassy.com/" slug))
             (history (format "https://github.com/alhassy/alhassy.github.io/commits/master/%s.org"
                              container-base)))
        (list (cons "file"        container-base)
              (cons "slug"        slug)
              (cons "container"   container-base)
              (cons "title"       title)
              (cons "date"        date)
              (cons "image"       image)
              (cons "description" description)
              (cons "tags"        tags-str)
              (cons "url"         url)
              (cons "history"     history)
              (cons "abstract"    abstract)
              (cons "draft"       (if draft? "t" nil))
              (cons "redirect"    (org-element-property :REDIRECT headline-node))
              (cons "site_nav"    (org-element-property :SITE_NAV headline-node)))))))

(defun blog--info-multiple (container-file)
  "Return a list of post-alists for all publishable top-level headings in CONTAINER-FILE.

CONTAINER-FILE must carry #+article_style: multiple.
Headings tagged :noexport: are excluded.
Headings tagged :draft: are included but marked."
  (with-temp-buffer
    (insert-file-contents container-file)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           ;; Collect all level-1 headings
           (top-headings
            (org-element-map tree 'headline
              (lambda (h)
                (when (= (org-element-property :level h) 1) h))))
           ;; Derive slugs.  A heading with an explicit :SLUG: property uses it
           ;; verbatim (the author takes responsibility for uniqueness).  All
           ;; other headings derive their slug from :TITLE: or the heading text
           ;; and go through blog--make-slugs-for-headings for dedup.
           (slugs
            (let* ((explicit  ; :SLUG: property, or nil
                    (mapcar (lambda (h) (org-element-property :SLUG h)) top-headings))
                   (titles    ; used only for headings without an explicit slug
                    (mapcar (lambda (h)
                              (or (org-element-property :TITLE h)
                                  (org-element-property :raw-value h)))
                            top-headings))
                   ;; Compute deduped slugs for the headings that need it,
                   ;; passing a placeholder for those with explicit slugs so the
                   ;; indices stay aligned.
                   (deduped (blog--make-slugs-for-headings
                             (cl-mapcar (lambda (exp title) (or exp title))
                                        explicit titles))))
              ;; Explicit :SLUG: wins over the deduped result.
              (cl-mapcar (lambda (exp deduped) (or exp deduped))
                         explicit deduped))))
      ;; Build alists, skipping noexport headings (blog--info-subtree returns nil for them)
      (delq nil
            (cl-mapcar (lambda (h slug) (blog--info-subtree h container-file slug))
                       top-headings slugs)))))

(defun blog--subtree-stale-p (heading-point slug info)
  "Return non-nil when the article at HEADING-POINT needs republishing.

Stale when any of:
  • ~/blog/SLUG.html does not exist
  • the :MODIFIED: property is absent on the heading
  • the HTML file predates MODIFIED (mtime < MODIFIED date)
  • the article has a :REDIRECT: and the redirected file is newer than MODIFIED"
  (let* ((html-file  (expand-file-name (concat slug ".html") blog-posts-directory))
         (modified   (save-excursion
                       (goto-char heading-point)
                       (org-entry-get (point) "MODIFIED")))
         (redirect   (cdr (assoc "redirect" info))))
    (or (not (file-exists-p html-file))
        (not modified)
        ;; HTML mtime older than the recorded MODIFIED date?
        (time-less-p (file-attribute-modification-time
                      (file-attributes html-file))
                     (date-to-time modified))
        ;; For redirect articles: included file newer than MODIFIED?
        (and redirect
             (let ((rpath (expand-file-name redirect)))
               (and (file-exists-p rpath)
                    (time-less-p (date-to-time modified)
                                 (file-attribute-modification-time
                                  (file-attributes rpath)))))))))

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

(defun blog--greeting (&optional tag)
  "Return the index/tag-page greeting string, optionally specialised to TAG."
  (format "Here are some of my latest thoughts%s... badge:Made_with|Lisp|success|https://alhassy.github.io/ElispCheatSheet/CheatSheet.pdf|Gnu-Emacs such as doc:thread-first and doc:loop (•̀ᴗ•́)و tweet:https://alhassy.com @@html:<br><br>@@"
          (if tag (concat " on " tag) "")))

(defun blog--card (post)
  "Return the Org source for one article card as a tagged top-level heading.

The heading carries the post's tags so per-tag exports can be built
by filtering blog-posts directly — no copy-then-delete needed.
The heading text does not appear in the HTML output (title:nil)."
  (let ((tags (s-join ":" (s-split " " (map-elt post "tags") t))))
    (concat
     (format "* %s %s\n" (@title post) (if (s-blank? tags) "" (format ":%s:" tags)))
     "#+begin_export html\n"
     (format "<h2 class=\"title\"><a href=\"%s\">%s</a></h2>\n" (@url post) (@title post))
     (format "<center>%s</center>\n" (@tags post))
     (@image post "resources/")
     "\n"
     (or (@abstract post) "")
     "\n"
     (format "<p style=\"text-align:right\"> badge:Read|more|green|%s|read-the-docs </p>\n" (@url post))
     "#+end_export\n")))

(defun blog--make-page-buffer (posts greeting export-file-name)
  "Return a fresh Org buffer for POSTS with GREETING, targeting EXPORT-FILE-NAME.
Caller is responsible for killing the buffer when done."
  (let ((buf (generate-new-buffer " *blog-page*")))
    (with-current-buffer buf
      (insert
       (format "#+EXPORT_FILE_NAME: %s\n" export-file-name)
       "#+options: toc:nil title:nil html-postamble:nil broken-links:t\n"
       "#+begin_export html\n"
       org-static-blog-page-preamble "\n"
       org-static-blog-page-header "\n"
       "#+end_export\n"
       "#+html: <br>\n"
       greeting "\n"
       "#+html: <br>\n"
       (mapconcat #'blog--card posts "\n")
       "\n#+begin_export html\n"
       "<hr> <center> <em> Thanks for reading everything! 😁 Bye! 👋 </em>"
       " &nbsp;|&nbsp; <a href=\"https://alhassy.github.io/rss.xml\">RSS feed</a>"
       " </center> <br/>\n"
       (blog--license)
       "\n#+end_export\n")
      (org-mode)
      ;; org-special-block-extras provides the `badge:`, `doc:`, `tweet:` etc.
      ;; link handlers used throughout the card template (blog--card).
      ;; Without it those links pass through verbatim into the rendered HTML.
      (org-special-block-extras-mode 1)
      (setq org-html-head-extra ""))
    buf))

(defun blog-make-index-page ()
  "Assemble index.html and every tag page.

Builds one Org buffer per output file, each populated directly from
the relevant subset of blog-posts — no copy-then-delete."
  (cl-flet ((export-page (posts greeting dest)
               (let ((buf (blog--make-page-buffer posts greeting dest)))
                 (unwind-protect
                     (with-current-buffer buf (org-html-export-to-html))
                   (with-current-buffer buf (set-buffer-modified-p nil))
                   (kill-buffer buf)))))
    (export-page blog-posts
                 (blog--greeting)
                 (concat-to-dir blog-publish-directory "index.html"))
    (dolist (tag blog-tags)
      (message "=> Generating tag page: %s..." tag)
      (export-page (seq-filter (lambda (p)
                                 (member tag (s-split " " (map-elt p "tags") t)))
                               blog-posts)
                   (blog--greeting tag)
                   (concat-to-dir blog-publish-directory (concat "tag-" (blog--tag-slug tag) ".html"))))))

(defun blog-make-all-tag-pages ()
  "Regenerate index.html and all tag pages. Calls blog-make-index-page."
  (interactive)
  (blog-make-index-page))

(defun blog--make-tag-pages-for-tags (_tags)
  "Regenerate index.html and all tag pages.
The TAGS argument is accepted for compatibility but ignored — the
unified buffer approach always rebuilds everything in one pass."
  (blog-make-index-page))

(defun blog--compute-posts-and-pages ()
  "Scan ~/blog/*.org and return (posts . pages).

Every .org file is processed uniformly — there is no special-cased directory.
Container files (#+article_style: multiple) yield many post entries; standalone
files yield one.  A file with #+site_nav: contributes a nav-page entry instead
of (or in addition to, if it also has #+date:) a post entry.
Files with neither #+date: nor #+site_nav: (e.g. MathJaxPreamble.org) are skipped.

posts — dated, non-site_nav entries, sorted newest-first.
pages — site_nav entries, unsorted."
  (let ((posts '())
        (pages '()))
    (dolist (file (f-files blog-posts-directory))
      (when (s-ends-with? ".org" file)
        (let ((infos (if (equal "multiple" (blog--article-style file))
                         (blog--info-multiple file)
                       (list (blog--info file)))))
          (dolist (info infos)
            (cond
             ((map-elt info "site_nav") (push info pages))
             ((map-elt info "date")     (push info posts)))))))
    (cons (sort posts (lambda (a b)
                        (time-less-p (date-to-time (@date b))
                                     (date-to-time (@date a)))))
          pages)))

(defun blog--rebuild-preamble ()
  "Regenerate org-static-blog-page-preamble from blog-pages.
Falls back to blog--preamble-fallback when blog-pages is empty.
Called automatically by blog--refresh-posts; also useful to call
interactively after editing :SITE_NAV: headings."
  (setq org-static-blog-page-preamble
        (if (null blog-pages)
            (blog--preamble-fallback)
          (concat
           "<div class=\"header\">\n"
           "  <a href=\"https://alhassy.github.io/\" class=\"logo\">Life & Computing Science</a>\n"
           "  <br/>\n"
           (mapconcat (lambda (p)
                        (format "  <a href=\"%s\">%s</a>\n" (@url p) (map-elt p "site_nav")))
                      blog-pages "")
           "</div>"))))

(defun blog--refresh-posts ()
  "Recompute blog-posts, blog-pages, and blog-tags from source org files."
  (let ((result (blog--compute-posts-and-pages)))
    (setq blog-posts (car result))
    (setq blog-pages (cdr result))
    (setq blog-tags
          (sort (seq-uniq (-flatten (seq-map (lambda (it) (s-split " " (map-elt it "tags")))
                                             blog-posts)))
                #'string<))
    (blog--rebuild-preamble)))

(defvar blog-posts nil
  "All post metadata, sorted newest-first. Initialized at end of file; refresh with (blog--refresh-posts).")

(defvar blog-pages nil
  "Site navigation page metadata (subtrees with :SITE_NAV: t).
These appear as header links on every page but not as blog post cards.
Initialized at end of file; refresh with (blog--refresh-posts).")

(defvar blog-tags nil
  "Tags for my blog articles. Initialized at end of file; refresh with (blog--refresh-posts).")

(org-deflink blog
  "Provide the styles for 'www.alhassy.com's header and footer.

The use of 'blog:footer' aims to provide a clickable list of tags, produce an HTMLized version of the Org source,
and provides a Disqus comments sections. For details, consult the blog--footer function.

Finally, I want to avoid any `@@backend:...@@' from appearing in the browser frame's title.
We accomplish this with the help of some handy-dandy JavaScript: Just use 'blog:sanitise-title'.
"
      (pcase o-label
        ("header" (concat
                   org-static-blog-page-preamble
                   org-static-blog-page-header
                   "<link href=\"https://alhassy.github.io/org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   "<link href=\"https://alhassy.github.io/floating-toc.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   "<link href=\"https://alhassy.github.io/blog-banner.css\" rel=\"stylesheet\" type=\"text/css\" />"
                   ;; The use of the "post-title" class is so that the org-static-blog-assemble-rss method can work as intended.
                   (thread-last (org-static-blog-get-title (buffer-file-name))
                                (s-replace-regexp "@@html:" "")
                                (s-replace-regexp "@@" "")
                                (format "<br><center><h1 class=\"post-title\">%s</h1></center>"))))
        ("footer" (blog--footer (buffer-file-name)))
        ("sanitise-title" "<script> window.parent.document.title =  window.parent.document.title.replace(/@@.*@@/, \"\") </script>")
        (_ "")))

(defun blog--style-setup (_backend)
  "Insert blog header (fancy title), tags, blog image (before \"* Abstract\"), and footer (links to tags).

There are default options: TOC is at 2 levels, no classic Org HTML postamble nor drawers are shown.
Notice that if you explicitly provide options to change the toc, date, or show drawers, etc;
then your options will be honoured. (Since they will technically come /after/ the default options,
which I place below at the top of the page.)
"
  (goto-char (point-min))
  (let ((post (blog--info (buffer-file-name))))
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
            (@image post "resources/")
            "\n")

    ;; Wrap contents of "* Abstract" section in the "abstract" Org-special-block
    ;; (In case we are narrowed, we only act when we can find the Abstract.)
    ;; TODO: Replace this with (@abstract (blog--info (buffer-file-name))), or: (@abstract post)
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

(defun blog--show-preview (url)
  "Display URL in an xwidget buffer to the right of the current Org window.

Guarantees [Org source | xwidget] side-by-side layout:
- If there is already an xwidget window on the right, reuse it.
- Otherwise split the current window to the right and open there.
- The Org source window is never taken over."
  (let* ((org-win   (selected-window))
         (xw-buf    (seq-find (lambda (b)
                                (eq 'xwidget-webkit-mode
                                    (buffer-local-value 'major-mode b)))
                              (buffer-list)))
         (xw-win    (and xw-buf (get-buffer-window xw-buf))))
    (if (and xw-win (not (eq xw-win org-win)))
        ;; Reuse the existing xwidget window — just navigate.
        (progn
          (select-window xw-win)
          (xwidget-webkit-browse-url url)
          (select-window org-win))
      ;; No usable xwidget window — split right and open there.
      (let ((right-win (split-window org-win nil 'right)))
        (select-window right-win)
        (xwidget-webkit-browse-url url)
        (select-window org-win)))))

(cl-defun blog-preview ()
  "Enable preview-on-save, dispatching on #+article_style.

For standalone files (default): existing behaviour — hooks blog--style-setup and
enables org-preview-html-mode so every C-x C-s regenerates the preview.

For multiple-style files: adds a buffer-local after-save-hook that calls
blog-preview-subtree, which previews just the heading at point."
  (interactive)
  ;; Let's ensure we have no xwidget buffer lying around, otherwise Emacs might hang.
  (-let [kill-buffer-query-functions nil]
    (mapcar #'kill-buffer (--filter (equal 'xwidget-webkit-mode (buffer-local-value 'major-mode it)) (buffer-list))))
  (if (equal "multiple" (blog--article-style (buffer-file-name)))
      ;; Multiple-style: preview heading at point on every save (buffer-local hook).
      (add-hook 'after-save-hook #'blog-preview-subtree nil t)
    ;; Standalone: existing behaviour.
    (add-hook 'org-export-before-processing-hook #'blog--style-setup)
    (setq org-preview-html-viewer 'xwidget)
    (org-preview-html-mode)))

(defun blog-preview-subtree ()
  "Preview the top-level heading at point as a standalone blog article.

For use in multiple-style (#+article_style: multiple) files.
Called automatically by the buffer-local after-save-hook set up by blog-preview.

Opens (or reuses) an xwidget window to the right of the Org source buffer,
maintaining a stable [Org source | xwidget] side-by-side layout."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (unless (equal "multiple" (blog--article-style (buffer-file-name)))
      (user-error "Not a multiple-style file; use blog-preview instead"))
    ;; Navigate to enclosing top-level heading.
    (save-excursion
      (unless (org-at-heading-p) (outline-previous-heading))
      (while (> (org-outline-level) 1) (org-up-heading-safe))
      (let* ((tags  (mapcar #'downcase (org-get-tags)))
             (_ (when (member "noexport" tags)
                  (user-error "Heading is tagged :noexport: — nothing to preview")))
             (title     (org-get-heading t t t t))
             (slug      (blog--make-slug (or (org-entry-get (point) "TITLE") title)))
             (all-infos (blog--info-multiple (buffer-file-name)))
             (html-out  (expand-file-name (concat slug ".html") blog-posts-directory)))
        (message "=> Previewing %s..." title)
        (blog--publish-single-subtree (point) (buffer-file-name) all-infos slug)
        (when (file-exists-p html-out)
          ;; blog--show-preview reuses the existing xwidget window when present,
          ;; so the [Org | xwidget] split is stable across repeated saves.
          (blog--show-preview (concat "file://" (expand-file-name html-out))))))))

(defun blog--footer (post-file-name)
  "Returns the HTML rendering the htmlised source, version history, and comment box at the end of a post.

This function is called for every post and the returned string is appended to the post body, as a postamble.

For container sub-articles, the temp buffer carries #+htmlized_source_url: and #+history_url:
synthetic keywords; blog--info reads them and we use them here instead of auto-deriving from the
temp file path."
  (let* ((post (blog--info (buffer-file-name)))
         (source-badge
          (if-let (url (@htmlized_source_url post))
              ;; Container sub-article: source htmlized separately to <slug>.org.html;
              ;; just emit the badge pointing to it.
              (concat "<a class=\"tooltip\""
                      " title=\"See the colourised Org source of this article;"
                      " i.e., what I typed to get this nice webpage\""
                      " href=\"" url "\"><img"
                      " src=\"https://img.shields.io/badge/-Source-informational?logo=read-the-docs\"></a>")
            ;; Standalone: existing behaviour — htmlize and return badge.
            (blog--htmlize-file post-file-name))))
    (concat
     "<hr>"
     "<center>"
     source-badge
     "&ensp;"
     (@history post)
     ;;
     ;; Consider only add this to posts tagged "arabic"?
     (blog--css-arabic-font-setup)
     ;;
     "<br>"
   "<a href=\"https://www.buymeacoffee.com/alhassy\"><img src="
   "\"https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee\">"
   "</a>"
   ;;
   "<br><strong> Generated by Emacs and Org-mode (•̀ᴗ•́)و </strong>"
   (blog--license)
   ;; (blog--comments) ;; TODO. Not working as intended; low priority.
   "</center>"
   ;; The next line is required to make the org-static-blog-assemble-rss method work.
   "<div hidden> <div id=\"postamble\" class=\"status\"> </div> </div>"
   (blog--read-remaining-js))))

(defun blog--htmlize-file (file-name)
  "Generate an htmlized version of a given source file; return an HTML badge linking to the colourised file."
  (let ((org-hide-block-startup nil))
    (with-temp-buffer
      (find-file file-name)
      (org-mode)
      (outline-show-all)
      (switch-to-buffer (htmlize-buffer))
      (write-file (expand-file-name (concat (f-base file-name) ".org.html") blog-publish-directory))
      (kill-buffer)))
(concat
"<a class=\"tooltip\" title=\"See the colourised Org source of this article; i.e., what I typed to get this nice webpage\" href=\""
   (f-base file-name) ".org.html\"><img
   src=\"https://img.shields.io/badge/-Source-informational?logo=read-the-docs\"></a>"))

(defun blog--license ()
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

(defun blog--comments ()
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

(defun blog--read-remaining-js ()
  "Get the HTML required to make use of ReadRemaining.js"

  ;; [Maybe Not True] ReadReamining.js does not work well with xWidget browser within Emacs
  (if (equal (bound-and-true-p org-preview-html-viewer) 'xwidget)
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
   ;; NOPE: org-html-head-extra  ;; Altered by 'org-special-block-extras'
   (concat
   "<meta name=\"author\" content=\"Musa Al-hassy\">
    <meta name=\"referrer\" content=\"no-referrer\">"
   "<link href=\"resources/usual-org-front-matter.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link href=\"resources/org-notes-style.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link href=\"resources/floating-toc.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link href=\"resources/blog-banner.css\" rel=\"stylesheet\" type=\"text/css\" />"
   "<link rel=\"icon\" href=\"resources/favicon.png\">")
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

(defun blog--preamble-fallback ()
  "Hardcoded fallback preamble used when blog-pages is not yet populated."
  "<div class=\"header\">
  <a href=\"https://alhassy.github.io/\" class=\"logo\">Life & Computing Science</a>
  <br/>
  <a href=\"https://alhassy.github.io/AlBasmala\">AlBasmala</a>
  <a href=\"https://alhassy.github.io/rss.xml\">RSS</a>
  <a href=\"https://alhassy.github.io/about\">About</a>
</div>")

;; Seed the preamble; blog--refresh-posts (called at end of file) will
;; rebuild it from :SITE_NAV: t entries once blog-pages is populated.
(setq org-static-blog-page-preamble (blog--preamble-fallback))

;; Table captions should be below the tables
(setq org-html-table-caption-above nil
      org-export-latex-table-caption-above nil)

(advice-add 'org-html--translate :before-until 'blog--display-toc-as-Ξ)
;; (advice-remove 'org-html--translate 'display-toc-as-Ξ)

(defun blog--display-toc-as-Ξ (phrase info)
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

(defun blog--ensure-useful-section-anchors (&rest _)
  "Org sections without an ID are given one based on its title.

All non-alphanumeric characters are cleverly replaced with '-'.

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

;; Anchor assignment is an interactive-authoring concern — it should only
;; happen while you can still edit the generated id, i.e. during C-x C-s
;; preview.  CI must not mutate source files, and (undo)/(message-box) don't
;; work headlessly anyway.

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

(defun blog--css-arabic-font-setup ()
  "Return CSS/HTML for Arabic font rendering.
For a one-off use in an article, prepend #+html: to the result."
  "
  <link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Amiri'>
  <style>
     body {font-family: 'Amiri', sans-serif;}
     table {font-family:  'Scheherazade'; font-size: 105%; }
   </style>")

(cl-defun blog--git (cmd &rest args)
  "Execute git command CMD, which may have %s placeholders whose values are positional in ARGS."
  (let ((default-directory (expand-file-name blog-posts-directory)))
    (shell-command (apply #'format (concat "git " cmd) args))))

(defun blog--multiple-style-p ()
  "Return non-nil when the current buffer is a multiple-style container."
  (equal "multiple" (blog--article-style (buffer-file-name))))

(defun blog--commit-message (default)
  "Return a git commit message: prompt if C-u prefix, else use DEFAULT."
  (if current-prefix-arg (read-string "Commit message: ") default))

(defun blog--find-info-by-slug (slug infos)
  "Return the first entry in INFOS whose slug matches SLUG, or nil."
  (seq-find (lambda (a) (equal (@slug a) slug)) infos))


(defvar my/blogging-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-x C-s")
                (lambda ()
                  (interactive)
                  (blog--ensure-useful-section-anchors)
                  (save-buffer)
                  (if (blog--multiple-style-p) (blog-preview-subtree) (blog-preview))))
    (define-key m (kbd "M-RET")
                (lambda ()
                  (interactive)
                  (if (blog--multiple-style-p) (blog-new-post) (blog-new-article))))
    (define-key m (kbd "C-c i i") #'blog-insert-image)
    (define-key m (kbd "C-c i s") #'blog-insert-screenshot)
    m)
  "Keymap for my/blogging-mode.")

(define-minor-mode my/blogging-mode
  "Buffer-local minor mode for editing blog articles in AlBasmala style.

Binds:
  C-x C-s  — save + live preview (dispatches on article style)
  M-RET    — new article / new post (dispatches on article style)
  C-c i i  — insert image from file (C-u to rename before committing)
  C-c i s  — take a screenshot and insert it

Publishing is not bound to a key: push your .org source to master and CI
runs `blog-publish-all' on a fresh checkout.

On activation:
  - enables org-special-block-extras-mode (badges, doc: links, tooltips)
  - switches browse-url to xwidget-webkit for in-Emacs previews

On deactivation:
  - disables org-special-block-extras-mode
  - restores browse-url to the system browser (Arc/Chrome etc.)"
  :lighter " Blog"
  :keymap my/blogging-mode-map
  (if my/blogging-mode
      (progn
        (require 'org-special-block-extras)
        (require 'org-preview-html)
        (org-special-block-extras-mode 1)
        (setq browse-url-browser-function 'xwidget-webkit-browse-url))
    (org-special-block-extras-mode -1)
    (setq browse-url-browser-function 'browse-url-default-browser)))


(defun blog--htmlize-subtree (heading-point slug)
  "Htmlize the subtree at HEADING-POINT in the current buffer to ~/blog/SLUG.org.html.

This produces a per-article colourised source view for container sub-articles.
We copy the subtree content to a temp buffer, narrow to the pasted content,
htmlize, and write the result."
  (save-excursion
    (goto-char heading-point)
    (org-copy-subtree))
  (let ((tmp-buf (generate-new-buffer " *blog-htmlize-subtree*")))
    (unwind-protect
        (with-current-buffer tmp-buf
          (org-mode)
          (org-paste-subtree 1)
          (outline-show-all)
          (switch-to-buffer (htmlize-buffer))
          (write-file (expand-file-name (concat slug ".org.html") blog-publish-directory))
          (set-buffer-modified-p nil)
          (kill-buffer))
      (when (buffer-live-p tmp-buf)
        (with-current-buffer tmp-buf (set-buffer-modified-p nil))
        (kill-buffer tmp-buf)))))


(defun blog--publish-single-subtree (heading-point container-file all-infos slug)
  "Export the subtree at HEADING-POINT in the current buffer to ~/blog/SLUG.html.

ALL-INFOS is the result of (blog--info-multiple CONTAINER-FILE).
SLUG is the pre-computed slug for this heading (from ALL-INFOS).

The subtree is copied into a temp .org file populated with synthetic
file-level keywords so that blog--style-setup runs unchanged."
  (let* ((info    (blog--find-info-by-slug slug all-infos))
         (tmp-org (make-temp-file "albasmala-" nil ".org"))
         (tmp-buf (find-file-noselect tmp-org)))
    (unwind-protect
        (progn
          ;; 1. Populate temp file with synthetic file-level keywords.
          ;;    blog--info reads these via regex when blog--style-setup calls
          ;;    (blog--info buffer-file-name) during export.
          (with-current-buffer tmp-buf
            (erase-buffer)
            (insert
             "#+title: "                (cdr (assoc "title"       info)) "\n"
             "#+date: <"               (cdr (assoc "date"         info)) ">\n"
             "#+fileimage: "           (cdr (assoc "image"        info)) "\n"
             "#+filetags: "            (cdr (assoc "tags"         info)) "\n"
             "#+description: "        (cdr (assoc "description"  info)) "\n"
             (if (equal "t" (cdr (assoc "draft" info))) "#+draft: t\n" "")
             ;; Synthetic overrides — blog--info prefers these over auto-derived values.
             "#+history_url: "         (cdr (assoc "history"      info)) "\n"
             "#+htmlized_source_url: " "https://alhassy.com/" slug ".org.html\n"
             "\n")
            (save-buffer))

          ;; 2. Populate the temp file body.
          ;;
          ;;    When the heading carries a :REDIRECT: property we simply emit a
          ;;    #+include: directive pointing at the external file — Org's own
          ;;    include machinery handles the rest during export.
          ;;
          ;;    Otherwise we copy the subtree, paste it, axe the redundant
          ;;    top-level heading line, and promote all children one level so
          ;;    blog--style-setup's "^* Abstract" search finds them correctly:
          ;;
          ;;      Before promotion   →   After
          ;;      * Article Title        (deleted)
          ;;      ** Abstract :ignore:   * Abstract :ignore:
          ;;      ** Introduction ...    * Introduction ...
          (let ((redirect (cdr (assoc "redirect" info))))
            (if redirect
                (with-current-buffer tmp-buf
                  (goto-char (point-max))
                  (insert (format "#+include: \"%s\"\n"
                                  (expand-file-name redirect)))
                  (save-buffer))
              (save-excursion
                (goto-char heading-point)
                (org-copy-subtree))
              (with-current-buffer tmp-buf
                (goto-char (point-max))
                (org-paste-subtree 1)
                (goto-char (point-min))
                (when (re-search-forward "^\\* " nil t)
                  (delete-region (line-beginning-position) (line-beginning-position 2)))
                (org-map-entries #'org-promote t)
                (save-buffer))))

          ;; 3. Export through the full blog--style-setup pipeline.
          (with-current-buffer tmp-buf
            (add-hook 'org-export-before-processing-hook #'blog--style-setup)
            (let ((default-directory (file-name-directory tmp-org)))
              (org-html-export-to-html))
            (remove-hook 'org-export-before-processing-hook #'blog--style-setup))

          ;; 4. Move the resulting HTML to blog-publish-directory/<slug>.html.
          (let ((html-out (concat (file-name-sans-extension tmp-org) ".html")))
            (when (file-exists-p html-out)
              (rename-file html-out
                           (expand-file-name (concat slug ".html") blog-publish-directory)
                           t)))

          ;; 5. Stamp :MODIFIED: on the source heading so future runs can detect
          ;;    whether the HTML is up-to-date without re-exporting.
          (save-excursion
            (goto-char heading-point)
            (org-set-property "MODIFIED" (format-time-string "%Y-%m-%d")))

          ;; 6. Per-article colourised source: htmlize the subtree narrowed copy.
          (blog--htmlize-subtree heading-point slug))

      ;; Cleanup temp files regardless of errors.
      ;; Mark the buffer unmodified before killing — this is the only reliable way
      ;; to prevent "Buffer modified, kill anyway?" prompts regardless of what
      ;; kill-buffer-query-functions contains.
      (when (buffer-live-p tmp-buf)
        (with-current-buffer tmp-buf (set-buffer-modified-p nil))
        (kill-buffer tmp-buf))
      (when (file-exists-p tmp-org) (delete-file tmp-org))
      (let ((tmp-html (concat (file-name-sans-extension tmp-org) ".html")))
        (when (file-exists-p tmp-html) (delete-file tmp-html))))))

(defun blog--publish-multiple-articles (container-file)
  "Publish each top-level heading of CONTAINER-FILE as a separate HTML article.

Writes the derived slug back to the heading as a :SLUG: property so future
publishes are stable even if the heading title changes.

Returns the list of slugs that were published."
  (let ((all-infos (blog--info-multiple container-file))
        (results   '()))
    (with-current-buffer (find-file-noselect container-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* " nil t)
          (beginning-of-line)
          (let* ((tags  (mapcar #'downcase (org-get-tags)))
                 (title (org-get-heading t t t t))
                 (slug  (or (org-entry-get (point) "SLUG")
                            (blog--make-slug (or (org-entry-get (point) "TITLE") title)))))
            (unless (member "noexport" tags)
              ;; Persist the slug so it survives future title edits.
              (unless (org-entry-get (point) "SLUG")
                (org-set-property "SLUG" slug))
              (let ((info (blog--find-info-by-slug slug all-infos)))
                (if (blog--subtree-stale-p (point) slug info)
                    (progn
                      (message "=> Publishing subtree: %s (%s)..." title slug)
                      (blog--publish-single-subtree (point) container-file all-infos slug)
                      (push slug results))
                  (message "=> Skipping up-to-date subtree: %s (%s)" title slug)
                  (push slug results)))))
          (org-end-of-subtree t t)))
      (save-buffer))
    (nreverse results)))


;; Initialize blog-posts and blog-tags now that all helpers are defined.
(blog--refresh-posts)

(defun blog--sync-assets ()
  "Copy static assets into blog-publish-directory so relative HTML paths resolve.

Called at the end of `blog-publish-all' so that public/ is self-contained and
can be deployed as-is to gh-pages without the master-branch source tree."
  (let ((dist (file-name-as-directory (expand-file-name blog-publish-directory))))
    (make-directory dist t)
    (dolist (asset '("resources"
                     "readremaining.js-readremainingjs"
                     "floating-toc.css"))
      (let ((src (expand-file-name asset blog-posts-directory)))
        (when (file-exists-p src)
          (if (file-directory-p src)
              (copy-directory src (expand-file-name asset dist) t t t)
            (copy-file src (expand-file-name asset dist) t)))))))

(defun blog--publishable-p (file)
  "Return non-nil if FILE is an article that should be published.
A file is publishable when it has a #+date: keyword (standalone post) or
#+article_style: multiple (container of subtree articles).
Infrastructure files (AlBasmala.el, MathJaxPreamble.org, etc.) have neither
and are silently skipped."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (or (re-search-forward "^#\\+date:" nil t)
        (re-search-forward "^#\\+article_style:[ ]*multiple" nil t))))

(defun blog-publish-all ()
  "Batch-publish every article and regenerate the index.  The sole CI entry point.

Exports all posts → public/, rebuilds index + tag pages + RSS, copies static
assets.  public/ is then deployed by CI to gh-pages so URLs stay flat:
alhassy.com/foo not alhassy.com/public/foo.

Dispatches on #+article_style: per file —
  standalone (default) → one .org → one HTML file,
  multiple            → one subtree → one HTML file (via `blog--publish-multiple-articles')."
  (add-hook 'org-export-before-processing-hook #'blog--style-setup)
  (make-directory blog-publish-directory t)
  (blog--refresh-posts)
  (blog--validate-unique-slugs)
  (dolist (f (f-entries blog-posts-directory
                        (lambda (x) (and (s-suffix? ".org" x)
                                         (blog--publishable-p x)))))
    (with-current-buffer (find-file-noselect f)
      ;; badge:/doc:/tweet: links in articles are registered as Org link types
      ;; by org-special-block-extras-mode.  It's a buffer-local minor mode, so
      ;; turn it on per buffer — without it those links leak verbatim to HTML.
      (org-special-block-extras-mode 1)
      (if (blog--multiple-style-p)
          (progn
            (message "=> Exporting all articles from %s..." (f-base f))
            (blog--publish-multiple-articles f))
        (let ((base (f-base f)))
          (org-html-export-to-html)
          (rename-file (concat base ".html")
                       (expand-file-name (concat base ".html") blog-publish-directory) t)
          (message "⇒ HTMLizing article %s..." base)
          (blog--htmlize-file f)))))
  (blog-make-index-page)
  (blog--sync-assets))

(defun blog-insert-image (file)
  "Copy FILE into ~/blog/resources/, git-add it, and insert an Org link at point.

With a \\[universal-argument] prefix, prompts for a new filename after the
default name is pre-filled so you can rename the resource before committing."
  (interactive "fImage file: ")
  (let* ((default-name (f-filename file))
         (dest-name (if current-prefix-arg
                        (read-string "Name for image (with extension): " default-name)
                      default-name))
         (dest (expand-file-name dest-name (expand-file-name "resources/" blog-posts-directory))))
    (copy-file file dest t)
    (blog--git "add resources/%s" dest-name)
    (insert (format "[[file:resources/%s]]" dest-name))
    (org-display-inline-images nil t (line-beginning-position) (line-end-position))))

(defun blog-insert-screenshot ()
  "Take an interactive screenshot, move it to ~/blog/resources/, git-add, insert link.

Uses macOS screencapture -i (crosshair selector).  After the screenshot is taken
you are prompted for a meaningful name; the timestamp default is just a fallback."
  (interactive)
  (let* ((tmp (make-temp-file "blog-screenshot-" nil ".png")))
    (shell-command (format "screencapture -i %s" (shell-quote-argument tmp)))
    (if (not (file-exists-p tmp))
        (message "Screenshot cancelled.")
      (let* ((default-name (format "screenshot-%s.png" (format-time-string "%Y%m%d-%H%M%S")))
             (dest-name (read-string "Name for screenshot (with extension): " default-name))
             (dest (expand-file-name dest-name (expand-file-name "resources/" blog-posts-directory))))
        (rename-file tmp dest t)
        (blog--git "add resources/%s" dest-name)
        (insert (format "[[file:resources/%s]]" dest-name))
        (org-display-inline-images nil t (line-beginning-position) (line-end-position))))))

(defun blog--all-slug-sources ()
  "Return an alist of (slug . source-description) for every article in blog-posts.

source-description is a human-readable string: the article title and its
file/container, suitable for error messages."
  (mapcar (lambda (p)
            (cons (@slug p)
                  (format "\"%s\" (%s)"
                          (or (map-elt p "title") (@slug p))
                          (or (map-elt p "container") (@file p) (@slug p)))))
          blog-posts))

(defun blog--validate-unique-slugs ()
  "Error when any two articles share a slug.

Scans blog-posts (which already covers both standalone and container
articles) and signals user-error on the first duplicate, naming the title
and source of the conflicting pair.

The effective slug for a post is its explicit :SLUG: property (container
subtrees) or its file basename (standalone articles).  Both land at the
same URL, so both must be globally unique."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (p blog-posts)
      (let* ((slug   (or (@slug p) (@file p)))  ; effective URL slug
             (source (format "\"%s\" (%s)"
                             (or (map-elt p "title") slug)
                             (or (map-elt p "container") (@file p) slug)))
             (prior  (gethash slug seen)))
        (if prior
            (let ((msg (format "Duplicate slug \"%s\":\n  already claimed by %s\n  also claimed by %s\n  Change one of the :SLUG: properties."
                               slug prior source)))
              (if noninteractive (error msg) (user-error msg)))
          (puthash slug source seen))))))

(defun blog--validate-no-orphan-html ()
  "Warn about public/*.html (and *.org.html) files with no corresponding known source.

An HTML file X.html is \"known\" when X is a slug in blog-posts (covers both
standalone and container articles) or matches the reserved-file pattern.
The companion colourised source X.org.html is valid precisely when X.html is valid.
Reports orphans as a hard error in CI (noninteractive) and a warning interactively."
  (let* ((all-slugs   (seq-uniq
                        (cl-loop for p in blog-posts
                                 when (@slug p) collect (@slug p)
                                 when (@file p) collect (f-base (@file p)))))
         (reserved-rx  (rx bos (or "index" "rss" "sitemap" "404" "AlBasmala"
                                   (seq "tag-" (+ anything)))
                           eos))
         (known-p      (lambda (base)
                         (or (string-match-p reserved-rx base)
                             (member base all-slugs))))
         ;; Check plain .html files (exclude *.org.html — handled separately).
         (orphan-html  (seq-filter
                        (lambda (f)
                          (let ((base (f-base f)))
                            (and (not (s-ends-with? ".org" base))
                                 (not (funcall known-p base)))))
                        (f-glob "*.html" blog-publish-directory)))
         ;; Check *.org.html: valid iff the slug part (strip trailing ".org") is known.
         (orphan-org-html
          (seq-filter
           (lambda (f)
             (let ((base (f-base f)))  ; e.g. "foo.org"
               (and (s-ends-with? ".org" base)
                    (not (funcall known-p (f-base base))))))
           (f-glob "*.org.html" blog-publish-directory)))
         (orphans (append orphan-html orphan-org-html)))
    (when orphans
      (let ((msg (format "Orphan HTML files (no Org source or :SLUG: match): %s"
                         (s-join ", " (mapcar #'f-filename orphans)))))
        (if noninteractive (error msg) (message "⚠ %s" msg))))))
