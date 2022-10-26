(defvar blog/tags
  '(emacs faith category-theory order-theory
    lisp types packages haskell agda
    c frama-c program-proving)
  "Tags for my blog articles.")

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
            "\n\n* Abstract :ignore: \n #+begin_abstract\n" desc
            "\n#+end_abstract"
            "\n\n* ???")
    (save-buffer)
    (blog/preview)))
