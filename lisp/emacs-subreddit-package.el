;;; emacs-subreddit-package.el --- because package.el sucks ass  -*- lexical-binding:t -*-

(require 'package)
(require 'project)

(defsubst emacs-subreddit-package-where ()
  (directory-file-name (expand-file-name (project-root (project-current)))))

(defsubst emacs-subreddit-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      "lisp/emacs-subreddit.el"
      (emacs-subreddit-package-where)))
    (package-buffer-info)))

(defun emacs-subreddit-package-name ()
  (concat "emacs-subreddit-" (package-version-join
			      (package-desc-version
			       (emacs-subreddit-package-desc)))))

(defun emacs-subreddit-package-inception ()
  "To get a -pkg.el file, you need to run `package-unpack'.
To run `package-unpack', you need a -pkg.el."
  (let ((pkg-desc (emacs-subreddit-package-desc))
	(pkg-dir (expand-file-name (emacs-subreddit-package-name)
				   (emacs-subreddit-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (copy-file (expand-file-name
		"lisp/emacs-subreddit.el"
		(emacs-subreddit-package-where))
	       (expand-file-name "emacs-subreddit.el" pkg-dir))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
