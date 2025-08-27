;;; emacs-subreddit.el --- r/emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 commandlinesystems.com

;; Authors: dickmao <github id: dickmao>
;; URL: https://github.com/commercial-emacs/emacs-subreddit
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (spinner "1.7.3"))

;;; Commentary:

;; M-x emacs-subreddit
;;
;; From either summary or article buffers:
;; RET                  read comment
;; n                    read next comment
;; p                    read previous comment
;; r                    reply
;; g                    pull new posts
;; c                    mark all read
;; x                    un/show read
;; s                    isearch
;; q                    dismiss
;; SPC                  scroll through
;; T T                  un/display threaded
;; T H                  un/collapse threads
;;

;;; Code:

(require 'gnus-agent)
(require 'gnus-sum)
(require 'gnus-bcklg)
(require 'gnus-msg)
(require 'gnus-cite)
(require 'spinner)
(require 'mm-url)

(defconst emacs-subreddit-newsgroup-name "emacs-subreddit:"
  "How I hate gnus's string-embedded data.")

(defvar emacs-subreddit-error nil
  "Prevailing error.")

(defvar emacs-subreddit-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom.")

(defvar emacs-subreddit-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author.")

(defvar emacs-subreddit-headers nil
  "List headers from GROUP.")

(defun emacs-subreddit--reply (fun)
  (let ((message-mode-hook (copy-sequence message-mode-hook)))
    (add-hook 'message-mode-hook #'emacs-subreddit-bespokify -50)
    (add-hook 'message-mode-hook
	      (lambda () (setq-local message-syntax-checks
				     'dont-check-for-anything-just-trust-me)))
    (call-interactively fun)))

(defun emacs-subreddit-summary-reply ()
  "gnus-configure-posting-styles gets into message-mode-hook."
  (interactive)
  (emacs-subreddit--reply #'gnus-summary-followup-with-original))

(defun emacs-subreddit-article-reply ()
  "gnus-configure-posting-styles gets into message-mode-hook."
  (interactive)
  (emacs-subreddit--reply #'gnus-article-followup-with-original))

(defun emacs-subreddit--get-header (article-number)
  "Get header indexed ARTICLE-NUMBER for GROUP."
  (elt emacs-subreddit-headers (1- article-number)))

(defun emacs-subreddit-find-header (id)
  "O(n) search for header with ID."
  (when-let ((found (seq-position emacs-subreddit-headers id
                                  (lambda (plst id)
                                    (equal id (plist-get plst :id))))))
    (emacs-subreddit--get-header (1+ found))))

(defsubst emacs-subreddit-refs-for (name &optional depth)
  "Get message ancestry for NAME up to DEPTH."
  (unless depth
    (setq depth most-positive-fixnum))
  (when (> depth 0)
    (nreverse (cl-loop with parent-id = (gethash name emacs-subreddit-refs-hashtb)
                       for level = 0 then level
                       for name = parent-id then
                       (gethash name emacs-subreddit-refs-hashtb)
                       until (null name)
                       collect name
                       until (>= (cl-incf level) depth)))))

(defvar emacs-subreddit-directory (nnheader-concat gnus-directory "reddit")
  "Where to retrieve last read state.")

(defvar-local emacs-subreddit--rpc-id 0
  "Bump it up.")

(defvar-local emacs-subreddit--rpc-callbacks nil
  "Alist of cells (ID . CALLBACK), where CALLBACK takes buffer and string.")

(defmacro emacs-subreddit--rpc-id (proc)
  `(buffer-local-value 'emacs-subreddit--rpc-id (process-buffer ,proc)))

(defmacro emacs-subreddit--rpc-callbacks (proc)
  `(buffer-local-value 'emacs-subreddit--rpc-callbacks (process-buffer ,proc)))

(defcustom emacs-subreddit-max-render-bytes 300e3
  "`quoted-printable-encode-region' bogs when spyware gets out of hand."
  :type 'integer
  :group 'emacs-subreddit)

(defcustom emacs-subreddit-rpc-sync-timeout 6
  "Timeout for talking to PRAW."
  :type 'integer
  :group 'emacs-subreddit)

(defcustom emacs-subreddit-localhost "127.0.0.1"
  "Some users keep their browser in a separate domain.
Do not set this to \"localhost\" as a numeric IP is required
for the oauth handshake."
  :type 'string
  :group 'emacs-subreddit)

(defun emacs-subreddit--rpc-filter ()
  (let ((static '("")))
    (apply-partially
     #'emacs-subreddit--dispose-output
     (gv-ref static))))

(defun emacs-subreddit-rpc-sync (callback kwargs method &rest args)
  "Pipe request into PROC with generator KWARGS calling METHOD ARGS.
Install CALLBACK for future response that accepts buffer and json reply
as arguments.  Then wait for response."
  (unless callback (setq callback #'ignore))
  (when-let ((proc (emacs-subreddit-rpc-get))
	     (id (apply #'emacs-subreddit-rpc-async callback kwargs method args))
	     (iteration-seconds 1)
	     (elapsed 0))
    (cl-loop while (and (process-live-p proc)
			(alist-get id (emacs-subreddit--rpc-callbacks proc))
			(< elapsed emacs-subreddit-rpc-sync-timeout))
	     do (cl-incf elapsed iteration-seconds)
             do (accept-process-output proc iteration-seconds 0))
    (custom-reevaluate-setting 'emacs-subreddit-rpc-sync-timeout)
    (when (alist-get id (emacs-subreddit--rpc-callbacks proc))
      ;; ID still extant.  Didn't work.
      (setf (emacs-subreddit--rpc-callbacks proc)
	    (assoc-delete-all id (emacs-subreddit--rpc-callbacks proc)))
      ;; get rid of a possibly stank BASE*
      (set-process-filter (emacs-subreddit-rpc-get)
			  (emacs-subreddit--rpc-filter))
      (if (process-live-p proc)
	  (error "emacs-subreddit-rpc-sync: response timed out")
	(error "emacs-subreddit-rpc-sync: check ' *subreddit-stderr*'")))
    (when emacs-subreddit-error
      (let ((s (error-message-string emacs-subreddit-error)))
	(setq emacs-subreddit-error nil)
	(user-error "emacs-subreddit: %s" s)))))

(defun emacs-subreddit-rpc-async (callback kwargs method &rest args)
  "Pipe request into PROC with generator KWARGS calling METHOD ARGS.
Install CALLBACK for future response that takes buffer and string."
  (unless (hash-table-p kwargs)
    (setq kwargs #s(hash-table)))
  (when-let ((proc (emacs-subreddit-rpc-get))
	     (id (cl-incf (emacs-subreddit--rpc-id proc)))
	     (request `(:method ,method
			:id ,id
			:params (:args ,(apply json-array-type args)
				 :kwargs ,kwargs)))
	     (encoded (json-encode (append '(:jsonrpc "2.0") request))))
    (prog1 id
      (setf (alist-get id (emacs-subreddit--rpc-callbacks proc))
	    (apply-partially callback (process-buffer proc)))
      (gnus-message 5 "emacs-subreddit-rpc-async: send %s" encoded)
      (process-send-string proc (concat encoded "\n")))))

(defun emacs-subreddit-sentinel (process event)
  "Wipe headers state when PROCESS dies from EVENT."
  (unless (equal "open" (substring event 0 4))
    (gnus-message 3 "emacs-subreddit-sentinel: process %s %s"
             (car (process-command process))
             (replace-regexp-in-string "\n$" "" event))
    (setq emacs-subreddit-headers nil)
    (gnus-backlog-shutdown)))

(defun emacs-subreddit--message-user (beg end _prev-len)
  "Message alert with `buffer-substring' from BEG to END."
  (let* ((string (buffer-substring beg end))
         (magic "::user::")
	 (msg (when (string-prefix-p magic string)
		(string-trim-right (substring string (length magic))))))
    (when msg
      (if (string-prefix-p "Please" msg)
	  (setq emacs-subreddit-rpc-sync-timeout 420) ;give user a chance
	(custom-reevaluate-setting 'emacs-subreddit-rpc-sync-timeout))
      (message "emacs-subreddit: %s" msg))))

(defun emacs-subreddit--dispose-output (base* proc add)
  "BASE* is a pointer to half-baked json from a previous iteration."
  (let ((json (concat (car (gv-deref base*)) add)))
    (condition-case err
	(with-temp-buffer
	  (save-excursion (insert json))
	  (when-let ((plst (prog1 (json-parse-buffer :object-type 'plist :null-object nil)
			     (setcar (gv-deref base*)
				     (buffer-substring (point) (point-max)))))
		     (id (plist-get plst :id))
		     (cb (prog1 (alist-get id (emacs-subreddit--rpc-callbacks proc))
			   (setf (emacs-subreddit--rpc-callbacks proc)
				 (assoc-delete-all id (emacs-subreddit--rpc-callbacks proc))))))
	    (if-let ((result (plist-member plst :result)))
		(funcall cb (cadr result))
	      (let ((err (plist-get plst :error)))
		(user-error "%s" (or (plist-get err :data) err))))))
      (user-error
       (setq emacs-subreddit-error err))
      ((json-end-of-file json-parse-error) ;assume half-baked
       (setcar (gv-deref base*) json))
      (error
       (gnus-message 5 "emacs-subreddit--dispose-output: %s"
		     (error-message-string err))
       (setcar (gv-deref base*) "")
       ;; all bets off
       (setf (emacs-subreddit--rpc-callbacks proc) nil)))))

(defun emacs-subreddit-elpa-dir ()
  (let ((elpa-dir (directory-file-name (file-name-directory
					(locate-library "emacs-subreddit")))))
    (if (equal "lisp" (file-name-nondirectory elpa-dir))
        (directory-file-name (file-name-directory elpa-dir))
      elpa-dir)))

(defun emacs-subreddit-rpc-get ()
  "Retrieve the PRAW process."
  (let ((praw-command (list (expand-file-name "bin/app" (emacs-subreddit-elpa-dir))
			    "--localhost" emacs-subreddit-localhost
			    "--log" (expand-file-name
				     "emacs-subreddit-rpc-log."
				     (file-name-as-directory
				      temporary-file-directory))))
	(proc-buffer (get-buffer-create " *subreddit*")))
    (or (get-buffer-process proc-buffer)
	(make-process :name "subreddit"
		      :buffer (with-current-buffer proc-buffer
				(special-mode)
				(let ((inhibit-read-only t))
				  (erase-buffer))
				(current-buffer))
		      :command praw-command
		      :connection-type 'pipe
		      :filter (emacs-subreddit--rpc-filter)
		      :noquery t
		      :sentinel #'emacs-subreddit-sentinel
		      :stderr (with-current-buffer (get-buffer-create
						    " *subreddit-stderr*")
				(special-mode)
				(let ((inhibit-read-only t))
				  (erase-buffer))
				(add-hook 'after-change-functions
					  #'emacs-subreddit--message-user
					  nil :local)
				(buffer-name)))
	(error "emacs-subreddit-rpc-get: cannot run %s"
	       (mapconcat #'identity praw-command " ")))))

(defsubst emacs-subreddit--base10 (base36)
  "Convert BASE36 reddit name encoding to a base10 integer."
  (apply #'+ (seq-map-indexed
              (lambda (elt idx)
                (* (expt 36 idx)
                   (if (>= elt ?a) (+ 10 (- elt ?a)) (- elt ?0))))
              (reverse base36))))

(defsubst emacs-subreddit--shift-ranges (delta ranges)
  "Shift back by DELTA the elements of RANGES, removing any negative entries."
  (cl-remove-if-not (lambda (e)
                      (cond ((numberp e) (> e 0))
                            (t (> (cdr e) 0))))
                    (mapcar (lambda (e)
                              (cond ((numberp e) (- e delta))
                                    (t `(,(max 1 (- (car e) delta)) .
                                         ,(- (cdr e) delta)))))
                            ranges)))

(defun emacs-subreddit-sort-by-number-of-articles-in-thread (t1 t2)
  "Whichever of the T1 or T2 has the most articles."
  (> (gnus-summary-number-of-articles-in-thread t1)
     (gnus-summary-number-of-articles-in-thread t2)))

(eval-and-compile
  (defconst emacs-subreddit-specials
    `(,@(if (get 'gnus-secondary-select-methods 'byte-obsolete-variable)
	 '((gnus-select-method '(emacs-subreddit "")) ;we didn't custom-set
	   (gnus-select-methods (list gnus-select-method)) ;we didn't custom-set
	   (gnus-newsrc-file (expand-file-name "newsrc.eld" (emacs-subreddit-elpa-dir)))
	   (gnus-dot-newsrc (expand-file-name "newsrc" (emacs-subreddit-elpa-dir)))
	   (gnus-background-get-unread-articles nil))
	 '((gnus-select-method '(emacs-subreddit ""))
	   (gnus-startup-file (expand-file-name "newsrc" (emacs-subreddit-elpa-dir)))
	   (gnus-secondary-select-methods nil)
	   (gnus-current-startup-file (gnus-make-newsrc-file gnus-startup-file))))
      (nntp-server-buffer nil)
      (debug-on-error t)
      (gnus-summary-line-format-spec nil)
      (gnus-summary-dummy-line-format-spec nil)
      (gnus-format-specs gnus-format-specs)
      (gnus-article-mode-line-format-spec nil)
      (gnus-summary-mode-line-format-spec nil)
      (gnus-summary-mark-positions nil)
      (gnus-verbose 4)
      (gnus-buffers nil)
      (gnus-updated-mode-lines nil)
      (gnus-init-file nil)
      (gnus-use-dribble-file nil)
      (gnus-newsrc-alist nil)
      (gnus-newsrc-hashtb nil)
      (gnus-active-hashtb (gnus-make-hashtable 4000))
      (gnus-directory (expand-file-name "News" (emacs-subreddit-elpa-dir)))
      (gnus-newsgroup-name emacs-subreddit-newsgroup-name)
      (gnus-newsgroup-marked nil)
      (gnus-newsgroup-spam-marked nil)
      (gnus-newsgroup-unreads nil)
      (gnus-current-headers nil)
      (gnus-newsgroup-data nil)
      (gnus-summary-buffer nil)
      (gnus-article-buffer nil)
      (gnus-original-article-buffer nil)
      (gnus-article-current nil)
      (gnus-current-article nil)
      (gnus-reffed-article-number nil)
      (gnus-current-score-file nil)
      (gnus-newsgroup-charset nil)
      (gnus-newsgroup-unreads nil)
      (gnus-newsgroup-unselected nil)
      (gnus-newsgroup-reads nil)
      (gnus-newsgroup-expunged-tally 0)
      (gnus-newsgroup-marked nil)
      (gnus-newsgroup-spam-marked nil)
      (gnus-newsgroup-killed nil)
      (gnus-newsgroup-cached nil)
      (gnus-newsgroup-saved nil)
      (gnus-newsgroup-kill-headers nil)
      (gnus-newsgroup-replied nil)
      (gnus-newsgroup-forwarded nil)
      (gnus-newsgroup-expirable nil)
      (gnus-newsgroup-processable nil)
      (gnus-newsgroup-downloadable nil)
      (gnus-newsgroup-unfetched nil)
      (gnus-newsgroup-undownloaded nil)
      (gnus-newsgroup-unsendable nil)
      (gnus-newsgroup-bookmarks nil)
      (gnus-newsgroup-dormant nil)
      (gnus-newsgroup-unseen nil)
      (gnus-newsgroup-seen nil)
      (gnus-newsgroup-unexist nil)
      (gnus-newsgroup-articles nil))))

(defmacro emacs-subreddit-with-temp-buffer (&rest body)
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(when-let ((,temp-buffer (generate-new-buffer " *temp*" t))
		(sum (get-buffer (gnus-summary-buffer-name emacs-subreddit-newsgroup-name))))
       (with-current-buffer ,temp-buffer
	 (dolist (var (mapcar #'car emacs-subreddit-specials))
	   (set (make-local-variable var)
		(buffer-local-value var sum)))
         (unwind-protect
	     (progn ,@body)
           (and (buffer-name ,temp-buffer)
                (kill-buffer ,temp-buffer)))))))

(defmacro emacs-subreddit-with-temp-file (file &rest body)
  (declare (indent 1) (debug t))
  (let ((temp-file (make-symbol "temp-file")))
    `(let ((,temp-file ,file))
       (emacs-subreddit-with-temp-buffer
	 (prog1 (progn ,@body)
	   (write-region nil nil ,temp-file nil 0))))))

(defmacro emacs-subreddit-let* (lets &rest body)
  (declare (indent 1) (debug t))
  `(let* (,@emacs-subreddit-specials
	  ,@lets)
     ,@body))

(defsubst emacs-subreddit--message-gate ()
  (equal emacs-subreddit-newsgroup-name (car-safe gnus-message-group-art)))

(defun emacs-subreddit--fix-from ()
  "Must fix the From header, always."
  (when (emacs-subreddit--message-gate)
    (save-excursion
      (message-replace-header
       "From"
       (emacs-subreddit--who-am-i)))))

(defsubst emacs-subreddit-hack-name-to-id (name)
  "Get x from t1_x (NAME)."
  (cl-subseq name 3))

(defvar emacs-subreddit--whoami nil)

(defun emacs-subreddit--who-am-i ()
  "Get login name from PRAW user_attr."
  (unless emacs-subreddit--whoami
    (emacs-subreddit-rpc-sync
     (lambda (_b s)
       (setq emacs-subreddit--whoami s))
     nil "user_attr" "name"))
  emacs-subreddit--whoami)

;;;###autoload
(defun emacs-subreddit ()
  (interactive)
  (if (gnus-alive-p)
      (message "Nice try")
    ;; eat shit
    (add-hook 'gnus-select-article-hook
	      (lambda ()
		(with-current-buffer gnus-article-buffer
		  (emacs-subreddit-bespokify))))
    (add-function :override (symbol-function 'gnus)
		  (lambda (&rest _args) (message "Nice try")))
    (add-hook 'gnus-summary-mode-hook #'emacs-subreddit-summary-map)
    (add-hook 'gnus-article-mode-hook #'emacs-subreddit-article-map)
    ;; Minor modes kill gnus-get-buffer-create's advice's work.
    (add-hook 'gnus-article-mode-hook #'emacs-subreddit-bespokify)
    (add-hook 'gnus-summary-mode-hook #'emacs-subreddit-bespokify)
    (add-function :after (symbol-function 'gnus-get-buffer-create)
		  (lambda (name)
		    (with-current-buffer name
		      (emacs-subreddit-bespokify))))
    (add-hook 'gnus-message-setup-hook #'emacs-subreddit--fix-from)
    ;; `gnus-news-group-p' requires valid method post-mail to return t
    (add-to-list 'gnus-valid-select-methods '("emacs-subreddit" post-mail) t)

    ;; the let'ing to nil of `gnus-summary-display-article-function'
    ;; in `gnus-summary-select-article' dates back to antiquity.
    (add-function
     :around (symbol-function 'gnus-summary-display-article)
     (lambda (f &rest args)
       (let ((gnus-summary-display-article-function
              (symbol-function 'emacs-reddit--display-article)))
	 (apply f args))))

    ;; Add prompting for replying to thread root to gnus-summary-followup.
    ;; The interactive spec of gnus-summary-followup is putatively preserved.
    (let* ((prompt-loose
            (lambda (f &rest args)
              (or (when-let
		      ((article-number (gnus-summary-article-number))
		       (header (emacs-subreddit--get-header article-number))
		       (root-name (car (emacs-subreddit-refs-for (plist-get header :name))))
		       (rootless (or (not (stringp root-name))
				     (not (string-prefix-p "t3_" root-name))
				     (not (emacs-subreddit-find-header
					   (emacs-subreddit-hack-name-to-id root-name)))))
		       (reply-root (read-char-choice
				    "Reply to [m]essage or [r]oot: " '(?m ?r)))
		       (q-root (eq reply-root ?r)))
		    (let* ((link-header (apply-partially #'message-add-header
							 "Reply-Root: yes"))
			   (add-link-header (apply-partially #'add-hook
							     'message-header-setup-hook
							     link-header))
			   (remove-link-header (apply-partially #'remove-hook
                                                                'message-header-setup-hook
                                                                link-header)))
                      (funcall add-link-header)
		      (unwind-protect
			  (apply f args)
			(funcall remove-link-header)))
		    t)
		  (apply f args))))
	   (advise-gnus-summary-followup
            (lambda ()
              (add-function :around (symbol-function 'gnus-summary-followup) prompt-loose)))
	   (suspend-prompt-loose
            (lambda (f &rest args)
              (remove-function (symbol-function 'gnus-summary-followup) prompt-loose)
              (unwind-protect
		  (apply f args)
                (funcall advise-gnus-summary-followup))))
	   (advise-gnus-summary-cancel-article
            (lambda ()
              (add-function :around (symbol-function 'gnus-summary-cancel-article)
                            suspend-prompt-loose))))
      (funcall advise-gnus-summary-cancel-article)
      (funcall advise-gnus-summary-followup))

    (add-function
     :around (symbol-function 'message-supersede)
     (lambda (f &rest args)
       (add-function :override
                     (symbol-function 'mml-insert-mml-markup)
                     #'ignore)
       (unwind-protect
	   (prog1 (apply f args)
             (remove-function (symbol-function 'mml-insert-mml-markup) #'ignore)
             (save-excursion
               (save-restriction
		 (emacs-subreddit--fix-from)
		 (message-goto-body)
		 (narrow-to-region (point) (point-max))
		 (goto-char (point-max))
		 (mm-inline-text-html nil)
		 (delete-region (point-min) (point)))))
	 (remove-function (symbol-function 'mml-insert-mml-markup) #'ignore))))

    (add-function
     :around (symbol-function 'message-send-news)
     (lambda (f &rest args)
       (let* ((dont-ask (lambda (prompt)
			  (when (cl-search "mpty article" prompt) t)))
              (link-p (message-fetch-field "Link"))
              (message-shoot-gnksa-feet (if link-p t message-shoot-gnksa-feet))
	      (message-inhibit-body-encoding t))
	 (unwind-protect
             (progn
               (when link-p
		 (add-function :before-until (symbol-function 'y-or-n-p) dont-ask))
               (apply f args))
	   (remove-function (symbol-function 'y-or-n-p) dont-ask)))))

    (add-function
     :around (symbol-function 'gnus-summary-post-news)
     (lambda (f &rest args)
       (let* ((post-type (read-char-choice "[l]ink / [t]ext: " '(?l ?t)))
              (link-header (apply-partially #'message-add-header "Link: https://"))
              (add-link-header (apply-partially #'add-hook
                                                'message-header-setup-hook
                                                link-header))
              (remove-link-header (apply-partially #'remove-hook
						   'message-header-setup-hook
						   link-header)))
	 (cl-case post-type
	   (?l (funcall add-link-header)))
	 (unwind-protect
             (apply f args)
	   (funcall remove-link-header)))))

    (add-function
     :filter-return (symbol-function 'message-make-fqdn)
     (lambda (val)
       (if (and (emacs-subreddit--message-gate)
		(cl-search "--so-tickle-me" val))
	   "reddit.com"
	 val)))

    (add-function
     :around (symbol-function 'message-is-yours-p)
     (lambda (f &rest args)
       (when (emacs-subreddit--message-gate)
	 (add-function :override
		       (symbol-function 'message-make-from)
		       #'emacs-subreddit--who-am-i))
       (unwind-protect
           (apply f args)
	 (remove-function (symbol-function 'message-make-from) #'emacs-subreddit--who-am-i))))

    (let ((gnus-newsgroup-name emacs-subreddit-newsgroup-name))
      (switch-to-buffer
       (or (when-let ((b (get-buffer (gnus-summary-buffer-name gnus-newsgroup-name))))
	     (if (cl-some (lambda (timer) (eq (timer--function timer)
					      #'emacs-subreddit--reflect-state))
			  timer-idle-list)
		 b
	       ;; remove carcass from earlier fail.
	       (prog1 nil (kill-buffer b))))
	   (progn (emacs-subreddit-summarize)
		  (or (get-buffer (gnus-summary-buffer-name gnus-newsgroup-name))
		      (current-buffer))))))))

(defmacro emacs-subreddit-assume-in-summary (&rest body)
  "If we are not in an summary buffer, go there, and execute BODY.  Restore."
  (declare (indent 0) (debug t))
  `(let ((bname (gnus-summary-buffer-name emacs-subreddit-newsgroup-name)))
     (if (get-buffer bname)
	 (with-current-buffer bname ,@body)
       (error "wtf"))))

(defun emacs-subreddit-prev-article ()
  "Gutted gnus-summary-prev-article."
  (interactive)
  (emacs-subreddit-next-article nil :backward))

(defun emacs-subreddit-next-article (unread backward)
  "Gutted gnus-summary-next-article."
  (interactive (list nil nil))
  (emacs-subreddit-assume-in-summary
    (when (and (gnus-summary-search-forward unread nil backward)
               (or (gnus-summary-display-article (gnus-summary-article-number))
		   (eq (gnus-summary-article-mark) gnus-canceled-mark)))
      (gnus-summary-position-point))))

(defun emacs-subreddit-redisplay ()
  (emacs-subreddit-assume-in-summary
    (gnus-activate-group gnus-newsgroup-name) ;ridonk but it moves seen's to unread's.
    (gnus-select-newsgroup gnus-newsgroup-name)
    (setq gnus-newsgroup-active (copy-tree (gnus-active gnus-newsgroup-name)))
    (if (string-match-p (regexp-quote "commercial") (emacs-version))
	(gnus-update-format-specifications 'summary 'summary-mode 'summary-dummy)
      (gnus-update-format-specifications nil 'summary 'summary-mode 'summary-dummy))
    (gnus-update-summary-mark-positions)
    (gnus-summary-prepare)
    (gnus-summary-maybe-hide-threads)
    ;; (gnus-summary-auto-select-subject)
    ;; Don't mark any articles as selected if we haven't done that.
    (setq overlay-arrow-position nil)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    (gnus-summary-limit-to-unread)
    (setq-local gnus-newsgroup-prepared t)
    (unless gnus-newsgroup-unreads
      ;; Crimson Business Editor Tagline
      (message "emacs-subreddit: No news is good news"))))

(defun emacs-subreddit-summarize ()
  "Only generate post selection.  Does nothing with remote."
  (cl-letf (((symbol-function 'gnus-read-active-file-p) (lambda (&rest _args) t))
	    ((symbol-function 'gnus-group-list-groups) #'ignore)
	    ((symbol-function 'gnus-group-update-group) #'ignore)
	    ((symbol-function 'gnus-summary-update-info) #'emacs-subreddit--reflect-state))
    (emacs-subreddit-let* ()
      ;; begin gutted gnus-setup-news
      (gnus-read-newsrc-file)
      ;; end gutted gnus-setup-news

      (nnheader-init-server-buffer)
      (gnus-subscribe-newsgroup gnus-newsgroup-name)

      (with-current-buffer (gnus-get-buffer-create
			    (gnus-summary-buffer-name gnus-newsgroup-name))
	;; begin gutted gnus-summary-setup-buffer
	(defvar gnus-summary-mode-group)
	(let ((gnus-summary-mode-group gnus-newsgroup-name))
	  (gnus-summary-mode))
	(setq-local gnus-summary-buffer (current-buffer))
	(gnus-summary-set-local-parameters gnus-newsgroup-name)
	;; end gutted gnus-summary-setup-buffer

	(add-hook 'kill-buffer-hook
		  (lambda ()
		    (kill-process (emacs-subreddit-rpc-get))
		    (cancel-function-timers #'emacs-subreddit--reflect-state)
		    (dolist (b gnus-buffers)
		      (unless (eq b (current-buffer))
			(let (kill-buffer-query-functions)
			  (kill-buffer b)))))
		  nil :local)
	(run-with-idle-timer 30 :repeat #'emacs-subreddit--reflect-state)

	;; begin gutted gnus-summary-read-group-1
	(emacs-subreddit-pull :sync)
	;; end gutted gnus-summary-read-group-1
	)
      (when(string-prefix-p "emacs-subreddit: Ahh" (current-message))
	(kill-buffer (gnus-summary-buffer-name gnus-newsgroup-name))))))

(defun emacs-subreddit-dismiss ()
  "Gutted gnus-summary-exit."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (emacs-subreddit--reflect-state)
    (when (buffer-live-p gnus-article-buffer)
      (with-current-buffer gnus-article-buffer
	(mm-destroy-parts gnus-article-mime-handles)
	(setq gnus-article-mime-handle-alist nil
	      gnus-article-mime-handles nil))
      (bury-buffer gnus-article-buffer))
    (gnus-configure-windows 'summary :collapse)
    (when (eq orig-buffer gnus-summary-buffer)
      (gnus-kill-buffer gnus-article-buffer)
      (bury-buffer))))

(defun emacs-subreddit--reflect-state ()
  "Gutted gnus-summary-update-info."
  (if-let ((b (get-buffer (gnus-summary-buffer-name emacs-subreddit-newsgroup-name))))
      (with-current-buffer b
	;; Gutted gnus-update-read-articles
	(let ((unread (gnus-sorted-union gnus-newsgroup-unreads
					 gnus-newsgroup-unselected))
	      (active (or gnus-newsgroup-active (gnus-active gnus-newsgroup-name)))
	      (info (gnus-get-info gnus-newsgroup-name))
	      (prev 1)
	      read)
	  (when (and info active)
	    ;; Remove any negative articles numbers.
	    (while (and unread (< (car unread) 0))
	      (setq unread (cdr unread)))
	    ;; Remove any expired article numbers
	    (while (and unread (< (car unread) (car active)))
	      (setq unread (cdr unread)))
	    ;; Compute the ranges of read articles by looking at the list of
	    ;; unread articles.
	    (while unread
	      (when (/= (car unread) prev)
		(push (if (= prev (1- (car unread))) prev
			(cons prev (1- (car unread))))
		      read))
	      (setq prev (1+ (car unread))
		    unread (cdr unread)))
	    (when (<= prev (cdr active))
	      (push (cons prev (cdr active)) read))
	    (setq read (if (> (length read) 1) (nreverse read) read))
	    ;; Enter this list into the group info.
	    (setf (gnus-info-read info) read)
	    ;; Set the number of unread articles in gnus-newsrc-hashtb.
	    (gnus-get-unread-articles-in-group info (gnus-active gnus-newsgroup-name))))
	(gnus-update-marks)

	;; Gutted gnus-save-newsrc-file
	(defvar gnus-current-startup-file)
	(emacs-subreddit-with-temp-file (if (boundp 'gnus-newsrc-file)
					    gnus-newsrc-file
					  (concat gnus-current-startup-file ".eld"))
	  (let ((coding-system-for-write gnus-ding-file-coding-system)
		(standard-output (current-buffer)))
	    (gnus-gnus-to-quick-newsrc-format))))
    (cancel-function-timers #'emacs-subreddit--reflect-state)))

(defsubst emacs-subreddit--earliest-among (indices lvp)
  "Return (list-to-iterate . next-earliest) from INDICES.
INDICES are thus far iterators.
LVP is a list of vectors of plists.
Used in the interleaving of submissions and comments."
  (let (earliest next-earliest)
    (dolist (plst-idx
             (cl-remove-if-not
	      #'car
              (seq-map-indexed
	       (lambda (plst idx) (cons plst idx))
	       (seq-mapn
		(lambda (v i)
                  (if (< i (length v)) (aref v i)))
		lvp indices)))
             (list (cdr earliest)
                   (when next-earliest
                     (plist-get (car next-earliest) :created_utc))))
      (cond ((null earliest)
             (setq earliest plst-idx))
            ((< (plist-get (car plst-idx) :created_utc)
                (plist-get (car earliest) :created_utc))
             (setq next-earliest earliest)
             (setq earliest plst-idx))
            ((null next-earliest)
             (setq next-earliest plst-idx))))))

(defun emacs-subreddit--sort-headers (&rest lvp)
  "Sort headers for LVP (list of vectors of plists)."
  (let ((indices (make-list (length lvp) 0))
        result)
    (while (not (equal indices (mapcar #'length lvp)))
      (cl-destructuring-bind (to-iterate bogey-created)
          (emacs-subreddit--earliest-among indices lvp)
        (cl-loop with arr = (elt lvp to-iterate)
                 for j in (number-sequence (elt indices to-iterate) (1- (length arr)))
                 for plst = (aref arr j)
                 for created = (plist-get plst :created_utc)
                 until (> created (or bogey-created most-positive-fixnum))
                 do (cl-incf (elt indices to-iterate))
                 do (push plst result))))
    (nreverse result)))

(defun emacs-subreddit-sort-append-headers (&rest lvp)
  "Append to headers the LVP (list of vector of plists)."
  (setq emacs-subreddit-headers
	(nconc emacs-subreddit-headers
               (apply #'emacs-subreddit--sort-headers lvp))))

(defun emacs-subreddit--filter-after (after-this vop)
  "Get elements created AFTER-THIS in VOP (vector of plists)."
  (cl-loop for elt-idx in (seq-map-indexed
                           (lambda (elt idx) (cons elt idx)) vop)
           until (>= (plist-get (car elt-idx) :created_utc) after-this)
           finally return (seq-drop vop (or (cdr elt-idx) 0))))

(defun emacs-subreddit--make-message-id (fullname)
  "Construct a valid Gnus message id from FULLNAME."
  (format "<%s@reddit.com>" fullname))

(defun emacs-subreddit--make-references (fullname)
  "Construct a space delimited string of message ancestors of FULLNAME."
  (mapconcat (lambda (ref) (emacs-subreddit--make-message-id ref))
             (emacs-subreddit-refs-for fullname) " "))

(defun emacs-subreddit--make-header (article-number)
  "Construct full headers of articled indexed ARTICLE-NUMBER."
  (let* ((header (emacs-subreddit--get-header article-number))
         (score (plist-get header :score))
         (num-comments (plist-get header :num_comments)))
    (make-full-mail-header
     article-number
     (or (plist-get header :title)
         (concat "Re: " (plist-get header :link_title)))
     (plist-get header :author)
     (format-time-string "%a, %d %h %Y %T %z (%Z)" (plist-get header :created_utc))
     (emacs-subreddit--make-message-id (plist-get header :name))
     (emacs-subreddit--make-references (plist-get header :name))
     0 0 nil
     (append `((X-Reddit-Name . ,(plist-get header :name)))
             `((X-Reddit-ID . ,(plist-get header :id)))
             (when-let ((it (plist-get header :permalink)))
               `((X-Reddit-Permalink . ,it)))
             (and (integerp score)
                  `((X-Reddit-Score . ,(number-to-string score))))
             (and (integerp num-comments)
                  `((X-Reddit-Comments . ,(number-to-string num-comments))))))))

(defun emacs-subreddit-retrieve-headers (article-numbers &rest _args)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (i article-numbers)
      (nnheader-insert-nov (emacs-subreddit--make-header i)))
    'nov))

(defsubst emacs-subreddit--citation-wrap (author body)
  "Cite AUTHOR using `gnus-message-cite-prefix-regexp' before displaying BODY.
Originally written by Paul Issartel."
  (with-temp-buffer
    (insert body)
    (mm-url-remove-markup)
    (mm-url-decode-entities)
    (fill-region (point-min) (point-max))
    (let* ((trimmed-1 (replace-regexp-in-string "\\(\\s-\\|\n\\)+$" "" (buffer-string)))
           (trimmed (replace-regexp-in-string "^\\(\\s-\\|\n\\)+" "" trimmed-1)))
      (concat author " wrote:<br>\n"
              "<pre>\n"
              (cl-subseq (replace-regexp-in-string "\n" "\n> " (concat "\n" trimmed)) 1)
              "\n</pre>\n\n"))))

(defun emacs-subreddit--parse-http (b)
  (with-current-buffer b
    (goto-char (point-min))
    (let* ((begin-data (save-excursion (re-search-forward "^\r?\n" nil t)))
	   (header (buffer-substring (point-min) (1- begin-data)))
	   (data (buffer-substring begin-data (point-max)))
	   (_  (string-match "Content-Type:\\s-*\\([[:graph:]]+\\)" header))
	   (content-type (match-string 1 header)))
      (cl-destructuring-bind (type _subtype) (split-string content-type "/")
	(cond ((equal type "image")
	       (format "<img src=\"data:%s;base64,%s\" />"
		       content-type
		       (base64-encode-string (encode-coding-string data 'binary) t)))
	      ((equal type "text") data)
	      (t (error "passing on %s" content-type)))))))

(defun emacs-subreddit-request-type (_group &optional _article)
  "A deffoo."
  'news)

(defun emacs-subreddit-server-opened (&optional _server)
  "A deffoo"
  t)

(defun emacs-subreddit-message (&optional _server)
  "A deffoo."
  "")

(defun emacs-subreddit-open-server (_server &optional _defs)
  "A deffoo."
  t)

(defun emacs-subreddit-close-group (_group &optional _server)
  "A deffoo."
  t)

(defun emacs-subreddit-request-article (article-number &optional _group _server buffer)
  "A deffoo."
  (with-current-buffer (or buffer nntp-server-buffer)
    (erase-buffer)
    (let* ((header (emacs-subreddit--get-header article-number))
           (mail-header (emacs-subreddit--make-header article-number))
           (score (cdr (assq 'X-Reddit-Score (mail-header-extra mail-header))))
           (permalink (cdr (assq 'X-Reddit-Permalink (mail-header-extra mail-header))))
           (body (when-let ((it (plist-get header :name)))
                   (emacs-subreddit--get-body it))))
      (when body
        (insert
         "Newsgroups: r/emacs\n"
         "Subject: " (mail-header-subject mail-header)  "\n"
         "From: " (or (mail-header-from mail-header) "nobody") "\n"
         "Date: " (mail-header-date mail-header) "\n"
         "Message-ID: " (mail-header-id mail-header) "\n"
         "References: " (mail-header-references mail-header) "\n"
         (if permalink
             (format "Archived-at: <https://www.reddit.com%s>\n"
                     permalink)
           "")
         "Score: " score "\n"
         "\n")
        (mml-insert-multipart "alternative")
        (mml-insert-tag 'part 'type "text/html"
                        'disposition "inline"
                        'charset "utf-8")
        (save-excursion (mml-insert-tag '/part))
        (when-let
            ((parent-name (plist-get header :parent_id)) ;; parent_id is full
             (parent-author (or (gethash parent-name emacs-subreddit-authors-hashtb)
                                "Someone"))
             (parent-body (emacs-subreddit--get-body parent-name)))
          (insert (emacs-subreddit--citation-wrap parent-author parent-body)))
        (if (and (eq (plist-get header :is_self) :json-false)
                 (plist-get header :url))
	    (condition-case err
		(let ((b (url-retrieve-synchronously (plist-get header :url))))
		  (unwind-protect
		      (let ((data (emacs-subreddit--parse-http b)))
			(if (> (length data) emacs-subreddit-max-render-bytes)
			  (insert (emacs-subreddit--br-tagify body))
			(insert data)))
		    (when (buffer-live-p b)
		      (kill-buffer b))))
	      (error (gnus-message 5 "emacs-subreddit-request-article: %s %s"
				   (plist-get header :url)
				   (error-message-string err))
		     (insert (emacs-subreddit--br-tagify body))))
          (insert (emacs-subreddit--br-tagify body)))
        (insert "\n")
        (if (mml-validate)
            (message-encode-message-body)
          (gnus-message 2 "emacs-subreddit-request-article: Invalid mml:\n%s"
                        (buffer-string)))
        (cons gnus-newsgroup-name article-number)))))

(defun emacs-subreddit-request-group (_group &rest _args)
  "A deffoo."
  (let* ((info (or (gnus-get-info gnus-newsgroup-name)
		   (list gnus-newsgroup-name
			 gnus-level-default-subscribed
			 nil nil
			 (gnus-method-simplify
			  (gnus-group-method gnus-newsgroup-name)))))
	 (params (gnus-info-params info))
	 (newsrc-read-ranges (gnus-info-read info))
	 (newsrc-mark-ranges (gnus-info-marks info))
	 (newsrc-seen-cons (gnus-group-parameter-value params 'last-seen t))
	 (newsrc-seen-index (car newsrc-seen-cons))
	 (newsrc-seen-id (cdr newsrc-seen-cons))
	 (num-headers (length emacs-subreddit-headers))
	 (status (format "211 %d %d %d %s" num-headers 1 num-headers gnus-newsgroup-name)))
    (nnheader-insert "%s\n" status)

    ;; remind myself how this works:
    ;; old-praw (1 - 20=emkdjrx)
    ;; read-ranges (1 - 10)                   (15 - 20)
    ;; unread-ranges       (11, 12, 13, 14)
    ;; new-praw    (12 13 14 15 16 17 18 19 20 - 100)
    ;; 20=emkdjrx in old-praw is 9=emkdjrx in new-praw.  index shift is 20-9=+11
    ;; new-unread-ranges   (0,  1,   2,  3)
    ;; new-read-ranges                        (4 - 9)

    ;; seen-indices are one-indexed !
    (let* ((newsrc-seen-index-now
            (if (or (not (stringp newsrc-seen-id))
                    (zerop (emacs-subreddit--base10 newsrc-seen-id)))
		1
              (cl-loop with cand
                       for plst in (reverse emacs-subreddit-headers)
                       for i = (length emacs-subreddit-headers) then (1- i)
                       if (= (emacs-subreddit--base10 (plist-get plst :id))
                             (emacs-subreddit--base10 newsrc-seen-id))
                       return i ;; do not go to finally
                       if (> (emacs-subreddit--base10 (plist-get plst :id))
                             (emacs-subreddit--base10 newsrc-seen-id))
		       do (setq cand i)
                       finally return (or cand 0))))
           (updated-seen-index (- num-headers
                                  (or (seq-position
                                       (reverse emacs-subreddit-headers) nil
                                       (lambda (plst _e)
                                         (not (plist-get plst :title))))
				      -1)))
           (updated-seen-id (when-let ((it (nth (1- updated-seen-index) emacs-subreddit-headers)))
                              (plist-get it :id)))
           (delta (if newsrc-seen-index
                      (max 0 (- newsrc-seen-index newsrc-seen-index-now))
                    0))
           (newsrc-read-ranges-shifted
            (emacs-subreddit--shift-ranges delta newsrc-read-ranges))
           (newsrc-mark-ranges-shifted
            (mapcar (lambda (what-ranges)
                      (cl-case (car what-ranges)
			(seen `(seen (1 . ,num-headers)))
			(t (cons (car what-ranges)
                                 (emacs-subreddit--shift-ranges delta (cdr what-ranges))))))
                    newsrc-mark-ranges)))
      (setf (gnus-info-read info) newsrc-read-ranges-shifted)
      (gnus-info-set-marks info newsrc-mark-ranges-shifted)
      (when updated-seen-id
	(while (assq 'last-seen params)
          (gnus-alist-pull 'last-seen params))
	(gnus-info-set-params
         info
         (cons `(last-seen ,updated-seen-index . ,updated-seen-id) params)
         t))
      (unless (listp (gnus-info-method info))
	(gnus-info-set-method info (gnus-group-method gnus-newsgroup-name) t))
      (gnus-set-info gnus-newsgroup-name info))))

(defun emacs-subreddit-toggle-read ()
  (interactive)
  (emacs-subreddit-assume-in-summary
    (if (or (text-property-any (point-min) (point-max)
			       'face 'gnus-summary-normal-read)
	    (text-property-any (point-min) (point-max)
			       'face 'gnus-summary-normal-ancient))
	(call-interactively #'gnus-summary-limit-to-unread)
      (call-interactively #'gnus-summary-insert-old-articles))))

(defun emacs-subreddit-toggle-collapse ()
  (interactive)
  (emacs-subreddit-assume-in-summary
    (if (seq-find (lambda (ov) (eq 'gnus-sum (overlay-get ov 'invisible)))
		  (overlays-in (point-min) (point-max)))
	(call-interactively #'gnus-summary-show-all-threads)
      (call-interactively #'gnus-summary-hide-all-threads))))

(defun emacs-subreddit-pull (&optional sync-p)
  "Request scan equivalent."
  (interactive (list nil))
  (emacs-subreddit--reflect-state)
  (let* (comments
	 (spin-stopper (spinner-start))
	 (timeout (run-with-timer 30 nil spin-stopper))
	 (cb_comments (lambda (_b comments*)
			(emacs-subreddit-assume-in-summary
			  (setq comments comments*))))
	 (cb_submissions
	  (lambda (_b submissions)
	    (emacs-subreddit-assume-in-summary
	      (cancel-timer timeout)
	      (funcall spin-stopper)
	      (unless (zerop (length comments))
		(setq submissions
		      (emacs-subreddit--filter-after
		       (- (plist-get (aref comments 0) :created_utc) 7200)
		       submissions)))
	      (seq-doseq (e comments) ;:parent_id is fullname
		(puthash (plist-get e :name) (plist-get e :parent_id)
			 emacs-subreddit-refs-hashtb))
	      (seq-doseq (e (vconcat submissions comments))
		(puthash (plist-get e :name) (plist-get e :author)
			 emacs-subreddit-authors-hashtb))
              (gnus-message 5 "emacs-subreddit-pull: +%s comments +%s submissions"
                            (length comments) (length submissions))
	      (emacs-subreddit-sort-append-headers submissions comments)
	      (emacs-subreddit-redisplay))))
	 (rpc (if sync-p #'emacs-subreddit-rpc-sync #'emacs-subreddit-rpc-async)))
    ;; output pipe is serial, so callback sequence is determined.
    (funcall rpc cb_comments nil "comments" "emacs")
    (funcall rpc cb_submissions nil "submissions" "emacs")))

(defsubst emacs-subreddit--dense-time (time*)
  "Convert TIME to a floating point number."
  (let ((time (time-convert time* 'list)))
    (+ (* (car time) 65536.0)
       (cadr time)
       (/ (or (car (cdr (cdr time))) 0) 1000000.0))))

(defun gnus-user-format-function-S (header)
  "Jay Wiggles."
  (condition-case nil
      (let* ((date (mail-header-date header))
	     (then (emacs-subreddit--dense-time
		    (apply #'encode-time (parse-time-string date))))
	     (now (emacs-subreddit--dense-time (current-time)))
	     (diff (- now then))
	     (str (cond ((>= diff (* 86400.0 7.0 52.0))
			 (if (>= diff (* 86400.0 7.0 52.0 10.0))
			     (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
			   (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
			((>= diff (* 86400.0 30.0))
			 (if (>= diff (* 86400.0 30.0 10.0))
			     (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
			   (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
			((>= diff (* 86400.0 7.0))
			 (if (>= diff (* 86400.0 7.0 10.0))
			     (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
			   (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
			((>= diff 86400.0)
			 (if (>= diff (* 86400.0 10.0))
			     (format "%3dd" (floor (/ diff 86400.0)))
			   (format "%3.1fd" (/ diff 86400.0))))
			((>= diff 3600.0)
			 (if (>= diff (* 3600.0 10.0))
			     (format "%3dh" (floor (/ diff 3600.0)))
			   (format "%3.1fh" (/ diff 3600.0))))
			((>= diff 60.0)
			 (if (>= diff (* 60.0 10.0))
			     (format "%3dm" (floor (/ diff 60.0)))
			   (format "%3.1fm" (/ diff 60.0))))
			(t
			 (format "%3ds" (floor diff)))))
	     (stripped
	      (replace-regexp-in-string "\\.0" "" str)))
        (concat (cond
                 ((= 2 (length stripped)) "  ")
                 ((= 3 (length stripped)) " ")
                 (t ""))
                stripped))
    ;; print some spaces and pretend nothing happened.
    (error "    ")))

(defsubst emacs-subreddit--current-article-number ()
  "`gnus-article-current' is a global variable that gets clobbered."
  (or (cdr gnus-message-group-art)
      (and (gnus-buffer-live-p gnus-summary-buffer)
           (with-current-buffer gnus-summary-buffer
             (cdr gnus-article-current)))))

(defsubst emacs-subreddit--current-group ()
  "`gnus-article-current' is a global variable that gets clobbered."
  (or (car gnus-message-group-art)
      (with-current-buffer gnus-summary-buffer
        (car gnus-article-current))))

(defun emacs-subreddit--br-tagify (body)
  "Reddit-html BODY shies away from <BR>.  Should it?"
  (replace-regexp-in-string "\n" "<br>" body))

(defun emacs-subreddit--get-body (name)
  "Get full text of submission or comment NAME."
  (let (result)
    (emacs-subreddit-rpc-sync (lambda (_b s) (setq result s))
			      nil "body" "emacs" name)
    result))

(defun emacs-subreddit--fallback-link ()
  "Cannot render submission."
  (when-let ((current-group (emacs-subreddit--current-group))
             (current-article (emacs-subreddit--current-article-number)))
    (let* ((header (emacs-subreddit--get-header current-article))
	   (name (plist-get header :name))
           (body (when name (emacs-subreddit--get-body name))))
      (with-current-buffer gnus-original-article-buffer
        (article-goto-body)
        (delete-region (point) (point-max))
        (when body
          (insert (emacs-subreddit--br-tagify body)))))))

(defun emacs-subreddit--display-article (article &optional all-headers _header)
  "In case of shr failures, dump original link."
  (condition-case err
      (gnus-article-prepare article all-headers)
    (error
     (gnus-message 7 "emacs-subreddit--display-article: '%s' (falling back...)"
                   (error-message-string err))
     (emacs-subreddit--fallback-link)
     (gnus-article-prepare article all-headers))))

(defun emacs-subreddit--browse-root (&rest _args)
  "What happens when I click on Subject."
  (when-let ((article-number (emacs-subreddit--current-article-number))
             (header (emacs-subreddit--get-header article-number))
             (permalink (plist-get header :permalink)))
    (cl-loop for name in (emacs-subreddit-refs-for (plist-get header :name))
             for header1 = (emacs-subreddit-find-header
                            (emacs-subreddit-hack-name-to-id name))
             for permalink1 = (plist-get header1 :permalink)
             until permalink1
             finally (browse-url (format "https://www.reddit.com%s"
                                         (or permalink1 permalink ""))))))

(defun emacs-subreddit--header-button-alist ()
  "Construct a buffer-local `gnus-header-button-alist' for emacs-subreddit."
  (let* ((result (copy-alist gnus-header-button-alist))
         (references-value (assoc-default "References" result
                                          (lambda (x y) (string-match-p y x))))
         (references-key (car (rassq references-value result))))
    (setq result (cl-delete "^Subject:" result :test (lambda (x y) (cl-search x (car y)))))
    (setq result (cl-delete references-key result :test (lambda (x y) (cl-search x (car y)))))
    (push (append '("^\\(Message-I[Dd]\\|^In-Reply-To\\):") references-value) result)
    (push '("^Subject:" ".+" 0 (>= gnus-button-browse-level 0)
            emacs-subreddit--browse-root 0)
          result)
    result))

(set 'gnus-parameters (assoc-delete-all "^emacs-subreddit$" gnus-parameters))
(add-to-list
 'gnus-parameters
 `("^emacs-subreddit"
   (gnus-refer-article-method 'current)
   (gnus-summary-make-false-root 'adopt)
   (gnus-cite-hide-absolute 5)
   (gnus-cite-hide-percentage 0)
   (gnus-cited-lines-visible '(2 . 2))
   (gnus-article-date-lapsed-new-header t)
   (gnus-article-update-date-headers nil)
   (gnus-novice-user nil)
   (gnus-sum-thread-tree-single-indent "  ")
   (gnus-treat-date-lapsed 'head)
   (gnus-signature-separator '("^-- $" "^-- *$" "^_____+$"))
   (gnus-read-active-file nil)
   (gnus-read-newsrc-file nil)
   (gnus-thread-ignore-subject nil)
   (gnus-treat-hide-citation-maybe t)
   (gnus-treat-strip-cr t)
   (gnus-treat-strip-leading-blank-lines t)
   (gnus-treat-strip-multiple-blank-lines t)
   (gnus-treat-strip-trailing-blank-lines t)
   (gnus-treat-unsplit-urls t)
   (gnus-tree-minimize-window nil)
   (gnus-auto-extend-newsgroup nil)
   (gnus-add-timestamp-to-message t)
   (gnus-summary-line-format "%3t%U%R%uS %I%(%*%-10,10f  %s%)\n")
   (gnus-thread-sort-functions (quote (emacs-subreddit-sort-by-number-of-articles-in-thread)))
   (gnus-subthread-sort-functions (quote (gnus-thread-sort-by-number)))
   (gnus-summary-display-article-function
    (function emacs-subreddit--display-article))
   (gnus-header-button-alist
    (quote ,(emacs-subreddit--header-button-alist)))
   (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:"))))

(defun emacs-subreddit-bespokify ()
  "Buffer localize shit-show of global variables."
  (when (get-buffer (gnus-summary-buffer-name emacs-subreddit-newsgroup-name))
    (eval `(setq-local
	    ,@(cl-mapcan
	       (lambda (pair) `(,(car pair)
				(emacs-subreddit-assume-in-summary
				 (symbol-value ',(car pair)))))
	       emacs-subreddit-specials))
	  :lexical)))

(defun emacs-subreddit-catchup-and-dismiss ()
  "Mark all unread articles in this group as read, then exit."
  (interactive)
  (when (gnus-summary-catchup nil nil nil 'fast)
    (emacs-subreddit-assume-in-summary
      (gnus-summary-limit-to-unread))
    (emacs-subreddit-dismiss)))

(defun emacs-subreddit-article-goto-next-page ()
  (interactive)
  (cl-letf (((symbol-function 'gnus-summary-jump-to-group) #'ignore))
    (gnus-article-goto-next-page)))

(defun emacs-subreddit-summary-prev-page (&optional _lines _move)
  (interactive "P" gnus-summary-mode)
  (when (gnus-summary-article-number)
    (cl-letf (((symbol-function 'gnus-summary-jump-to-group) #'ignore)
	      (gnus-summary-goto-unread 'never))
      (call-interactively #'gnus-summary-prev-page))))

(defun emacs-subreddit-summary-next-page (&optional lines circular stop)
  (interactive)
  (when (gnus-summary-article-number)
    (cl-letf (((symbol-function 'gnus-summary-jump-to-group) #'ignore))
      (let ((article (gnus-summary-article-number))
	    (article-window (get-buffer-window gnus-article-buffer t))
	    endp)
	;; If the buffer is empty, we have no article.
	(unless article
	  (error "No article to select"))
	(gnus-configure-windows 'article)
	(if (eq (cdr (assq article gnus-newsgroup-reads)) gnus-canceled-mark)
	    (emacs-subreddit-next-article nil nil)
	  (if (or (null gnus-current-article)
		  (null gnus-article-current)
		  (/= article (cdr gnus-article-current))
		  (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	      ;; Selected subject is different from current article's.
	      (gnus-summary-display-article article)
	    (when article-window
	      (gnus-eval-in-buffer-window gnus-article-buffer
		(setq endp (or (gnus-article-next-page lines)
			       (gnus-article-only-boring-p))))
	      (when endp
		(cond ((or stop gnus-summary-stop-at-end-of-message)
		       (gnus-message 3 "End of message"))
		      (circular
		       (gnus-summary-beginning-of-article))
		      ((or lines
			   (not gnus-paging-select-next))
		       (gnus-message 3 "End of message"))
		      ((null lines)
		       (emacs-subreddit-next-article nil nil)))))))
	(gnus-summary-recenter)
	(gnus-summary-position-point)))))

(defvar-keymap emacs-subreddit-article-map :suppress 'nodigits
  :parent button-buffer-map
  "SPC" #'emacs-subreddit-article-goto-next-page
  "S-SPC" #'gnus-article-goto-prev-page
  "h" #'gnus-article-show-summary
  "s" #'gnus-article-show-summary
  "<" #'beginning-of-buffer
  ">" #'end-of-buffer
  "r" #'emacs-subreddit-article-reply
  "n" #'emacs-subreddit-next-article
  "p" #'emacs-subreddit-prev-article
  "c" #'emacs-subreddit-catchup-and-dismiss
  "g" #'emacs-subreddit-pull
  "q" #'emacs-subreddit-dismiss
  "x" #'emacs-subreddit-toggle-read)

(defvar-keymap emacs-subreddit-summary-map :suppress 'nodigits
  "SPC" #'emacs-subreddit-summary-next-page
  "S-SPC" #'emacs-subreddit-summary-prev-page
  "RET" #'gnus-summary-scroll-up
  "M-RET" #'gnus-summary-scroll-down
  "C-k" #'gnus-summary-kill-same-subject
  "C-M-t" #'gnus-summary-toggle-threads
  "C-M-s" #'gnus-summary-show-thread
  "C-M-h" #'gnus-summary-hide-thread
  "C-M-f" #'gnus-summary-next-thread
  "C-M-b" #'gnus-summary-prev-thread
  "M-<down>" #'gnus-summary-next-thread
  "M-<up>" #'gnus-summary-prev-thread
  "C-w" #'gnus-summary-mark-region-as-read
  "C-t" #'toggle-truncate-lines
  "n" #'emacs-subreddit-next-article
  "p" #'emacs-subreddit-prev-article
  "c" #'emacs-subreddit-catchup-and-dismiss
  "g" #'emacs-subreddit-pull
  "r" #'emacs-subreddit-summary-reply
  "q" #'emacs-subreddit-dismiss
  "x" #'emacs-subreddit-toggle-read
  "s" #'gnus-summary-isearch-article

  "T" (define-keymap :prefix 'gnus-summary-thread-map
        "T" #'gnus-summary-toggle-threads
        "H" #'emacs-subreddit-toggle-collapse))

(defun emacs-subreddit-summary-map ()
  (use-local-map emacs-subreddit-summary-map))

(defun emacs-subreddit-article-map ()
  (use-local-map emacs-subreddit-article-map))

(defsubst emacs-subreddit--extract-name (from)
  "String match on something looking like t1_es076hd in FROM."
  (and (stringp from) (string-match "\\(t[0-9]+_[a-z0-9]+\\)" from) (match-string 1 from)))

;; C-c C-c from followup buffer
;; message-send-and-exit
;; message-send
;; message-send-method-alist=message-send-news-function=message-send-news
;; gnus-request-post
;; emacs-subreddit-request-post
(defun emacs-subreddit-request-post (&optional _server)
  "A deffoo."
  (let* ((ret t)
         (kwargs (make-hash-table))
         (title (or (message-fetch-field "Subject")
                    (error "`emacs-subreddit-request-post': no subject field")))
         (link (message-fetch-field "Link"))
         (reply-p (not (null message-reply-headers)))
         (edit-name (emacs-subreddit--extract-name (message-fetch-field "Supersedes")))
         (cancel-name (emacs-subreddit--extract-name (message-fetch-field "Control")))
         (root-p (message-fetch-field "Reply-Root"))
         (article-number (emacs-subreddit--current-article-number))
         (group (if (numberp article-number)
                    (gnus-group-real-name (emacs-subreddit--current-group))
                  (or (message-fetch-field "Newsgroups")
                      (error "emacs-subreddit-request-post: no newsgroups field"))))
         (header (when (numberp article-number)
                   (emacs-subreddit--get-header article-number)))
         (body
          (save-excursion
            (save-restriction
              (message-goto-body)
              (narrow-to-region (point) (point-max))
              (buffer-string)))))
    (cond (cancel-name (emacs-subreddit-rpc-sync #'ignore kwargs "remove" cancel-name))
          (edit-name (emacs-subreddit-rpc-sync #'ignore kwargs "edit" edit-name body))
          (reply-p (if (and header (plist-get header :name))
                       (emacs-subreddit-rpc-sync #'ignore kwargs "reply"
						 (plist-get header :name)
						 body (stringp root-p))
                     (backtrace)
                     (error "emacs-subreddit-request-post: no current article, header=%s name=%s"
                            header
                            (when header (plist-get header :name)))))
          (link (let* ((parsed-url (url-generic-parse-url link))
                       (host (url-host parsed-url)))
                  (if (and (stringp host) (not (zerop (length host))))
                      (progn
                        (puthash 'url link kwargs)
                        (emacs-subreddit-rpc-sync #'ignore kwargs "submit" group title))
                    ;; gnus-error might be better here
                    (error "emacs-subreddit-request-post: invalid url \"%s\"" link)
                    (setq ret nil))))
          (t (puthash 'selftext body kwargs)
             (emacs-subreddit-rpc-sync #'ignore kwargs "submit" group title)))
    ret))

(provide 'emacs-subreddit)
;;; emacs-subreddit.el ends here
