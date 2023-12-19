(require 'widget)
(require 'cl-lib)
(require 'json)
(require 'wid-edit)


(comment
  (symbol-file 'foxmacs-dashboard)
  (foxmacs-install-deps)
  (foxmacs-ensure-venv))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defclass foxmacs-dashbord-state ()
  ((port :initarg :port
         :initform 6000
         :type integer
         :documentation "The firefox remote debugger port.")
   (host :initarg :host
         :initform "localhost"
         :type string
         :documentation "The firefox remote debugger host.")
   (selected-frame :initarg :selected-frame
                   :initform nil
                   :documentation "The selected frame."
                   :accessor fomxacs-state-selected-frame)
   (python-process :initarg :python-process
                   :initform nil
                   :documentation "The python process running geckordp.")))

;; (with-current-buffer (get-buffer "*foxmacs*")
;;   (slot-value foxmacs--state 'port))

(cl-defmethod foxmacs-state-ensure-connection ((state foxmacs-dashbord-state))
  (when-let ((proc (slot-value state 'python-process)))
    (when (process-live-p proc)
      (foxmacs-process-stop)))
  (setf (slot-value state 'python-process) (foxmacs-process-start))

  (let ((host (slot-value state 'host))
        (port (slot-value state 'port)))
    (foxmacs-process-command `(:command "connect" :payload (:host ,host :port ,port)))))


;; (makunbound 'foxmacs--state)
;; (setq foxmacs--state (make-instance 'foxmacs-dashbord-state))
(defvar foxmacs--state nil "The state of the foxmacs dashboard.")


;; (makunbound 'foxmacs-dashboard-mode-map)
(defvar-keymap foxmacs-dashboard-mode-map
  :doc "Keymap for `foxmacs-dashboard-mode'."
  :parent widget-keymap
  "q" #'foxmacs-dashboard-quit
  "g" 'foxmacs-dashboard-render)

;; (define-key foxmacs-dashboard-mode-map (kbd "g") 'foxmacs-dashboard-render)

;; (define-minor-mode foxmacs-dashboard-minor-mode
;;   "Minor mode for org-ai-on-project."
;;   :lighter " foxmacs-dashboard"
;;   :keymap foxmacs-dashboard-mode-map
;;   :group 'foxmacs)

;; (add-hook 'foxmacs-dashboard-mode-hook
;;           (lambda () (setq-local show-trailing-whitespace nil)))



(define-derived-mode foxmacs-dashboard-mode nil "foxmacs-dashboard"
  "Major mode for the foxmacs dashboard.
\\{foxmacs-dashboard-mode-map}"
  ;; Here you can run mode setup code, like keybindings.
  ;; (define-key foxmacs-dashboard-mode-map (kbd "q") 'quit-window)
  ;; (local-set-key (kbd "g") #'rk/rust-dependencies-refresh)
  ;; ... other keybindings
  ;; (widget-minor-mode 1)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local show-trailing-whitespace nil)
  ;; (read-only-mode 1)
  (print "foxmacs enabled"))

(defmacro foxmacs-dashboard--with-buffer (&rest body)
  "Ensure/setup `org-ai-on-project--buffer-name` and execute BODY."
  `(progn
     ;; (kill-buffer (get-buffer-create "*foxmacs*"))
     (switch-to-buffer (get-buffer-create "*foxmacs*"))
     ;; (kill-all-local-variables)
     (with-current-buffer "*foxmacs*"
       (foxmacs-dashboard-mode)
       ,@body
       (widget-setup)
       ;; (widget-minor-mode 1)
       ;;(use-local-map widget-keymap)
       )))

(defvar-local foxmacs-dashboard--eval-js-input-widget nil)
(defvar-local foxmacs-dashboard--eval-js-result-widget nil)

(defvar-local foxmacs-dashboard--eval-html-input-widget nil)
(defvar-local foxmacs-dashboard--eval-html-result-widget nil)

;; (with-current-buffer "*foxmacs*"
;;     (widget-value foxmacs-dashboard--eval-js-input-widget))

(defun foxmacs-dashboard-eval-js ()
  ""
  (interactive)
  (with-current-buffer "*foxmacs*"
    (let* ((code (widget-value foxmacs-dashboard--eval-js-input-widget))
           (result (foxmacs-process-command `(:command "eval-js" :payload (:code ,code :frame ,(foxmacs-state-selected-frame foxmacs--state))))))
      (save-excursion
        (widget-value-set
         foxmacs-dashboard--eval-js-result-widget
         (let ((json-encoding-pretty-print t))
           (json-encode (gethash "result" result))))))))

(defun foxmacs-dashboard-render ()
  ""
  (interactive)
  (let (last-js-input
        last-html-input)
    (when-let ((buf (get-buffer "*foxmacs*")))
      (with-current-buffer buf
        (setq last-js-input (widget-value foxmacs-dashboard--eval-js-input-widget))
        (setq last-html-input (widget-value foxmacs-dashboard--eval-html-input-widget))
        (kill-all-local-variables)
        (remove-overlays)))
   (foxmacs-dashboard--with-buffer
    (unless foxmacs--state
      (setq foxmacs--state (make-instance 'foxmacs-dashbord-state)))

    ;; (message ".......%s" foxmacs--state)
    (foxmacs-state-ensure-connection foxmacs--state)
    (let ((pos (point)))
      (let ((inhibit-read-only t))
        (erase-buffer))

      (let* ((tab-info (foxmacs-process-command '(:command "tab-info")))
             (title (gethash "title" tab-info))
             (url (gethash "url" tab-info)))
        (widget-insert "Title: ")
        (widget-insert title)
        (widget-insert "\n")
        (widget-insert "URL: ")
        (widget-insert url))

      (widget-insert "\n\n")

      (let ((frames (gethash "frames" (foxmacs-process-command '(:command "list-frames")))))



        (print frames)
        (widget-insert "Frames:\n")

        (apply #'widget-create
               'radio-button-choice
               :value "One"
               :notify (lambda (wid &rest ignore)
                         (setf (foxmacs-state-selected-frame foxmacs--state) (widget-value wid)))

               '(item :tag "Default" :value nil)

               (cl-loop for frame across frames
                        collect (let ((id (gethash "id" frame))
                                      (url (gethash "url" frame))
                                      (title (gethash "title" frame))
                                      (toplevel (gethash "isTopLevel" frame)))
                                  `(item :tag ,(format "%s: url=%s title=%s toplevel=%s\n" id url title toplevel)
                                         :value ,id)))))

      (widget-insert "\n\n")

      (widget-insert "JavaScript:\n")

      (let ((eval-js-keymap
             (let ((map (copy-keymap widget-text-keymap)))
               ;; Since the widget code uses a `field' property to identify fields,
               ;; ordinary beginning-of-line does the right thing.
               ;;  (define-key map "\C-a" 'widget-beginning-of-line)
               (define-key map (kbd "C-c C-c") 'foxmacs-dashboard-eval-js)
               map)))
        (setq-local foxmacs-dashboard--eval-js-input-widget
                    (widget-create 'text
                                   :format "%v"
                                   :notify (lambda (widget &rest _ignore)
                                             (message "got text %s" (widget-value widget)))
                                   :keymap eval-js-keymap
                                   (or last-js-input "// eval javascript\n\n"))))

      (widget-insert "\n\n")

      (setq-local foxmacs-dashboard--eval-js-result-widget
                  (widget-create 'text :format "%v" ""))

      (widget-insert "\n\n")

      (widget-insert "DOM:\n")

      (let ((eval-js-keymap
             (let ((map (copy-keymap widget-text-keymap)))
               ;; Since the widget code uses a `field' property to identify fields,
               ;; ordinary beginning-of-line does the right thing.
               ;;  (define-key map "\C-a" 'widget-beginning-of-line)
               (define-key map (kbd "C-c C-c") 'foxmacs-dashboard-eval-html)
               map)))
        (setq-local foxmacs-dashboard--eval-html-input-widget
                    (widget-create 'text
                                   :format "%v"
                                   :notify (lambda (widget &rest _ignore)
                                             (message "got text %s" (widget-value widget)))
                                   :keymap eval-js-keymap
                                   (or last-html-input "# xpath or CSS selector\n"))))


      (widget-insert "\n\n")

      ;; (widget-create 'push-button
      ;;                :notify (lambda (&rest ignore)
      ;;                          (message "reload???")
      ;;                          ;;(foxmacs-process-command '(:command "reload"))
      ;;                          )
      ;;                ;; :tag "Reload"
      ;;                :format "%[%t%]"
      ;;                "testing")

      (goto-char pos)))))



(defun foxmacs-dashboard ()
  ""
  (interactive)
  (foxmacs-dashboard-render))

(defun foxmacs-dashboard-quit ()
  ""
  (interactive)
  (foxmacs-process-stop)
  (quit-window))

(comment
  (foxmacs-dashboard)
)


(provide 'foxmacs-dashboard)
