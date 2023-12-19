(require 'widget)
(require 'cl-lib)
(require 'json)
(require 'wid-edit)

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
  ;; (when-let ((proc (slot-value state 'python-process)))
  ;;   (when (process-live-p proc)
  ;;     (foxmacs-process-stop)))
  (unless (and (slot-value state 'python-process)
               (process-live-p (slot-value state 'python-process)))
    (setf (slot-value state 'python-process) (foxmacs-process-start))
    (let ((host (slot-value state 'host))
          (port (slot-value state 'port)))
      (foxmacs-process-connect))))


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
  (toggle-truncate-lines 1)
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
           (result (foxmacs-eval-js code)))
      (save-excursion
        (widget-value-set
         foxmacs-dashboard--eval-js-result-widget
         (foxmacs-eval-result-result result))))))

(defun foxmacs-dashboard-render ()
  ""
  (interactive)
  (let (last-js-input
        last-html-input)
    (when-let ((buf (get-buffer "*foxmacs*")))
      (with-current-buffer buf
        (when foxmacs-dashboard--eval-js-input-widget
          (setq last-js-input (widget-value foxmacs-dashboard--eval-js-input-widget)))
        (when foxmacs-dashboard--eval-html-input-widget
         (setq last-html-input (widget-value foxmacs-dashboard--eval-html-input-widget)))
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

      ;; (with-slots (title url visible) (car (foxmacs-tab-infos))
      ;;   title)

      (lexical-let ((tabs (foxmacs-tab-infos)))
        (apply #'widget-create
              'radio-button-choice
              :value (cl-loop for info in tabs
                              when (foxmacs-tab-info-visible info)
                              return (foxmacs-tab-info-handle info))
              :notify (lambda (wid &rest ignore)
                        (let ((tab (cl-find-if (lambda (info)
                                                 (string= (foxmacs-tab-info-handle info) (widget-value wid)))
                                               tabs)))
                          (foxmacs-tab-info-activate tab)
                        ;;(setf (foxmacs-state-selected-frame foxmacs--state) (widget-value wid))
                        ))

              (cl-loop for info in (foxmacs-tab-infos)
                       collect (with-slots (title url visible handle) info
                                 `(item :tag ,(format "%s \"%s\"" url title)
                                        :value ,handle)))))

      (widget-insert "\n\n")

      (let ((frames (foxmacs-frames)))
        (widget-insert "Frames:\n")

        (apply #'widget-create
               'radio-button-choice
               :value (slot-value foxmacs--state 'selected-frame)
               :notify (lambda (wid &rest ignore)
                         (setf (foxmacs-state-selected-frame foxmacs--state) (widget-value wid)))

               '(item :tag "Default" :value nil)

               (cl-loop for frame in frames
                        collect (with-slots (id src) frame
                                  `(item :tag ,(format "id=%s src=%s\n" id src)
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
                                   (or last-js-input "// eval javascript\n"))))

      (widget-insert "\n")

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
  (foxmacs-process-reset)
  (when foxmacs--state
    (setf (slot-value foxmacs--state 'selected-frame) nil))
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
