(require 'cl-lib)
(require 'json)
(require 'uuidgen)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; foxmacs venv

(defcustom foxmacs-venv-dir (expand-file-name ".foxmacs/venv" user-emacs-directory)
  "The directory where the foxmacs virtualenv is located."
  :type 'string
  :group 'foxmacs)

(defun foxmacs-ensure-venv ()
  (unless (file-exists-p foxmacs-venv-dir)
    (make-directory foxmacs-venv-dir t)
    (let ((default-directory foxmacs-venv-dir))
      (shell-command "python -m venv ."))
    (foxmacs-install-deps)))

(defun foxmacs-python-executable ()
  (expand-file-name "bin/python" foxmacs-venv-dir))

(defun foxmacs-python-file ()
  (let ((cur-dir (file-name-directory (symbol-file 'foxmacs-dashboard))))
    (expand-file-name "foxmacs.py" cur-dir)))

(defun foxmacs-install-deps ()
  (foxmacs-ensure-venv)
  (let ((default-directory foxmacs-venv-dir))
    (shell-command "bin/pip install geckordp pydantic")))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; foxmacs process

(defvar foxmacs-process nil "Process handle for the python foxmacs process.")

(defun foxmacs-process-start ()
  "Starts the subprocess and sets up the communication."
  (when-let ((buf (get-buffer-create "*foxmacs-process*")))
    (with-current-buffer buf
      (erase-buffer)))

  (setq foxmacs-process
        (make-process
         :name "foxmacs-process"    ; A name for the process (for identification).
         :buffer "*foxmacs-process*" ; The buffer to which stdout and stderr will be redirected.
         :command `(,(foxmacs-python-executable)
                    ,(foxmacs-python-file))
         :sentinel 'foxmacs-process-sentinel ; Function to call on status change.
         :filter 'foxmacs-process-filter)))    ; Function to process output from stdout.

(defun foxmacs-process-sentinel (process event)
  "This function is called when the process status changes."
  (message "Process: %s had the event: %s" process event))

(defvar foxmacs-process-output-hook nil
  "Hook that is called when the process outputs something.")

(defun foxmacs-process-filter (process output)
  "This function receives the output of the process."
  (let ((output-buffer (process-buffer process)))
    (when (buffer-live-p output-buffer)
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert (format "%s\n" output))
        (let* ((payload (condition-case err
                           (let ((json-object-type 'hash-table))
                            (json-read-from-string output))
                         (error
                          (error "unable to parse json:\n%s" output))))
               (error (gethash 'error payload))
               (stacktrace (gethash 'stacktrace payload)))
          (setq last-output payload)
          (run-hook-with-args 'foxmacs-process-output-hook payload)
          (when error
            (insert (format "foxmacs error: %s\n%s" error stacktrace))
            (error "foxmacs error: %s\n%s" error stacktrace)))))))

(defun foxmacs-process-send-input (input)
  "Sends INPUT to the process and flushes its input."
  (when (and foxmacs-process (process-live-p foxmacs-process))
    (process-send-string foxmacs-process (concat input "\n"))))

(defun foxmacs-process-stop ()
  "Stops the process if it is running."
  (when (and foxmacs-process (process-live-p foxmacs-process))
    (delete-process foxmacs-process)))

(defvar foxmacs-process-command-queue nil "Queue of commands to be sent to the foxmacs process.")
(defvar foxmacs-process-command-in-progress nil "The command that is currently being processed.")

(defun foxmacs-process-command (cmd)
  ""
  (unless (plist-get cmd :id)
    (setq cmd (plist-put cmd :id (uuidgen-4))))

  (if foxmacs-process-command-in-progress
      (push cmd foxmacs-process-command-queue)

    (let ((id (plist-get cmd :id)))
      ;; (print (format "sending command: %s" cmd))
      (setq foxmacs-process-command-in-progress cmd)
      (lexical-let* (result)
        (add-hook 'foxmacs-process-output-hook (lambda (payload)
                                                 (remove-hook 'foxmacs-process-output-hook (car foxmacs-process-output-hook))
                                                 (setq result payload)
                                                 (setq foxmacs-process-command-in-progress nil)
                                                 (when foxmacs-process-command-queue
                                                   (foxmacs-process-command (pop foxmacs-process-command-queue)))))
        (foxmacs-process-send-input (json-encode cmd))
        (while (or (not result)
                   (not (string-equal id (gethash "id" result))))
          (accept-process-output foxmacs-process 0.1))
        result))))

(comment
  (json-read-from-string "{\"status\": \"ok\"}")

  (switch-to-buffer "*foxmacs-process*")
  (foxmacs-process-send-input "hello")

  (progn (foxmacs-process-stop) (foxmacs-process-start))

  (foxmacs-process-send-input (json-encode '(:command "connect" :payload (:host "localhost" :port 6000))))
  (foxmacs-process-send-input (json-encode '(:command "tab-info")))
  (foxmacs-process-send-input (json-encode '(:command "list-frames")))
  (foxmacs-process-send-input (json-encode '(:command "reload")))

  (foxmacs-process-send-input (json-encode '(:command "exit")))

  (setq foxmacs-process-command-in-progress nil)
  (setq foxmacs-process-command-queue nil)
  foxmacs-process-output-hook

  (foxmacs-process-command '(:command "tab-info"))
  (foxmacs-process-command '(:command "list-frames"))
  (foxmacs-process-command '(:command "eval-js" :payload (:code "window.location.href")))
  (foxmacs-process-command '(:command "eval-js" :payload (:code "var x = 1 + 2")))
  (foxmacs-process-command '(:command "eval-js" :payload (:code "x")))

  (pfuture-new)

  (hash-table-keys last-output)
  (map 'list 'identity (gethash "frames" last-output))
  (mapcar (lambda (x) (gethash "url" x)) (gethash "frames" last-output))
)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'foxmacs-browser)
