(require 'js2-mode)

(require 'foxmacs-selenium)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define-minor-mode foxmacs-js-mode
  "JS mode for foxmacs"
  :lighter " JS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-b") 'foxmacs-js-send-buffer)
            (define-key map (kbd "C-c C-c") 'foxmacs-js-send-defun-or-region)
            (define-key map (kbd "C-x C-e") 'foxmacs-js-send-last-sexp)
            ;; (define-key foxmacs-js-mode-map (kbd "C-x C-e") 'foxmacs-js-send-last-sexp)
            ;; (define-key foxmacs-js-mode-map (kbd "C-c C-c") 'foxmacs-js-send-defun-or-region)
            ;; (define-key foxmacs-js-mode-map (kbd "C-c C-b") 'foxmacs-js-send-buffer)
            map))

(defun foxmacs-js-send-defun-or-region (arg)
  (interactive "P")
  (if (use-region-p)
      (foxmacs-js-send-region (region-beginning) (region-end) arg)

    (when-let* ((node (or (js2-mode-function-at-point)
                          (js2-node-find-child-before (point) (js2-node-at-point))
                          (js2-node-at-point)))
                (start (js2-node-abs-pos node))
                (end (js2-node-abs-end node)))
      (foxmacs-js-send-region start end arg))))

(defun foxmacs-js-send-buffer (arg)
  (interactive "P")
  (foxmacs-js-send-region (point-min) (point-max) arg))

(defun foxmacs-js-send-last-sexp (arg)
  (interactive "P")
  (if (eq major-mode 'js2-mode)
      (when-let* ((last-node (js2-node-find-child-before (point) (js2-node-at-point)))
                  (start (js2-node-abs-pos last-node))
                  (end (js2-node-abs-end last-node)))
        (foxmacs-js-send-region start end arg))
    (foxmacs-js-send-region (line-beginning-position) (line-end-position) arg)))

(defun foxmacs-js-send-region (start end &optional insert)
  (let* ((code (buffer-substring start end))
         (result (foxmacs-eval-result-result (foxmacs-eval-js code))))
    (flash-region start end nil 1)
    (if insert
        (let ((lines (split-string result "\n")))
          (goto-char end)
          (insert "\n\n")
          (dolist (line lines)
            (insert (format "// %s\n" line))))
      (let ((overlay (make-overlay end end)))
        (overlay-put overlay 'before-string (format " => %s" result))
        (overlay-put overlay 'face '(:weight bold))
        (run-with-timer 1 nil 'delete-overlay overlay)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

  (with-current-buffer "scratch.js"
    ;; (js2-node-string (js2-node-at-point))
    ;; (js2-node-string (js2-node-find-child-before (point) (js2-parse)))
    ;; (js2-node-string (js2-node-find-child-before (point) (js2-node-at-point)))
    ;; (js2-node-end (js2-node-find-child-before (point) (js2-node-at-point)))
    ;; (js2-node-string
    ;;  (js2-node-last-child (js2-parse)))
    (let ((node (or (js2-mode-function-at-point)
                    (js2-node-find-child-before (point) (js2-node-at-point))
                    (js2-node-at-point))))
      (js2-node-string node))
    )

)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'foxmacs-js)
