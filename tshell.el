
(defvar tshell-buffer "*tshell*")
(defvar tshell-out-buffer "*tshell-out*")

(defvar tshell-shell-prompt "$ ")
(defvar tshell-elisp-prompt "> ")
(defvar tshell-current-prompt tshell-shell-prompt)

(defvar tshell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'tshell-dispatch)
    (define-key map (kbd "C-c SPC") #'tshell-command)
    (define-key map (kbd "C-c @") #'tshell-command-region)
    (define-key map (kbd "RET") #'tshell-eval-input)
    (define-key map (kbd "C-M-x") #'tshell-eval-command)
    map))

(define-derived-mode tshell-mode fundamental-mode "Tshell"
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{tshell-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (setq-local tshell-mode t))

(defun tshell ()
  (interactive)
  ;; Creat out buffer
  (get-buffer-create tshell-out-buffer)
  (let ((buffer (switch-to-buffer (get-buffer-create tshell-buffer))))
    (with-current-buffer tshell-buffer
      (tshell-mode)
      (when (bobp)
        (insert "# Welcome to *tshell*\n")
        (insert "# Type `C-c C-c' to activate transient\n"))
      (insert "\n")
      (insert tshell-current-prompt))))


;;; Public stuff

(defun tshell-eval-input ()
  "Either eval current input line."
  (interactive)
  ;; TODO: only eval if line start with a prompt?
  (if (not (eobp))
      (tshell-eval-command)
    (tshell-eval-command)
    (insert "\n")
    (insert tshell-current-prompt)))

(defun tshell-eval-command ()
  "Evaluate current command (right now command means line)."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (cond
     ((string-prefix-p tshell-shell-prompt line)
      (when (eobp)
        (setq tshell-current-prompt tshell-shell-prompt))
      (tshell-shell-eval (string-remove-prefix tshell-shell-prompt line)))
     ((string-prefix-p tshell-elisp-prompt line)
      (when (eobp)
        (setq tshell-current-prompt tshell-elisp-prompt))
      (tshell-elisp-eval (string-remove-prefix tshell-elisp-prompt line))))))

(defun tshell-shell-eval (line)
  "Evaluate LINE in the shell mode."
  ;; Some elementary preprocessing.
  (setq line (string-trim-right line))
  (cond
   ((string-prefix-p "cd " line)
    (tshell-out-insert (string-remove-prefix "cd " line))
    (cd (expand-file-name (string-remove-prefix "cd " line))))
   ((string-prefix-p "> " line)
    (tshell-shell-kill)
    (with-current-buffer tshell-out-buffer
      (shell-command-on-region (point-min)
                               (point-max)
                               (string-remove-prefix "> " line)
                               tshell-out-buffer)))
   (t
    (tshell-shell-kill)
    (async-shell-command line tshell-out-buffer))))

(defun tshell-elisp-eval (line)
  "Evaluate LINE in the elisp mode."
  (with-current-buffer tshell-out-buffer
    (erase-buffer)
    (insert (pp-to-string (eval (car (read-from-string line)))))))

(defun tshell-out-insert (str)
  "Insert STR into `tshell-out-buffer'."
  (with-current-buffer tshell-out-buffer
    (insert str)))

(defun tshell-shell-kill ()
  "Kill out buffer process if it's running."
  (if (process-live-p (get-buffer-process tshell-out-buffer))
      (when (yes-or-no-p "A command is running. Kill it?")
        (kill-process (get-buffer-process tshell-out-buffer)))))


;;; Private stuff


;;; Transient interface

(transient-define-prefix tshell-dispatch ()
  "Invoke a tshell command from a list of available commands."
  ["Transient and dwim commands"
   [("l" "ls" (lambda () (interactive) (async-shell-command "ls" tshell-out-buffer)))
    ("x" "xargs" (lambda () (interactive) (tshell-command-region "xargs ")))
    ("SPC" "run" (lambda () (interactive) (tshell-command)))
    ("C-SPC" "run-region" (lambda () (interactive) (tshell-command-region)))]])

(defvar tshell--command-history nil)

(defun tshell-command (&optional initial-content)
  (interactive)
  (let ((cmd (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Tshell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                            "Tshell command: ")
                        initial-content nil
			(let ((filename
			       (cond
				(buffer-file-name)
				((eq major-mode 'dired-mode)
				 (dired-get-filename nil t)))))
			  (and filename (file-relative-name filename))))))
    (when cmd
      (async-shell-command cmd tshell-out-buffer))))

(defun tshell-command-region (&optional initial-content)
  (interactive)
  (let ((cmd (read-shell-command (if shell-command-prompt-show-cwd
                                     (format-message "Tshell on region command in `%s': "
                                                     (abbreviate-file-name
                                                      default-directory))
                                   "Tshell command on region: ")
                                 initial-content nil
			         (let ((filename
			                (cond
				         (buffer-file-name)
				         ((eq major-mode 'dired-mode)
				          (dired-get-filename nil t)))))
			           (and filename (file-relative-name filename))))))
    (when cmd
      (with-current-buffer tshell-out-buffer
        (shell-command-on-region (if (region-active-p)
                                     (region-beginning)
                                   (point-min))
                                 (if (region-active-p)
                                     (region-end)
                                   (point-max))
                                 cmd
                                 (current-buffer))))))
