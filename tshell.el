;; -*- lexical-binding: t -*-

(defvar tshell-buffer "*tshell*")
(defvar tshell-out-buffer "*tshell-out*")

(defvar tshell-shell-prompt "$ ")
(defvar tshell-elisp-prompt "> ")
(defvar tshell-current-prompt tshell-shell-prompt)

(defvar * nil "Most recent value evaluated in Tshell.")

(defvar tshell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'tshell-dispatch)
    (define-key map (kbd "C-c SPC") #'tshell-command)
    (define-key map (kbd "C-c @") #'tshell-command-region)
    (define-key map (kbd "C-c C-d") #'tshell-command-cd)
    (define-key map (kbd "C-c C-l") #'tshell-command-ls)
    (define-key map (kbd "C-c C-y") #'tshell-yank)
    (define-key map (kbd "RET") #'tshell-eval-input)
    (define-key map (kbd "C-M-x") #'tshell-eval-command)
    map))

(defvar tshell-font-lock-keywords '(("^$ " . font-lock-function-name-face)
                                    ("^> " . font-lock-variable-name-face)
                                    ("^[$>:] \\<\\(\\w+\\)\\>" . (1 font-lock-type-face))))

(define-derived-mode tshell-mode fundamental-mode "Tshell"
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{tshell-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (setq-local tshell-mode t)
  (setq-local font-lock-defaults '(tshell-font-lock-keywords))
  (setq-local * nil)
  (setq header-line-format '(:eval (format "%s %s"
                                           (propertize
                                            (directory-file-name (abbreviate-file-name default-directory))
                                            'face 'font-lock-variable-name-face)
                                           tshell-current-prompt)))
  (add-hook 'completion-at-point-functions #'tshell-completion-at-point nil t))

(defun tshell ()
  (interactive)
  ;; Create shell and out buffers first.
  (let ((buffer (get-buffer-create tshell-buffer))
        (out-buffer (get-buffer-create tshell-out-buffer)))
    (pop-to-buffer buffer)
    (unless (get-buffer-window out-buffer)
      (split-window-below 16)
      (switch-to-buffer-other-window out-buffer)
      (select-window (get-buffer-window buffer)))
    (with-current-buffer tshell-buffer
      (unless (and (boundp 'tshell-mode) tshell-mode)
        (tshell-mode)
        (insert "# Welcome to *tshell*\n")
        (insert "# Type `C-c C-c' to activate transient\n")
        (insert "\n")
        (insert tshell-current-prompt)))))


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
  (let ((line (string-trim-right (thing-at-point 'line))))
    (cond
     ((string-equal ": undo" line)
      (tshell-undo))
     ;; $ shell eval
     ((string-prefix-p tshell-shell-prompt line)
      (tshell-shell-eval (string-remove-prefix tshell-shell-prompt line))
      (setq tshell-current-prompt tshell-shell-prompt))
     ;; > elisp eval
     ((string-prefix-p tshell-elisp-prompt line)
      (tshell-elisp-eval (string-remove-prefix tshell-elisp-prompt line))
      (setq tshell-current-prompt tshell-elisp-prompt)
      (display-buffer tshell-out-buffer 'other-window))
     (t (message "Unknown prompt")))))

(defun tshell-shell-eval (line)
  "Evaluate LINE in the shell mode."
  ;; Some elementary preprocessing.
  (cond
   ((string-equal "cd" line)
    (cd "~")
    (force-mode-line-update))
   ((string-prefix-p "cd " line)
    (cd (expand-file-name (string-remove-prefix "cd " line)))
    (force-mode-line-update))
   ((string-prefix-p "e " line)
    (let ((file (expand-file-name (string-remove-prefix "e " line))))
      (with-current-buffer (window-buffer (other-window 1))
        (find-file file))))
   ;; Send out buffer as stdin
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

;; TODO: eval in tshell-buffer instead because we want to use buffer
;; local variables from the shell buffer.
(defun tshell-elisp-eval (line)
  "Evaluate LINE in the elisp mode."
  (with-current-buffer tshell-out-buffer
    ;; Save last shell output to "*"
    (when (equal tshell-current-prompt tshell-shell-prompt)
      (setq * (buffer-substring-no-properties (point-min) (point-max))))
    (erase-buffer)
    (let ((result (eval (car (read-from-string line)))))
      (setq * result)
      (insert (pp-to-string result)))))

(defun tshell-out-insert (str)
  "Insert STR into `tshell-out-buffer'."
  (with-current-buffer tshell-out-buffer
    (insert str)))

(defun tshell-shell-kill ()
  "Kill out buffer process if it's running."
  (when (process-live-p (get-buffer-process tshell-out-buffer))
      (when (yes-or-no-p "A command is running. Kill it?")
        (kill-process (get-buffer-process tshell-out-buffer)))))

(defun tshell-undo ()
  "Undo changes in out buffer."
  (with-current-buffer tshell-out-buffer
    (undo 1)
    ;; Reset "*"
    (cond
     ((string-equal tshell-current-prompt tshell-shell-prompt)
      (setq * (buffer-substring-no-properties (point-min) (point-max))))
     ((string-equal tshell-current-prompt tshell-elisp-prompt)
      (setq * (car (read-from-string (buffer-substring-no-properties (point-min) (point-max)))))))))


;;; Private stuff

(defun tshell-completion-at-point ()
  "tshell's `completion-at-point' function."
  ;; FIXME: this is very unreliable
  (when (fboundp 'fish-completion--list-completions)
    (let* ((start (save-excursion (beginning-of-line) (+ (point) 2)))
           (end (point))
           (line (buffer-substring-no-properties start end))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (list (car bounds)
            (cdr bounds)
            (completion-table-dynamic
             `(lambda (_)
                (fish-completion--list-completions ,line)))))))

;;; Transient interface

(transient-define-prefix tshell-dispatch ()
  "Invoke a tshell command from a list of available commands."
  ["Transient and dwim commands"
   [("d" "cd" tshell-command-cd)
    ("l" "ls" tshell-command-ls)
    ("x" "xargs" (lambda () (interactive) (tshell-command-region "xargs ")))
    ("SPC" "run" (lambda () (interactive) (tshell-command)))
    ("C-SPC" "run-region" (lambda () (interactive) (tshell-command-region)))]])

(defun tshell-command-cd ()
  "Change directory."
  (interactive)
  (call-interactively #'cd))

(defun tshell-command-ls ()
  "Run `ls'."
  (interactive)
  (async-shell-command "ls -1" tshell-out-buffer))

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

(defun tshell-yank ()
  "Yank contents of the output buffer at point."
  (interactive)
  (insert-buffer tshell-out-buffer))

(provide 'tshell)
