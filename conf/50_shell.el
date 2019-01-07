;;; shell の存在を確認
(defun my/check-shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))


;;; shell 名
(setq shell-file-name (my/check-shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)


;;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)


;;; エスケープを綺麗に表示する
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;; multi-term
(require 'multi-term)
(setq multi-term-program shell-file-name)

;; multi-term-toggle
(defun my/multi-term-toggle ()
  (interactive)
  (progn (multi-term-dedicated-toggle)
         (if (multi-term-dedicated-exist-p) (multi-term-dedicated-select))))


;;; Eshell
;; eshell alias
(setq eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -lh")
        (list "la" "ls -a")
        (list "emacs" "find-file $1")
        (list "m" "find-file $1")
        (list "mc" "find-file $1")
        (list "d" "dired .")
        (list "l" "eshell/less $1 $2"))))

;; written by Stefan Reichoer <reichoer@web.de>
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (interactive)
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))

;; make-new-shell
(defun eshell/make-new-eshell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

;; shell-toggle
(require 'shell-toggle)
(setq shell-toggle-launch-shell 'shell-toggle-eshell)
(setq shell-toggle-full-screen-window-only t)

;; shell-pop
(custom-set-variables
 '(shell-pop-shell-type (quote ("eshell" "*eshell*"
                                (lambda nil (eshell shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-c t")
 '(shell-pop-window-height 30)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom"))
