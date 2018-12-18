;;; python
(require 'python)


;;; jedi
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)


;;; global-flycheck-mode
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))


;;; js2-mode
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;;; mark-down-mode
(add-to-list 'auto-mode-alist '("\\.md'" . mark-down-mode))
(setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github.css --quiet")


;;; semantic-mode
(semantic-mode t)
