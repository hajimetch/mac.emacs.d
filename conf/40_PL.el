;;; python
(use-package python
  :bind
  (:map python-mode-map
        ("C-c C-f"  . py-yapf-buffer)))


;;; jedi
(use-package jedi-core
  :after python
  :hook (python-mode . jedi:setup)
  :custom
  (jedi:complete-on-dot t)
  (jedi:use-shortcuts t)
  :config (add-to-list 'company-backends 'company-jedi)
  )


;;; flycheck
(use-package flycheck
  :bind
  (:map flycheck-mode-map
        ("C-c C-d"  . flycheck-list-errors))
  :hook (after-init . global-flycheck-mode)
  :config
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-pos-tip-mode t))


;;; semantic-mode
(semantic-mode t)


;;; web-mode
(use-package web-mode
  :mode
  (("\\.html\\'"    . web-mode)
   ("\\.css\\'"     . web-mode)
   ("\\.jsx\\'"     . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.ctp\\'"     . web-mode)
   ("\\.jsp\\'"     . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'"     . web-mode)))


;;; js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))


;;; mark-down-mode
(use-package mark-down-mode
  :mode ("\\.md'"   . mark-down-mode)
  :custom (markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github.css --quiet"))


;;; php-mode
(use-package php-mode
  :custom (php-manual-url 'ja))


;;; gtags
(use-package helm-gtags
  :after gtags
  :hook
  ((python-mode     . helm-gtags-mode)
   (emacs-lisp-mode . helm-gtags-mode))
  :custom (helm-gtags-auto-update t)
  :config (setenv "GTAGSLABEL" "pygments"))


;;; git-gutter
(use-package git-gutter
  :config (global-git-gutter-mode t))
