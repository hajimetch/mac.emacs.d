;;; Helm
(require 'helm)

;; helm-mini
(setq helm-mini-default-sources
      (quote
       (helm-source-buffers-list
        helm-source-recentf
        helm-source-files-in-current-dir
        )))

;; 表示する最大候補数
(setq helm-candidate-number-limit 100)

;; Helm バッファのサイズ
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode t)

;; Helm バッファが常にウィンドウの下側に来るように設定
(setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

;; Helm その他の設定
(setq helm-scroll-amount 8
      helm-split-window-inside-p t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t)

(helm-mode t)


;;; helm-c-yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/Dropbox/Emacs/snippets/mysnippets" ; 自作スニペット
        "~/Dropbox/Emacs/snippets/yasnippets" ; デフォルトスニペット
        ))
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode t)


;;; helm-migemo-mode
(helm-migemo-mode t)


;;; helm-swoop
(require 'helm-swoop)

;; リストを循環しない
(setq helm-swoop-move-to-line-cycle nil)


;;; helm-rg
(require 'helm-rg)


;;; Projectile
(projectile-mode t)
(setq projectile-completion-system 'helm)

;; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

;; fd を使用
(setq projectile-git-command "fd . -0")
(setq projectile-generic-command "fd . -0")


;;; helm-find-file から browse-project を呼び出す
(defun my/helm-ff-run-browse-project ()
  "Call helm-ff-run-browse-project with C-u."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'helm-ff-run-browse-project))
