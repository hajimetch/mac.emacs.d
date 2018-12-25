;;; Alt to Meta
(setq mac-option-modifier 'meta)
(when window-system
  (setq mac-command-modifier 'meta))


;;; ddskk
(bind-key "C-S-j"          'skk-j-mode-set-henkan)
(bind-key "C-:"            'skk-latin-mode)
(bind-keys :map skk-j-mode-map
           ("S-SPC" .      skk-previous-candidate)
           ("C-n" .        my/skk-next-candidate)
           ("C-p" .        my/skk-previous-candidate)
           ("<down>" .     my/skk-next-candidate)
           ("<up>" .       my/skk-previous-candidate))


;;; Helm
(bind-key "M-x"            'helm-M-x)
(bind-key "C-x C-x"        'helm-mini)
(bind-key "C-x C-f"        'helm-find-files)
(bind-key "C-c b"          'helm-resume)
(bind-key "C-c g"          'helm-rg)
(bind-key "C-c i"          'helm-semantic-or-imenu)
(bind-key "C-c m"          'helm-man-woman)
(bind-key "C-c y"          'helm-yas-complete)
(bind-key "C-c C-SPC"      'helm-all-mark-rings)
(bind-key "C-S-y"          'helm-show-kill-ring)
(bind-key "C-c <f1>"       'helm-info)
(bind-key "<M-f1>"         'helm-apropos)
(bind-keys :map helm-map
           ("TAB" .        helm-execute-persistent-action)
           ("C-z" .        helm-select-action)
           ("M-b" .        helm-ff-run-browse-project-with-prefix-arg))


;;; Projectile
(bind-keys :map projectile-mode-map
           ("C-q" .        projectile-command-map))


;;; org-mode
(bind-key "C-c a"          'org-agenda)
(bind-key "C-c c"          'org-capture)
(bind-key "C-c o"          'org-switchb)
(bind-key "C-c l"          'org-store-link)
(bind-key "C-c ("          'org-clock-in)
(bind-key "C-c )"          'org-clock-out)


;;; python-mode
(bind-key "C-c q"          'quickrun)
(bind-keys :map python-mode-map
           ("TAB" .        company-complete)
           ("C-c C-d" .    flycheck-list-errors)
           ("C-c C-f" .    py-yapf-buffer)
           ("C-c C-n" .    flycheck-next-error)
           ("C-c C-p" .    flycheck-previous-error)
           ("C-c C-r" .    helm-jedi-related-names))


;;; multiple-cursor
(bind-key "C->"            'mc/mark-next-like-this)
(bind-key "C-<"            'mc/mark-previous-like-this)
(bind-key "C-c e"          'mc/edit-lines)
(bind-key "C-c h"          'mc/mark-all-like-this)


;;; expand-region
(bind-key "C-="            'er/expand-region)
(bind-key "C--"            'er/contract-region)


;;; isearch
(bind-keys :map isearch-mode-map
           ("C-d" .        isearch-delete-char)
           ("C-e" .        isearch-edit-string)
           ("C-y" .        isearch-yank-kill)
           ("TAB" .        isearch-yank-word)
           ("C-g" .        (lambda() (interactive) (isearch-done))))


;;; helm-swoop
(bind-key "M-s"            'helm-swoop)
(bind-keys :map helm-swoop-map
           ("C-s" .        helm-next-line)
           ("C-r" .        helm-previous-line))


;;; Company
(bind-key "TAB"            'company-complete)
(bind-keys :map company-active-map
           ("C-d" .        company-filter-candidates)
           ("C-n" .        company-select-next)
           ("C-p" .        company-select-previous))
(bind-keys :map company-search-map
           ("C-n" .        company-select-next)
           ("C-p" .        company-select-previous))


;;; Web browse
(bind-key "C-c w"          'helm-google-suggest)
(bind-key "C-c C-w"        'eww)
(bind-keys :map eww-mode-map
           ("w" .          eww-copy-page-url))


;;; Font Size
;; Adjust
(bind-key "M-<wheel-up>"   '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-="            '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-<wheel-down>" '(lambda() (interactive) (text-scale-decrease 1)))
(bind-key "M--"            '(lambda() (interactive) (text-scale-decrease 1)))

;; Reset
(bind-key "M-0"            '(lambda() (interactive) (text-scale-set 0)))


;;; Switch Tab
(bind-key "M-]"            'tabbar-forward-tab)
(bind-key "M-["            'tabbar-backward-tab)


;;; Move Windows
(bind-key "C-t"            'other-window)
(bind-key "M-t"            'rotate-window)


;;; Other key bindings
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))    ; C-hでバックスペース
(bind-key "C-m"            'newline-and-indent)             ; 改行時自動インデント
(bind-key "C-x g"          'magit-status)                   ; magitステータス
(bind-key "C-x k"          'kill-this-buffer)               ; バッファを閉じる
(bind-key "M-k"            'kill-this-buffer)               ; バッファを閉じる
(bind-key "C-c d"          'dictionary)                     ; 辞書参照
(bind-key "C-c j"          'open-junk-file)                 ; junk-file作成
(bind-key "C-c p"          'ps-print-region)                ; PDF作成
(bind-key "C-c r"          'revert-buffer-no-confirm)       ; バッファ更新
(bind-key "C-c s"          'whitespace-cleanup)             ; 不要な空白を削除
(bind-key "C-c ;"          'comment-dwim)                   ; コメントアウト
(bind-key "C-c C-,"        'howm-menu)                      ; howmメニュー
(bind-key "C-c <C-return>" 'toggle-truncate-lines)          ; 右端で折り返す
(bind-key "C-c TAB"        'indent-region)                  ; 範囲インデント
(bind-key "<backtab>"      '(lambda() (interactive) (insert "	")))
                                                            ; インデント
(bind-key "<f1>"           'help-for-help)                  ; ヘルプ参照
