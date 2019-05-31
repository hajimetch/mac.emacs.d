;;; Alt to Meta
(setq mac-option-modifier 'meta)
(when window-system
  (setq mac-command-modifier 'meta))


;;; PL
(bind-key "C-c p"          'previous-error)
(bind-key "C-c n"          'next-error)


;;; isearch
(bind-keys :map isearch-mode-map
           ("C-d" .        isearch-delete-char)
           ("C-e" .        isearch-edit-string)
           ("C-g" .        (lambda() (interactive) (isearch-done)))
           ("C-y" .        isearch-yank-kill)
           ("TAB" .        isearch-yank-word)
           ("M-s" .        helm-swoop-from-isearch))

;;; Font Size
;; Adjust
(bind-key "M-<wheel-up>"   '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-="            '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-<wheel-down>" '(lambda() (interactive) (text-scale-decrease 1)))
(bind-key "M--"            '(lambda() (interactive) (text-scale-decrease 1)))

;; Reset
(bind-key "M-0"            '(lambda() (interactive) (text-scale-set 0)))


;;; Other key bindings
(bind-key "C-h" (kbd "DEL") key-translation-map) ; C-hでバックスペース
(bind-key "C-m"            'newline-and-indent) ; 改行時自動インデント
(bind-key "C-x k"          'kill-this-buffer)   ; バッファを閉じる
(bind-key "M-k"            'kill-this-buffer)   ; バッファを閉じる
(bind-key "C-c d"          'my/dictionary)      ; 辞書参照
(bind-key "C-c r"          'my/revert-buffer)   ; バッファ更新
(bind-key "C-c t"          'my/eshell-pop)      ; eshellを開く
(bind-key "C-c <C-return>" 'toggle-truncate-lines) ; 右端で折り返す
(bind-key "C-c TAB"        'indent-region)         ; 範囲インデント
(bind-key* "C-t"           'other-window) ; 他のウィンドウに移動
(bind-key "<backtab>"                     ; TAB挿入
          '(lambda() (interactive) (insert "	")))
(bind-key "<f1>"           'help-for-help) ; ヘルプ参照
