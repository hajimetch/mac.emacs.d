;;; Library
(use-package cl-lib)


;;; Path
(use-package exec-path-from-shell
  :if window-system
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


;;; Desktop
(desktop-save-mode t)

;; テーマをデスクトップ復元後にロード
(add-to-list 'desktop-globals-to-save 'custom-enabled-themes)
(defun my/desktop-load-theme ()
  "Load custom theme."
  (interactive)
  (dolist (th custom-enabled-themes) (load-theme th)))
(add-hook 'desktop-after-read-hook 'my/desktop-load-theme)


;;; 文字コード
(set-language-environment "Japanese")     ; 言語環境
(set-default-coding-systems 'utf-8-unix)  ; デフォルト文字コード
(prefer-coding-system 'utf-8-unix)        ; Text File / 新規バッファ
(set-file-name-coding-system 'utf-8-unix) ; ファイル名
(set-keyboard-coding-system 'utf-8-unix)  ; キーボード入力
(set-terminal-coding-system 'utf-8-unix)  ; ターミナル
(setq locale-coding-system 'utf-8-unix)   ; システムメッセージ

;; Mac のファイル名正規化等を扱えるようにする
(use-package ucs-normalize)

;; 環境依存文字 文字化け対応
(set-charset-priority 'ascii
                      'japanese-jisx0208
                      'latin-jisx0201
                      'katakana-jisx0201
                      'iso-8859-1
                      'cp1252
                      'unicode)
(set-coding-system-priority 'utf-8
                            'euc-jp
                            'iso-2022-jp
                            'cp932)


;;; 検索
;; 大文字・小文字を区別しない
(setq-default case-fold-search nil)
(setq read-buffer-completion-ignore-case t)    ; バッファ名検索
(setq read-file-name-completion-ignore-case t) ; ファイル名検索

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll nil)

;; migemo
(use-package migemo
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-coding-system 'utf-8-unix)
  (defvar migemo-user-dictionary nil)
  (defvar migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))

;; anzu
(use-package anzu
  :config
  (global-anzu-mode t))


;;; バックアップ(xxx~)
(setq make-backup-files     t)    ; 自動バックアップの実行有無
(setq version-control       t)    ; バックアップファイルへの番号付与
(setq kept-new-versions   100)    ; 最新バックアップファイルの保持数
(setq kept-old-versions     1)    ; 最古バックアップファイルの保持数
(setq delete-old-versions   t)    ; バックアップファイル削除の実行有無

;; バックアップ(xxx~)の格納ディレクトリ
(setq backup-directory-alist '((".*" . "~/Dropbox/Emacs/backups/mac")))

;; バッファ保存時に毎回バックアップする
(defun my/setq-buffer-backed-up-nil (&rest _)
  "Function used to always backup buffer when saved."
  (interactive) (setq buffer-backed-up nil))
(advice-add 'save-buffer :before 'my/setq-buffer-backed-up-nil)


;;; 自動保存ファイル(#xxx#)
;; 作成する
(setq auto-save-default     t)

;; 保存の間隔
(setq auto-save-timeout    10)          ; 秒
(setq auto-save-interval  100)          ; 打鍵

;; 自動保存ファイル(#xxx#)の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*", (expand-file-name "~/Dropbox/Emacs/backups/mac/") t)))


;;; 自動保存のリスト(~/.emacs.d/auto-save-list/.saves-xxx)
;; 下記プレフィックスで作成する
(setq auto-save-list-file-prefix "~/Dropbox/Emacs/backups/mac/saves-")


;;; ロックファイル(.#xxx)
;; 作成しない
(setq create-lockfiles    nil)


;;; 特定のファイルではバックアップを作成しない
(defvar my/backup-inhibit-file-name-regexp "recentf"
  "Regexp of file name not for backup.")
(defun my/backup-enable-predicate (filename)
  "Function used to inhibit from backing up files specified by var my/backup-inhibit-file-name-regexp."
  (save-match-data
    (and (not (string-match my/backup-inhibit-file-name-regexp filename))
         (normal-backup-enable-predicate filename))))
(setq backup-enable-predicate 'my/backup-enable-predicate)


;;; recentf 関連
(use-package recentf-ext)

;; recentf から除外するファイル
(setq recentf-exclude (list "recentf"
                            (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME"))))

;; recentf に保存するファイル数
(setq recentf-max-saved-items 1000)

;; *Messages* に不要な出力を行わないようにする
(defmacro my/with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; 30秒ごとに recentf を保存
(run-with-idle-timer 30 t '(lambda ()
                             (my/with-suppressed-message (recentf-save-list))))


;;; undo 関連
;; undohist
(use-package undohist
  :config
  (undohist-initialize)
  (setq undohist-ignored-files '("COMMIT_EDITMSG")))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode t))


;;; abbrev file
(setq abbrev-file-name "~/Dropbox/Emacs/abbrev_defs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(setq save-abbrevs 'silently)


;;; ediff
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; diff のバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;; Company 関連
;; company
(use-package company
  :config
  (global-company-mode t))

;; company-quickhelp
(use-package company-quickhelp
  :config
  (company-quickhelp-mode t))


;;; which-key
(use-package which-key
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode t))


;;; howm
(use-package howm
  :init
  (setq howm-view-title-header "*")
  (setq howm-prefix (kbd "C-x ,"))
  :config

  ;; ファイルパス
  (setq howm-directory "~/Dropbox/Emacs/howm/")
  (setq howm-keyword-file (concat howm-directory ".howm-keys"))
  (setq howm-history-file (concat howm-directory ".howm-history"))
  (setq howm-menu-file (concat howm-directory "menu.txt"))

  ;; howm-menu の言語を日本語に
  (setq howm-menu-lang 'ja)

  ;; メモを保存と同時に閉じる
  (defun my/howm-save-buffer-and-kill()
    "Save howm buffer and exit."
    (interactive)
    (when (and (buffer-file-name)
               (howm-buffer-p))
      (save-buffer)
      (kill-buffer nil)))

  ;; メモを保存せずに閉じる
  (defun my/howm-kill-buffer()
    "Save howm buffer and exit."
    (interactive)
    (when (and (buffer-file-name)
               (howm-buffer-p))
      (kill-buffer nil))))


;;; Mac 標準辞書アプリと連携
(defun my/dictionary ()
  "dictionary.app"
  (interactive)
  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end)
    (if (and mark-active
             (<= (region-beginning) pt) (<= pt (region-end)) )
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (setq end (progn (forward-word) (point)))
        (setq beg (progn (backward-word) (point)))
        ))
    (start-process "dictionary.app" "*dictionary-region*"
                   "open"
                   (concat "dict:///"
                           (url-hexify-string
                            (buffer-substring-no-properties beg end))))))


;;; その他
;; dired バッファを並べる
(setq dired-dwim-target t)

;; ファイルが #! から始まる場合、+x を付けて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Error 対応 (Emacs' unknown and untrusted authority TLS error)
(use-package gnutls
  :config
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

;; Error 対応 (insert-directory: Listing directory failed but `access-file' worked)
(use-package ls-lisp
  :config
  (setq ls-lisp-use-insert-directory-program nil))

;; Error 対応 (ad-handle-definition: `tramp-read-passwd' got redefined)
(setq ad-redefinition-action 'accept)
