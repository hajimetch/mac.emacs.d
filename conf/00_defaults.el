;;; path
(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(when window-system
  (exec-path-from-shell-initialize))


;;; Emacs' unknown and untrusted authority TLS error 対応
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")


;;; desktop-save-mode
(desktop-save-mode t)


;;; 文字コード
(set-language-environment "Japanese")

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-hfs)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; システムメッセージの文字コード
(setq locale-coding-system 'utf-8-hfs)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; Mac のファイル名正規化などを扱えるようにする
(require 'ucs-normalize)

;; 環境依存文字 文字化け対応
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)


;;; smartparens
(require 'smartparens-config)

;; prog-mode では常に smartparens-mode
(add-hook 'prog-mode-hook #'smartparens-mode)


;;; ediff
(require 'ediff)

;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; diff のバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;; スクロール
;; スクロール時のカーソル位置を維持
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-margin に影響せず）
(setq scroll-step 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; recenter-top-bottom のポジション
(setq recenter-positions '(middle top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)


;;; 検索
;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search nil)

;; バッファー名の検索
(setq read-buffer-completion-ignore-case t)

;; ファイル名の検索
(setq read-file-name-completion-ignore-case t)

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll nil)


;;; migemo
(require 'migemo)

(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(defvar migemo-user-dictionary nil)
(defvar migemo-regex-dictionary nil)

(load-library "migemo")
(migemo-init)


;;; anzu
(global-anzu-mode t)


;;; Company
(require 'company)
(require 'company-quickhelp)
(global-company-mode t)
(company-quickhelp-mode t)


;;; which-key
(which-key-setup-side-window-bottom)
(which-key-mode t)


;;; 自動保存
(require 'real-auto-save)
(setq real-auto-save-interval 10)  ; 自動保存間隔
(add-hook 'find-file-hook 'real-auto-save-mode)


;;; バックアップ(xxx~)
(setq make-backup-files     t)     ; 自動バックアップの実行有無
(setq version-control       t)     ; バックアップファイルへの番号付与
(setq kept-new-versions  2000)     ; 最新バックアップファイルの保持数
(setq kept-old-versions     1)     ; 最古バックアップファイルの保持数
(setq delete-old-versions   t)     ; バックアップファイル削除の実行有無

;; 保存時に毎回バックアップ
(defun setq-buffer-backed-up-nil (&rest _) (interactive) (setq buffer-backed-up nil))
(advice-add 'save-buffer :before 'setq-buffer-backed-up-nil)

;; バックアップ(xxx~)の格納ディレクトリ
(setq backup-directory-alist '((".*" . "~/Dropbox/Emacs/backups/mac")))


;;; 自動保存ファイル(#xxx#)
;; 作成しない
(setq auto-save-default   nil)


;;; 自動保存のリスト(~/.emacs.d/auto-save-list/.saves-xxx)
;; 作成しない
(setq auto-save-list-file-name   nil)
(setq auto-save-list-file-prefix nil)


;;; ロックファイル(.#xxx)
;; 作成しない
(setq create-lockfiles    nil)


;;; バッファ再読み込み関数
(defun my/revert-buffer ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))


;;; recentf 関連
(require 'recentf)
(require 'recentf-ext)

;; *Messages* に無駄な表示を出さない
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; 除外するファイル
(setq recentf-exclude '("recentf"))
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))

;; recentf に保存するファイルの数
(setq recentf-max-saved-items 1000)

;; 30秒ごとに recentf を保存
(run-with-idle-timer 30 t '(lambda ()
   (with-suppressed-message (recentf-save-list))))


;;; abbrev file
(setq abbrev-file-name "~/Dropbox/Emacs/abbrev_defs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(setq save-abbrevs 'silently)


;;; undohist
(require 'undohist)
(undohist-initialize)


;;; howm
(require 'howm)

;; ファイルパス
(setq howm-directory "~/Dropbox/Emacs/howm")
(setq howm-keyword-file "~/Dropbox/Emacs/howm/.howm-keys")
(setq howm-history-file "~/Dropbox/Emacs/howm/.howm-history")
(setq howm-menu-file "~/Dropbox/Emacs/howm/0000-00-00-000000.txt")

;; howm-menu の言語を日本語に
(setq howm-menu-lang 'ja)


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


;;; Misc
;; global-hungry-delete-mode
(global-hungry-delete-mode t)

;; 選択領域を削除キーで一括削除
(delete-selection-mode t)

;; 矩形選択可能にする
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; C-k で行末の改行も消去
(setq kill-whole-line t)

;; 読み取り専用バッファでもカットでコピー可能
(setq kill-read-only-ok t)

;; dired バッファを並べる
(require 'dired)
(setq dired-dwim-target t)

;; TAB 無効化
(setq-default indent-tabs-mode nil)

;; TAB 幅を 4 に設定
(setq-default tab-width 4)

;; Listing directory failed but access-file worked 対応
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

;; ad-handle-definition 対応
(setq ad-redefinition-action 'accept)

;; 読み取り専用モードで開く
(add-hook 'find-file-hooks 'view-mode)
