;;; 初期画面の非表示
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)


;;; フレーム
(when window-system
  (setq default-frame-alist
        (append '((line-spacing         . 0  ) ; 文字間隔
                  (left-fringe          . 10 ) ; 左フリンジ幅
                  (right-fringe         . 12 ) ; 右フリンジ幅
                  (menu-bar-lines       . 1  ) ; メニューバー
                  (tool-bar-lines       . nil) ; ツールバー
                  (vertical-scroll-bars . nil) ; スクロールバー
                  (alpha                . 95 ) ; 透明度
                  ) default-frame-alist)))
(setq initial-frame-alist default-frame-alist)


;;; タイトル
(when window-system
  (setq display-time-string-forms
        '((format "%s/%s/%s" year month day)
          (format "(%s:%s)" 24-hours minutes)))
  (display-time)
  (setq frame-title-format '("Emacs " emacs-version
                             " - " global-mode-string
                             (:eval (if (buffer-file-name) " - %f" " - %b")))))


;;; 行番号
;; バッファ中の行番号表示はしない(パフォーマンス対策)
(global-linum-mode 0)


;;; 空白文字
;; 空白を視覚化
(use-package whitespace
  :config
  (setq whitespace-style '(face         ; faceで可視化
                           tabs         ; タブ
                           trailing     ; 行末
                           spaces       ; スペース
                           empty        ; 先頭/末尾の空行
                           space-mark   ; 表示のマッピング(space)
                           tab-mark     ; 表示のマッピング(tab)
                           ))

  ;; 全角スペースとタブを目立たせる
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\□])
          (tab-mark ?\t [?\xBB ?\t]) ))

  ;; 色設定
  (defvar my/fg-color "red1")
  (defvar my/bg-color "red4")
  (set-face-attribute 'whitespace-trailing nil
                      :foreground (face-attribute 'default :foreground)
                      :background my/bg-color)
  (set-face-attribute 'whitespace-tab nil
                      :foreground my/fg-color
                      :background (face-attribute 'default :background))
  (set-face-attribute 'whitespace-space nil
                      :foreground my/fg-color
                      :background (face-attribute 'default :background))
  (set-face-attribute 'whitespace-empty nil
                      :foreground (face-attribute 'default :foreground)
                      :background my/bg-color)

  (global-whitespace-mode t))

;; タブ
(setq-default indent-tabs-mode nil)     ; タブ無効化
(setq-default tab-width 4)              ; タブ幅を 4 に設定


;;; フォント
(when window-system
  (set-face-attribute 'default nil :family "Ricty Diminished Discord" :height 155)
  (set-face-attribute 'variable-pitch nil :family "Ricty Diminished Discord" :height 155)
  (set-face-attribute 'fixed-pitch nil :family "Ricty Diminished Discord" :height 155)
  (set-face-attribute 'tooltip nil :family "Ricty Diminished Discord" :height 155))


;;; 括弧
;; 括弧にカラフルな色を付ける
(use-package color)
(use-package rainbow-delimiters
  :after color
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emacs-startup . my/rainbow-delimiters-using-stronger-colors))
  :config
  (defun my/rainbow-delimiters-using-stronger-colors ()
    "Run rainbow-delimiters using stronger colors."
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))))

;; 自動的に括弧を付ける
(use-package smartparens-config
  :hook (prog-mode . smartparens-mode))


;;; モードライン
;; 行列番号の表示
(line-number-mode t)
(column-number-mode t)

;; リージョン内の行数と文字数をモードラインに表示する
(defun my/count-lines-and-chars ()
  "Function used to show number of lines and chars of region in modeline."
  (if mark-active
      (format "(%dlines, %dchars) "
              (count-lines (region-beginning)(region-end))
              (- (region-end)(region-beginning)))
    ""))

;; モードラインカスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   (:eval (my/count-lines-and-chars))
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "- L%%l/L%d" (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )

;; cp932 エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8 エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ")


;;; ハイライト
;; カーソル行ハイライト
(use-package hl-line
  :config
  ;; ハイライトを無効にするメジャーモードの指定
  (defvar my/global-hl-line-timer-exclude-modes '(todotxt-mode)
    "Major mode for disabling hl-line.")

  ;; ハイライトに0.03秒の猶予を与える(パフォーマンス対策)
  (defun my/global-hl-line-timer-function ()
    "Function used to smooth cursor movement."
    (unless (memq major-mode my/global-hl-line-timer-exclude-modes)
      (global-hl-line-unhighlight-all)
      (let ((global-hl-line-mode t))
        (global-hl-line-highlight))))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.03 t 'my/global-hl-line-timer-function)))

;; ハイライトで視覚的フィードバック
(use-package volatile-highlights
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (volatile-highlights-mode t))

;; 対応する括弧をハイライト
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲をハイライト
(transient-mark-mode t)


;;; カーソル
(setq-default cursor-in-non-selected-windows t) ; 非アクティブでもカーソル表示
(setq-default cursor-type '(bar . 2))           ; カーソルの形状
(blink-cursor-mode 0)                           ; カーソルを点滅しない
(use-package point-undo)                        ; カーソル位置のアンドゥ


;;; バッファ
;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; 行の文字数の目印を付ける
(use-package fill-column-indicator
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "dim gray")
  (define-globalized-minor-mode global-fci-mode
    fci-mode (lambda () (fci-mode t)))
  (global-fci-mode t))

;; バッファの終端を明示する
(setq-default indicate-empty-lines t)

;; バッファ再読み込み関数
(defun my/revert-buffer ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))


;;; ウィンドウ
;; ElScreen
(use-package elscreen
  :config
  (elscreen-start))

;; shackle
(use-package shackle
  :config
  (setq shackle-rules
        '((compilation-mode :align below :ratio 0.3)
          ("*Completions*" :align below :ratio 0.3)
          ("*Help*" :align below :ratio 0.4)
          ("*eshell*" :align below :ratio 0.4 :popup t)
          ("*候補*" :align below :ratio 0.3)
          ("*SKK annotation*" :align below :ratio 0.3)))
  (shackle-mode t))

;; rotete-window でカーソルを元のウィンドウに残す
(defadvice rotate-window (after rotate-cursor activate)
  (other-window 1))


;;; スクロール
(setq scroll-preserve-screen-position t) ; カーソル位置を維持
(setq scroll-margin 0)                  ; スクロール開始の残り行数
(setq scroll-conservatively 10000)      ; 1行ずつスクロール
(setq next-screen-context-lines 1)      ; 画面スクロール時の重複行数
(setq recenter-positions '(middle top bottom)) ; recenter時のポジション
(setq hscroll-margin 1)                 ; 横スクロール開始の残り列数
(setq hscroll-step 1)                   ; 1列ずつスクロール


;;; 選択領域・カット
(global-hungry-delete-mode t)   ; なるべく一括削除
(delete-selection-mode t)       ; 選択領域も一括削除
(setq kill-whole-line t)        ; C-k で行末の改行も削除
(setq kill-read-only-ok t)      ; 読み取り専用バッファもカットでコピー

;; 矩形選択有効化
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;; その他
(setq ring-bell-function 'ignore)       ; アラートのビープ音は消す
(setq auto-image-file-mode t)           ; 画像ファイルは画像として表示
