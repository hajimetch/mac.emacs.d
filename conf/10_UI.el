;;; 初期画面の非表示
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)


;;; フレーム
(when window-system
  (progn
    (setq default-frame-alist
      (append '((line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 10 ) ; 左フリンジ幅
                (right-fringe         . 12 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                (tool-bar-lines       . nil) ; ツールバー
                (vertical-scroll-bars . nil) ; スクロールバー
                (alpha                . 95 ) ; 透明度
                ) default-frame-alist))))
(setq initial-frame-alist default-frame-alist)

;; フレームタイトル
(setq frame-title-format
      '("Emacs " emacs-version (buffer-file-name " - %f")))


;;; 行番号
;; バッファ中の行番号表示
(global-linum-mode 0)                 ; 表示しない(パフォーマンス対策)


;;; whitespace
(require 'whitespace)

;; 空白を視覚化
(setq whitespace-style '(face           ; faceで可視化
                         tabs           ; タブ
                         trailing       ; 行末
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング(space)
                         tab-mark       ; 表示のマッピング(tab)
                         ))

;; 全角のスペースとタブを目立たせる
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark ?\t [?\xBB ?\t]) ))

;; Whitespace 色設定
(defvar my/fg-color "dark gray")
(defvar my/bg-color "black")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground my/fg-color)
(set-face-attribute 'whitespace-tab nil
                    :background nil
                    :foreground my/fg-color)
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground my/fg-color)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color
                    :foreground my/fg-color)

;; デフォルトで視覚化を有効に
(global-whitespace-mode t)


;;; フォント
(when window-system
  (set-face-attribute 'default nil :family "Ricty Diminished Discord" :height 155)
  (set-face-attribute 'variable-pitch nil :family "Ricty Diminished Discord" :height 155)
  (set-face-attribute 'fixed-pitch nil :family "Ricty Diminished Discord" :height 155)
  (set-face-attribute 'tooltip nil :family "Ricty Diminished Discord" :height 155))


;;; ElScreen
(require 'elscreen)
(elscreen-start)


;;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 括弧の色を強調する設定
(require 'cl-lib)
(require 'color)

(defun my/rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'my/rainbow-delimiters-using-stronger-colors)


;;; モードライン
;; 行列番号の表示
(line-number-mode t)
(column-number-mode t)

;; モードライン カスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   global-mode-string
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "L%%l/L%d " (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )
(setq mode-line-frame-identification " ")

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


;;; カーソル・ハイライト
;; カーソル行をハイライト
(require 'hl-line)

;; ハイライトを無効にするメジャーモードを指定
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode))

;; ハイライトに0.03秒の猶予を与え、カーソル移動を軽くする
(defun my/global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'my/global-hl-line-timer-function))

;; カーソルの点滅を止める
(blink-cursor-mode 0)

;; 非アクティブウィンドウのカーソル表示
(setq-default cursor-in-non-selected-windows t)

;; カーソルの形状
(setq-default cursor-type '(bar . 2))

;; 括弧のハイライト
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲をハイライト
(transient-mark-mode t)

;; volatile-highlights
(require 'volatile-highlights)
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)
(volatile-highlights-mode t)


;;; バッファ
;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows nil)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;; fill-column-indicator
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "dim gray")
(define-globalized-minor-mode global-fci-mode
  fci-mode (lambda () (fci-mode t)))
(global-fci-mode t)


;;; ダイアログボックスを抑制する
(defalias 'message-box 'message)
(setq use-dialog-box nil)


;;; Misc
;; アラートのビープ音を消す
(setq ring-bell-function 'ignore)

;; png, jpg などのファイルを画像として表示
(setq auto-image-file-mode t)
