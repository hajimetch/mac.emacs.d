;; ---------------------------------------------------------
;; SKK の設定
;; ---------------------------------------------------------
;;; 全般
(require 'skk)
(setq skk-user-directory "~/Dropbox/Emacs/ddskk/") ; 設定ファイルパス
(when (require 'skk nil t)
  (setq default-input-method "japanese-skk") ; emacs上での日本語入力にskkを使う
  (require 'skk-study))                      ; 変換学習機能の追加

(setq skk-server-prog "/usr/local/bin/google-ime-skk") ; google-ime-skkの場所
(setq skk-server-inhibit-startup-server nil) ; 辞書サーバが起動していなかったときに Emacs からプロセスを立ち上げる
(setq skk-server-host "localhost")           ; サーバー機能を利用
(setq skk-server-portnum 55100)              ; ポートはgoogle-ime-skk
(setq skk-share-private-jisyo t)             ; 複数 skk 辞書を共有
(setq skk-cursor-latin-color "#5BFBD0")      ; アスキーモードのカーソルの色


;;; 候補表示
(setq skk-show-candidates-always-pop-to-buffer t) ; 候補をバッファに表示
(setq skk-show-annotation t)                      ; 注釈を表示
(setq skk-annotation-delay 0)                     ; 即座に表示


;;; *候補*バッファ
(let ((target '(("*候補*"           :position bottom :height 10 :noselect t)
                ("*SKK annotation*" :position bottom :height 10 :noselect t))))
  (dolist (e target)
    (setq popwin:special-display-config
          (cons e popwin:special-display-config))))

;; 背景色
(setq skk-candidate-buffer-background-color "grey30"
skk-candidate-buffer-background-color-odd "grey20")

;; fringe を消す
(setq skk-candidate-buffer-display-fringes nil)

;; 表示の調整
(setq skk-treat-candidate-appearance-function
      #'(lambda (candidate listing-p)
          (let* ((value (skk-treat-strip-note-from-word candidate))
                 (cand (car value))	;候補
                 (note (cdr value))	;注釈
                 (sep (if note		;セパレータ
                          (propertize (if (skk-annotation-display-p 'list)
                                          " = "
                                        " !")
                                      'face 'skk-emacs-jisx0208-latin-face)
                        nil)))
            (cond (note
                   (put-text-property 0 (length cand)
                                      'face 'skk-emacs-jisx0201-face cand)
                   (put-text-property 0 (length note)
                                      'face 'skk-emacs-katakana-face note)
                   (cons cand (cons sep note)))
                  (t
                   (put-text-property 0 (length cand)
                                      'face 'skk-emacs-hiragana-face cand)
                   cand)))))


;;; 動的候補表示
(setq skk-dcomp-activate t)             ; 動的補完
(setq skk-dcomp-multiple-activate t)    ; 動的補完の複数候補表示
(setq skk-dcomp-multiple-rows 10)       ; 動的補完の候補表示件数

;; 動的補完の複数表示群のフェイス
(set-face-foreground 'skk-dcomp-multiple-face "Black")
(set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
(set-face-bold 'skk-dcomp-multiple-face nil)

;; 動的補完の複数表示郡の補完部分のフェイス
(set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
(set-face-bold 'skk-dcomp-multiple-trailing-face nil)

;; 動的補完の複数表示郡の選択対象のフェイス
(set-face-foreground 'skk-dcomp-multiple-selected-face "White")
(set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
(set-face-bold 'skk-dcomp-multiple-selected-face nil)


;;; 動作
(setq skk-verbose t)                  ; 詳細なメッセージを表示
(setq skk-comp-circulate t)           ; 見出し語の補完時の候補の表示順
(setq skk-egg-like-newline t)         ; Enterで改行しない
(setq skk-auto-insert-paren t)        ; 閉じカッコを自動的に
(setq skk-auto-start-henkan nil)      ; 区切り文字で変換しない
(setq skk-share-private-jisyo t)      ; 個人辞書を複数Emacsで共有
(setq skk-delete-implies-kakutei nil) ; ▼モードで一つ前の候補を表示する
(setq skk-previous-candidate-keys '("x")) ; 前候補表示キーからC-pを除外
(setq skk-search-katakana 'jisx0201-kana) ; カタカナを変換候補に入れる
(setq skk-henkan-strict-okuri-precedence t) ; 送り仮名が厳密に正しい候補を優先して表示
(setq skk-use-auto-enclose-pair-of-region t) ; リージョンを括弧で囲む
(bind-key "C-j" 'skk-kakutei minibuffer-local-map) ; ミニバッファでは C-j を改行にしない
(require 'skk-hint)                       ; ヒント
(add-hook 'skk-load-hook                  ; 自動的に入力モードを切り替え
      (lambda ()
        (require 'context-skk)))


;;; 言語
(setq skk-japanese-message-and-error t) ; エラーを日本語に
(setq skk-show-japanese-menu t)         ; メニューを日本語に


;;; 個人辞書の自動保存
(defvar skk-auto-save-jisyo-interval 600)
(defun skk-auto-save-jisyo ()
  (skk-save-jisyo)
  )
(run-with-idle-timer skk-auto-save-jisyo-interval
                     skk-auto-save-jisyo-interval
                     'skk-auto-save-jisyo)


;;; 基本辞書
(setq skk-large-jisyo "~/Dropbox/Emacs/ddskk/SKK-JISYO.L")


;;; かな変換トグル
(defun my/skk-set-henkan ()
  (interactive)
  (cond (skk-j-mode
         (skk-mode t)
         (skk-j-mode-on)
         (skk-set-henkan-point-subr))
        (t
         (skk-mode t)
         (skk-j-mode-on))))


;;; 次候補を表示
(defun my/skk-next-candidate ()
  (interactive)
  (cond ((eq skk-henkan-mode 'on)
         (skk-comp-wrapper t))
        ((eq skk-henkan-mode 'active)
         (skk-start-henkan t))
        (t (next-line))))


;;; 前候補を表示
(defun my/skk-previous-candidate ()
  (interactive)
  (cond ((eq skk-henkan-mode 'on)
         (skk-comp-previous t))
        ((eq skk-henkan-mode 'active)
         (skk-previous-candidate t))
        (t (previous-line))))


;;; macOS の IME を無効にする
(defun my/eisuu-key ()
  (interactive)
  (call-process "osascript" nil t nil "-e" "tell application \"System Events\" to key code 102"))
(add-hook 'focus-in-hook 'my/eisuu-key)


;;; markdown-mode
(skk-wrap-newline-command markdown-enter-key)


;;; チュートリアルのパス
(setq skk-tut-file "~/Dropbox/Emacs/ddskk/SKK.tut")
