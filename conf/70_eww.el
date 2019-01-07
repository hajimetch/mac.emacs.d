;;; eww-mode
(require 'eww)


;;; eww 検索エンジンを google.co.jp に
(setq eww-search-prefix "http://www.google.co.jp/search?q=")


;;; eww 背景色の設定
(defvar eww-disable-colorize t)
(defun my/shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'my/shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'my/shr-colorize-region--disable)
(defun my/eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun my/eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))


;;; 現在の url を eww で開く
(defun my/browse-url-with-eww ()
  (interactive)
  (let ((url-region (bounds-of-thing-at-point 'url)))
    ;; url
    (if url-region
        (eww-browse-url (buffer-substring-no-properties (car url-region)
                                                        (cdr url-region))))
    ;; org-link
    (setq browse-url-browser-function 'eww-browse-url)
    (org-open-at-point-global)))


;;; 画像表示の設定
(defun my/eww-disable-images ()
  "eww で画像表示させない"
  (interactive)
  (setq-local shr-put-image-function 'my/shr-put-image-alt)
  (eww-reload))
(defun my/eww-enable-images ()
  "eww で画像表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun my/shr-put-image-alt (spec alt &optional flags)
  (insert alt))

;; はじめから非表示
(defun my/eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'my/shr-put-image-alt))
(add-hook 'eww-mode-hook 'my/eww-mode-hook--disable-image)
