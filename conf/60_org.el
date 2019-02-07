;;; org-mode
(require 'org)

;; 画像をインラインで表示
(setq org-startup-with-inline-images t)

;; インデントモードにする
(setq org-startup-indented t)

;; org-mode での強調表示を可能に
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; .org ファイルは自動的に org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-default-notes-file のディレクトリ
(setq org-directory "~/Dropbox/Emacs/org/")

;; org-default-notes-file のファイル名
(setq org-default-notes-file "~/Dropbox/Emacs/org/default.org")

;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))

;; TODO 状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)" "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))

;; LOGBOOK drawer に時間を格納
(setq org-clock-into-drawer t)

;; DONE の時刻を記録
(setq org-log-done 'time)

;; 文字化け対策
(setq system-time-locale "C")

;; アンダースコア後下付き文字になるのを回避
(setq org-export-with-sub-superscripts nil)


;;; org-capture-template
(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "~/Dropbox/Emacs/org/task.org" "Task")
         "* TODO %?\n%U" :empty-lines 1)
        ("T" "Task with Clipboard" entry
         (file+headline "~/Dropbox/Emacs/org/task.org" "Task")
         "* TODO %?\n%U\n%c" :empty-lines 1)
        ("n" "Note" entry
         (file+headline "~/Dropbox/Emacs/org/note.org" "Note")
         "* %?\n%U" :empty-lines 1)
        ("N" "Note with Clipboard" entry
         (file+headline "~/Dropbox/Emacs/org/note.org" "Note")
         "* %?\n%U\n%c" :empty-lines 1)))


;;; open-junk-file
(require 'open-junk-file)
(setq open-junk-file-format "~/Dropbox/Emacs/junk/%Y-%m-%d-%H%M%S.")


;;; org-clock
;; Emacs 終了時に org-clock-out
(defun my/org-clock-out-and-save ()
  "Save buffers and stop clocking when kill emacs."
  (when (org-clock-is-active)
    (org-clock-out)
    (save-some-buffers t)))
(add-hook 'kill-emacs-hook #'my/org-clock-out-and-save)

;; 1分未満を記録しない
(setq org-clock-out-remove-zero-time-clocks t)

;; タスク名をタイトルバーに表示
(setq org-clock-clocked-in-display 'frame-title)


;;; Archive ファイルを datetree で管理
(setq org-archive-location "~/Dropbox/Emacs/org/archive.org::datetree/")


;;; 日本語 PDF を出力する
;; LaTeX article class
(setq org-export-latex-classes
      '(("article"
         "\\documentclass[11pt,a4paper]{jsarticle}
\\usepackage{amsmath}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage[dvipdfmx,hiresbb]{graphicx}
\\usepackage[dvipdfmx]{color}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; LaTeX から PDF を作成するコマンド
(when (executable-find "platexpdf")
  (setq org-latex-pdf-process '("platexpdf %f")))
