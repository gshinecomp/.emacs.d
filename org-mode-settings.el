;; 参考
;; https://github.com/snufkon/emacs_settings/blob/master/dot_emacs/mode/org-mode-settings.el
(require 'org-install)
(require 'org)

;; 参考
;; http://d.hatena.ne.jp/rubikitch/20100819/org
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory (expand-file-name "~/org-file/")
    org-agenda-files (expand-file-name "Agenda.org" org-directory)
    org-default-journal-file (expand-file-name "Journal.org" org-directory)
    org-default-archive-file (expand-file-name "Archive.org" org-directory)
    org-archive-location (concat org-default-archive-file "::datetree/")
    org-mobile-directory "~/org-file/MobileOrg/"
    org-mobile-inbox-for-pull org-default-journal-file
)
(setq org-default-notes-file (concat org-directory "note.org"))
;(setq org-directory "~/org-file/")
;(setq org-default-notes-file (concat org-directory "note.org"))
;(setq org-agenda-files (list
;			org-default-notes-file
;			(concat org-directory "project/smn.org")
;			(concat org-directory "project/smn_minute.org")
;			(concat org-directory "project/antlers.org")
;			(concat org-directory "project/sonet.org")))
(global-set-key (kbd "C-c a") 'org-agenda)

;; 参考
;; http://d.hatena.ne.jp/tamura70/20100208/org
;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)
      
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline nil "Inbox")
         "** TODO %?\n   %i\n   %a\n   %t")
        ("b" "Bug" entry
         (file+headline nil "Inbox")
         "** TODO %?   :bug:\n   %i\n   %a\n   %t")
        ("i" "Idea" entry
         (file+headline nil "New Ideas")
         "** %?\n   %i\n   %a\n   %t")))
(global-set-key (kbd "C-c c") 'org-capture)

;;;; ---------------------------------------------------------------------------
;; 参考: http://d.hatena.ne.jp/tamura70/20100227/org
;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; DONEの時刻を記録
(setq org-log-done 'time)

(require 'org-habit)
