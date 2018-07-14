;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;(require 'package)
;(package-initialize)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; I keep all my emacs-related stuff under ~/emacs
;(defvar emacs-root (if (or (eq system-type 'cygwin)
;			 (eq system-type 'gnu/linux)
;			 (eq system-type 'linux))
;		 "/home/stevey/" 		 "c:/home/stevey/")
; "My home directory — the root of my personal emacs load-path.")

(setq default-directory "~/doc")

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))

;; -------------------------------------------------------------------
;; from https://github.com/LdBeth/Emacs-for-Noobs/blob/master/Emacs%20on%20Windows--invoking%20external%20programs.org
;;Emacs 在 Windows 上使用時要不可避免的調用外部程序，一般情況下是要把可執行文件的路 徑加入到 exec-path 裡面即可。如：
;;
; (add-to-list 'exec-path "~/ImageMagick-7.0.8-6-portable-Q16-x86/")

;;但是，對某些程序來說，上面的方法沒有產生應有的效果。winterTTr 提供了 一個比較省事的方法: emacs 調用外部命令的環境設置 ，主要原因是外部程序的調 用有時也需要 emacs 的子進程去調用，所以需要把路徑加入到 PATH 裡面。 把下面的代碼加進配置即可。

  (defun wttr/prepend-to-exec-path (path)
    "prepand the path to the emacs intenral `exec-path' and \"PATH\" env variable.
Return the updated `exec-path'"
    (setenv "PATH" (concat (expand-file-name path)
                           path-separator
                           (getenv "PATH")))
    (setq exec-path
          (cons (expand-file-name path)
                exec-path)))

  (mapc #'wttr/prepend-to-exec-path
        (reverse ( list
        ;; your path to program
        ;"~/ImageMagick-7.0.8-6-portable-Q16-x86/"
        ;"C:/Program Files (x86)/ImageMagick-6.9.10-Q8/"
        "~/ImageMagick-6.9.10-6-portable-Q16-x86/"
        ;(concat Emacs "program/Aspell/bin/")
           )))

;; -------------------------------------------------------------------

(load-file "~/.emacs.d/org-mode-settings.el")

; for image resize
;(setq mm-inline-large-images "resize")
(with-eval-after-load 'org
    (load-file "~/.emacs.d/org-mode-resize.el")
)
;; -------------------------------------------------------------------
