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
        "~/ImageMagick-7.0.8-6-portable-Q16-x86/"
        ;(concat Emacs "program/Aspell/bin/")
           )))

;; -------------------------------------------------------------------
;; from https://emacs-china.org/t/topic/1933/48
;; Emacs 25.2.1, Windows7 32bit, Org 9.0.8. 亲测可用。 在Emacs的环境变量里加上Imagemagick（我使用的是它的便携版，官网下载，里面必需包括mogrify.exe程序），然后把jkitchin的程序代码写在配置中，就可以对图片进行缩放了。
;; fix by js.wang(Erik), modified to Emacs 25.3.1 + ImageMagick-7.0.8-6-portable-Q16-x86 (use mogrify)

(with-eval-after-load 'org
;; * Rescaling inline-images
;; This will eventually be obsolete if this makes it into org-mode
(defvar org-inline-image-resize-function
  #'org-inline-image-resize
  "Function that takes a filename and resize argument and returns
 a new filename pointing to the resized image.")


(defun org-inline-image-resize (fname resize-options)
  "Resize FNAME with RESIZE-OPTIONS.
RESIZE-OPTIONS are passed to \"mogrify resized-fname -resize resize-options\".
RESIZE-OPTIONS could be:

N% to scale the image by a percentage.
N to set the width, keeping the aspect ratio constant.
xN to set the height, keeping the aspect ratio constant.
NxM! to set the width and height, ignoring the aspect ratio.

See http://www.imagemagick.org/Usage/resize/#resize for more options."
  (let* ((md5-hash (with-temp-buffer (insert-file-contents fname)
				     (insert (format "%s" resize-options))
				     (md5 (buffer-string))))
	 (resized-fname (concat (expand-file-name
				 md5-hash
				 temporary-file-directory)
				"."
				(file-name-extension fname)))
	 (cmd (format "mogrify -resize %s %s"
		      resize-options
		      resized-fname)))
    (if (not (executable-find "mogrify"))
	(progn
	  (message "No mogrify executable found. To eliminate this message, set  `org-inline-image-resize-function' to nil or install imagemagick from http://www.imagemagick.org/script/binary-releases.php")
	  fname)
      (unless (file-exists-p resized-fname)
	(copy-file fname resized-fname)
	(shell-command cmd))
      resized-fname)))

;; this is copied and modified from org.el
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.  BEG and END default to the buffer
boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let* ((case-fold-search t)
	    (file-extension-re (image-file-name-regexp))
	    (link-abbrevs (mapcar #'car
				  (append org-link-abbrev-alist-local
					  org-link-abbrev-alist)))
	    ;; Check absolute, relative file names and explicit
	    ;; "file:" links.  Also check link abbreviations since
	    ;; some might expand to "file" links.
	    (file-types-re (format "[][]\\[\\(?:file\\|[./~]%s\\)"
				   (if (not link-abbrevs) ""
				     (format "\\|\\(?:%s:\\)"
					     (regexp-opt link-abbrevs))))))
       (while (re-search-forward file-types-re end t)
	 (let ((link (save-match-data (org-element-context))))
	   ;; Check if we're at an inline image, i.e., an image file
	   ;; link without a description (unless INCLUDE-LINKED is
	   ;; non-nil).
	   (when (and (equal "file" (org-element-property :type link))
		      (or include-linked
			  (null (org-element-contents link)))
		      (string-match-p file-extension-re
				      (org-element-property :path link)))
	     (let ((file (expand-file-name
			  (org-link-unescape
			   (org-element-property :path link)))))
	       (when (file-exists-p file)
		 (let ((width
			;; Apply `org-image-actual-width' specifications.
			(cond

            ;; fix for image resize
			 ((and (not (image-type-available-p 'imagemagick))
			       (not org-inline-image-resize-function))
			  nil)

			 ((eq org-image-actual-width t) nil)
			 ((listp org-image-actual-width)
			  (or
			   ;; First try to find a width among
			   ;; attributes associated to the paragraph
			   ;; containing link.
			   (let ((paragraph
				  (let ((e link))
				    (while (and (setq e (org-element-property
							 :parent e))
						(not (eq (org-element-type e)
							 'paragraph))))
				    e)))
			     (when paragraph
			       (save-excursion
				 (goto-char (org-element-property :begin paragraph))
				 (when
				     (re-search-forward
				      "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
				      (org-element-property
				       :post-affiliated paragraph)
				      t)
				   (match-string 1)))))
				   ;(string-to-number (match-string 1))))))
			   ;; Otherwise, fall-back to provided number.
			   (car org-image-actual-width)))
			 ((numberp org-image-actual-width)
			  org-image-actual-width)))
		       (old (get-char-property-and-overlay
			     (org-element-property :begin link)
			     'org-image-overlay)))
		   (if (and (car-safe old) refresh)
		       (image-refresh (overlay-get (cdr old) 'display))
		     
            ;; add for image resize
		     (when org-inline-image-resize-function
		       (setq file (funcall  org-inline-image-resize-function file width)
			     width nil))

		     (let ((image (create-image file
						(and width 'imagemagick)
						nil
						:width width)))
		       (when image
			 (let ((ov (make-overlay
				    (org-element-property :begin link)
				    (progn
				      (goto-char
				       (org-element-property :end link))
				      (skip-chars-backward " \t")
				      (point)))))
			   (overlay-put ov 'display image)
			   (overlay-put ov 'face 'default)
			   (overlay-put ov 'org-image-overlay t)
			   (overlay-put
			    ov 'modification-hooks
			    (list 'org-display-inline-remove-overlay))
			   (push ov org-inline-image-overlays)))))))))))))))
)

