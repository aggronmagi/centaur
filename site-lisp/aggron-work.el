;; aggron-wrok.el --- fast work note.	-*- lexical-binding: t -*-

(defgroup aggron-work nil
  "Fast Create record file"
  :group 'aggron-work)

(defcustom aggron-work-file '(format-time-string "%Y%m%d.org")
  "快速打开的文件名称"
  :group 'aggron-work)

(defcustom aggron-work-dir "~/org/work/record/"
  "文件目录"
  :type 'directory
  :group 'aggron-work)

(defcustom aggron-work-file-title '(concat "#+startup: showall\n"
										   "#+title: " (format-time-string "%Y-%m-%d\n"))
  "文件头"
  :group 'aggron-work)

;; (setq aggron-work-file-title '(concat "#+startup:  content\n"
;; 									 "#+title: " (format-time-string "%Y-%m-%d记录\n\n* ")))
;; (setq aggron-work-file '(format-time-string "%Y%m%d.org"))
;; (message (eval aggron-work-file-title))
;; (message (expand-file-name (concat aggron-work-dir (eval aggron-work-file))))

;;;###autoload
(defun aggron-work-open()
  "打开文件"
  (interactive)
  (let ((file-name (expand-file-name (concat aggron-work-dir (eval aggron-work-file)))))
    (if (file-exists-p file-name)
		(find-file file-name)
      (find-file file-name)
	  (insert (eval aggron-work-file-title))
	  (save-buffer)
	  (goto-char (point-max)))
    file-name)
  )

(global-set-key (kbd "<f8>") 'aggron-work-open)

(provide 'aggron-work)
