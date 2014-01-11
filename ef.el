(defcustom ef-efeature-sets-alist nil
  "Keeps efeature sets alist. `Key` is a name of a set and
  `values` are efeatures which are associated with that set. ")

(defcustom ef-path (expand-file-name "~/.emacs.d/efeatures/")
  "Path to the files which will be loaded as efeatures.")

(defun ef-load-file (file)
  "Save load elisp file. Output the errors if any."
  (condition-case err
      (load-file file)
    (error
     (message "Error in file %s while loading efeature: %s"
              file (error-message-string err))
     nil)))

(defun ef-load-feature (efeature)
  "Load given `efeature`."
  (let ((file (format "%s%s.el" ef-path (symbol-name efeature))))
    (if (file-readable-p file)
        (ef-load-file file)
      (message "File %s either doesn't exist or is not readable." file))))

(defun define-efeature-set (name efeatures)
  "Define a feature set with a `name` and list of `features`
which will be associated with the efeature set."
  (setq ef-efeature-sets-alist
        (acons name efeatures ef-efeature-sets-alist)))

(defun ef-load-efeature-set (efeature-set)
  "Load efeature set."
  (interactive)
  (let ((efeatures (cdr (assoc efeature-set ef-efeature-sets-alist))))
    (print efeatures)
    (dolist (efeature efeatures)
      (ef-load-feature efeature))))

(provide 'ef)
