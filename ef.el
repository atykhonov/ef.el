;;; ef.el --- a handy tool for maintaining GNU Emacs configuration.

;; Copyright (C) 2013 Andrey Tykhonov
;; Free Software Foundation, Inc.

;; Author: Andrey Tykhonov <atykhonov at gmail.com>
;; Version: 0.1.0
;; Keywords: ef.el

;; This file is NOT part of GNU Emacs.

;; ef.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ef.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ef.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This tool provides a handy way for maintaining growing GNU Emacs
;; configuration.
;;
;; Commands:
;;
;;

;;; Code:

(defgroup ef nil
  "A handy way in maintaining GNU Emacs configuration."
  :group 'extensions
  :group 'convenience
  :version "24.3"
  :link '(emacs-commentary-link "ef.el"))

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

(defun ef-load-efeature (efeature)
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
