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
;; Well, here is a short story.
;;
;; "    People who pontificate about Unix's technical superiority often
;; don't mention what may ultimately be its most important strength,
;; the one that underlies all its successes. Unix is fun to hack.
;;
;;      Unix boosters seem almost ashamed to acknowledge this
;; sometimes, as though admitting they're having fun might damage
;; their legitimacy somehow. But it's true; Unix is fun to play with
;; and develop for, and always has been." (Eric S. Raymond, "The Art
;; of Unix programming.")
;;
;; The same is very true for GNU Emacs as well. It is really fun to
;; play with GNU Emacs and develop for.
;;
;; When I have spare time I like to investigate GNU Emacs. I could
;; read documentation, make some configuration customizations, reading
;; configuration files of others etc. The last ones are the source of
;; great findings.
;;
;; When I find some interesting code snippet I very often wish to try
;; it and thus add it to my .emacs configuration file. And I add these
;; snippets expecially if they are short and it is not hard to test
;; and see how they work. But sometimes I meet a huge configuration
;; files which I would like to also investigate and see how some
;; pieces will work for me but often I afraid to just copy/paste to my
;; .emacs file as sometimes there could be dependency breakages,
;; version incompatibility and other potential errors which could
;; appear exactly after code evaluation or after emacs
;; restarting. Such errors breaks work flow and force to comment out
;; or to resolve issues which very often are not important in
;; particular period of time.
;;
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
      (ef-load-efeature efeature))))

(provide 'ef)
