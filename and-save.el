;;; and-save.el --- Save keybindings and use-package declarations -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Ben MacLaurin <hi@benmaclaurin.com>
;; Maintainer: Ben MacLaurin <hi@benmaclaurin.com>
;; Created: 2024
;; Version: 0.01
;; Package-Requires: ((emacs "29") (use-package "2.4"))
;; Homepage: https://github.com/ben-maclaurin/and-save
;; Keywords: convenience, keybindings, packages

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; and-save provides two main features:
;; 1. global-set-key-and-save: Sets a global key binding and saves it to your init file
;; 2. use-package-and-save: Evaluates a use-package declaration and saves it to your init file
;;
;; Both functions automatically persist their changes to your init file.

;;; Code:

(require 'use-package)
(require 'package)

(defgroup and-save nil
  "Settings for and-save package."
  :prefix "and-save-"
  :group 'applications)

(defcustom and-save-init-file user-init-file
  "The init file where declarations will be saved."
  :type 'file
  :group 'and-save)

;;;###autoload
(defun global-set-key-and-save (key command)
  "Like global-set-key, but also appends the key binding to init file.
KEY is the key sequence to bind.
COMMAND is the command to bind it to."
  (interactive "KKey to bind: \nCCommand: ")
  (global-set-key key command)
  
  (let* ((key-str (key-description key))
         (binding-str (format "\n(global-set-key (kbd \"%s\") '%s)" 
                            key-str command))
         (init-file (or and-save-init-file "~/.emacs")))
    
    (with-temp-buffer
      (insert binding-str)
      (append-to-file (point-min) (point-max) init-file))
    
    (message "Bound %s to %s and saved to %s" 
             key-str command (file-name-nondirectory init-file))))

(defun and-save--get-available-packages ()
  "Get a list of available package names from package archives."
  (unless package-archive-contents
    (package-refresh-contents))
  (mapcar #'car package-archive-contents))

;;;###autoload
(defun use-package-and-save (package &rest args)
  "Like use-package, but also appends the declaration to init file.
PACKAGE is the package name to declare.
ARGS are the use-package keywords and their values."
  (interactive
   (list (intern
          (completing-read "Package: "
                          (mapcar #'symbol-name (and-save--get-available-packages))))
         (read-from-minibuffer "Arguments (in list form, e.g., (:ensure t :defer t)): ")))
  
  ;; Evaluate the use-package declaration
  (apply #'use-package-handler/:ensure package t args)
  
  (let* ((declaration (format "\n(use-package %s%s)"
                             package
                             (if args
                                 (concat " " (prin1-to-string args))
                               "")))
         (init-file (or and-save-init-file "~/.emacs")))
    
    ;; Save to init file
    (with-temp-buffer
      (insert declaration)
      (append-to-file (point-min) (point-max) init-file))
    
    (message "Declared use-package for %s and saved to %s"
             package (file-name-nondirectory init-file))))

;;;###autoload
(defmacro use-package-save (name &rest args)
  "A macro wrapper around use-package-and-save.
NAME is the package name.
ARGS are the use-package keywords and their values."
  (declare (indent defun))
  `(use-package-and-save ',name ',args))

(provide 'and-save)
;;; and-save.el ends here
