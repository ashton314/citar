;;; citar-typst.el --- Typst support for citar -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;; This is a small package to make citar usable for Typst documents.

;;; Code:

(require 'citar)
(require 'treesit)

(defconst citar-typst-citation-key-regexp
  "@\\([-_[:alnum:]]+\\)"
  "Regular expression to match a Typst citation key.")

;;;###autoload
(defun citar-typst-local-bib-files ()
  (let ((query (treesit-query-compile
                'typst
                '((call
                   item: (_) @fn
                   (group (_) @bib-file)
                   (:equal @fn "bibliography"))))))
    (when-let* ((local-bib
                 (treesit-node-text
                  (cdr (assoc 'bib-file
                              (treesit-query-capture (treesit-buffer-root-node) query))))))
      (substring-no-properties local-bib 1 -1))))

;;;###autoload
(defun citar-typst-insert-keys ()
  "Insert space-separated and @-prefixed KEYS in a Typst buffer."
  (insert (mapconcat (lambda (k) (concat "@" k)) keys " ")))

;;;###autoload
(defun citar-typst-insert-citation (keys &optional invert-prompt command)
  "Insert a citation consisting of KEYS."
  ;; FIXME: if inside a citation, add after the current key.
  (insert (string-join (mapcar #'(lambda (k) (format "@%s" k)) keys) " ")))

;;;###autoload
(defun citar-typst-insert-edit (&optional _arg)
  "Prompt for keys and call `citar-typst-insert-citation."
  (citar-typst-insert-citation (citar-select-refs)))

;;;###autoload
(defun citar-typst-key-at-point ()
  (message "Implement me!")
  nil)

;;;###autoload
(defun citar-typst-citation-at-point ()
  (save-excursion
    (backward-word)
    (when (re-search-forward citar-typst-citation-key-regexp nil 'noerror)
      (match-string-no-properties 1))))

;;;###autoload
(defun citar-typst-list-keys ()
  (message "Implement me!")
  '())

(provide 'citar-typst)
