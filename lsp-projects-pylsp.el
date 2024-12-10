;;; lsp-projects-pylsp.el --- lsp-project pylsp integration -*- lexical-binding:t -*-

;; Copyright (C) 2024 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; pylsp integrations for lsp-projects

;;; Code:

(require 'lsp-projects)

;; Customs

(defgroup lsp-projects-pylsp nil
  "lsp-projects custom group for pylsp"
  :group 'lsp-projects)

(defcustom lsp-projects-pylsp-client-modifier-venv-path ".venv-lsp"
  "Path to the pylsp venv, relative to project root or absolute."
  :type 'string
  :group 'lsp-projects-pylsp)

(defcustom lsp-projects-pylsp-client-modifiers '()
  "List of modifiers for the pylsp client.

A client modifier is a function that takes an `lsp--client' and returns a
modified copy. Modifiers in this list will be applied from left to right to the
corresponding member of `lsp-clients' during `lsp--filter-clients'."
  :type '(repeat function)
  :group 'lsp-projects-pylsp)

(defcustom lsp-projects-pylsp-auto-open nil
  "If non-nil, try auto-open upon pylsp workspace init.

Saying try because auto-open only actually happens if
`lsp-projects-pylsp-find-auto-open-files' has a non-nil function value."
  :type 'boolean
  :group 'lsp-projects-pylsp)

(defcustom lsp-projects-pylsp-find-auto-open-files
  'lsp-projects--pylsp-find-auto-open-files-sensible
  "Auto-open file finder for pylsp workspaces."
  :type 'function
  :group 'lsp-projects-pylsp)

;; Client modifiers

(defun lsp-projects-pylsp-client-modifier-venv (client)
  "pylsp client modifier: point CLIENT to the project-local venv pylsp install.

Return an `lsp--client' copy with an updated :new-connection field."
  (let* ((root (project-root (project-current)))
         (venv-path lsp-projects-pylsp-client-modifier-venv-path)
         ;; (venv-abs-path (lsp-projects--with-python-venv venv-path
         ;;                  (executable-find "pylsp")))
         (venv-abs-path (expand-file-name
                         (file-name-concat
                          (if (file-name-absolute-p venv-path) "" root)
                          venv-path
                          "bin/pylsp")))
         (modified-client (copy-lsp--client client)))
    (setf (lsp--client-new-connection modified-client)
          (lsp-stdio-connection venv-abs-path))
    modified-client))

;; Auto-open

 ;; This can probably be generalized to other languages.
(defun lsp-projects--pylsp-find-auto-open-files-sensible ()
  "Return absolute paths of all project files suitable for pylsp.

What this does is get all files that both:

- are project files according to the project manager (project.el)
- would trigger `python-mode', according to `auto-mode-alist' (naively)"
  (let ((files (project-files (project-current)))
        (file-pattern (car (rassoc 'python-mode auto-mode-alist))))
    (seq-filter
     (lambda (file)
       (string-match-p file-pattern (file-name-nondirectory file)))
     files)))

(provide 'lsp-projects-pylsp)

;;; lsp-projects-pylsp.el ends here
