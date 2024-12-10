;;; lsp-projects.el --- project awareness for lsp-mode -*- lexical-binding:t -*-

;; Copyright (C) 2024 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/jcfk/lsp-projects

;; Package-Requires: ((emacs "29.4") (lsp-mode "9.0.1"))

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

;; See homepage.

;;; Code:

;; Customs

(defgroup lsp-projects nil
  "lsp-projects custom group"
  :group 'lsp-mode)

;; Client modifiers

(defun lsp-projects--modify-client (client)
  "Copy and modify CLIENT, according to its type."
  (let ((id (lsp--client-server-id client)))
    (lsp-projects--with-server-id id
      (if lsp-projects-id-client-modifiers
          (seq-reduce (lambda (a b) (funcall b a))
                      lsp-projects-id-client-modifiers client)
        client))))

(defun lsp-projects--lsp--filter-modified-clients (pred)
  "Like `lsp--filter-clients', but modified with `lsp-projects--modify-client'."
  (seq-filter pred
              (seq-map #'lsp-projects--modify-client
                       (hash-table-values lsp-clients))))

;; Auto-open hooks

(defun lsp-projects--open-all-files (files)
  "Open all FILES into `lsp--cur-workspace'.

Check first that FILE is not already open in the workspace."
  (let ((workspace-files
         (seq-map #'buffer-file-name
                  (lsp--workspace-buffers lsp--cur-workspace))))
    (seq-do
     (lambda (file)
       (when (not (member file workspace-files))
         (find-file-noselect file)))
     files)))

 ;; Check that project root is not home dir?
(defun lsp-projects--try-auto-open-after-did-open ()
  "Attempt auto-open of suitable project files after opening a single file.

To be used as an `lsp-after-open-hook'."
  (let ((id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace))))
    (lsp-projects--with-server-id id
      (when (and lsp-projects-id-auto-open lsp-projects-id-find-auto-open-files)
        (lsp-projects--open-all-files (funcall lsp-projects-id-find-auto-open-files))))))

;; Server integrations

(defconst lsp-projects--sibling-packages '(lsp-projects-pylsp)
  "List of features for sibling packages which will need to be loaded.")

(defun lsp-projects--load-sibling-packages ()
  "Load all sibling packages which are not yet loaded."
  (seq-do
   (lambda (feature)
     (when (not (featurep feature))
       (require feature)))
   lsp-projects--sibling-packages))

(defconst lsp-projects--with-server-id-capture-formats
  '("lsp-projects-%s-client-modifiers"
    "lsp-projects-%s-auto-open"
    "lsp-projects-%s-find-auto-open-files")
  "These are symbol name format strings for `lsp-projects--with-server-id'.

For a given format string s (ex. lsp-projects-%s-auto-open), the macro
`lsp-projects--with-server-id' with id (ex. pylsp) captures the symbol
(format s \"id\") (ex. lsp-projects-id-auto-open), giving it the value of the
symbol (format s id) (ex. lsp-projects-pylsp-auto-open).")

 ;; Probably not good form, but cool.
(defmacro lsp-projects--with-server-id (id &rest body)
  "Run BODY with anaphoric \"id\" symbols routed to their implementation.

Captured symbols are listed in `lsp-projects--with-server-id-capture-formats'."
  (declare (indent 1))
  `(let ,(seq-map
          (lambda (fmt)
            `(,(intern-soft (format fmt "id"))
              (symbol-value (intern-soft (format ,fmt ,id)))))
          lsp-projects--with-server-id-capture-formats)
     ,@body))

;; Minor mode

;;;###autoload
(define-minor-mode lsp-projects-mode
  "Toggle lsp-projects-mode to enable project-aware `lsp-mode' utilities."
  :group 'lsp-projects
  :global t
  (if lsp-projects-mode
      (progn
        ;; Load other files
        (lsp-projects--load-sibling-packages)

        ;; Client modification
        (advice-add 'lsp--filter-clients
                    :override
                    'lsp-projects--lsp--filter-modified-clients)

        ;; Auto-open hooks
        (add-hook 'lsp-after-open-hook
                  'lsp-projects--try-auto-open-after-did-open))

    (advice-remove 'lsp--filter-clients
                   'lsp-projects--lsp--filter-clients-advice)
    (remove-hook 'lsp-after-open-hook
                 'lsp-projects--try-auto-open-after-did-open)))

(provide 'lsp-projects)

;;; lsp-projects.el ends here
