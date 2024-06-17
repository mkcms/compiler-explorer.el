;;; compiler-explorer.el --- Compiler explorer client (godbolt.org)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: c, tools
;; Version: 0.5.0
;; Homepage: https://github.com/mkcms/compiler-explorer.el
;; Package-Requires: ((emacs "27.1") (plz "0.9") (eldoc "1.15.0") (map "3.3.1") (seq "2.23"))

;; This program is free software; you can redistribute it and/or modify
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

;;
;;; compiler-explorer.el
;;
;; Package that provides a simple client for https://godbolt.org service.
;;
;;
;;; Usage
;;
;; M-x `compiler-explorer' is the main entry point.  It will ask you for a
;; language and display source&compilation buffers.  Type something in the
;; source buffer; the compilation buffer will automatically update with
;; compiled asm code.  Another buffer displays output of the compiled and
;; executed program.
;;
;; M-x `compiler-explorer-set-compiler' changes the compiler for current
;;session.
;;
;; M-x `compiler-explorer-set-compiler-args' sets compilation options.
;;
;; M-x `compiler-explorer-add-library' asks for a library version and adds
;; it to current compilation.  M-x `compiler-explorer-remove-library'
;; removes them.
;;
;; M-x `compiler-explorer-set-execution-args' sets the arguments for the
;; executed program.
;;
;; M-x `compiler-explorer-set-input' reads a string from minibuffer that
;; will be used as input for the executed program.
;;
;; M-x `compiler-explorer-jump' jumps to ASM block for the source line at
;; point and vice versa.
;;
;; M-x `compiler-explorer-load-example' prompts for a name of a builtin
;; example and loads it.
;;
;; M-x `compiler-explorer-new-session' kills the current session and
;; creates a new one, asking for source language.
;;
;; M-x `compiler-explorer-previous-session' lets you cycle between
;; previous sessions.
;;
;; M-x `compiler-explorer-make-link' generates a link for current
;; compilation so it can be opened in a browser and shared.
;;
;; M-x `compiler-explorer-layout' cycles between different layouts.
;;
;; M-x `compiler-explorer-exit' kills the current session.
;;
;;

;;; Code:

(require 'ansi-color)
(require 'browse-url)
(require 'color)
(require 'compile)
(require 'json)
(require 'plz)
(require 'pulse)
(require 'ring)
(require 'subr-x)
(require 'url-util)

;; `require'-ing these does not guarantee they are loaded as they are preloaded
;; in Emacs.
;;
;; This hack was stolen from the built-in eglot.el.
(eval-and-compile
  (if (< emacs-major-version 28)
      (progn
        (load "eldoc" nil 'nomessage)
        (load "seq" nil 'nomessage)
        (load "map" nil 'nomessage))
    (require 'eldoc)
    (require 'seq)
    (require 'map)))

(defgroup compiler-explorer nil "Client for compiler explorer service."
  :group 'tools)


;; API

(defvar compiler-explorer-url "https://godbolt.org")

(defun compiler-explorer--url (&rest chunks)
  "Make compiler explorer API endpoint URL from CHUNKS.
The last element can be an alist of (FIELD . VALUE) entries.
This alist will be encoded and appended to the URL as
URL parameters: ?FIELD1=VALUE1&FIELD2=VALUE2..."
  (let (params)
    (when-let ((last (car (last chunks)))
               (is-alist (listp last)))
      (setq params last chunks (butlast chunks)))
    (concat
     compiler-explorer-url "/api/" (string-join chunks "/")
     (when params
       (concat "?" (string-join (mapcar (pcase-lambda (`(,k . ,v))
                                          (concat (url-hexify-string k)
                                                  "="
                                                  (url-hexify-string v)))
                                        params)
                                "&"))))))

(defun compiler-explorer--parse-json ()
  "Parse buffer as json plist."
  (let ((json-object-type 'plist))
    (json-read)))

(cl-defun compiler-explorer--request-sync
    (what url &rest args
          &key
          (method 'get)
          (headers '(("Accept" . "application/json")))
          (as #'compiler-explorer--parse-json)
          &allow-other-keys)
  "Perform sync `plz' request for URL, displaying WHAT with progress reporter."
  (let ((pr (and what (make-progress-reporter what))))
    (unwind-protect
        (apply #'plz method url :headers headers :as as args)
      (and pr (progress-reporter-done pr)))))

(defvar compiler-explorer--languages nil)
(defun compiler-explorer--languages ()
  "Get all languages."
  (or compiler-explorer--languages
      (setq compiler-explorer--languages
            (compiler-explorer--request-sync
             "Fetching list of languages"
             (compiler-explorer--url
              "languages"
              `(("fields" . ,(string-join '("id" "name" "extensions" "example"
                                            "defaultCompiler")
                                          ","))))))))

(defvar compiler-explorer--compilers nil)
(defun compiler-explorer--compilers ()
  "Get all compilers."
  (or compiler-explorer--compilers
      (setq compiler-explorer--compilers
            (compiler-explorer--request-sync
             "Fetching list of compilers"
             (compiler-explorer--url
              "compilers"
              `(("fields" . ,(string-join '("id" "lang" "name" "groupName"
                                            "instructionSet"
                                            "supportsExecute"
                                            "supportsBinary"
                                            "supportsBinaryObject"
                                            "supportsLibraryCodeFilter"
                                            "supportsDemangle"
                                            "supportsIntel"
                                            "disabledFilters")
                                          ","))))))))

(defvar compiler-explorer--libraries (make-hash-table :test #'equal))
(defun compiler-explorer--libraries (id)
  "Get available libraries for language ID."
  (or (map-elt compiler-explorer--libraries id)
      (setf (map-elt compiler-explorer--libraries id)
            (compiler-explorer--request-sync
             (format "Fetching %S libraries" id)
             (compiler-explorer--url "libraries" id)))))

(defvar compiler-explorer--asm-opcode-docs-cache
  (make-hash-table :test #'equal)
  "Hash table mapping opcodes to their docs.

Keys are strings of the form \\='ISET:OPCODE\\=', where ISET is the
compiler's :instructionSet.

Values are strings, which contain the documentation for the
opcode.  Values can also be t, which means no documentation is
available for the key.")

(defun compiler-explorer--asm-opcode-doc (instruction-set opcode callback)
  "Get documentation for OPCODE in INSTRUCTION-SET and call CALLBACK.
Opcode should be a string.  INSTRUCTION-SET should be a valid
:instructionSet of some compiler.

The return value will be:
- nil if documentation is not available
- t if it is (and callback was called)
- the symbol \\='async if a request to get that documentation was
sent, but the documentation is not available yet."
  (let* ((key (format "%s:%s" instruction-set opcode))
         (cache compiler-explorer--asm-opcode-docs-cache)
         (resp (gethash key cache)))
    (cond
     ((stringp resp) (funcall callback resp) t)
     ((eq resp t) nil)
     (t
      (plz 'get
        (compiler-explorer--url "asm" instruction-set opcode)
        :headers '(("Accept" . "application/json"))
        :then (pcase-lambda ((map :tooltip))
                (funcall callback tooltip)
                (puthash key tooltip cache))
        :else (lambda (_err) (puthash key t cache))
        :as #'compiler-explorer--parse-json)
      'async))))

(defvar compiler-explorer--examples nil)
(defun compiler-explorer--examples (&optional lang)
  "Get an alist of the examples.
Keys are example names, values are example objects as returned by the API.
If LANG is non-nil, return only examples for language with that id."
  (let ((examples
         (or compiler-explorer--examples
             (setq compiler-explorer--examples
                   (compiler-explorer--request-sync
                    (format "Fetching %S examples" (or lang "all"))
                    (concat compiler-explorer-url "/source/builtin/list"))))))
    (remq 'none
          (mapcar
           (lambda (example)
             (or
              (and lang (not (string= lang (plist-get example :lang))) 'none)
              (cons (plist-get example :name) example)))
           examples))))

(defvar compiler-explorer--cached-example-data (make-hash-table :test #'equal)
  "Keys are strings of the form LANG:EXAMPLE-FILE.
Values are the example objects from API.")

(defun compiler-explorer--example (lang file)
  "Get a single example FILE in LANG."
  (let ((key (format "%s:%s" lang file)))
    (or (map-elt compiler-explorer--cached-example-data key)
        (setf
         (map-elt compiler-explorer--cached-example-data key)
         (compiler-explorer--request-sync
          (format "Fetching %S example %s" lang file)
          (concat
           compiler-explorer-url "/source/builtin/load/" lang "/" file))))))


;; Compilation

(defconst compiler-explorer--buffer "*compiler-explorer*"
  "Buffer with source code.")

(defconst compiler-explorer--compiler-buffer "*compiler-explorer compilation*"
  "Buffer with ASM code.")

(defconst compiler-explorer--output-buffer "*compiler-explorer output*"
  "Combined compiler stdout&stderr.")

(defconst compiler-explorer--exe-output-buffer
  "*compiler-explorer execution output*"
  "Buffer with execution output.")

(defvar compiler-explorer--language-data nil
  "Language data for current session.")

(defvar compiler-explorer--compiler-data nil
  "Compiler data for current session.")

(defvar compiler-explorer--selected-libraries nil
  "Alist of libraries for current session.
Keys are library ids, values are lists (VERSION ENTRY), where
VERSION is the id string of the version and ENTRY is the library
entry from function `compiler-explorer--libraries'.")

(defvar compiler-explorer--compiler-arguments ""
  "Arguments for the compiler.")

(defvar compiler-explorer--execution-arguments ""
  "Arguments for the program executed.")

(defvar compiler-explorer--execution-input ""
  "Stdin for the program executed.")

(defvar compiler-explorer--last-compilation-request nil
  "Last request (response) for current compilation.")

(defvar compiler-explorer--last-exe-request nil
  "Last request (response) for current execution.")

(defvar compiler-explorer-response-limit-bytes (* 1000 1000)
  "Limit in bytes for responses to compilation requests.
If a compilation response is larger than this, it is not parsed
with `json-parse', and a message is displayed.")

(defun compiler-explorer--parse-json-compilation ()
  "Parse current buffer as json, but only if it's size is reasonable."
  (cond
   ((< (buffer-size) compiler-explorer-response-limit-bytes)
    (compiler-explorer--parse-json))
   (t
    `(:asm [(:text ,(format
                     "ERROR: Response too large to parse. (%s kB, limit %s kB)"
                     (/ (buffer-size) 1000)
                     (/ compiler-explorer-response-limit-bytes 1000)))
            (:text "Increase the limit by setting ")
            (:text "`compiler-explorer-response-limit-bytes'")]))))

(defcustom compiler-explorer-output-filters '(:binary nil
                                                      :binaryObject nil
                                                      :commentOnly t
                                                      :demangle t
                                                      :directives t
                                                      :intel t
                                                      :labels t
                                                      :libraryCode t
                                                      :trim nil
                                                      :debugCalls nil)
  "Compiler output filters."
  :type '(plist :key-type
                (choice
                 (const :tag "Compile to binary" :binary)
                 (const :tag "Compile to binary object" :binaryObject)
                 (const :tag "Comments" :commentOnly)
                 (const :tag "Demangle C++ symbols" :demangle)
                 (const :tag "Directives" :directives)
                 (const :tag "Intel ASM syntax" :intel)
                 (const :tag "Unused labels" :labels)
                 (const :tag "Library code" :libraryCode)
                 (const :tag "Trim whitespace" :trim)
                 (const :tag "Debug intrinsics" :debugCalls))
                :value-type boolean))

(defun compiler-explorer--filter-enabled-p (filter)
  "Return non-nil if FILTER can be used in the current session."
  (pcase-let (((map :supportsBinary :supportsBinaryObject
                    :supportsLibraryCodeFilter :supportsDemangle
                    :supportsIntel
                    :disabledFilters)
               compiler-explorer--compiler-data))
    (and (cl-case filter
           (:binary (eq t supportsBinary))
           (:binaryObject (eq t supportsBinaryObject))
           (:libraryCode (eq t supportsLibraryCodeFilter))
           (:demangle (eq t supportsDemangle))
           (:intel (eq t supportsIntel))
           (t t))
         (not (seq-contains-p disabledFilters
                              (substring (symbol-name filter) 1))))))

(defun compiler-explorer--output-filters ()
  "Get output filters options in a form suitable for making a request."
  (cl-loop for (k v) on compiler-explorer-output-filters by #'cddr
           if (compiler-explorer--filter-enabled-p k)
           nconc (list k (or v :json-false))))

(defvar compiler-explorer--inhibit-request nil
  "If non-nil, inhibit making the async compilation/execution request.
This can be temporarily let-bound to defer making async requests
when multiple functions try to do it in a block of code.")

(defun compiler-explorer--request-async-1 ()
  "Subr of `compiler-explorer--request-async'."
  (pcase-dolist
      (`(,executorRequest ,symbol ,handler)
       `((:json-false
          compiler-explorer--last-compilation-request
          compiler-explorer--handle-compilation-response)
         ,@(when (eq t (plist-get compiler-explorer--compiler-data
                                  :supportsExecute))
             '((t
                compiler-explorer--last-exe-request
                compiler-explorer--handle-execution-response)))))
    (when-let ((last (symbol-value symbol)))
      ;; Abort last request
      (when (process-live-p last)
        (delete-process last)))
    (let (proc)
      (setq
       proc
       (set symbol
            (plz 'post
              (compiler-explorer--url
               "compiler" (plist-get compiler-explorer--compiler-data :id)
               "compile")
              :headers '(("Accept" . "application/json")
                         ("Content-Type" . "application/json"))
              :body (let ((json-object-type 'plist))
                      (json-encode
                       `(
                         :source
                         ,(with-current-buffer compiler-explorer--buffer
                            (buffer-string))
                         :options
                         (
                          :userArguments ,compiler-explorer--compiler-arguments
                          :executeParameters
                          (
                           :args ,compiler-explorer--execution-arguments
                           :stdin ,compiler-explorer--execution-input)
                          :compilerOptions
                          (
                           :skipAsm :json-false
                           :executorRequest ,executorRequest)
                          :filters ,(compiler-explorer--output-filters)
                          :tools []
                          :libraries
                          [,@(mapcar
                              (pcase-lambda (`(,id ,version ,_))
                                `(:id ,id :version ,version))
                              compiler-explorer--selected-libraries)])
                         :allowStoreCodeDebug :json-false)))
              :as #'compiler-explorer--parse-json-compilation
              :then (lambda (resp)
                      (unless (plz-error-p resp)
                        (funcall handler resp)
                        (process-put proc 'compiler-explorer-response-data
                                     resp))))))))
  (unless (eq t (plist-get compiler-explorer--compiler-data :supportsExecute))
    (setq compiler-explorer--last-exe-request nil)
    (compiler-explorer--handle-compiler-with-no-execution))
  (compiler-explorer--build-overlays nil)
  (force-mode-line-update))

(defun compiler-explorer--request-async ()
  "Queue compilation and execution and return immediately.
This calls `compiler-explorer--handle-compilation-response' and
`compiler-explorer--handle-execution-response' once the responses arrive."
  (unless compiler-explorer--inhibit-request
    (compiler-explorer--request-async-1)))

(defvar compiler-explorer--project-dir)

;; TODO: This variable is present only because in Emacs-26
;; `replace-buffer-contents' does not take optional arguments that allow
;; controlling performance.
;;
;; It should be removed when we set minimum required version to Emacs-27 or
;; later.
(defcustom compiler-explorer-replace-insert-nondestructively 25000
  "Replace buffer contents nondestructively if it's size is less than this.

When handling compilation response, the disassembled code is
inserted into a temporary buffer; if the size of this temporary
buffer is less than this, and if the size of the current
disassembly buffer is less than this value, the contents of the
current disassembly buffer are replaced with the contents of the
temporary buffer with `replace-buffer-contents'.  This is slow
for large buffers, but has the advantage of properly preserving
point.

If the size of any of the two buffers is larger than this, the
contents are replaced destructively and point is not preserved."
  :type 'integer)

(defun compiler-explorer--replace-buffer-contents (target source)
  "Replace contents of buffer TARGET with SOURCE."
  (let ((limit compiler-explorer-replace-insert-nondestructively))
    (with-current-buffer target
      (let ((inhibit-read-only t))
        ;; We need to remove all overlays applied by ansi-color first,
        ;; otherwise sometimes `replace-buffer-contents' will improperly merge
        ;; the existing ANSI-coded regions with the new text.
        (delete-all-overlays)
        (set-text-properties (point-min) (point-max) nil)

        (if (and (< (buffer-size target) limit)
                 (< (buffer-size source) limit))
            (replace-buffer-contents source)
          (erase-buffer)
          (insert-buffer-substring source))))))

(defvar compiler-explorer-document-opcodes)
(defvar compiler-explorer-source-to-asm-mappings)
(defun compiler-explorer--handle-compilation-response (response)
  "Handle compilation response contained in RESPONSE."
  (pcase-let (((map :asm :stdout :stderr :code) response)
              (compiler (get-buffer compiler-explorer--compiler-buffer))
              (output (get-buffer compiler-explorer--output-buffer))
              (source-to-asm-mappings nil))
    (with-current-buffer compiler
      (with-temp-buffer
        (seq-do
         (pcase-lambda ((map :text (:source (and source (map :line :file)))))
           (let (mapping)
             (when (and compiler-explorer-source-to-asm-mappings line
                        (plist-member source :file) (null file) (> line 0))
               (if (setq mapping (assq line source-to-asm-mappings))
                   (push (point) (cdr mapping))
                 (push (cons line (list (point))) source-to-asm-mappings))))
           (insert text "\n"))
         asm)
        (compiler-explorer--replace-buffer-contents compiler (current-buffer)))

      (when compiler-explorer-source-to-asm-mappings
        (compiler-explorer--build-overlays source-to-asm-mappings)))

    ;; Update output buffer
    (with-current-buffer output
      (with-temp-buffer
        (insert (mapconcat (lambda (line) (plist-get line :text))
                           stdout "\n")
                "\n")
        (insert (mapconcat (lambda (line) (plist-get line :text))
                           stderr "\n")
                "\n")
        (insert (format "Compiler exited with code %s" code))
        (compiler-explorer--replace-buffer-contents output (current-buffer)))
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))

      (with-demoted-errors "compilation-parse-errors: %s"
        (let ((inhibit-read-only t))
          (compilation-parse-errors (point-min) (point-max))))))
  (force-mode-line-update t))

(defun compiler-explorer--handle-compiler-with-no-execution ()
  "Update the execution output buffer with info about unsupported compiler.
This will write the list of supported compilers in the execution
output buffer."
  (with-current-buffer compiler-explorer--exe-output-buffer
    (let ((inhibit-read-only t)
          (keymap (make-keymap))
          (compiler (plist-get compiler-explorer--compiler-data :name))
          (lang-id (plist-get compiler-explorer--language-data :id)))
      (define-key keymap [mouse-1] 'compiler-explorer-set-compiler)
      (erase-buffer)
      (save-excursion
        (insert (format "Error: The %s compiler does not support execution."
                        (propertize compiler 'face 'underline)))
        (insert "\n\n")
        (insert "The current language supports execution with these "
                "compilers (click to set):\n")

        (seq-do
         (pcase-lambda ((map :lang :id :supportsExecute :name))
           (when (and (eq t supportsExecute) (string= lang lang-id))
             (insert " "
                     (propertize name
                                 'compiler-explorer-compiler-id id
                                 'face 'link
                                 'mouse-face 'highlight
                                 'keymap keymap
                                 'help-echo "Click to set")
                     "\n")))
         (compiler-explorer--compilers))))))

(defun compiler-explorer--handle-execution-response (response)
  "Handle execution response contained in RESPONSE."
  (pcase-let (((map :stdout :stderr :code) response))
    (with-current-buffer compiler-explorer--exe-output-buffer
      (let ((inhibit-read-only t)
            (buf (current-buffer)))
        (with-temp-buffer
          (insert "Program stdout:\n")
          (insert (mapconcat (lambda (line) (plist-get line :text))
                             stdout "\n")
                  "\n")
          (insert "Program stderr:\n")
          (insert (mapconcat (lambda (line) (plist-get line :text))
                             stderr "\n")
                  "\n")
          (insert (format "Program exited with code %s" code))
          (compiler-explorer--replace-buffer-contents buf (current-buffer)))
        (ansi-color-apply-on-region (point-min) (point-max))))))


;; UI

(defun compiler-explorer--header-line-format-common ()
  "Get the mode line template used in compiler explorer mode."
  (let* ((is-exe (eq (current-buffer)
                     (get-buffer compiler-explorer--exe-output-buffer)))
         (resp (if is-exe
                   compiler-explorer--last-exe-request
                 compiler-explorer--last-compilation-request)))
    (propertize
     (concat "CE: "
             (cond
              ((and (null resp) is-exe
                    (eq :json-false
                        (plist-get compiler-explorer--compiler-data
                                   :supportsExecute)))
               (propertize "ERROR" 'face 'error))
              ((null resp) "")
              ((process-live-p resp) "Wait...")
              ((/= 0 (process-exit-status resp))
               (propertize "ERROR" 'face 'error
                           'help-echo (format
                                       "Status: %s\nCode: %s"
                                       (process-status resp)
                                       (process-exit-status resp))))
              (t
               (pcase-let (((map :stdout :stderr)
                            (process-get resp
                                         'compiler-explorer-response-data)))
                 (propertize
                  (format "%s (%s/%s)"
                          (propertize "Done" 'face 'success)
                          (length stdout)
                          (propertize (format "%s" (length stderr))
                                      'face (if (> (length stderr) 0) 'error)))
                  'help-echo (with-current-buffer
                                 compiler-explorer--output-buffer
                               ;; Get at most 30 output lines
                               (save-excursion
                                 (goto-char (point-min))
                                 (forward-line 30)
                                 (concat
                                  (buffer-substring (point-min)
                                                    (line-beginning-position))
                                  (unless (= (line-beginning-position)
                                             (point-max))
                                    (concat "... message truncated. "
                                            "See output buffer to "
                                            "show all.\n"))
                                  "\nmouse-1: "
                                  " Show output buffer."))))))))
     'mouse-face 'mode-line-highlight
     'keymap (let ((map (make-keymap)))
               (define-key map [header-line mouse-1]
                 #'compiler-explorer-show-output)
               map))))

(defvar compiler-explorer--last-layout)
(defvar compiler-explorer-layouts)

(defun compiler-explorer--header-line-format-source ()
  "Get mode line construct for displaying header line in source buffer."
  `(
    (:eval (compiler-explorer--header-line-format-common))
    " | "
    ,(propertize
      (plist-get compiler-explorer--language-data :name)
      'mouse-face 'header-line-highlight
      'keymap (let ((map (make-keymap)))
                (define-key map [header-line mouse-1]
                            #'compiler-explorer-new-session)
                (define-key map [header-line mouse-2]
                            #'compiler-explorer-previous-session)
                map)
      'help-echo (concat "mouse-1: New session\n"
                         "mouse-2: Previous session"))
    " | "
    ,(propertize
      (format "Layout: %d" compiler-explorer--last-layout)
      'mouse-face 'header-line-highlight
      'keymap (let ((map (make-keymap)))
                (define-key map [header-line mouse-1]
                            (lambda ()
                              (interactive)
                              (compiler-explorer-layout
                               (1+ compiler-explorer--last-layout))))
                (define-key map [header-line mouse-2]
                            (lambda ()
                              (interactive)
                              (compiler-explorer-layout
                               (if (zerop compiler-explorer--last-layout)
                                   (1- (length compiler-explorer-layouts))
                                 (1- compiler-explorer--last-layout)))))
                map)
      'help-echo (concat "mouse-1: Next layout\n"
                         "mouse-2: Previous layout"))
    ))

(defun compiler-explorer--header-line-format-compiler ()
  "Get mode line construct for displaying header line in compilation buffers."
  `(
    (:eval (compiler-explorer--header-line-format-common))
    " | "
    ,(propertize
      (plist-get compiler-explorer--compiler-data :name)
      'mouse-face 'header-line-highlight
      'keymap (let ((map (make-keymap)))
                (define-key map [header-line mouse-1]
                  #'compiler-explorer-set-compiler)
                map)
      'help-echo "mouse-1: Select compiler")
    " | "
    ,(propertize
      (format "Libs: %s"  (length compiler-explorer--selected-libraries))
      'mouse-face 'header-line-highlight
      'keymap (let ((map (make-keymap)))
                (define-key map [header-line mouse-1]
                            #'compiler-explorer-add-library)
                (define-key map [header-line mouse-2]
                            #'compiler-explorer-remove-library)
                map)
      'help-echo (concat
                  "Libraries:\n"
                  (mapconcat
                   (pcase-lambda (`(,_ ,vid ,(map :name :versions)))
                     (format "%s %s" name
                             (cl-loop for version across versions
                                      for id = (plist-get version :id)
                                      when (string= id vid)
                                      return (plist-get version :version))))
                   compiler-explorer--selected-libraries
                   "\n")
                  "\n\n"
                  "mouse-1: Add library\n"
                  "mouse-2: Remove library\n"))
    " | "
    ,(propertize (format "Args: '%s'" compiler-explorer--compiler-arguments)
                 'mouse-face 'header-line-highlight
                 'keymap (let ((map (make-keymap)))
                           (define-key map [header-line mouse-1]
                             #'compiler-explorer-set-compiler-args)
                           map)
                 'help-echo "mouse-1: Set arguments")))

(defun compiler-explorer--header-line-format-executor ()
  "Get mode line construct for displaying header line in execution buffers."
  `(
    (:eval (compiler-explorer--header-line-format-common))
    " | "
    ,(propertize
      (format "Input: %s chars" (length compiler-explorer--execution-input))
      'mouse-face 'header-line-highlight
      'keymap (let ((map (make-keymap)))
                (define-key map [header-line mouse-1]
                  #'compiler-explorer-set-input)
                map)
      'help-echo "mouse-1: Set program input")
    " | "
    ,(propertize
      (format "Args: '%s'" compiler-explorer--execution-arguments)
      'mouse-face 'header-line-highlight
      'keymap (let ((map (make-keymap)))
                (define-key map [header-line mouse-1]
                  #'compiler-explorer-set-execution-args)
                map)
      'help-echo "mouse-1: Set program arguments")))

(defvar compiler-explorer-mode-map)

(defun compiler-explorer--define-menu ()
  "Define a menu in the menu bar for `compiler-explorer' commands."
  (easy-menu-define compiler-explorer-menu
    compiler-explorer-mode-map "Compiler Explorer"
    `("Compiler Explorer"
      ["Previous session" compiler-explorer-previous-session]
      ("New session"
       ,@(mapcar
          (pcase-lambda ((map :name))
            (vector name (lambda ()
                           (interactive)
                           (compiler-explorer-new-session name t))))
          (seq-sort-by (lambda (lang) (plist-get lang :name))
                       #'string<
                       (compiler-explorer--languages))))
      ("Load example"
       ,@(mapcar
          (lambda (name)
            (vector name (lambda ()
                           (interactive)
                           (compiler-explorer-load-example name))))
          (mapcar #'car
                  (compiler-explorer--examples
                   (plist-get compiler-explorer--language-data :id)))))
      "--"
      ("Compiler"
       ,@(let ((compilers
                (cl-remove-if
                 (pcase-lambda ((map :lang))
                   (not
                    (string= lang (plist-get
                                   compiler-explorer--language-data :id))))
                 (compiler-explorer--compilers)))
               (by-group (make-hash-table :test #'equal)))
           (cl-loop for compiler across compilers
                    for name = (plist-get compiler :name)
                    for group-name = (plist-get compiler :groupName)
                    for group = (map-elt by-group group-name)
                    if group do (nconc group (list name))
                    else do (setf (map-elt by-group group-name) (list name)))

           (seq-sort-by
            #'car #'string<
            (map-apply
             (lambda (group compilers-in-group)
               (cl-list*
                (if (string-empty-p group) "Other compilers" group)
                (mapcar (lambda (name)
                          (vector name
                                  (lambda ()
                                    (interactive)
                                    (compiler-explorer-set-compiler name))))
                        compilers-in-group)))
             by-group))))
      ["Set compilation arguments" compiler-explorer-set-compiler-args]
      ("Add library"
       :enable (not
                (seq-empty-p
                 (compiler-explorer--libraries
                  (plist-get compiler-explorer--language-data :id))))
       ,@(mapcar
          (pcase-lambda ((map :name :id :versions))
            (cl-list*
             name
             :enable `(null (assoc ,id compiler-explorer--selected-libraries))
             (mapcar
              (pcase-lambda ((map :version (:id version-id)))
                (vector
                 (format "%s %s" name version)
                 (lambda ()
                   (interactive)
                   (compiler-explorer-add-library id version-id))))
              versions)))
          (compiler-explorer--libraries
           (plist-get compiler-explorer--language-data :id))))
      ("Remove library"
       :enable (not (null compiler-explorer--selected-libraries))
       ,@(mapcar
          (pcase-lambda (`(,library-id ,version-id ,(map :name :versions)))
            (vector (format "%s %s"
                            name
                            (cl-loop for version across versions
                                     for id = (plist-get version :id)
                                     when (string= id version-id)
                                     return (plist-get version :version)))
                    (lambda ()
                      (interactive)
                      (compiler-explorer-remove-library library-id))))
          compiler-explorer--selected-libraries))
      "--"
      ["Set execution arguments" compiler-explorer-set-execution-args]
      ["Set execution input" compiler-explorer-set-input]
      "--"
      ("Output filters"
       ,@(cl-loop
          for (key v) on compiler-explorer-output-filters by #'cddr
          for is-enabled = (compiler-explorer--filter-enabled-p key)
          with name-alist =
          (cl-loop for elt in
                   (cdaddr
                    (get 'compiler-explorer-output-filters 'custom-type))
                   collect (cons (car (last elt)) (car (last elt 2))))
          collect (vector (or (cdr (assoc key name-alist)))
                          `(lambda ()
                             (interactive)
                             (setq compiler-explorer-output-filters
                                   (plist-put compiler-explorer-output-filters
                                              ,key (not ,v)))
                             (compiler-explorer--request-async)
                             (compiler-explorer--define-menu))
                          :style 'toggle
                          :selected v
                          :enable is-enabled)))
      ["Source to ASM mappings"
       (lambda ()
         (interactive)
         (setq compiler-explorer-source-to-asm-mappings
               (not compiler-explorer-source-to-asm-mappings))
         (compiler-explorer--request-async))
       :style toggle :selected compiler-explorer-source-to-asm-mappings]
      ["Next layout" compiler-explorer-layout]
      ["Copy link to this session" compiler-explorer-make-link]
      "--"
      ["Exit" compiler-explorer-exit])))

;; Other internal functions

(defvar compiler-explorer--last-session)
(defvar compiler-explorer-mode)
(defvar compiler-explorer--recompile-timer)
(defvar compiler-explorer--cleaning-up nil)

(defun compiler-explorer--cleanup-1 (&optional skip-save-session)
  "Kill current session.
If SKIP-SAVE-SESSION is non-nil, don't attempt to save the last session."
  (if (and
       (not skip-save-session)
       (buffer-live-p (get-buffer compiler-explorer--buffer))
       (stringp (plist-get compiler-explorer--language-data :example))
       (not (string=
             (string-trim
              (plist-get compiler-explorer--language-data :example))
             (with-current-buffer compiler-explorer--buffer
               (string-trim (buffer-string))))))
      ;; Save last session.  Don't insert it into the ring, as that would make
      ;; us cycle between only 2 sessions when calling
      ;; `compiler-explorer-previous-session'.
      ;;
      ;; Don't save it if it is unmodified from example.
      (setq compiler-explorer--last-session
            (compiler-explorer--current-session))
    (setq compiler-explorer--last-session nil))

  ;; Abort last request and cancel the timer for recompilation.
  (with-demoted-errors "compiler-explorer--cleanup: %s"
    (when-let ((req compiler-explorer--last-compilation-request))
      (when (process-live-p req)
        (delete-process req)))
    (when-let ((req compiler-explorer--last-exe-request))
      (when (process-live-p req)
        (delete-process req)))
    (when compiler-explorer--recompile-timer
      (cancel-timer compiler-explorer--recompile-timer)))

  ;; Kill all of our buffers.
  (mapc (lambda (buffer)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((kill-buffer-query-functions nil))
                ;; Give `kill-buffer-hook' a chance to run, but if they fail to
                ;; kill the buffer, kill it forcibly without running them.
                (unless (save-current-buffer
                          (ignore-errors (kill-buffer buffer)))
                  (let ((kill-buffer-hook nil))
                    (kill-buffer (current-buffer))))))))
        (list (get-buffer compiler-explorer--buffer)
              (get-buffer compiler-explorer--compiler-buffer)
              (get-buffer compiler-explorer--output-buffer)
              (get-buffer compiler-explorer--exe-output-buffer)))

  (setq compiler-explorer--last-compilation-request nil)
  (setq compiler-explorer--recompile-timer nil)
  (setq compiler-explorer--last-exe-request nil)
  (setq compiler-explorer--compiler-data nil)
  (setq compiler-explorer--selected-libraries nil)
  (setq compiler-explorer--language-data nil)
  (setq compiler-explorer--compiler-arguments "")
  (setq compiler-explorer--execution-arguments "")
  (setq compiler-explorer--execution-input "")

  (when compiler-explorer--project-dir
    (with-demoted-errors "compiler-explorer--cleanup: delete-directory: %s"
      (delete-directory compiler-explorer--project-dir t)))
  (setq compiler-explorer--project-dir nil)

  (when compiler-explorer-mode
    (compiler-explorer-mode -1)))

(defun compiler-explorer--cleanup (&optional skip-save-session)
  "Kill current session.
If SKIP-SAVE-SESSION is non-nil, don't attempt to save the last session."
  (unless compiler-explorer--cleaning-up
    (let ((compiler-explorer--cleaning-up t))
      (compiler-explorer--cleanup-1 skip-save-session))))


;; Source<->ASM overlays

(defun compiler-explorer--overlay-bg-base (percent)
  "Get the color for overlay background, PERCENT darker from default."
  (when-let ((bg (face-background 'default nil t)))
    (unless (string= bg "unspecified-bg")
      (color-darken-name bg percent))))

(defface compiler-explorer-1
  `((t :background ,(compiler-explorer--overlay-bg-base 46)
       :extend t))
  "One of the faces used for coloring code&ASM regions.")

(defface compiler-explorer-2
  `((t :background ,(compiler-explorer--overlay-bg-base 28)
       :extend t))
  "One of the faces used for coloring code&ASM regions.")

(defface compiler-explorer-3
  `((t :background ,(compiler-explorer--overlay-bg-base 17)
       :extend t))
  "One of the faces used for coloring code&ASM regions.")

(defface compiler-explorer-4
  `((t :background ,(compiler-explorer--overlay-bg-base 10)
       :extend t))
  "One of the faces used for coloring code&ASM regions.")

(defface compiler-explorer-5
  `((t :background ,(compiler-explorer--overlay-bg-base 6)
       :extend t))
  "One of the faces used for coloring code&ASM regions.")

(defcustom compiler-explorer-source-to-asm-mappings t
  "If non-nil, decorate the source and ASM buffers.
The added overlays show which portion of source code maps to ASM
instructions.  Calling `compiler-explorer-jump' when point is
inside one of these colored blocks jumps to and highlights the
corresponding overlay in the other buffer."
  :type 'boolean)

(defun compiler-explorer--build-overlays (regions)
  "Add source<->ASM mapping overlays in REGIONS.
REGIONS should be a list of conses (LINE . POINTS), where LINE is
the line number in source buffer, and POINTS is a list of points
that are inside lines in the ASM buffer that map to this source
line."
  (with-current-buffer compiler-explorer--compiler-buffer
    (remove-overlays nil nil 'compiler-explorer--overlay t))
  (with-current-buffer compiler-explorer--buffer
    (remove-overlays nil nil 'compiler-explorer--overlay t))

  (setq regions (sort regions #'car-less-than-car))

  (let ((faces (list 'compiler-explorer-1 'compiler-explorer-2
                     'compiler-explorer-3 'compiler-explorer-4
                     'compiler-explorer-5))
        face
        source-overlay asm-overlays
        prev-ov)
    (pcase-dolist (`(,line-num . ,points-in-asm) regions)
      (setq face (car faces))
      (setq faces (append (cdr faces) (list face)))
      (setq asm-overlays nil source-overlay nil)

      (with-current-buffer compiler-explorer--buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- line-num))
            (let ((ov (make-overlay (line-beginning-position)
                                    (line-beginning-position 2))))
              (overlay-put ov 'compiler-explorer--overlay t)
              (overlay-put ov 'compiler-explorer--overlay-group (list ov))
              (overlay-put ov 'face face)
              (overlay-put ov 'priority -100)
              (setq source-overlay ov)))))

      (with-current-buffer compiler-explorer--compiler-buffer
        (dolist (pt points-in-asm)
          (goto-char pt)
          (setq prev-ov (car asm-overlays))
          (cond
           ;; Merge adjacent overlays
           ((and prev-ov (= (overlay-end prev-ov)
                            (line-beginning-position)))
            (move-overlay prev-ov (overlay-start prev-ov)
                          (line-beginning-position 2)))
           ((and prev-ov (= (overlay-start prev-ov)
                            (line-beginning-position 2)))
            (move-overlay prev-ov (line-beginning-position)
                          (overlay-end prev-ov)))
           (t
            (let ((ov (make-overlay (line-beginning-position)
                                    (line-beginning-position 2))))
              (overlay-put ov 'compiler-explorer--overlay t)
              (overlay-put ov 'compiler-explorer--target source-overlay)
              (overlay-put ov 'face face)
              (overlay-put ov 'priority -100)

              (push ov asm-overlays))))))

      (setq asm-overlays (seq-sort-by #'overlay-start #'< asm-overlays))
      (dolist (ov asm-overlays)
        (overlay-put ov 'compiler-explorer--overlay-group asm-overlays))
      (overlay-put source-overlay 'compiler-explorer--target
                   (car asm-overlays)))))


;; Stuff/hacks for integration with other packages

(defcustom compiler-explorer-make-temp-file t
  "If non-nil, make a temporary file/dir for a `compiler-explorer' session.
This is required for integration with some other packages, for
example `compilation-mode' - with this, you can navigate to
errors in the source buffer by clicking on the links in compiler
output buffer.

This also sets up a transient project for the source buffer, so
you can use packages that require one.

When the session is killed, the temporary directory is deleted."
  :type 'boolean)

(defvar compiler-explorer--project-dir nil)
(defun compiler-explorer--project-find-function (_dir)
  "Return project with a temporary directory in a compiler explorer session."
  (and compiler-explorer--project-dir
       `(transient . ,compiler-explorer--project-dir)))

(defvar compiler-explorer--filename-regexp "<source>\\|\\(example[.][^.]+$\\)")
(defun compiler-explorer--compilation-parse-errors-filename
    (filename)
  "Wrapper for parsing FILENAME in compiler output buffer.
This allows navigating to errors in source code from that buffer."
  (when (string-match-p compiler-explorer--filename-regexp filename)
    (file-name-nondirectory
     (buffer-file-name (get-buffer compiler-explorer--buffer)))))

(defcustom compiler-explorer-document-opcodes t
  "If non-nil, provide documentation for opcodes in ASM buffers.
This uses `eldoc' to output documentation for opcodes at point in
the minibuffer and separate help buffers."
  :type 'boolean)

(defun compiler-explorer--compilation-eldoc-documentation-function (callback)
  "Call CALLBACK with the documentation for opcode at point.
This is eldoc function for compiler explorer."
  (when-let ((opcode (thing-at-point 'symbol)))
    (compiler-explorer--asm-opcode-doc
     (plist-get compiler-explorer--compiler-data :instructionSet)
     opcode
     (lambda (doc)
       (funcall callback
                (with-temp-buffer
                  (text-mode)
                  (insert doc)
                  (fill-paragraph)
                  (buffer-string))
                :thing opcode)))))

;; Session management

(defcustom compiler-explorer-sessions-file
  (expand-file-name "compiler-explorer" user-emacs-directory)
  "File where sessions are persisted."
  :type 'file)

(defcustom compiler-explorer-sessions 5
  "Size of the session ring."
  :type 'integer)

(defvar compiler-explorer--session-ring
  (let ((ring (make-ring compiler-explorer-sessions)))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents compiler-explorer-sessions-file)
        (let ((elts (read (current-buffer)))
              version)
          (if (and (consp elts) (integerp (car elts)))
              (setq version (car elts) elts (cdr elts))
            (setq version 0))
          (when (> version 1)
            (error "Session file is incompatible"))
          (dolist (e elts)
            (setq version (plist-get e :version))
            (when (and version (> version 1))
              (setq e nil))
            (when e
              (ring-insert ring e))))))
    ring))

(defun compiler-explorer--current-session ()
  "Serialize current session as plist."
  `(
    :version 1
    :lang-name ,(plist-get compiler-explorer--language-data :name)
    :compiler ,(plist-get compiler-explorer--compiler-data :id)
    :libs ,(mapcar (pcase-lambda (`(,id ,vid ,_)) (cons id vid))
                   compiler-explorer--selected-libraries)
    :args ,compiler-explorer--compiler-arguments
    :exe-args ,compiler-explorer--execution-arguments
    :input ,compiler-explorer--execution-input
    :source ,(with-current-buffer (get-buffer compiler-explorer--buffer)
               (buffer-substring-no-properties (point-min) (point-max)))))

(defun compiler-explorer--restore-session (session)
  "Restore serialized SESSION.
It must have been created with `compiler-explorer--current-session'."
  (pcase-let
      (((map :version :lang-name :compiler :libs :args :exe-args :input
             :source)
        session))
    (or version (setq version 0))
    (when (> version 1)
      (error "Don't know how to restore session version %s" version))
    (pcase-dolist (`(,sym ,val ,pred) `((version ,version integerp)
                                        (lang-name ,lang-name stringp)
                                        (compiler ,compiler stringp)
                                        (libs ,libs listp)
                                        (args ,args stringp)
                                        (exe-args ,exe-args stringp)
                                        (input ,input stringp)
                                        (source ,source stringp)))
      (unless (funcall pred val)
        (error "Invalid %s: %s" sym val)))

    (let ((compiler-explorer--inhibit-request t))
      (compiler-explorer-new-session lang-name compiler)
      (with-current-buffer (get-buffer compiler-explorer--buffer)
        (let ((inhibit-modification-hooks t))
          (erase-buffer)
          (insert source)
          (set-buffer-modified-p nil)))
      (pcase-dolist (`(,id . ,vid) libs)
        (compiler-explorer-add-library id vid))
      (compiler-explorer-set-compiler-args args)
      (compiler-explorer-set-execution-args exe-args)
      (compiler-explorer-set-input input))
    (compiler-explorer--request-async)
    (compiler-explorer--define-menu)))

(defvar compiler-explorer--last-session nil)

(defun compiler-explorer--save-sessions ()
  "Save all sessions to a file."
  (let ((current-session
         (or (and (get-buffer compiler-explorer--buffer)
                  (compiler-explorer--current-session))
             compiler-explorer--last-session)))
    (when current-session
      (ring-insert compiler-explorer--session-ring current-session))
    (with-temp-file compiler-explorer-sessions-file
      (insert ";; Auto-generated file; don't edit -*- mode: lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (print
         (cons 1                        ;version
               (ring-elements compiler-explorer--session-ring))
         (current-buffer))))))


;; User commands & modes

(defun compiler-explorer--active-p ()
  "Return non-nil if we're in a `compiler-explorer' session."
  (bufferp (get-buffer compiler-explorer--buffer)))

(defvar compiler-explorer--recompile-timer nil
  "Timer for recompilation.")

(defun compiler-explorer--after-change (&rest _args)
  "Schedule recompilation after buffer is modified."
  (when compiler-explorer--recompile-timer
    (cancel-timer compiler-explorer--recompile-timer))
  (setq compiler-explorer--recompile-timer
        (run-with-timer 0.5 nil #'compiler-explorer--request-async))

  ;; Prevent 'kill anyway?' when killing the buffer.
  (restore-buffer-modified-p nil))

(defvar compiler-explorer-mode-map (make-sparse-keymap)
  "Keymap used in all compiler explorer buffers.")

(define-minor-mode compiler-explorer--local-mode
  "Minor mode used in all compiler explorer buffers."
  :lighter ""
  (add-hook 'kill-buffer-hook #'compiler-explorer--cleanup nil t)
  (add-hook 'project-find-functions
            #'compiler-explorer--project-find-function nil t)

  (when compiler-explorer--project-dir
    (setq-local default-directory compiler-explorer--project-dir))

  (pcase (buffer-name)
    ((pred (equal compiler-explorer--buffer))
     (setq header-line-format
           `(:eval (compiler-explorer--header-line-format-source)))
     (add-hook 'after-change-functions
               #'compiler-explorer--after-change nil t))
    ((pred (equal compiler-explorer--compiler-buffer))
     (setq header-line-format
           `(:eval (compiler-explorer--header-line-format-compiler)))
     (setq truncate-lines t)           ;Make the ASM view more like godbolt.org
     (when compiler-explorer-document-opcodes
       (add-hook
        'eldoc-documentation-functions
        'compiler-explorer--compilation-eldoc-documentation-function nil t)
       (setq-local eldoc-documentation-function 'eldoc-documentation-compose)
       (eldoc-mode +1)))
    ((pred (equal compiler-explorer--output-buffer))
     (setq-local compilation-parse-errors-filename-function
                 #'compiler-explorer--compilation-parse-errors-filename))
    ((pred (equal compiler-explorer--exe-output-buffer))
     (setq header-line-format
           `(:eval (compiler-explorer--header-line-format-executor))))))

(defun compiler-explorer--local-mode-maybe-enable ()
  "Enable `compiler-explorer--local-mode' if required."
  (when (memq (current-buffer)
              (list
               (get-buffer compiler-explorer--buffer)
               (get-buffer compiler-explorer--compiler-buffer)
               (get-buffer compiler-explorer--output-buffer)
               (get-buffer compiler-explorer--exe-output-buffer)))
    (compiler-explorer--local-mode +1)))

(define-globalized-minor-mode compiler-explorer-mode
  compiler-explorer--local-mode
  compiler-explorer--local-mode-maybe-enable
  :lighter " CE"
  :keymap compiler-explorer-mode-map
  (unless compiler-explorer-mode
    (compiler-explorer--cleanup)))

(defun compiler-explorer-show-output ()
  "Show compiler stdout&stderr buffer."
  (interactive)
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (display-buffer compiler-explorer--output-buffer))

(defvar compiler-explorer-params-change-hook nil
  "Hook called when parameters change.
Each function is called with two arguments: WHAT and VALUE.  WHAT
is a symbol, either:
 - `input'
 - `compiler'
 - `compiler-args'
 - `execution-args'

VALUE is the new value, a string.")

(defun compiler-explorer-jump (&optional which)
  "Jump to corresponding ASM block or source code line.
From source buffer, jump to the first ASM block for the line at
point.  From ASM buffer, jump to the source buffer and line for
the instruction at point.

From Lisp, WHICH is the index of the block to jump to (modulo the
number of blocks for the source code line associated with point).

Interactively, with a non-numeric prefix argument, jumps to the
NEXT region that maps to this source line, or to the source line
itself.  Thus, repeatedly calling this command with non-numeric
prefix argument will go through all related ASM blocks for one
source code block.

With a numeric prefix argument, jumps to the Nth ASM block for
the same source line."
  (interactive "P")
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (if-let ((ov (cl-find-if
                (lambda (ov) (overlay-get ov 'compiler-explorer--overlay))
                (overlays-at (point)))))
      (let* ((group (overlay-get ov 'compiler-explorer--overlay-group))
             (index-of-this-ov (cl-position ov group))
             (requested-within-group
              (% (if (numberp which) (1- which) (1+ index-of-this-ov))
                 (length group)))
             (target-ov (if (or (null which)
                                (and (not (numberp which))
                                     ;; Are we in the last ASM block for this
                                     ;; line?
                                     (= index-of-this-ov (1- (length group)))))
                            ;; Jump to the other buffer, e.g. source from ASM
                            (overlay-get ov 'compiler-explorer--target)
                          (nth requested-within-group group)))
             (buf (overlay-buffer target-ov)))
        (setq group (overlay-get target-ov 'compiler-explorer--overlay-group))

        (when (null which)
          (setq target-ov
                (car (seq-sort-by (lambda (ov)
                                    (with-current-buffer (overlay-buffer ov)
                                      (abs (- (overlay-start ov) (point)))))
                                  #'< group))))

        (pop-to-buffer buf)
        (with-current-buffer buf
          (pulse-momentary-highlight-overlay target-ov)
          (goto-char (overlay-start target-ov)))
        (message "%s block %d/%d"
                 (if (eq (current-buffer)
                         (get-buffer compiler-explorer--buffer))
                     "Source" "ASM")
                 (1+ (cl-position target-ov group))
                 (length group)))
    (error "No corresponding ASM or source code block at point")))

(defun compiler-explorer-set-input (input)
  "Set the input to use as stdin for execution to INPUT, a string."
  (interactive (list (if (compiler-explorer--active-p)
                         (read-from-minibuffer
                          "Stdin: " compiler-explorer--execution-input)
                       (user-error "Not in a `compiler-explorer' session"))))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (setq compiler-explorer--execution-input input)
  (compiler-explorer--request-async)
  (run-hook-with-args 'compiler-explorer-params-change-hook 'input input))

(defvar compiler-explorer-set-compiler-args-history nil
  "Minibuffer history for `compiler-explorer-set-compiler-args'.")

(defvar compiler-explorer--popular-arguments-cache
  (make-hash-table :test #'equal)
  "Hash table of popular arguments per compiler.")

(defun compiler-explorer--capf-popular-arguments ()
  "Complete the argument at point using popular arguments."
  (let* ((cid (plist-get compiler-explorer--compiler-data :id))
         (bounds (or (bounds-of-thing-at-point 'symbol)
                     (cons (point) (point))))
         (arguments
          (or
           (map-elt compiler-explorer--popular-arguments-cache cid)
           (setf (map-elt compiler-explorer--popular-arguments-cache cid)
                 (compiler-explorer--request-sync
                  nil
                  (compiler-explorer--url "popularArguments" cid)
                  :headers '(("Accept" . "application/json"))
                  :as (lambda ()
                        (let ((json-key-type 'string))
                          (json-parse-buffer))))))))
    (list
     (car bounds) (cdr bounds) (hash-table-keys arguments)
     :annotation-function (lambda (completion)
                            (when-let* ((elt (map-elt arguments completion))
                                        (desc (map-elt elt "description")))
                              (concat " " desc))))))

(defun compiler-explorer-set-compiler-args (args)
  "Set compilation arguments to the string ARGS and recompile."
  (interactive
   (if (compiler-explorer--active-p)
       (minibuffer-with-setup-hook
           (lambda ()
             (setq-local
              completion-at-point-functions
              (list #'compiler-explorer--capf-popular-arguments)))
         (list (read-from-minibuffer
                "Compiler arguments: "
                compiler-explorer--compiler-arguments
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map minibuffer-local-map)
                  (define-key map "\t" #'completion-at-point)
                  (define-key map [M-up] #'minibuffer-previous-completion)
                  (define-key map [M-down] #'minibuffer-next-completion)
                  (define-key map [?\M-\r] #'minibuffer-choose-completion)
                  map)
                nil 'compiler-explorer-set-compiler-args-history)))
     (user-error "Not in a `compiler-explorer' session")))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (setq compiler-explorer--compiler-arguments args)
  (compiler-explorer--request-async)
  (run-hook-with-args 'compiler-explorer-params-change-hook
                      'compiler-args args))

(defun compiler-explorer-set-execution-args (args)
  "Set execution arguments to the string ARGS and recompile."
  (interactive (list (if (compiler-explorer--active-p)
                         (read-from-minibuffer
                          "Execution arguments: "
                          compiler-explorer--execution-arguments)
                       (user-error "Not in a `compiler-explorer' session"))))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (setq compiler-explorer--execution-arguments args)
  (compiler-explorer--request-async)
  (run-hook-with-args 'compiler-explorer-params-change-hook
                      'execution-args args))

(defun compiler-explorer-set-compiler (name-or-id)
  "Select compiler NAME-OR-ID for current session.
Interactively, prompts for the name of a compiler.  With a prefix
argument, prompts only for the name of a compiler that supports
execution."
  (interactive
   (list
    (and
     (or (compiler-explorer--active-p)
         (user-error "Not in a `compiler-explorer' session"))
     (or
      (get-text-property (point) 'compiler-explorer-compiler-id)
      (let* ((lang compiler-explorer--language-data)
             (default (plist-get lang :defaultCompiler))
             (compilers (mapcar
                         (pcase-lambda ((map :name :id :supportsExecute :lang))
                           (list name id supportsExecute lang))
                         (compiler-explorer--compilers))))
        (completing-read (concat "Compiler"
                                 (when current-prefix-arg " (with execution)")
                                 ": ")
                         compilers
                         (pcase-lambda (`(_ _ ,supports-execute ,lang-id))
                           (and
                            ;; Only compilers for current language
                            (string= lang-id (plist-get lang :id))
                            (or (not current-prefix-arg)
                                (eq t supports-execute))))
                         t
                         (car (cl-find default compilers
                                       :test #'string= :key #'cadr))))))))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (pcase-let*
      (((map (:id lang-id) :defaultCompiler) compiler-explorer--language-data)
       (name-or-id (or name-or-id defaultCompiler))
       (compiler-data (seq-find
                       (pcase-lambda ((map :id :name :lang))
                         (and (member name-or-id (list id name))
                              (string= lang lang-id)))
                       (compiler-explorer--compilers))))
    (unless compiler-data
      (error "No compiler %S for lang %S" name-or-id lang-id))
    (setq compiler-explorer--compiler-data compiler-data)
    (with-current-buffer (get-buffer compiler-explorer--compiler-buffer)
      (compiler-explorer--request-async)

      (pop-to-buffer (current-buffer))

      (compiler-explorer--define-menu)

      (run-hook-with-args 'compiler-explorer-params-change-hook
                          'compiler (plist-get compiler-data :name)))))

(defun compiler-explorer-add-library (id version-id)
  "Add library ID with VERSION-ID to current compilation."
  (interactive
   (let* ((lang (or (and (compiler-explorer--active-p)
                         (plist-get compiler-explorer--language-data :id))
                    (user-error "Not in a `compiler-explorer' session")))
          (candidates (cl-reduce #'nconc
                                 (mapcar
                                  (pcase-lambda ((map :name :id :versions))
                                    (seq-map
                                     (pcase-lambda ((map :version (:id vid)))
                                       `(,(concat name " " version) ,id ,vid))
                                     versions))
                                  (compiler-explorer--libraries lang))))
          (res (completing-read
                "Add library: " candidates
                ;; Ignore libraries that are already added.
                (pcase-lambda (`(,_ ,id ,_))
                  (null (assoc id compiler-explorer--selected-libraries)))
                t)))
     (cdr (assoc res candidates))))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (let* ((libentry
          (cl-find id (compiler-explorer--libraries
                       (plist-get compiler-explorer--language-data :id))
                   :key (lambda (l) (plist-get l :id))
                   :test #'string=))
         (version-entry
          (cl-find version-id (plist-get libentry :versions)
                   :key (lambda (v) (plist-get v :id))
                   :test #'string=)))
    (unless libentry
      (error "Library with id %S is invalid for the current language" id))
    (unless version-entry
      (error "Version id %S is invalid for library %S" version-id id))
    (push (list id version-id libentry) compiler-explorer--selected-libraries)
    (compiler-explorer--request-async))

  ;; Repopulate list of libraries to remove
  (compiler-explorer--define-menu))

(defun compiler-explorer-remove-library (id)
  "Remove library with ID.
It must have previously been added with
`compiler-explorer-add-library'."
  (interactive
   (if (compiler-explorer--active-p)
       (let* ((libs-by-name
               (mapcar (pcase-lambda (`(,_ ,_ ,entry))
                         (cons (plist-get entry :name) entry))
                       compiler-explorer--selected-libraries))
              (choice
               (completing-read "Remove library: "
                                (mapcar #'car libs-by-name) nil t))
              (entry (cdr (assoc choice libs-by-name))))
         (list (plist-get entry :id)))
     (user-error "Not in a `compiler-explorer' session")))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (setq compiler-explorer--selected-libraries
        (delq (assoc id compiler-explorer--selected-libraries)
              compiler-explorer--selected-libraries))
  (compiler-explorer--request-async)

  ;; Repopulate list of libraries to remove
  (compiler-explorer--define-menu))

(defun compiler-explorer-previous-session ()
  "Cycle between previous sessions, latest first."
  (interactive)
  (when (and (ring-empty-p compiler-explorer--session-ring)
             (null compiler-explorer--last-session))
    (error "No previous sessions"))
  (let ((prev (or compiler-explorer--last-session
                  (ring-remove compiler-explorer--session-ring))))
    (setq compiler-explorer--last-session nil)
    (condition-case nil
        (prog1 t (compiler-explorer--restore-session prev))
      (error
       (compiler-explorer--cleanup 'skip-save-session)
       (display-warning
        'compiler-explorer "Previous session appears to be corrupt" :warning)
       nil))))

(defvar compiler-explorer-layouts
  '((source . asm)
    (source . [asm output])
    (source [asm output] . exe))
  "List of layouts.

A layout can be either:

  - a symbol (one of `source', `asm', `output', `exe')
    means fill the available space with that buffer
  - a cons (left . right) - recursively apply layouts
    left and right after splitting available space horizontally
  - a vector [upper lower] - recursively apply layouts
    above and below after splitting available space vertically
  - a number, n - apply n-th layout in this variable")

(defcustom compiler-explorer-default-layout 0
  "The default layout to use.
See `compiler-explorer-layouts' for available layouts."
  :type 'sexp)

(defvar compiler-explorer--last-layout 0)

(defcustom compiler-explorer-dedicate-windows t
  "Make all windows dedicated to their buffers.
If non-nil, all compiler explorer windows will be bound to the
buffers they are displaying via `set-window-dedicated-p' and
other, unrelated buffers will not be displayable in these
windows."
  :type 'boolean)

(defun compiler-explorer-layout (&optional layout)
  "Layout current frame.
Interactively, applies layout defined in variable
`compiler-explorer-default-layout'.  When this command is called
repeatedly (`repeat'), it will cycle between all layouts in
`compiler-explorer-layouts'.

LAYOUT must be as described in `compiler-explorer-layouts'."
  (interactive
   (list
    (or (and (numberp current-prefix-arg) current-prefix-arg)
        (when (eq last-command #'compiler-explorer-layout)
          (1+ compiler-explorer--last-layout)))))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (cl-labels
      ((override-window-buffer
         (window buffer)
         (set-window-buffer window buffer)
         (when compiler-explorer-dedicate-windows
           (set-window-dedicated-p window t)))
       (do-it
         (spec)
         (pcase-exhaustive spec
           ((and (pred numberp) n)
            (do-it (nth n compiler-explorer-layouts)))
           ('source (override-window-buffer (selected-window)
                                            compiler-explorer--buffer))
           ('asm (override-window-buffer (selected-window)
                                         compiler-explorer--compiler-buffer))
           ('output (override-window-buffer
                     (selected-window)
                     (get-buffer compiler-explorer--output-buffer)))
           ('exe (override-window-buffer
                  (selected-window)
                  (get-buffer compiler-explorer--exe-output-buffer)))
           (`(,left . ,right)
            (let ((right-window (split-window-right)))
              (do-it left)
              (with-selected-window right-window
                (do-it right))))
           (`[,upper ,lower]
            (let ((lower-window (split-window-vertically)))
              (do-it upper)
              (with-selected-window lower-window
                (do-it lower)))))))
    (or layout (setq layout compiler-explorer-default-layout))
    (when (numberp layout)
      (setq layout (% layout (length compiler-explorer-layouts)))
      (setq compiler-explorer--last-layout layout))
    (when (window-dedicated-p)
      (unless compiler-explorer--local-mode
        (select-window (split-window-horizontally)))
      (set-window-dedicated-p (selected-window) nil))
    (delete-other-windows)
    (do-it layout)
    (balance-windows)))

(defun compiler-explorer-load-example (example)
  "Load an example named EXAMPLE.
Interactively, this prompts for an example to load for the current language."
  (interactive
   (list
    (and (or (compiler-explorer--active-p)
             (user-error "Not in a `compiler-explorer' session"))
         (completing-read
          "Load example: "
          (compiler-explorer--examples
           (plist-get compiler-explorer--language-data :id))
          nil t))))
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (if-let* ((lang (plist-get compiler-explorer--language-data :id))
            (all (compiler-explorer--examples lang))
            (data (cdr (assoc example all))))
      (with-temp-buffer
        (insert (plist-get
                 (compiler-explorer--example lang (plist-get data :file))
                 :file))
        (compiler-explorer--replace-buffer-contents
         (get-buffer compiler-explorer--buffer)
         (current-buffer)))
    (error "Unknown example %S" example)))

(defun compiler-explorer-make-link (&optional open)
  "Save URL to current session in the kill ring.
With an optional prefix argument OPEN, open that link in a browser."
  (interactive "P")
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (let* ((compiler
          `(
            :id ,(plist-get compiler-explorer--compiler-data :id)
            :libs [,@(mapcar
                      (pcase-lambda (`(,id ,version ,_))
                        `(:id ,id :version ,version))
                      compiler-explorer--selected-libraries)]
            :options ,compiler-explorer--compiler-arguments
            :filters ,(compiler-explorer--output-filters)))
         (state
          `(:sessions
            [(
              :id 1
              :language ,(plist-get compiler-explorer--language-data :id)
              :source ,(with-current-buffer
                           (get-buffer compiler-explorer--buffer)
                         (buffer-string))
              :compilers [,compiler]
              :executors [
                          (
                           :arguments ,compiler-explorer--execution-arguments
                           :compiler ,compiler
                           :stdin ,compiler-explorer--execution-input)
                          ])]))
         (response
          (plz 'post
            (concat compiler-explorer-url "/shortener")
            :headers '(("Accept" . "application/json")
                       ("Content-Type" . "application/json"))
            :body (let ((json-object-type 'plist))
                    (json-encode state))
            :as #'compiler-explorer--parse-json))
         (url (plist-get response :url)))
    (message (kill-new url))
    (when open (browse-url-xdg-open url))))

(defvar compiler-explorer-new-session-hook '(compiler-explorer-layout)
  "Hook run after creating new session.
The source buffer is current when this hook runs.")

(defun compiler-explorer-new-session-1 (lang &optional compiler interactive)
  "Start new session for LANG.
This is a subr of `compiler-explorer-new-session' that uses given
LANG, COMPILER, INTERACTIVE."
  ;; Clean everything up
  (compiler-explorer--cleanup)
  (when-let ((session compiler-explorer--last-session))
    (ring-insert compiler-explorer--session-ring session)
    (setq compiler-explorer--last-session nil))

  ;; Enter session mode
  (compiler-explorer-mode +1)

  ;; Generate temporary directory if needed
  (setq compiler-explorer--project-dir
        (and compiler-explorer-make-temp-file
             (make-temp-file "compiler-explorer" 'dir)))

  ;; Generate all the buffers
  (pcase-dolist (`(,buf ,mode ,ro)
                 `((,compiler-explorer--buffer fundamental-mode nil)
                   (,compiler-explorer--compiler-buffer asm-mode t)
                   (,compiler-explorer--output-buffer compilation-mode t)
                   (,compiler-explorer--exe-output-buffer text-mode t)))
    (with-current-buffer (generate-new-buffer buf)
      (with-demoted-errors "compiler-explorer-new-session-1: %S"
        (funcall mode))
      (setq buffer-read-only ro)
      (setq buffer-undo-list ro)))

  ;; Do the rest of the initialization: set up the source buffer and set the
  ;; compiler.

  (pcase-let* ((lang-data (or (cl-find lang (compiler-explorer--languages)
                                       :key (lambda (l) (plist-get l :name))
                                       :test #'string=)
                              (error "Language %S does not exist" lang)))
               ((map :extensions :id :example) lang-data))
    (setq compiler-explorer--language-data lang-data)

    ;; Prefetch
    (ignore (compiler-explorer--libraries id))
    (ignore (compiler-explorer--examples id))

    (with-current-buffer compiler-explorer--buffer
      ;; Find major mode by extension
      (cl-loop for ext across extensions
               for filename = (expand-file-name (concat "test" ext)
                                                default-directory)
               while (eq major-mode 'fundamental-mode)
               do (let ((buffer-file-name filename))
                    (with-demoted-errors "compiler-explorer-new-session-1: %S"
                      (set-auto-mode))))

      (insert example)
      (save-current-buffer
        (condition-case err
            (compiler-explorer-set-compiler compiler)
          (error (if interactive
                     (call-interactively #'compiler-explorer-set-compiler)
                   (signal (car err) (cdr err))))))

      (when compiler-explorer--project-dir
        (setq buffer-file-name
              (expand-file-name (concat "source" (aref extensions 0))
                                compiler-explorer--project-dir))
        (let ((save-silently t)) (save-buffer)))

      (compiler-explorer--define-menu)

      (pop-to-buffer (current-buffer))
      (run-hooks 'compiler-explorer-new-session-hook))))

(defun compiler-explorer-new-session (lang &optional compiler)
  "Create a new compiler explorer session with language named LANG.
If COMPILER (name or id) is non-nil, set that compiler.

If a session already exists, it is killed and saved to the
session ring.

Always runs hooks in `compiler-explorer-new-session-hook' at the
end, with the source buffer as current.

If COMPILER is t, then use the default compiler for this
language, and if that fails, prompt the user to select another
compiler."
  (interactive
   (list (completing-read "Language: "
                          (mapcar (lambda (lang) (plist-get lang :name))
                                  (compiler-explorer--languages))
                          nil t)
         t))
  (let (success)
    (unwind-protect
        (progn
          (let ((compiler-explorer--inhibit-request t))
            (compiler-explorer-new-session-1 lang
                                             (if (eq compiler t) nil compiler)
                                             (eq compiler t)))
          (compiler-explorer--request-async)
          (setq success t))
      (unless success
        (compiler-explorer--cleanup 'skip-save-session)))))

(defun compiler-explorer-exit ()
  "Kill the current session."
  (interactive)
  (unless (compiler-explorer--active-p)
    (error "Not in a `compiler-explorer' session"))
  (compiler-explorer--cleanup))

(defvar compiler-explorer-hook '(compiler-explorer-layout)
  "Hook run at the end of `compiler-explorer'.
This hook can be used to run code regardless whether a session
was created/restored.")

;;;###autoload
(defun compiler-explorer ()
  "Open a compiler explorer session.
If a live session exists, just pop to the source buffer.
If there are saved sessions, restore the last one.
Otherwise, create a new session (`compiler-explorer-new-session').

The hook `compiler-explorer-hook' is always run at the end."
  (interactive)
  (let ((buffer (get-buffer compiler-explorer--buffer)))
    (cond
     (buffer (pop-to-buffer buffer) (compiler-explorer--request-async))
     ((and (or compiler-explorer--last-session
               (not (ring-empty-p compiler-explorer--session-ring)))
           (compiler-explorer-previous-session)))
     (t
      (call-interactively #'compiler-explorer-new-session))))
  (run-hooks 'compiler-explorer-hook))

(add-hook 'kill-emacs-hook #'compiler-explorer--save-sessions)

(provide 'compiler-explorer)
;;; compiler-explorer.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
