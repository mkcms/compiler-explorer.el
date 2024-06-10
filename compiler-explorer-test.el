;;; compiler-explorer-test.el --- Tests for compiler-explorer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: tests

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

;;; Code:

(require 'compiler-explorer)
(require 'ert)

(defmacro compiler-explorer-test--with-session (lang compiler &rest body)
  (declare (indent 2) (debug (sexp sexp body)))
  `(let ((compiler-explorer-new-session-hook '()))
     (unwind-protect
         (progn
           (compiler-explorer-new-session ,lang ,compiler)
           ,@body)
       (compiler-explorer--cleanup))))

(defun compiler-explorer-test--wait ()
  "Wait until compilation finishes."
  (with-timeout (15 (error "Test timed out"))
    (while (or (member compiler-explorer--recompile-timer timer-list)
               (process-live-p
                compiler-explorer--last-compilation-request)
               (and compiler-explorer--last-exe-request
                    (process-live-p compiler-explorer--last-exe-request)))
      (accept-process-output nil 0.1))))

(defun compiler-explorer-test--help-message ()
  "Get the documentation string for thing at point."
  (with-current-buffer compiler-explorer--compiler-buffer
    (should (memq 'compiler-explorer--compilation-eldoc-documentation-function
                  eldoc-documentation-functions))
    (let (msg)
      (compiler-explorer--compilation-eldoc-documentation-function
       (lambda (x &rest _more) (setq msg x)))
      (with-timeout (15 (error "Test timed out"))
        (while (null msg)
          (accept-process-output nil 0.1)))
      msg)))

(defun compiler-explorer-test--insert (string)
  "Erase source buffer then insert STRING and wait until compilation finishes."
  (with-current-buffer compiler-explorer--buffer
    (erase-buffer)
    (insert string)
    (compiler-explorer-test--wait)))

(defun compiler-explorer-test--compilation-result ()
  "Get the contents of compilation buffer."
  (with-current-buffer compiler-explorer--compiler-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun compiler-explorer-test--execution-result ()
  "Get the contents of execution stdout buffer."
  (with-current-buffer compiler-explorer--exe-output-buffer
    (buffer-substring-no-properties (point-min) (point-max))))


;; Tests

(ert-deftest compiler-explorer-api-languages ()
  (let ((cpp (seq-find (lambda (l) (string= (plist-get l :name) "C++"))
                       (compiler-explorer--languages))))
    (should (stringp (plist-get cpp :id)))
    (should (stringp (plist-get cpp :defaultCompiler)))
    (should (stringp (plist-get cpp :example)))
    (should (cl-find ".cpp" (plist-get cpp :extensions) :test #'string=))))

(ert-deftest compiler-explorer-api-compilers ()
  (let ((gcc (seq-find (lambda (c)
                         (string= (plist-get c :name) "x86-64 gcc (trunk)"))
                       (compiler-explorer--compilers))))
    (should (stringp (plist-get gcc :id)))
    (should (memq :supportsExecute gcc))
    (let* ((lang (plist-get gcc :lang))
           (lang-data (seq-find (lambda (l) (string= (plist-get l :id) lang))
                                (compiler-explorer--languages))))
      (should (stringp lang))
      (should lang-data))))

(ert-deftest compiler-explorer-api-libraries ()
  (let* ((cpp (seq-find (lambda (x) (string= (plist-get x :name) "C++"))
                        (compiler-explorer--languages)))
         (libs (compiler-explorer--libraries (plist-get cpp :id))))
    (should libs)
    (let* ((lib (seq-find (lambda (l)
                            (string-match-p ".*Boost.*" (plist-get l :name)))
                          libs))
           (versions (plist-get lib :versions)))
      (should lib)
      (should (stringp (plist-get lib :id)))
      (should (vectorp versions))
      (let ((first (aref versions 0)))
        (should (stringp (plist-get first :id)))
        (should (stringp (plist-get first :version)))))))

(ert-deftest compiler-explorer-api-asm-opcode-docs ()
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (compiler-explorer--asm-opcode-doc "amd64" "pushq"
                                         (lambda (doc)
                                           (with-current-buffer buf
                                             (insert doc))))
      (with-timeout (5 (error "Test timed out"))
        (while (= (buffer-size) 0)
          (accept-process-output)))
      (goto-char (point-min))
      (should (search-forward "Decrements the stack pointer and then stores"))
      (should (search-forward "source operand on the top of the stack"))))
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (compiler-explorer--asm-opcode-doc "python" "MAKE_FUNCTION"
                                         (lambda (doc)
                                           (with-current-buffer buf
                                             (insert doc))))
      (with-timeout (5 (error "Test timed out"))
        (while (= (buffer-size) 0)
          (accept-process-output)))
      (goto-char (point-min))
      (should (search-forward "Pushes a new function object on the stack.")))))

(ert-deftest compiler-explorer-api-examples ()
  (let ((examples (compiler-explorer--examples)))
    (should examples)
    (should (listp examples))
    (should (cl-every #'consp examples))
    (mapc (pcase-lambda (`(,name . ,data))
            (should (stringp name))
            (should (listp data))
            (should (stringp (plist-get data :file)))
            (should (stringp (plist-get data :name)))
            (should (stringp (plist-get data :lang))))
          examples)
    (setq examples (compiler-explorer--examples "c++"))
    (should examples)
    (mapc (pcase-lambda (`(,_ . ,data))
            (setq data (compiler-explorer--example "c++" (plist-get data :file)))
            (should (stringp (plist-get data :file))))
          examples)))

(ert-deftest compiler-explorer-auto-mode ()
  "Test that major mode is set automatically from language."
  (compiler-explorer-test--with-session "C++" nil
    (with-current-buffer (get-buffer compiler-explorer--buffer)
      (should (eq major-mode 'c++-mode)))
    (with-current-buffer (get-buffer compiler-explorer--compiler-buffer)
      (should (eq major-mode 'asm-mode))))
  (compiler-explorer-test--with-session "Python" nil
    (with-current-buffer (get-buffer compiler-explorer--buffer)
      (should (eq major-mode 'python-mode)))))

(ert-deftest compiler-explorer-auto-recompilation ()
  "Test that modifying the source buffer triggers recompilation."
  (compiler-explorer-test--with-session "C++" nil
    (compiler-explorer-test--insert
     "int add(int a, int b) { return a + b; }\n")
    (string-match-p "add(int, int):"
                    (compiler-explorer-test--compilation-result))
    (compiler-explorer-test--insert
     "float sub(float a, float b) { return a - b; }\n")
    (should
     (string-match-p "sub(float, float):"
                     (compiler-explorer-test--compilation-result)))))

(ert-deftest compiler-explorer-switching-compilers ()
  "Check that switching compilers works and triggers recompilation."
  (compiler-explorer-test--with-session "C++" "x86-64 gcc (trunk)"
    (compiler-explorer-test--insert "
#ifdef __clang__
int foo(int) { return 1; }
#else
int bar(int) { return 2; }
#endif
")
    (should (string-match-p "bar(int):"
                            (compiler-explorer-test--compilation-result)))

    (compiler-explorer-set-compiler "x86-64 clang (trunk)")
    (compiler-explorer-test--wait)
    (should (string-match-p "foo(int):"
                            (compiler-explorer-test--compilation-result)))))

(ert-deftest compiler-explorer-asm-opcode-docs ()
  "Check that we can see ASM opcode documentation."
  (compiler-explorer-test--with-session "Assembly" "x86-64 clang (trunk)"
    (compiler-explorer-set-compiler "x86-64 clang (trunk)")

    (compiler-explorer-test--insert
"
foo:
    xor %rdx, %rax
    inc %rcx
    jmpq *%rax
")
    (compiler-explorer-test--wait)

    (with-current-buffer compiler-explorer--compiler-buffer

      (goto-char (point-min))
      (search-forward "xor")
      (forward-char -1)
      (should (string-match-p "Performs a bitwise exclusive OR"
                              (compiler-explorer-test--help-message)))

      (goto-char (point-min))
      (search-forward "inc")
      (forward-char -1)
      (should (string-match-p "Adds 1 to the destination operand"
                              (compiler-explorer-test--help-message)))

      (goto-char (point-min))
      (search-forward "jmp")
      (forward-char -1)
      (should (string-match-p "Transfers program control to a different point"
                              (compiler-explorer-test--help-message))))))

(ert-deftest compiler-explorer-loading-example ()
  "Check that we can load an example."
  (compiler-explorer-test--with-session "C++" "x86-64 clang (trunk)"
    (with-current-buffer compiler-explorer--buffer
      (erase-buffer))

    (compiler-explorer-load-example "Max array")

    (compiler-explorer-test--wait)

    (with-current-buffer compiler-explorer--buffer
      (should-not (string-empty-p (buffer-string))))

    (should-not
     (string-match-p
      "Compilation failed" (compiler-explorer-test--compilation-result)))))

(ert-deftest compiler-explorer-setting-compiler-args ()
  "Check that setting arguments works and triggers recompilation."
  (compiler-explorer-test--with-session "C++" nil
    (compiler-explorer-test--insert "int f() { return unknown; }\n")
    (should (string-match-p "Compilation failed"
                            (compiler-explorer-test--compilation-result)))

    (compiler-explorer-set-compiler-args "-Dunknown=123")
    (compiler-explorer-test--wait)
    (should (string-match-p "mov *eax, 123"
                            (compiler-explorer-test--compilation-result)))

    (compiler-explorer-set-compiler-args "-Dunknown=54321")
    (compiler-explorer-test--wait)
    (should (string-match-p "mov *eax, 54321"
                            (compiler-explorer-test--compilation-result)))))

(ert-deftest compiler-explorer-adding-removing-libs ()
  (compiler-explorer-test--with-session "C++" nil
    (compiler-explorer-test--insert "#include <boost/any.hpp>
int foo(boost::any a) { return 1; }")
    (should (string-match-p "Compilation failed"
                            (compiler-explorer-test--compilation-result)))

    (compiler-explorer-add-library "boost" "174")
    (compiler-explorer-test--wait)
    (should (string-match-p "foo(boost::any):"
                            (compiler-explorer-test--compilation-result)))

    (compiler-explorer-remove-library "boost")
    (compiler-explorer-test--wait)
    (should (string-match-p "Compilation failed"
                            (compiler-explorer-test--compilation-result)))))

(ert-deftest compiler-explorer-creates-temp-project ()
  (let ((compiler-explorer-make-temp-file t)
        dir)
    (compiler-explorer-test--with-session "C++" nil
      (with-current-buffer compiler-explorer--buffer
        (should buffer-file-name)
        (should (file-exists-p buffer-file-name))
        (let ((proj (project-current)))
          (should proj)
          (should (consp proj))
          (should (eq 'transient (car proj)))
          (should (stringp (cdr proj)))
          (should (file-directory-p (cdr proj)))
          (setq dir (cdr proj)))))
    (should-not (file-exists-p dir)))
  (let ((compiler-explorer-make-temp-file nil))
    (compiler-explorer-test--with-session "C++" nil
      (with-current-buffer compiler-explorer--buffer
        (should-not buffer-file-name)
        (should-not (project-current))))))

(ert-deftest compiler-explorer-execution ()
  (compiler-explorer-test--with-session "C++" nil
    (compiler-explorer-test--insert
     "
#include <stdio.h>
int main() {
    puts(\"test\");
    puts(\"foo\");
    return 44;
}")

    (compiler-explorer-test--wait)
    (should (string-match-p "test\nfoo\n"
                            (compiler-explorer-test--execution-result)))
    (should (string-match-p "exited with code 44"
                            (compiler-explorer-test--execution-result)))))

(ert-deftest compiler-explorer-execution-args ()
  (compiler-explorer-test--with-session "C++" nil
    (compiler-explorer-test--insert
     "
#include <stdio.h>
int main(int argc, char** argv) {
    printf(\"arg 1 = %s\\n\", argv[1]);
    printf(\"arg 2 = %s\\n\", argv[2]);
}")

    (compiler-explorer-set-execution-args "--first    --second")
    (compiler-explorer-test--wait)

    (should (string-match-p "arg 1 = --first"
                            (compiler-explorer-test--execution-result)))
    (should (string-match-p "arg 2 = --second"
                            (compiler-explorer-test--execution-result)))))

(ert-deftest compiler-explorer-execution-input ()
  (compiler-explorer-test--with-session "C++" nil
    (compiler-explorer-test--insert
     "
#include <stdio.h>
int main(int argc, char** argv) {
    char buf[32];
    buf[fread(buf, 1, sizeof(buf), stdin)] = 0;
    printf(\"Input: %s\", buf);
}")

    (compiler-explorer-set-input "FOO")
    (compiler-explorer-test--wait)

    (should (string-match-p "Input: FOO"
                            (compiler-explorer-test--execution-result)))))

(ert-deftest compiler-explorer-execution-support ()
  (compiler-explorer-test--with-session "C++" "ARM GCC trunk"
    (compiler-explorer-test--insert "int main() {}")

    (compiler-explorer-test--wait)

    (should (string-match-p
             "Error: the ARM GCC trunk compiler does not support execution"
             (compiler-explorer-test--execution-result)))))

(ert-deftest compiler-explorer-restoring-killed-session ()
  (compiler-explorer-test--with-session "C++" nil
    (with-current-buffer compiler-explorer--buffer
      (compiler-explorer-set-compiler "x86-64 clang (trunk)")
      (compiler-explorer-set-compiler-args "-Wall -Wextra")
      (compiler-explorer-set-execution-args "1 2 3")
      (compiler-explorer-set-input "test")
      (compiler-explorer-add-library "boost" "174")
      (erase-buffer)
      (insert "int foo();")
      (kill-buffer (current-buffer)))

    (should-not (buffer-live-p
                 (get-buffer compiler-explorer--compiler-buffer)))
    (should-not (buffer-live-p
                 (get-buffer compiler-explorer--output-buffer)))
    (should-not (buffer-live-p
                 (get-buffer compiler-explorer--exe-output-buffer)))
    (should-not compiler-explorer--language-data)
    (should-not compiler-explorer--compiler-data)
    (should-not compiler-explorer--selected-libraries)
    (should (string-empty-p compiler-explorer--compiler-arguments))
    (should (string-empty-p compiler-explorer--execution-arguments))
    (should (string-empty-p compiler-explorer--execution-input))

    (compiler-explorer-previous-session)
    (with-current-buffer compiler-explorer--buffer
      (should (string= (buffer-string) "int foo();")))
    (should (string= (plist-get compiler-explorer--language-data :name)
                     "C++"))
    (should (string= (plist-get compiler-explorer--compiler-data :name)
                     "x86-64 clang (trunk)"))
    (should (string= compiler-explorer--compiler-arguments "-Wall -Wextra"))
    (should (string= compiler-explorer--execution-arguments "1 2 3"))
    (should (string= compiler-explorer--execution-input "test"))
    (should (string= "boost" (caar compiler-explorer--selected-libraries)))
    (should (string= "174" (cadar compiler-explorer--selected-libraries)))))

(ert-deftest compiler-explorer-restoring-from-shortlink ()
  (let ((url nil)
        (compiler-explorer-new-session-hook nil))
    (let ((compiler-explorer--session-ring (make-ring 5))
          compiler-explorer--last-session)
      (compiler-explorer-test--with-session "C++" nil
        (with-current-buffer compiler-explorer--buffer
          (erase-buffer)
          (insert "// source _12345_")
          (compiler-explorer-set-compiler "x86-64 clang (trunk)")
          (compiler-explorer-set-compiler-args "-Wall -Wextra")
          (compiler-explorer-set-execution-args "1 2 3")
          (compiler-explorer-set-input "test")
          (compiler-explorer-add-library "boost" "174")
          (setq url (compiler-explorer-make-link)))))

    (compiler-explorer-restore-from-link url)

    (with-current-buffer compiler-explorer--buffer
      (should (equal (buffer-string) "// source _12345_"))
      (should (equal (plist-get compiler-explorer--language-data :name) "C++"))
      (should (equal (plist-get compiler-explorer--compiler-data :name)
                     "x86-64 clang (trunk)"))
      (should (equal compiler-explorer--compiler-arguments "-Wall -Wextra"))
      (should (equal compiler-explorer--execution-arguments "1 2 3"))
      (should (equal compiler-explorer--execution-input "test"))
      (should (equal (caar compiler-explorer--selected-libraries) "boost"))
      (should (equal (cadar compiler-explorer--selected-libraries) "174")))))

(ert-deftest compiler-explorer-mappings ()
  (compiler-explorer-test--with-session "C++" nil
    (with-current-buffer compiler-explorer--buffer
      (compiler-explorer-set-compiler-args "-O2")
      (erase-buffer)
      (insert "
__attribute__((noinline)) int foo() { asm (\"\"); return 987654321; }

__attribute__((noinline)) int bar() { asm (\"\"); return -1; }

 int quux(int c) {
    return
          foo()
        + bar()
        + c;
}
")

      (compiler-explorer-test--wait)
      (goto-char (point-min))

      (search-forward "foo")
      (save-current-buffer
        (compiler-explorer-jump)
        (should (equal (current-buffer)
                       (get-buffer compiler-explorer--compiler-buffer)))
        (should (string-match-p "987654321" (thing-at-point 'line)))
        (forward-line 1)
        (compiler-explorer-jump)
        (should (equal (current-buffer)
                       (get-buffer compiler-explorer--buffer)))
        (should (string-match-p "foo() {" (thing-at-point 'line))))

      (search-forward "bar")
      (save-current-buffer
        (compiler-explorer-jump)
        (should (string-match-p "-1" (thing-at-point 'line)))
        (forward-line 1)
        (compiler-explorer-jump)
        (should (string-match-p "bar() {" (thing-at-point 'line))))

      (search-forward "quux")
      (save-current-buffer
        (search-forward "foo")
        (compiler-explorer-jump)
        (should (string-match-p "call.*foo" (thing-at-point 'line))))
      (save-current-buffer
        (search-forward "bar")
        (compiler-explorer-jump)
        (should (string-match-p "call.*bar" (thing-at-point 'line))))
      (save-current-buffer
        (search-forward "+ c")
        (compiler-explorer-jump)
        (should (string-match-p "\\badd\\b\\|\\(\\blea\\b.*987654320\\)"
                                (thing-at-point 'line)))))))

(provide 'compiler-explorer-test)
;;; compiler-explorer-test.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
