;;; compiler-explorer-test.el --- Tests for compiler-explorer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Michał Krzywkowski

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

(defmacro ce-test--with-session (lang compiler &rest body)
  (declare (indent 2) (debug (sexp sexp body)))
  `(let ((ce-new-session-hook '())
         (ce-response-limit-bytes 1000000000))
     (unwind-protect
         (progn
           (ce-new-session ,lang ,compiler)
           ,@body)
       (ce--cleanup))))

(defun ce-test--wait ()
  "Wait until compilation finishes."
  (with-timeout (15 (error "Test timed out"))
    (while (or (member ce--recompile-timer timer-list)
               (process-live-p ce--last-compilation-request)
               (and ce--last-exe-request
                    (process-live-p ce--last-exe-request)))
      (accept-process-output nil 0.1))))

(defun ce-test--help-message ()
  "Get the documentation string for thing at point."
  (with-current-buffer ce--compiler-buffer
    (should (memq 'ce--compilation-eldoc-documentation-function
                  eldoc-documentation-functions))
    (let (msg)
      (ce--compilation-eldoc-documentation-function
       (lambda (x &rest _more) (setq msg x)))
      (with-timeout (15 (error "Test timed out"))
        (while (null msg)
          (accept-process-output nil 0.1)))
      msg)))

(defun ce-test--insert (string)
  "Erase source buffer then insert STRING and wait until compilation finishes."
  (with-current-buffer ce--buffer
    (erase-buffer)
    (insert string)
    (ce-test--wait)))

(defun ce-test--compilation-result ()
  "Get the contents of compilation buffer."
  (with-current-buffer ce--compiler-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ce-test--execution-result ()
  "Get the contents of execution stdout buffer."
  (with-current-buffer ce--exe-output-buffer
    (buffer-substring-no-properties (point-min) (point-max))))


;; Tests

(ert-deftest ce-api-languages ()
  (let ((cpp (seq-find (lambda (l) (string= (plist-get l :name) "C++"))
                       (ce--languages))))
    (should (stringp (plist-get cpp :id)))
    (should (stringp (plist-get cpp :defaultCompiler)))
    (should (stringp (plist-get cpp :example)))
    (should (cl-find ".cpp" (plist-get cpp :extensions) :test #'string=))))

(ert-deftest ce-api-compilers ()
  (let ((gcc (seq-find (lambda (c)
                         (string= (plist-get c :name) "x86-64 gcc (trunk)"))
                       (ce--compilers))))
    (should (stringp (plist-get gcc :id)))
    (should (memq :supportsExecute gcc))
    (let* ((lang (plist-get gcc :lang))
           (lang-data (seq-find (lambda (l) (string= (plist-get l :id) lang))
                                (ce--languages))))
      (should (stringp lang))
      (should lang-data))))

(ert-deftest ce-api-libraries ()
  (let* ((cpp (seq-find (lambda (x) (string= (plist-get x :name) "C++"))
                        (ce--languages)))
         (libs (ce--libraries (plist-get cpp :id))))
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

(ert-deftest ce-api-tools ()
  (let* ((cpp (seq-find (lambda (x) (string= (plist-get x :name) "C++"))
                        (ce--languages)))
         (tools (ce--tools (plist-get cpp :id))))
    (should tools)
    (let* ((clangtidy (map-elt tools "clangtidytrunk")))
      (should clangtidy)
      (should (plist-get clangtidy :id))
      (should (plist-get clangtidy :type))
      (should (plist-get clangtidy :languageId))
      (should (plist-get clangtidy :allowStdin)))))

(ert-deftest ce-api-asm-opcode-docs ()
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (ce--asm-opcode-doc "amd64" "pushq"
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
      (ce--asm-opcode-doc "python" "MAKE_FUNCTION"
                          (lambda (doc)
                            (with-current-buffer buf
                              (insert doc))))
      (with-timeout (5 (error "Test timed out"))
        (while (= (buffer-size) 0)
          (accept-process-output)))
      (goto-char (point-min))
      (should (search-forward "Pushes a new function object on the stack.")))))

(ert-deftest ce-api-examples ()
  (let ((examples (ce--examples)))
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
    (setq examples (ce--examples "c++"))
    (should examples)
    (mapc (pcase-lambda (`(,_ . ,data))
            (setq data (ce--example "c++" (plist-get data :file)))
            (should (stringp (plist-get data :file))))
          examples)))

(ert-deftest ce-auto-mode ()
  "Test that major mode is set automatically from language."
  (ce-test--with-session "C++" nil
    (with-current-buffer (get-buffer ce--buffer)
      (should (eq major-mode 'c++-mode)))
    (with-current-buffer (get-buffer ce--compiler-buffer)
      (should (eq major-mode 'asm-mode))))
  (ce-test--with-session "Python" nil
    (with-current-buffer (get-buffer ce--buffer)
      (should (eq major-mode 'python-mode)))))

(ert-deftest ce-auto-recompilation ()
  "Test that modifying the source buffer triggers recompilation."
  (ce-test--with-session "C++" nil
    (ce-test--insert
     "int add(int a, int b) { return a + b; }\n")
    (string-match-p "add(int, int):" (ce-test--compilation-result))
    (ce-test--insert "float sub(float a, float b) { return a - b; }\n")
    (should
     (string-match-p "sub(float, float):" (ce-test--compilation-result)))))

(ert-deftest ce-switching-compilers ()
  "Check that switching compilers works and triggers recompilation."
  (ce-test--with-session "C++" "x86-64 gcc 15.1"
    (ce-test--insert "
#ifdef __clang__
int foo(int) { return 1; }
#else
int bar(int) { return 2; }
#endif
")
    (should (string-match-p "bar(int):" (ce-test--compilation-result)))

    (ce-set-compiler "x86-64 clang (trunk)")
    (ce-test--wait)
    (should (string-match-p "foo(int):" (ce-test--compilation-result)))))

(ert-deftest ce-asm-opcode-docs ()
  "Check that we can see ASM opcode documentation."
  (ce-test--with-session "Assembly" "x86-64 clang (trunk)"
    (ce-set-compiler "x86-64 clang (trunk)")

    (ce-test--insert
     "
foo:
    xor %rdx, %rax
    inc %rcx
    jmpq *%rax
")
    (ce-test--wait)

    (with-current-buffer ce--compiler-buffer

      (goto-char (point-min))
      (search-forward "xor")
      (forward-char -1)
      (should (string-match-p "Performs a bitwise exclusive OR"
                              (ce-test--help-message)))

      (goto-char (point-min))
      (search-forward "inc")
      (forward-char -1)
      (should (string-match-p "Adds 1 to the destination operand"
                              (ce-test--help-message)))

      (goto-char (point-min))
      (search-forward "jmp")
      (forward-char -1)
      (should (string-match-p "Transfers program control to a different point"
                              (ce-test--help-message))))))

(ert-deftest ce-loading-example ()
  "Check that we can load an example."
  (ce-test--with-session "C++" "x86-64 clang (trunk)"
    (with-current-buffer ce--buffer
      (erase-buffer))

    (ce-load-example "Max array")

    (ce-test--wait)

    (with-current-buffer ce--buffer
      (should-not (string-empty-p (buffer-string))))

    (should-not
     (string-match-p "Compilation failed" (ce-test--compilation-result)))))

(ert-deftest ce-setting-compiler-args ()
  "Check that setting arguments works and triggers recompilation."
  (ce-test--with-session "C++" nil
    (ce-test--insert "int f() { return unknown; }\n")
    (should (string-match-p "Compilation failed"
                            (ce-test--compilation-result)))

    (ce-set-compiler-args "-Dunknown=123")
    (ce-test--wait)
    (should (string-match-p "mov *eax, 123" (ce-test--compilation-result)))

    (ce-set-compiler-args "-Dunknown=54321")
    (ce-test--wait)
    (should (string-match-p "mov *eax, 54321" (ce-test--compilation-result)))))

(ert-deftest ce-adding-removing-libs ()
  (ce-test--with-session "C++" nil
    (ce-test--insert "#include <boost/any.hpp>
int foo(boost::any a) { return 1; }")
    (should (string-match-p "Compilation failed"
                            (ce-test--compilation-result)))

    (ce-add-library "boost" "174")
    (ce-test--wait)
    (should (string-match-p "foo(boost::any):" (ce-test--compilation-result)))

    (ce-remove-library "boost")
    (ce-test--wait)
    (should (string-match-p "Compilation failed"
                            (ce-test--compilation-result)))))

(ert-deftest ce-tools ()
  (ce-test--with-session "C++" nil
    (ce-test--insert "#include <map>
#include <string>

int foo(  std::string   a) {     return    1   ; }")

    (ce-add-tool "clangformattrunk")
    (ce-add-tool "iwyu")
    (ce-add-tool "clangtidytrunk")
    (ce-add-tool "clangquerytrunk")
    (ce-set-tool-args "clangtidytrunk" "--help")
    (ce-set-tool-input "clangquerytrunk" "m functionDecl().bind(\"x\")")

    (ce-test--wait)
    (with-current-buffer (format ce--tool-buffer-format "clangformattrunk")
      (should (string= "#include <map>
#include <string>

int foo(std::string a) { return 1; }
"
                       (buffer-string))))
    (with-current-buffer (format ce--tool-buffer-format "iwyu")
      (goto-char (point-min))
      (should (search-forward "should remove these lines:\n- #include <map> "))
      (should (search-forward "The full include-list for <source>:
#include <string>  // for string\n---\n")))
    (with-current-buffer
        (format ce--tool-buffer-format "clangtidytrunk")
      (goto-char (point-min))
      (should
       (search-forward "USAGE: clang-tidy [options]")))
    (with-current-buffer
        (format ce--tool-buffer-format "clangquerytrunk")
      (goto-char (point-min))
      (should (search-forward "Match #4:"))
      (should (re-search-forward "note:.* binds here")))))

(ert-deftest ce-creates-temp-project ()
  (let ((ce-make-temp-file t)
        dir)
    (ce-test--with-session "C++" nil
      (with-current-buffer ce--buffer
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
  (let ((ce-make-temp-file nil))
    (ce-test--with-session "C++" nil
      (with-current-buffer ce--buffer
        (should-not buffer-file-name)
        (should-not (project-current))))))

(ert-deftest ce-execution ()
  (ce-test--with-session "C++" nil
    (ce-test--insert
     "
#include <stdio.h>
int main() {
    puts(\"test\");
    puts(\"foo\");
    return 44;
}")

    (ce-test--wait)
    (should (string-match-p "test\nfoo\n" (ce-test--execution-result)))
    (should (string-match-p "exited with code 44"
                            (ce-test--execution-result)))))

(ert-deftest ce-execution-args ()
  (ce-test--with-session "C++" nil
    (ce-test--insert
     "
#include <stdio.h>
int main(int argc, char** argv) {
    printf(\"arg 1 = %s\\n\", argv[1]);
    printf(\"arg 2 = %s\\n\", argv[2]);
}")

    (ce-set-execution-args "--first    --second")
    (ce-test--wait)

    (should (string-match-p "arg 1 = --first" (ce-test--execution-result)))
    (should (string-match-p "arg 2 = --second" (ce-test--execution-result)))))

(ert-deftest ce-execution-input ()
  (ce-test--with-session "C++" nil
    (ce-test--insert
     "
#include <stdio.h>
int main(int argc, char** argv) {
    char buf[32];
    buf[fread(buf, 1, sizeof(buf), stdin)] = 0;
    printf(\"Input: %s\", buf);
}")

    (ce-set-input "FOO")
    (ce-test--wait)

    (should (string-match-p "Input: FOO" (ce-test--execution-result)))))

(ert-deftest ce-execution-support ()
  (ce-test--with-session "C++" "ARM GCC trunk"
    (ce-test--insert "int main() {}")

    (ce-test--wait)

    (should (string-match-p
             "Error: the ARM GCC trunk compiler does not support execution"
             (ce-test--execution-result)))))

(ert-deftest ce-restoring-killed-session ()
  (ce-test--with-session "C++" nil
    (with-current-buffer ce--buffer
      (ce-set-compiler "x86-64 clang (trunk)")
      (ce-set-compiler-args "-Wall -Wextra")
      (ce-set-execution-args "1 2 3")
      (ce-set-input "test")
      (ce-add-library "boost" "174")
      (ce-add-tool "clangtidytrunk")
      (erase-buffer)
      (insert "int foo();")
      (kill-buffer (current-buffer)))

    (should-not (buffer-live-p (get-buffer ce--compiler-buffer)))
    (should-not (buffer-live-p (get-buffer ce--output-buffer)))
    (should-not (buffer-live-p (get-buffer ce--exe-output-buffer)))
    (should-not (buffer-live-p
                 (get-buffer "*compiler-explorer tool clangtidytrunk*")))
    (should-not ce--language-data)
    (should-not ce--compiler-data)
    (should-not ce--selected-libraries)
    (should (string-empty-p ce--compiler-arguments))
    (should (string-empty-p ce--execution-arguments))
    (should (string-empty-p ce--execution-input))

    (ce-previous-session)
    (with-current-buffer ce--buffer
      (should (string= (buffer-string) "int foo();")))
    (should (string= (plist-get ce--language-data :name) "C++"))
    (should (string= (plist-get ce--compiler-data :name)
                     "x86-64 clang (trunk)"))
    (should (string= ce--compiler-arguments "-Wall -Wextra"))
    (should (string= ce--execution-arguments "1 2 3"))
    (should (string= ce--execution-input "test"))
    (should (string= "boost" (caar ce--selected-libraries)))
    (should (string= "174" (cadar ce--selected-libraries)))
    (should (buffer-live-p
             (get-buffer "*compiler-explorer tool clangtidytrunk*")))))

(ert-deftest ce-session-ring ()
  (let ((ce--session-ring (make-ring 5))
        (ce-new-session-hook nil))
    (dotimes (index 5)
      (ce-test--with-session "C++" nil
        (with-current-buffer ce--buffer
          (erase-buffer)
          (insert (format "// test %s" index))
          (kill-buffer (current-buffer)))))

    (let ((ce--session-ring (ring-copy ce--session-ring)))

      ;; Session "// test 1" is the 4th (index 3) oldest
      (ce-previous-session 3)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 1") (buffer-string))))

      ;; Session "// test 4" is still the newest
      (ce-previous-session 0)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 4") (buffer-string))))

      ;; Session "// test 3" becomes newest after "// test 4"
      (ce-previous-session 0)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 3") (buffer-string))))
      (kill-buffer ce--buffer))

    (let ((ce--session-ring (ring-copy ce--session-ring)))

      ;; Session "// test 4" is the newest
      (ce-previous-session 0)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 4") (buffer-string))))
      (kill-buffer ce--buffer))

    (let ((ce--session-ring (ring-copy ce--session-ring)))

      ;; Session "// test 0" is the oldest
      (ce-previous-session 4)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 0") (buffer-string))))
      (kill-buffer ce--buffer))

    (let ((ce--session-ring (ring-copy ce--session-ring)))

      ;; Session "// test 3" is the second newest
      (ce-previous-session 1)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 3") (buffer-string))))
      (kill-buffer ce--buffer))

    (let ((ce--session-ring
           (ring-copy ce--session-ring)))

      ;; Cycling through all sessions
      (dotimes (i 15)
        (ce-previous-session)
        (with-current-buffer ce--buffer
          (should (string= (format "// test %s" (- 4 (mod i 5)))
                           (buffer-string))))))))

(ert-deftest ce-discard-session ()
  (let ((ce--session-ring (make-ring 5))
        (ce-new-session-hook nil))
    (dotimes (index 5)
      (ce-test--with-session "C++" nil
        (with-current-buffer ce--buffer
          (erase-buffer)
          (insert (format "// test %s" index))
          (kill-buffer (current-buffer)))))

    (let ((ce--session-ring (ring-copy ce--session-ring)))

      (ce-discard-session '(0 4))

      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 3") (buffer-string))))
      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 2") (buffer-string))))
      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 1") (buffer-string)))))

    (let ((ce--session-ring
           (ring-copy ce--session-ring)))

      (ce-discard-session '(1 2 4))

      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 4") (buffer-string))))
      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 1") (buffer-string)))))

    (let ((ce--session-ring
           (ring-copy ce--session-ring)))

      (ce-discard-session '(0))
      (ce-discard-session '(0))
      (ce-discard-session '(2))

      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 2") (buffer-string))))
      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 1") (buffer-string)))))

    (let ((ce--session-ring
           (ring-copy ce--session-ring)))

      (ce-previous-session)
      (ce-discard-session)
      (ce-exit)

      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 3") (buffer-string))))
      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 2") (buffer-string))))
      (ce-previous-session)
      (with-current-buffer ce--buffer
        (should (string= (format "// test 1") (buffer-string)))))))

(ert-deftest ce-restoring-from-shortlink ()
  (let ((url nil)
        (ce-new-session-hook nil))
    (let ((ce--session-ring (make-ring 5)))
      (ce-test--with-session "C++" nil
        (with-current-buffer ce--buffer
          (erase-buffer)
          (insert "// source _12345_")
          (ce-set-compiler "x86-64 clang (trunk)")
          (ce-set-compiler-args "-Wall -Wextra")
          (ce-set-execution-args "1 2 3")
          (ce-set-input "test")
          (ce-add-library "boost" "174")
          (ce-add-tool "clangtidytrunk")
          (ce-set-tool-args "clangtidytrunk" "--help")
          (setq url (ce-make-link)))))

    (ce-restore-from-link url)

    (with-current-buffer ce--buffer
      (should (equal (buffer-string) "// source _12345_"))
      (should (equal (plist-get ce--language-data :name) "C++"))
      (should (equal (plist-get ce--compiler-data :name)
                     "x86-64 clang (trunk)"))
      (should (equal ce--compiler-arguments "-Wall -Wextra"))
      (should (equal ce--execution-arguments "1 2 3"))
      (should (equal ce--execution-input "test"))
      (should (equal (caar ce--selected-libraries) "boost"))
      (should (equal (cadar ce--selected-libraries) "174"))
      (should (equal (caar ce--selected-tools) "clangtidytrunk"))
      (should (equal (caddar ce--selected-tools) "--help"))
      (should (bufferp (cadar ce--selected-tools))))))

(ert-deftest ce-mappings ()
  (ce-test--with-session "C++" nil
    (with-current-buffer ce--buffer
      (ce-set-compiler-args "-O2")
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

      (ce-test--wait)
      (goto-char (point-min))

      (search-forward "foo")
      (save-current-buffer
        (ce-jump)
        (should (equal (current-buffer) (get-buffer ce--compiler-buffer)))
        (should (string-match-p "987654321" (thing-at-point 'line)))
        (forward-line 1)
        (ce-jump)
        (should (equal (current-buffer) (get-buffer ce--buffer)))
        (should (string-match-p "foo() {" (thing-at-point 'line))))

      (search-forward "bar")
      (save-current-buffer
        (ce-jump)
        (should (string-match-p "-1" (thing-at-point 'line)))
        (forward-line 1)
        (ce-jump)
        (should (string-match-p "bar() {" (thing-at-point 'line))))

      (search-forward "quux")
      (save-current-buffer
        (search-forward "foo")
        (ce-jump)
        (should (string-match-p "call.*foo" (thing-at-point 'line))))
      (save-current-buffer
        (search-forward "bar")
        (ce-jump)
        (should (string-match-p "call.*bar" (thing-at-point 'line))))
      (save-current-buffer
        (search-forward "+ c")
        (ce-jump)
        (should (string-match-p "\\badd\\b\\|\\(\\blea\\b.*987654320\\)"
                                (thing-at-point 'line)))))))

(provide 'ce-test)
;;; ce-test.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; read-symbol-shorthands: (("ce-" . "compiler-explorer-"))
;; End:
