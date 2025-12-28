emacs ?= emacs
FILES := compiler-explorer.el compiler-explorer-test.el
ELC := $(FILES:.el=.elc)

SELECTOR ?= .*

PACKAGE_INIT := --eval '(package-initialize)'
SESSIONS_FILE := $$PWD/test-sessions.el
TEST_ARGS := $(shell printf "%s '(setq compiler-explorer-sessions-file \"%s\")'" \
	       --eval ${SESSIONS_FILE})

INSTALL_DEPENDENCIES := ${PACKAGE_INIT} --eval '(progn                             \
	(unless (package-installed-p (quote plz))                                  \
	  (push (quote ("melpa" . "https://melpa.org/packages/")) package-archives)\
	  (package-refresh-contents)                                               \
	  (package-install (quote plz))                                            \
	  (package-install (cadr (assoc (quote map) package-archive-contents)))    \
	  (package-install (cadr (assoc (quote seq) package-archive-contents)))    \
	  (package-install (cadr (assoc (quote eldoc) package-archive-contents)))  \
	  (package-install (quote package-lint))))'

# Sexp to fill paragraphs in the commentary section.
FILL_COMMENTARY := --eval '(progn                                                \
	(delete-trailing-whitespace)                                             \
	(setq fill-column 74)                                                    \
	(narrow-to-region (search-forward "Commentary:")                         \
	  (search-forward "Code:"))                                              \
	  (goto-char (point-min))                                                \
	(while (re-search-forward "^;; *." nil t)                                \
	  (goto-char (line-beginning-position))                                  \
	  (if (looking-at-p "^;;  +")                                            \
	      (forward-line 1)                                                   \
	    (fill-paragraph)                                                     \
	    (end-of-line)))                                                      \
	(save-buffer))'

KEYMAP := --eval '(dolist (elt                                                   \
	(list (cons (kbd "<f2> <f2>") (quote compiler-explorer))                 \
	      (cons (kbd "<f2> p") (quote compiler-explorer-previous-session))   \
	      (cons (kbd "<f2> n") (quote compiler-explorer-new-session))        \
	      (cons (kbd "<f2> c") (quote compiler-explorer-set-compiler))       \
	      (cons (kbd "<f2> a") (quote compiler-explorer-set-compiler-args))  \
	      (cons (kbd "<f2> l") (quote compiler-explorer-layout))             \
	      (cons (kbd "<f2> L") (quote compiler-explorer-make-link))          \
	      (cons (kbd "<f2> e") (quote compiler-explorer-show-output))        \
	      (cons (kbd "C-c e RET") (quote compiler-explorer-jump))            \
	      (cons (kbd "<f2> q") (quote compiler-explorer-exit))))             \
  (global-set-key (car elt) (cdr elt)))'

deps:
	${emacs} -Q --batch ${INSTALL_DEPENDENCIES}

compile: deps $(ELC)

%.elc: %.el
	${emacs} -Q --batch ${PACKAGE_INIT} -L .                                 \
	    --eval '(setq byte-compile-error-on-warn t)'                         \
	    -f batch-byte-compile $<

check: ${ELC}
	${emacs} -Q --batch ${PACKAGE_INIT} ${TEST_ARGS}                         \
	      -L . -l compiler-explorer -l compiler-explorer-test                \
	      --eval '(ert-run-tests-batch-and-exit "${SELECTOR}")'

%.lint-checkdoc: %.el
	@file=$$(mktemp)                                                          \
	&& ${emacs} -Q --batch $<                                                \
	  --eval '(checkdoc-file (buffer-file-name))' 2>&1 | tee $$file          \
	&& test -z "$$(cat $$file)"

%.lint-long-lines: %.el
	@grep -n -E "^.{80,}" $< | sed -r '1d;s/^([0-9]+).*/'$<':\1: Too long/;q1'

compiler-explorer.lint-package:
	@file=$$(mktemp)                                                                  \
	&& ${emacs} -Q --batch ${PACKAGE_INIT}                                           \
	  -f 'package-lint-batch-and-exit' compiler-explorer.el 2>$$file || true         \
	&& sed -i "/^Entering directory/d;/doesn't start with package's prefix/d" $$file \
	&& cat $$file                                                                    \
	&& test -z "$$(cat $$file)"

%.lint-package: %.el
	@true

%.lint: %.el %.lint-checkdoc %.lint-long-lines %.lint-package
	@true

lint: $(FILES:.el=.lint)

# Run emacs -Q with packages installed and compiler-explorer loaded
sandbox: ${ELC}
	${emacs} -Q ${PACKAGE_INIT} ${KEYMAP} ${TEST_ARGS}                       \
	        -L . -l compiler-explorer -l compiler-explorer-test

readme-to-el:
	sed README.md -r                                                         \
	    -e 's/^#+ (.*) #*$$/\n;;; \1/'       `# Rewrite headers`             \
	    -e '/^.*License.*/,/^<!/d'           `# Delete license`              \
	    -e '/^<!--/d'                        `# Remove comments`             \
	    -e '/```elisp/,/^```/ s/(.*)/  \1/'  `# Indent code`                 \
	    -e 's/^/;; /'                        `# Add lisp comment char`       \
	    -e 's/`M-x ([^`]+)`/M-x `\1'"'"'/g'  `# Elisp backticks`             \
	    -e 's/`([^`]+)`/`\1'"'"'/g'          `# Elisp backticks`             \
	    -e 's/Emacs package/Package/g'       `# It's obviously for Emacs`    \
	    -e 's/(\[compiler[- ]explorer\]){2,}/https:\/\/godbolt.org/g'        \
	    -e '/^;* \[compiler-explorer/d'                                      \
	    >  commentary.txt                                                    \
	&& sed -i commentary.txt -r -e '/^;; +```/d'                             \
	&& ( sed '1,/^;;; Commentary:/p;d' compiler-explorer.el                  \
	&& echo && cat commentary.txt && echo                                    \
	&& sed '/^;;; Code:/,//p;d' compiler-explorer.el ) > changed.txt         \
	&& rm commentary.txt && mv changed.txt compiler-explorer.el              \
	&& ${emacs} -Q --batch compiler-explorer.el ${FILL_COMMENTARY}

update-copyright-years:
	year=`date +%Y`;                                                         \
	sed -i *.el *.md -r                                                      \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
