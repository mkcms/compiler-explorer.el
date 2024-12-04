emacs ?= emacs
FILES := compiler-explorer.el compiler-explorer-test.el
ELC := $(FILES:.el=.elc)

SELECTOR ?= .*

PACKAGE_INIT := --eval '(package-initialize)'
TEST_ARGS := --eval '(setq compiler-explorer-sessions-file "test-sessions.el")'

INSTALL_DEPENDENCIES := ${PACKAGE_INIT} --eval '(progn                        \
	(unless (package-installed-p `plz)                                    \
	  (package-refresh-contents)                                          \
	  (package-install `plz)                                              \
	  (package-install (cadr (assoc `map package-archive-contents)))      \
	  (package-install (cadr (assoc `seq package-archive-contents)))      \
	  (package-install (cadr (assoc `eldoc package-archive-contents)))))'

# Sexp to fill paragraphs in the commentary section.
FILL_COMMENTARY := --eval '(progn                                             \
	(delete-trailing-whitespace)                                          \
	(setq fill-column 74)                                                 \
	(narrow-to-region (search-forward "Commentary:")                      \
                          (search-forward "Code:"))                           \
        (goto-char (point-min))                                               \
	(while (re-search-forward "^;; *." nil t)                             \
	  (fill-paragraph)                                                    \
          (end-of-line))                                                      \
	(save-buffer))'

KEYMAP := --eval '(dolist (elt                                                \
	`((,(kbd "<f2> <f2>") . compiler-explorer)                            \
	  (,(kbd "<f2> p") . compiler-explorer-previous-session)              \
	  (,(kbd "<f2> n") . compiler-explorer-new-session)                   \
	  (,(kbd "<f2> c") . compiler-explorer-set-compiler)                  \
	  (,(kbd "<f2> a") . compiler-explorer-set-compiler-args)             \
	  (,(kbd "<f2> l") . compiler-explorer-layout)                        \
	  (,(kbd "<f2> L") . compiler-explorer-make-link)                     \
	  (,(kbd "<f2> e") . compiler-explorer-show-output)                   \
	  (,(kbd "C-c e RET") . compiler-explorer-jump)                       \
	  (,(kbd "<f2> q") . compiler-explorer-exit)))                        \
  (global-set-key (car elt) (cdr elt)))'

.PHONY: deps
deps:
	${emacs} -Q --batch ${INSTALL_DEPENDENCIES}

compile: deps $(ELC)

%.elc: %.el
	${emacs} -Q --batch ${PACKAGE_INIT} -L .                              \
	    --eval '(setq byte-compile-error-on-warn t)'                      \
	    -f batch-byte-compile $<

check: ${ELC}
	${emacs} -Q --batch ${PACKAGE_INIT} ${TEST_ARGS}                      \
	      -L . -l compiler-explorer -l compiler-explorer-test             \
	      --eval '(ert-run-tests-batch-and-exit "${SELECTOR}")'

lint:
	file=$$(mktemp)                                                       \
	&& ${emacs} -Q --batch compiler-explorer.el                           \
		--eval '(checkdoc-file (buffer-file-name))' 2>&1 | tee $$file \
	&& test -z "$$(cat $$file)"                                           \
	&& (grep -n -E "^.{80,}" compiler-explorer.el `# Catch long lines`    \
	    | sed                                                             \
		-r '1d;2d;s/^([0-9]+).*/compiler-explorer.el:\1: Too long/;q1')

# Run emacs -Q with packages installed and compiler-explorer loaded
_baremacs: ${ELC}
	rm -f test-sessions.el;                                               \
	${emacs} -Q ${PACKAGE_INIT} ${KEYMAP} ${TEST_ARGS}                    \
	        -L . -l compiler-explorer -l compiler-explorer-test

readme-to-el:
	sed README.md -r                                                      \
	    -e 's/^#+ (.*) #*$$/\n;;; \1/'      `# Rewrite headers`           \
	    -e '/^.*License.*/,/^<!/d'          `# Delete license`            \
	    -e '/^<!--/d'                       `# Remove comments`           \
	    -e 's/^/;; /'                       `# Add lisp comment char`     \
	    -e 's/`M-x ([^`]+)`/M-x `\1'"'"'/g' `# Elisp backticks`           \
	    -e 's/Emacs package/Package/g'      `# It's obviously for Emacs`  \
	    -e 's/(\[compiler[- ]explorer\]){2,}/https:\/\/godbolt.org/g'     \
	    -e '/^;* \[compiler-explorer/d'                                   \
	    >  commentary.txt                                                 \
	&& ( sed '1,/^;;; Commentary:/p;d' compiler-explorer.el               \
	&& echo && cat commentary.txt && echo                                 \
	&& sed '/^;;; Code:/,//p;d' compiler-explorer.el ) > changed.txt      \
	&& rm commentary.txt && mv changed.txt compiler-explorer.el           \
	&& ${emacs} -Q --batch compiler-explorer.el ${FILL_COMMENTARY}

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.md -r                                                   \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
