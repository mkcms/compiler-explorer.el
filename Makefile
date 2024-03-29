emacs ?= emacs
FILES := compiler-explorer.el compiler-explorer-test.el
ELC := $(FILES:.el=.elc)

SELECTOR ?= .*

PACKAGE_INIT := -Q --eval '(package-initialize)'                              \
            --eval '(setq compiler-explorer-sessions-file "test-sessions.el")'

ARGS := --batch ${PACKAGE_INIT}

COMPILE_ARGS := --eval '(progn                                                \
	(unless (package-installed-p (quote request))                         \
	  (push (cons "melpa" "https://melpa.org/packages/") package-archives)\
	  (package-refresh-contents)                                          \
	  (package-install (quote request))))'

# Sexp to fill paragraphs in the commentary section.
FILL_COMMENTARY := --eval '(progn                                             \
	(delete-trailing-whitespace)                                          \
        (setq fill-column 74)                                                 \
	(fill-individual-paragraphs (search-forward "Commentary:")            \
	                            (search-forward "Code:"))                 \
	(save-buffer))'

KEYMAP := --eval '(dolist (elt                                                \
        `((,(kbd "<f2> <f2>") . compiler-explorer)                            \
          (,(kbd "<f2> p") . compiler-explorer-previous-session)              \
          (,(kbd "<f2> n") . compiler-explorer-new-session)                   \
          (,(kbd "<f2> c") . compiler-explorer-set-compiler)                  \
          (,(kbd "<f2> a") . compiler-explorer-set-compiler-args)             \
          (,(kbd "<f2> l") . compiler-explorer-layout)                        \
          (,(kbd "<f2> L") . compiler-explorer-make-link)                     \
          (,(kbd "<f2> e") . compiler-explorer-show-output)))                 \
  (global-set-key (car elt) (cdr elt)))'

compile: $(ELC)

%.elc: %.el
	${emacs} ${ARGS} ${COMPILE_ARGS} -L . -f batch-byte-compile $<

check: ${ELC}
	${emacs} ${ARGS} -L . -l compiler-explorer -l compiler-explorer-test  \
	      --eval '(ert-run-tests-batch-and-exit "${SELECTOR}")'

# Run emacs -Q with packages installed and compiler-explorer loaded
_baremacs:
	rm -f test-sessions.el;                                               \
	${emacs} ${PACKAGE_INIT} ${KEYMAP}                                    \
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
	&& ${emacs} -Q --batch ${FILL_COMMENTARY} compiler-explorer.el

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.md -r                                                   \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
