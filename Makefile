emacs ?= emacs
FILES := compiler-explorer.el compiler-explorer-test.el
ELC := $(FILES:.el=.elc)

SELECTOR ?= .*

ARGS := --batch -Q --eval '(package-initialize)'                              \
	    --eval '(setq compiler-explorer-sessions-file "test-sessions.el")'

COMPILE_ARGS := --eval '(progn                                                \
	(unless (package-installed-p (quote request))                         \
	  (push (cons "melpa" "https://melpa.org/packages/") package-archives)\
	  (package-refresh-contents)                                          \
	  (package-install (quote request))))'

# Sexp to fill paragraphs in the commentary section.
FILL_COMMENTARY := '(progn                                                    \
	(delete-trailing-whitespace)                                          \
	(fill-individual-paragraphs (search-forward "Commentary:")            \
	                            (search-forward "Code:"))                 \
	(save-buffer))'

compile: $(ELC)

%.elc: %.el
	${emacs} ${ARGS} ${COMPILE_ARGS} -L . -f batch-byte-compile $<

check: ${ELC}
	${emacs} ${ARGS} -L . -l compiler-explorer -l compiler-explorer-test  \
	      --eval '(ert-run-tests-batch-and-exit "${SELECTOR}")'

readme-to-el:
	sed README.md -r                                                      \
	    -e 's/^#+ (.*) #*$$/\n;;; \1/'      `# Rewrite headers`           \
	    -e '/^.*License.*/,/^<!/d'          `# Delete license`            \
	    -e '/^<!--/d'                       `# Remove comments`           \
	    -e 's/^/;; /'                       `# Add lisp comment char`     \
	    -e 's/`M-x ([^`]+)`/M-x `\1'"'"'/g' `# Elisp backticks`           \
	    -e 's/Emacs package/Package/g'      `# It's obviously for Emacs`  \
	    >  commentary.txt                                                 \
	&& ( sed '1,/^;;; Commentary:/p;d' compiler-explorer.el               \
	&& echo && cat commentary.txt && echo                                 \
	&& sed '/^;;; Code:/,//p;d' compiler-explorer.el ) > changed.txt      \
	&& rm commentary.txt && mv changed.txt compiler-explorer.el           \
	&& ${emacs} -Q --batch  compiler-explorer.el                          \
	    --eval ${FILL_COMMENTARY}

clean:
	rm -f *.elc
