FROM silex/emacs
COPY example/docker-init.el /root/.emacs.d/init.el
COPY example/docker-build.el /root/.emacs.d/docker-build.el
COPY *.el /root/.emacs.d/elisp/
COPY test /root/.emacs.d/test
RUN apt update
RUN apt install -y git graphviz gnuplot ledger
RUN emacs --batch --eval '(make-directory-autoloads "/root/.emacs.d/elisp" "/root/.emacs.d/elisp/rysco-docker-autoloads.el")'
RUN emacs --batch -l /root/.emacs.d/docker-build.el
# TODO: This is causing problems currently
#RUN emacs --batch --eval '(byte-recompile-directory "/root/.emacs.d/elisp" 0 t)'

