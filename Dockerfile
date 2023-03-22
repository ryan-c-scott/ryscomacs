from silex/emacs
COPY example/docker-init.el /root/.emacs.d/init.el
COPY *.el /root/.emacs.d/elisp/
RUN apt update
RUN apt install -y git graphviz gnuplot
RUN emacs --batch --eval '(make-directory-autoloads "/root/.emacs.d/elisp" "/root/.emacs.d/elisp/rysco-docker-autoloads.el")'
RUN emacs --batch -l /root/.emacs.d/init.el --eval '(byte-recompile-directory "/root/.emacs.d/elisp" 0 t)'

