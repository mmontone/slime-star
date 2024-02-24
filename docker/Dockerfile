FROM silex/emacs:27.2

RUN apt-get update && apt-get install -y make bzip2 wget git
RUN wget 'http://prdownloads.sourceforge.net/sbcl/sbcl-2.2.2-x86-64-linux-binary.tar.bz2' -O /tmp/sbcl.tar.bz2 && \
    mkdir /tmp/sbcl && \
    tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /tmp/sbcl && \
    cd /tmp/sbcl && \
    sh install.sh && \
    cd /tmp \
    rm -rf /tmp/sbcl

RUN cd /tmp && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --quit --eval '(quicklisp-quickstart:install)'
COPY sbclrc /root/.sbclrc

RUN sbcl --eval '(ql:quickload (list :asdf :alexandria :anaphora :drakma :dexador :fiveam :closer-mop :iterate :do-urlencode :yason :html-entities :slite :montezuma :chipz :archive :string-case :lisp-critic))' --quit

RUN mkdir ~/.emacs.d
RUN cd ~/.emacs.d && git clone --recursive https://github.com/mmontone/slime-star.git

ADD init.el /root/.emacs.d/