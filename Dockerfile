FROM silex/emacs:master

ENV LANG=en_US.UTF-8
WORKDIR /root

# For debug
# RUN mkdir -p ~/.emacs.d
# COPY . /root/.emacs.d/

RUN apt update && \
    apt install -y git &&\
    git clone https://github.com/braineo/fate-emacs ~/.emacs.d && \
    cd ~/.emacs.d && \
    chmod +x ./.travis/test-startup.sh && \
    ./.travis/test-startup.sh

