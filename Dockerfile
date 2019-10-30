FROM silex/emacs:master

ENV LANG=en_US.UTF-8
WORKDIR /root

# For debug
# RUN mkdir -p ~/.emacs.d
# COPY . /root/.emacs.d/

RUN apt-get update && apt-get install -y \
    wget \
    git &&\
    rm -rf /var/lib/apt/lists/* && \

    wget https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb && \
    dpkg -i ripgrep_11.0.2_amd64.deb && \
    rm ripgrep_11.0.2_amd64.deb && \
    rm -rf ~/.emacs.d && \
    git clone https://github.com/braineo/fate-emacs ~/.emacs.d && \
    cd ~/.emacs.d && \
    chmod +x ./.travis/test-startup.sh && \
    ./.travis/test-startup.sh

