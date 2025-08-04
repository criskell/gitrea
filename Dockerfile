FROM ubuntu:12.04

RUN sed -i 's|http://archive.ubuntu.com/ubuntu/|http://old-releases.ubuntu.com/ubuntu/|g' /etc/apt/sources.list && \
    sed -i 's|http://security.ubuntu.com/ubuntu|http://old-releases.ubuntu.com/ubuntu|g' /etc/apt/sources.list

RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    libgmp3c2 \
    libgmp-dev \
    make \
    m4 \
    ncurses-dev \
    xz-utils \
    libtinfo-dev \
    zlib1g-dev \
    libbz2-dev

RUN curl -LO https://downloads.haskell.org/~ghc/7.6.2/ghc-7.6.2-x86_64-unknown-linux.tar.bz2 && \
    tar -xjf ghc-7.6.2-x86_64-unknown-linux.tar.bz2 && \
    cd ghc-7.6.2 && \
    ./configure && \
    make install && \
    cd .. && \
    rm -rf ghc-7.6.2*

RUN curl -LO https://downloads.haskell.org/~cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-x86_64-unknown-linux.tar.xz && \
    tar -xf cabal-install-2.4.1.0-x86_64-unknown-linux.tar.xz -C /usr/local/bin cabal && \
    rm cabal-install-2.4.1.0-x86_64-unknown-linux.tar.xz

COPY . /workspace
WORKDIR /workspace

RUN cabal update && \
  cabal install --only-dependencies --force-reinstalls && \
  cabal configure && \
  ghc-pkg unregister bytestring-0.10.0.2 --force

CMD ["/bin/bash"]
