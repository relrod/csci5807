FROM ubuntu
MAINTAINER Ricky Elrod <ricky@elrod.me>

RUN apt-get update && apt-get install -y ghc cabal-install zlib1g-dev
RUN mkdir projectone
COPY . projectone
WORKDIR projectone
RUN cabal update && cabal install cabal-install -j
RUN ~/.cabal/bin/cabal sandbox init
RUN ~/.cabal/bin/cabal install --only-dependencies -j --constraint='blaze-markup < 0.6.3'
RUN ~/.cabal/bin/cabal configure
RUN ~/.cabal/bin/cabal install -j
