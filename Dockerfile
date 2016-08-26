FROM haskell:7.10.3

RUN cabal update
RUN cabal install happy yesod-bin
