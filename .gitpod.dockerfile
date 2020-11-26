FROM gitpod/workspace-full

USER root

# Install Nix
RUN addgroup --system nixbld \
  && adduser gitpod nixbld \
  && for i in $(seq 1 30); do useradd -ms /bin/bash nixbld$i &&  adduser nixbld$i nixbld; done \
  && mkdir -m 0755 /nix && chown gitpod /nix \
  && mkdir -p /etc/nix \
  && echo 'sandbox = false' > /etc/nix/nix.conf \
  && echo 'substituters = https://cache.nixos.org https://hydra.iohk.io' >> /etc/nix/nix.conf \
  && echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' >> /etc/nix/nix.conf

# Install Nix
CMD /bin/bash -l
USER gitpod
ENV USER gitpod
WORKDIR /home/gitpod

RUN touch .bash_profile \
 && curl https://nixos.org/releases/nix/nix-2.3.8/install | sh 

COPY default.nix default.nix
COPY shell.nix shell.nix
COPY cabal.project cabal.project
COPY stack.yaml stack.yaml
COPY .gitignore .gitignore
COPY nix nix

# ls */*.cabal | sed 's|\(.*\)|COPY \1 \1|'
COPY deployment-server/deployment-server.cabal deployment-server/deployment-server.cabal
COPY doc/plutus-doc.cabal doc/plutus-doc.cabal
COPY example/example.cabal example/example.cabal
COPY iots-export/iots-export.cabal iots-export/iots-export.cabal
COPY marlowe-actus/marlowe-actus.cabal marlowe-actus/marlowe-actus.cabal
COPY marlowe-playground-server/marlowe-playground-server.cabal marlowe-playground-server/marlowe-playground-server.cabal
COPY marlowe-symbolic/marlowe-symbolic.cabal marlowe-symbolic/marlowe-symbolic.cabal
COPY marlowe/marlowe.cabal marlowe/marlowe.cabal
COPY playground-common/playground-common.cabal playground-common/playground-common.cabal
COPY plutus-benchmark/plutus-benchmark.cabal plutus-benchmark/plutus-benchmark.cabal
COPY plutus-contract/plutus-contract.cabal plutus-contract/plutus-contract.cabal
COPY plutus-core/plutus-core.cabal plutus-core/plutus-core.cabal
COPY plutus-ledger/plutus-ledger.cabal plutus-ledger/plutus-ledger.cabal
COPY plutus-metatheory/plutus-metatheory.cabal plutus-metatheory/plutus-metatheory.cabal
COPY plutus-playground-server/plutus-playground-server.cabal plutus-playground-server/plutus-playground-server.cabal
COPY plutus-scb/plutus-scb.cabal plutus-scb/plutus-scb.cabal
COPY plutus-tx-plugin/plutus-tx-plugin.cabal plutus-tx-plugin/plutus-tx-plugin.cabal
COPY plutus-tx/plutus-tx.cabal plutus-tx/plutus-tx.cabal
COPY plutus-use-cases/plutus-use-cases.cabal plutus-use-cases/plutus-use-cases.cabal
COPY prettyprinter-configurable/prettyprinter-configurable.cabal prettyprinter-configurable/prettyprinter-configurable.cabal
COPY web-ghc/web-ghc.cabal web-ghc/web-ghc.cabal

RUN . /home/gitpod/.nix-profile/etc/profile.d/nix.sh \
  && nix-env -f nix/gitpod-shell.nix -iA buildInputs \
  && nix-env -f nix/gitpod-shell.nix -iA nativeBuildInputs
