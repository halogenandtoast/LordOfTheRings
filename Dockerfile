# syntax=docker/dockerfile:1.7

# Stage 1: build the Haskell binary with stack. The haskell image's ghc matches
# stack.yaml's compiler, which stack.yaml tells stack to use (system-ghc).
FROM haskell:9.14.1 AS builder

WORKDIR /build

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       ca-certificates curl gnupg lsb-release \
       libgmp-dev zlib1g-dev pkg-config \
  && install -d /usr/share/postgresql-common/pgdg \
  && curl -fsSLo /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
       https://www.postgresql.org/media/keys/ACCC4CF8.asc \
  && echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
       > /etc/apt/sources.list.d/pgdg.list \
  && apt-get update \
  && apt-get install -y --no-install-recommends libpq-dev \
  && rm -rf /var/lib/apt/lists/*

# Pull in just the dependency descriptors first so dependency compilation
# can be cached independently from source changes.
COPY stack.yaml stack.yaml.lock package.yaml LordOfTheRings.cabal ./
RUN stack build --only-dependencies

COPY . .
RUN stack install --local-bin-path /out LordOfTheRings:exe:LordOfTheRings-exe

# Stage 2: minimal runtime.
FROM debian:bookworm-slim AS runtime

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       libgmp10 \
       libtinfo6 \
       libpq5 \
       zlib1g \
       ca-certificates \
       netbase \
  && rm -rf /var/lib/apt/lists/* \
  && useradd --create-home --shell /usr/sbin/nologin lotr

USER lotr
WORKDIR /home/lotr

COPY --from=builder /out/LordOfTheRings-exe /usr/local/bin/LordOfTheRings-exe
# Decks resolve against the card browser's database.
COPY frontend/public/data/cards.json /home/lotr/cards.json

ENV PORT=3000
ENV CARDS_JSON=/home/lotr/cards.json
EXPOSE 3000

CMD ["LordOfTheRings-exe"]
