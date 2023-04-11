# syntax=docker/dockerfile:1
FROM --platform=linux/amd64 python:3.11.2

RUN apt-get update && apt install -y \  
    sudo \
    git \
    vim \ 
    ncurses-dev \
    neovim \ 
    ghc \
    wget \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt requirements.txt 

RUN bash -c "pip install -r requirements.txt"

RUN stack build
