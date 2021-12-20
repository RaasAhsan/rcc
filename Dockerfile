FROM gcc:11

RUN apt update && \
    apt install -y nasm
