FROM gcc:11

RUN apt update && \
    apt install -y nasm lsb-release wget software-properties-common && \
    bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
