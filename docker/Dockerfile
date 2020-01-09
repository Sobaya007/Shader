FROM nvidia/cudagl:10.1-base-ubuntu18.04

RUN apt update && apt install -y curl libfreeimage-dev libfreetype6-dev libglfw3-dev libassimp-dev libvulkan-dev gnupg2 xz-utils build-essential wget git

# Install Vulkan SDK
RUN wget -qO - http://packages.lunarg.com/lunarg-signing-key-pub.asc | apt-key add - && \
wget -qO /etc/apt/sources.list.d/lunarg-vulkan-1.1.121-bionic.list http://packages.lunarg.com/vulkan/1.1.121/lunarg-vulkan-1.1.121-bionic.list && \
apt update && apt install -y vulkan-sdk && apt upgrade -y && apt autoremove -y
ENV NVIDIA_DRIVER_CAPABILITIES compute,graphics,utility
COPY nvidia_icd.json /etc/vulkan/icd.d/nvidia_icd.json

# Download D Compiler
RUN apt install libcurl4-openssl-dev
RUN wget https://netcologne.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
RUN apt update --allow-insecure-repositories
RUN apt -y --allow-unauthenticated install --reinstall d-apt-keyring
RUN apt update && apt install -y dmd-compiler dub
