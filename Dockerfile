FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    emacs \
    ruby \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ARG USER
ARG GROUP

RUN userdel -r ubuntu

RUN groupadd ${USER} \
  && useradd ${USER} -g ${GROUP} -m

USER ${USER}

WORKDIR /home/${USER}/work

ENV IN_CONTAINER=1
ENV LC_ALL=C.utf8
