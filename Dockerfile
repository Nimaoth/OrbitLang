FROM ubuntu

RUN  apt-get update \
  && apt-get install -y wget curl xz-utils git

WORKDIR /tmp

# Install zig
RUN wget https://ziglang.org/download/0.8.0/zig-linux-x86_64-0.8.0.tar.xz
RUN tar xf zig-linux-x86_64-0.8.0.tar.xz
RUN cp -r ./zig-linux-x86_64-0.8.0 /home/zig
RUN ln -s /home/zig/zig /usr/bin/zig

# Install zls
WORKDIR /home/zls
RUN curl -L https://github.com/zigtools/zls/releases/download/0.9.0/x86_64-linux.tar.xz | tar -xJ --strip-components=1 -C .
RUN chmod +x zls
RUN ln -s /home/zls/zls /usr/bin/zls