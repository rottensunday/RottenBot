FROM ubuntu:latest
WORKDIR /app
COPY . /app
RUN /bin/sh -c 'apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates && \     
  rm -rf /var/lib/apt/lists/*'
# CMD ["/app/discord-bot-v1-exe"]
CMD echo "TEST"