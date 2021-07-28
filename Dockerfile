FROM debian:latest
WORKDIR /app
COPY . /app
CMD ["/app/discord-bot-v1-exe"]