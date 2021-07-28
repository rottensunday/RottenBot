FROM debian:latest
WORKDIR /app
COPY ~/.local/bin/discord-bot-v1-exe /app
COPY bot.db /app
CMD ["/app/discord-bot-v1-exe"]