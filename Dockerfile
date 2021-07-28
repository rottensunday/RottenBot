FROM debian:latest
WORKDIR /app
COPY ~/.local/bin /app
COPY bot.db /app
CMD ["/app/discord-bot-v1-exe"]