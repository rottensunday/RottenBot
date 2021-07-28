FROM debian:latest
WORKDIR /app
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /app
COPY bot.db /app
CMD ["/app/discord-bot-v1-exe"]