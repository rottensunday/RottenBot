FROM haskell:8.10
WORKDIR /app
COPY . /app
RUN stack build
CMD [ "stack", "exec", "discord-bot-v1-exe" ]