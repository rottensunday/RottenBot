FROM haskell:8.10
WORKDIR /app
COPY stack.yaml package.yaml /app/
RUN stack build --only-dependencies
COPY . /app
RUN stack build
CMD [ "stack", "exec", "discord-bot-v1-exe" ]