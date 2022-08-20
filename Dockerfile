#syntax=docker/dockerfile:1
FROM haskell:8.8
WORKDIR /app/meatbar
COPY . .
EXPOSE 8080
ENTRYPOINT stack run 