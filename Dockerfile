#syntax=docker/dockerfile:1
FROM haskell:8.8
WORKDIR /app/meatbar
COPY src/ src/
COPY webapp/ webapp/
COPY app/ app/
COPY meatbar.cabal . 
COPY package.yaml .
COPY stack.yaml .
COPY brittany.yaml .
COPY cloudwatch/amazon-cloudwatch-agent.json /opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json
EXPOSE 8080
RUN stack build --allow-different-user
RUN curl -O https://s3.amazonaws.com/amazoncloudwatch-agent/debian/amd64/latest/amazon-cloudwatch-agent.deb && \
    dpkg -i -E amazon-cloudwatch-agent.deb 
ENTRYPOINT stack run --allow-different-user 