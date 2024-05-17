FROM maven:3.9.6

# Install prerequisites
RUN apt-get update
RUN apt-get install -y protobuf-compiler

COPY . .

RUN mvn install

ENTRYPOINT ["/usr/bin/mvn", "exec:java"]
