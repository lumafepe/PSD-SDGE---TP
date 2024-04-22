# Use a base image with Erlang installed
FROM erlang:24

# Set the working directory inside the container
WORKDIR /app

# Copy the content of the src folder into the container
COPY src /app/src

#Copy protobuf specification
COPY messages.proto messages.proto
COPY rebar.config rebar.lock ./

RUN mkdir ebin

# Clone the git repository
RUN git clone https://github.com/tomas-abrahamsson/gpb.git \
    && cd gpb \
    && make 

RUN gpb/bin/protoc-erl -I. -maps -o src messages.proto 
RUN erlc -I gpb/include -o ebin src/messages.erl 
RUN rebar3 compile
# Expose the port
EXPOSE 4321

# Start the Erlang server
ENTRYPOINT ["rebar3", "shell", "application:start(server)"]
