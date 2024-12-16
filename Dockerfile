FROM gradle:jdk14

WORKDIR /root

RUN git clone https://github.com/crytic/solc-select.git && cd solc-select && git checkout add3f08609d902bdceb11c6007a8d59a50b49fb5
RUN ./solc-select/scripts/install.sh

ENV PATH="/root/.solc-select:${PATH}"

COPY build.gradle /hcc/build.gradle
COPY settings.gradle /hcc/settings.gradle
COPY ./src /hcc/src
COPY ./scripts/run.sh /hcc/run.sh

#compile
WORKDIR /hcc
RUN gradle clean build --no-daemon
ENTRYPOINT ["/hcc/run.sh"]
