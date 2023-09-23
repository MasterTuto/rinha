FROM swipl

USER 0

WORKDIR /var/rinha

COPY . /var/rinha/

RUN apt-get update && apt-get install -y make
RUN make

run cd /var/rinha

ENTRYPOINT [ "./rinha" ]