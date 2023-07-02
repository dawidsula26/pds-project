FROM haskell
RUN mkdir /front
COPY . /front
WORKDIR /front
RUN stack install
EXPOSE 8080
ENTRYPOINT ["front-exe"]
