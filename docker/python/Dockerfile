FROM python:3.6-slim as build
WORKDIR /build
ADD . /build
ENV WORKON_HOME=.
#RUN pip install --trusted-host pypi.python.org pew
#RUN pew new -d venv
#RUN pew in venv pip install  --trusted-host pypi.python.org -r requirements.txt
RUN mkdir wheelhouse
RUN pip3 wheel -r requirements.txt -w wheelhouse




FROM alpine:latest as production
RUN apk --no-cache add python3

RUN mkdir wheelhouse
COPY --from=0 /build/wheelhouse wheelhouse
ADD requirements.txt .
RUN pip3 install --no-cache-dir --no-index -f wheelhouse -r requirements.txt
RUN rm -rf wheelhouse

RUN mkdir /app
WORKDIR /app
ADD app.py /app

EXPOSE 8008
ENV NAME World
CMD ["python3", "app.py"]
