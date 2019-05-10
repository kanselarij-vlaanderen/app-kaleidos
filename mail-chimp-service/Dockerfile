FROM semtech/mu-javascript-template

WORKDIR /app
EXPOSE 80
ADD package.json /app/
RUN npm install
ADD . /app/
CMD node .
