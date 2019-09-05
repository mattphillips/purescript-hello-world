
exports.mkTestApplication = function(handler) {
  return req => res => () => {
    const express = require('express');
    const app = express();
    handler.value0(app)() // install the handler aka app.method(path, function)
    app(req, res)
  };
};

const httpMocks = require('node-mocks-http');
exports.mkRequest = httpMocks.createRequest;
exports.mkResponse = httpMocks.createResponse;

exports.getStatus = function(res) {
  return res.statusCode;
};

exports.getBody = function(res) {
  return res._getData();
};
