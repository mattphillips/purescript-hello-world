exports.mkTestApplication =  () => require('express')();

exports.mountApp = handler => application => {
  // install the handler aka application.method(path, function)
  handler.value0(application)()
  return application;
}

exports.sendRequest = app => req => res => () => app(req, res);

const httpMocks = require('node-mocks-http');

exports.mkRequest = httpMocks.createRequest;

exports._mkResponse = httpMocks.createResponse;

exports.getStatus = res => res.statusCode;

exports.getBody = res => res._getData();
