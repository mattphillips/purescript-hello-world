exports._sendStatus = function (resp, status) {
  return function () {
      resp.sendStatus(status);
  };
};
