const morgan = require('morgan');

exports.requestLogger = morgan('[:date[clf]] --> :method :url <-- :status :res[content-length]b - :response-time ms');
