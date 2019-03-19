require('./main.css');
require('../../src/Toast/Defaults.css');

var myApp = require('./App.elm');

myApp.Elm.App.init({
  node: document.getElementById('root')
});
