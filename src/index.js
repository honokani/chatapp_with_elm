require('./index.html');
require('./main.css');
//require('./favicon.ico');

const { Elm } = require("./Main.elm");
var mountNode = document.getElementById('elmapp');
var app = Elm.Main.init({node:mountNode});

