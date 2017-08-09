'use strict';

require('bootstrap-loader');
require("./styles.scss");

var Elm = require('./App');
var app = Elm.App.fullscreen();
