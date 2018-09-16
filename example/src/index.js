'use strict';

require("./styles.scss");

const {Elm} = require('./Main');
var app = Elm.Main.init({
        node: document.getElementById("elm-node")
    });
