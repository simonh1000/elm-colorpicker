'use strict';

require("./styles.css");

const {Elm} = require('./Main');
var app = Elm.Main.init({
        node: document.getElementById("elm-node")
    });
