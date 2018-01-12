'use strict';

require('./index.html');
var Elm = require('./Main');

var app = Elm.Main.embed(document.getElementById('main'));

app.ports.requestConfig.subscribe(function () {
    console.info('requested')
    app.ports.dropboxClientID.send('a key');
    console.info('sent')
});
