'use strict';

require('./index.html');
var Elm = require('./Main');
var Dropbox = require('dropbox')

var app = Elm.Main.embed(document.getElementById('main'));
app.ports.dropboxClientID.send(process.env.DROPBOX_APP_KEY);

app.ports.listFiles.subscribe((accessToken, pages) => {
    var dbx = new Dropbox({ accessToken });
    var listFiles = (fn, pages) =>
        fn
            .then((response) => {
                var files = response.entries.map((entry) => {
                    return {
                        tag: entry['.tag']
                        , key: entry.path_lower
                        , path: entry.path_display
                        , size: entry.size || null
                    };
                })
                // console.info(files)
                app.ports.fileList.send([files, Boolean(response.has_more)]);
                if (pages == 0 || --pages > 0 && response.has_more) {
                    listFiles(dbx.filesListFolderContinue({ cursor: response.cursor }), pages);
                }
            })
            .catch((error) => {
                console.log(error);
            });
    listFiles(dbx.filesListFolder({ path: '', recursive: true }), pages || 0);
});
