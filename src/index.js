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
                var more = (pages == null || --pages > 0) && response.has_more;
                app.ports.fileList.send([files, Boolean(more)]);
                if (more) {
                    listFiles(dbx.filesListFolderContinue({ cursor: response.cursor }), pages);
                }
            })
            .catch((error) => {
                console.log(error);
                app.ports.fileListError.send();
            });
    listFiles(dbx.filesListFolder({ path: '', recursive: true }), pages || null);
});

app.ports.getAccountInfo.subscribe((accessToken) => {
    var dbx = new Dropbox({ accessToken });
    dbx.usersGetCurrentAccount()
        .then((response) => {
            // console.info(response);
            var teamName = response.team ? response.team.name : "Personal";
            app.ports.setAccountInfo.send({
                teamName,
                name: response.name
            });
        })
        .catch((error) => {
            console.log(error);
        })
});
