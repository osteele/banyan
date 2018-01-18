import './src/Main.scss';
import './src/bootstrap.css'

import Dropbox from 'dropbox';
import Elm from './src/Main.elm';
import chart from './src/chart.js';

const accessTokenKey = "accessToken";
const app = Elm.Main.embed(document.getElementById('app'), {
    accessToken: localStorage[accessTokenKey] || null,
    clientId: process.env.DROPBOX_APP_KEY
});

app.ports.listFiles.subscribe(async (accessToken) => {
    const path = '';
    const dbx = new Dropbox({ accessToken });
    let cache = localStorage['fileTree'] && JSON.parse(localStorage['fileTree']);
    if (cache && cache.accessToken !== accessToken) {
        console.info('reset cache')
        cache = null;
    }
    cache = cache || { accessToken, entries: {} };
    let entries = Object.values(cache.entries);
    if (entries.length) {
        console.info('initial send', entries.length, 'entries');
        app.ports.receiveFileList.send([entries, true]);
    }
    let query = cache.cursor
        ? dbx.filesListFolderContinue({ cursor: cache.cursor })
        : dbx.filesListFolder({ path, recursive: true });
    while (query) {
        var response
        try {
            response = await query;
        } catch (error) {
            console.error(error);
            app.ports.receiveFileListError.send(); // error.name, error.message
        }

        let entries = response.entries.map((entry) => (
            {
                tag: entry['.tag']
                , key: entry.path_lower
                , path: entry.path_display
                , size: entry.size || null
            }
        ));

        // update the cache
        cache.cursor = response.cursor;
        entries.forEach((entry) => {
            cache.entries[entry.key] = entry;
        });
        localStorage['fileTree'] = JSON.stringify(cache);

        const more = Boolean(response.has_more);
        app.ports.receiveFileList.send([entries, more]);

        query = more && dbx.filesListFolderContinue({ cursor: response.cursor });
    }
});

app.ports.getAccountInfo.subscribe(async accessToken => {
    const dbx = new Dropbox({ accessToken });
    const { name, team } = await dbx.usersGetCurrentAccount();
    const teamName = team ? team.name : "Personal";
    app.ports.receiveAccountInfo.send({ teamName, name });
});

app.ports.storeAccessToken.subscribe((value) => {
    const key = accessTokenKey;
    if (value) {
        localStorage[key] = value;
    } else {
        localStorage.clear();
        // localStorage.removeItem(key)
    }
});

app.ports.signOut.subscribe(() => {
    localStorage.clear();
});

app.ports.chart.subscribe(([title, data]) => {
    const onClick = ({ key }) => key && app.ports.setPath.send(key);
    requestAnimationFrame(() => chart(title, data, onClick))
});
