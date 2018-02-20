import Dropbox from 'dropbox';
import 'semantic-ui-css/semantic.min.css';
import './src/Main.scss';

import Elm from './src/Main.elm';
import chart from './src/chart';

const accessTokenKey = 'accessToken';
const fileCacheKey = 'fileTree';

let accessToken = localStorage[accessTokenKey] || null;

const app = Elm.Main.embed(document.getElementById('app'), {
  accessToken,
  clientId: process.env.DROPBOX_APP_KEY,
});

app.ports.listFiles.subscribe(async ([followCursor, useCache]) => {
  const path = '';
  if (!accessToken) {
    app.ports.receiveFileListError.send('internal error: access token not set');
    return;
  }
  const dbx = new Dropbox({ accessToken });
  let cache = null;
  if (!useCache && localStorage[fileCacheKey]) {
    cache = JSON.parse(localStorage[fileCacheKey]);
  }
  if (cache && cache.accessToken !== accessToken) {
    console.info('reset cache');
    cache = null;
  }
  cache = cache || { accessToken, entries: {} };
  {
    const entries = Object.values(cache.entries);
    if (entries.length) {
      console.info('initial send', entries.length, 'entries');
      app.ports.receiveFileList.send([entries, true]);
    }
  }
  let query = cache.cursor
    ? dbx.filesListFolderContinue({ cursor: cache.cursor })
    : dbx.filesListFolder({ path, include_deleted: followCursor, recursive: true });
  // eslint-disable-next-line no-await-in-loop
  while (query) {
    let response;
    try {
      response = await query;
    } catch (error) {
      console.error('listFiles:', error);
      app.ports.receiveFileListError.send(error.message || error.error);
    }

    const entries = response.entries.map(entry => (
      {
        tag: entry['.tag'],
        key: entry.path_lower,
        path: entry.path_display,
        size: entry.size || null,
      }
    ));
    const deleted = response.entries.filter(({ tag }) => tag === 'deleted');
    if (deleted.length) { console.info('deleted', deleted); }

    // update the cache
    cache.cursor = response.cursor;
    entries.forEach((entry) => {
      cache.entries[entry.key] = entry;
    });
    // localStorage['fileTree'] = JSON.stringify(cache);

    const more = Boolean(response.has_more);
    app.ports.receiveFileList.send([entries, more]);

    query = more && dbx.filesListFolderContinue({ cursor: response.cursor });
  }
});

app.ports.getAccountInfo.subscribe(async (accessToken_) => {
  const dbx = new Dropbox({ accessToken: accessToken_ });
  const { name, team } = await dbx.usersGetCurrentAccount();
  const teamName = team ? team.name : 'Personal';
  app.ports.receiveAccountInfo.send({ teamName, name });
});

app.ports.storeAccessToken.subscribe((token) => {
  localStorage[accessTokenKey] = token;
  accessToken = token;
});

app.ports.signOut.subscribe(() => {
  localStorage.clear();
  accessToken = null;
});

app.ports.chart.subscribe(([title, data]) => {
  const onClick = ({ key }) => key && app.ports.setPath.send(key);
  requestAnimationFrame(() => chart(title, data, onClick));
});
