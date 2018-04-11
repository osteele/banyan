import Dropbox from 'dropbox';
import 'semantic-ui-css/semantic.min.css';
import './src/Main.scss';

import Elm from './src/Main.elm';
// eslint-disable-next-line import/no-named-as-default, import/no-named-as-default-member
import treemap from './src/treemap';

const accessTokenKey = 'accessToken';
const oldFileCacheKey = 'fileTree';
const filesCacheKey = 'files';

const app = Elm.Main.embed(document.getElementById('app'), {
  accessToken: localStorage[accessTokenKey] || null,
  clientId: process.env.DROPBOX_APP_KEY,
  files: localStorage[filesCacheKey] || null,
});

console.info(`read cache: ${(localStorage[filesCacheKey] || '').length} bytes`);

app.ports.saveFilesCache.subscribe((cacheValue) => {
  const json = JSON.stringify(cacheValue);
  delete localStorage[oldFileCacheKey];
  console.info(`write cache: ${json.length} bytes`);
  localStorage[filesCacheKey] = json;
});

app.ports.getAccountInfo.subscribe(async (accessToken) => {
  const dbx = new Dropbox({ accessToken });
  const info = await dbx.usersGetCurrentAccount();
  app.ports.receiveRawFullAccountInfo.send(info);
});

app.ports.storeAccessToken.subscribe((accessToken) => {
  localStorage[accessTokenKey] = JSON.stringify(accessToken);
});

app.ports.removeAccountInfo.subscribe(() => {
  localStorage.clear();
});

app.ports.renderTreemap.subscribe(([title, data]) => {
  const onClick = ({ key }) => key && app.ports.setPath.send(key);
  requestAnimationFrame(() => treemap(title, data, onClick));
});
