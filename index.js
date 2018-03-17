import Dropbox from 'dropbox';
import 'semantic-ui-css/semantic.min.css';
import './src/Main.scss';

import Elm from './src/Main.elm';
// eslint-disable-next-line import/no-named-as-default, import/no-named-as-default-member
import chart from './src/chart';

const accessTokenKey = 'accessToken';
const oldFileCacheKey = 'fileTree';
const filesCacheKey = 'files';

const app = Elm.Main.embed(document.getElementById('app'), {
  accessToken: localStorage[accessTokenKey] || null,
  clientId: process.env.DROPBOX_APP_KEY,
  files: localStorage[filesCacheKey] || null,
});

app.ports.saveFilesCache.subscribe((cacheValue) => {
  const json = JSON.stringify(cacheValue);
  delete localStorage[oldFileCacheKey];
  console.info('write cache', json.length, 'bytes');
  localStorage[filesCacheKey] = json;
});

app.ports.getAccountInfo.subscribe(async (accessToken) => {
  const dbx = new Dropbox({ accessToken });
  const { name, team } = await dbx.usersGetCurrentAccount();
  const teamName = team ? team.name : 'Personal';
  app.ports.receiveAccountInfo.send({ teamName, name });
});

app.ports.storeAccessToken.subscribe((token) => {
  localStorage[accessTokenKey] = token;
});

app.ports.removeAccountInfo.subscribe(() => {
  localStorage.clear();
});

app.ports.chart.subscribe(([title, data]) => {
  const onClick = ({ key }) => key && app.ports.setPath.send(key);
  requestAnimationFrame(() => chart(title, data, onClick));
});
