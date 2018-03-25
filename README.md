# Banyan

Visualize Dropbox file and directory sizes.

Live version: <https://banyan.osteele.com/>

![](./docs/screenshot.png)

## Develop

### Install

1.  Install [yarn](https://yarnpkg.com).
2.  Install npm and elm packages:

    ```bash
    $ yarn install
    ```

### Create a Dropbox API Key

1.  Sign into Dropbox.

2.  [Create a new app](https://www.dropbox.com/developers/apps/create). Select
    “Dropbox API”, and give it “Full Dropbox” access.

        ![](./docs/dropbox-create-app.png)

3.  Add `http://localhost:4000/` to the list of Redirect URIs.

4.  Set the `DROPBOX_APP_KEY` environment variable to the app's API key (highlighted
    below). If you use [direnv](https://direnv.net/), you can copy `.envrc.template`
    to `.envrc` and edit the key into that file.

![](./docs/dropbox-app-settings.png)

### Run

```bash
$ yarn start
```

### Test

```bash
$ yarn test        # run the tests once
$ yarn test:watch  # test in watch mode
```

### Lint

```bash
$ yarn lint
```

## Acknowledgements

Build with [Elm](http://elm-lang.org/),
[elm-dropbox](http://package.elm-lang.org/packages/avh4/elm-dropbox/latest),
[Highcharts](https://www.highcharts.com/products/highcharts/), [Semantic
UI](https://semantic-ui.com/), and [Poi](https://poi.js.org/#/).

The [Banyan tree
image](<https://commons.wikimedia.org/wiki/File:Banyan_tree_(PSF).png>) used in
this app's Dropbox OAuth page was donated to the Wikimedia Foundation by Pearson
Scott Foresman. It is in the public domain.

Highcharts is used under the Creative Commons (CC) Attribution-NonCommercial
licence for personal use and demonstrations.

## License

MIT
