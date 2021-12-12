# Refactory

This is a small web application to help with designing factories in
[Satisfactory][]. It's deployed to https://refactory.ignorare.net.

Game data is parsed from the Docs.json file distributed with the game (under
CommunityResources), with icons extracted from FactoryGame-WindowsNoEditor.pak.
The game data is preprocessed into a more usable form for the web application.


## Status

This is a minor hobby project. It's currently usable and may or may not acquire
new features. I'll try to keep it up to date with new Satisfactory releases,
assuming I'm available when they appear.

I reserve the right to lose interest in this project or in Satisfactory at any
time. The code is licensed under CC0, so by all means feel free to make your
own modified version to suit your needs.


## Developing

The application is built in [ClojureScript][], so you'll need [Clojure][] and
[yarn][] to get started. Run:

    yarn install

To install NPM dependencies. If you have `tmux` installed, you can run
`bin/watch` to start up [shadow-cljs][] and [brunch][] to compile the
ClojureScript and SCSS respectively. This will run two commands:

    clojure -M:dev:cljs watch app
    yarn run brunch watch

Other useful scripts:

    - **bin/lint-deps**: Run [clj-kondo][] on dependencies to populate the lint
      cache.
    - **bin/release**: Make a release build and stage it for deployment via
      GitHub Pages.


## Game data

The code under `src` includes both the web application and a tool to process
game data and assets. The processed data is checked in, so this is only needed
to update to a new version of Satisfactory.

To preprocess Docs.json, run:

    bin/prep game-data raw/docs.json app/assets/game.json

The path arguments are actually optional, as those are the defaults. Note that
(at time or writing) the Docs.json file distributed with the game is encoded
with UTF-16, so you have to convert it first. For example:

    iconv -f utf-16 -t utf-8 path/to/Docs.json > raw/docs.json

To extract icons from the game data, see [Extracting UI icons][] from the
Satisfactory wiki. The `prep` command can read `app/assets/game.json` and
output a list of all of the icons we'll need. Normally you would run:

    bin/prep icons --prefix / --rsync > icons.txt

To get an rsync include-file. Then you can use rsync to pull just the icons we
need from the images you extracted above.

    rsync -rdmv --include-from icons.txt path/to/Game app/assets/img/


[clj-kondo]: https://github.com/clj-kondo/clj-kondo
[ClojureScript]: https://clojurescript.org/
[Clojure]: https://clojure.org/
[Extracting UI icons]: https://satisfactory.fandom.com/wiki/Tutorial:Extracting_UI_icons
[brunch]: https://brunch.io/
[Satisfactory]: https://www.satisfactorygame.com/
[shadow-cljs]: https://github.com/thheller/shadow-cljs
[yarn]: https://yarnpkg.com/
