{ pkgs, lib, stations }:
let
  theStations = lib.mapAttrsToList (name: value: value // {name = name;}) stations;
in
pkgs.writeText "index.html" ''
  <!doctype html>
  <html>
    <head>
      <title>radio.kierán</title>
      <link
        rel="stylesheet"
        href="//cdn.rawgit.com/necolas/normalize.css/master/normalize.css"
      />
      <link
        rel="stylesheet"
        href="https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.2/css/bulma.min.css"
      />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <meta charset="utf-8" />
    </head>
    <body>
      <main class="section">
        <div class="subtitle is-3">Welcome to</div>
        <h1 class="title is-1">radio.kierán</h1>

        <div class="columns is-multiline">
          ${lib.concatMapStringsSep "\n" (station: ''
            <div class="column">
              <div class="box">
                <strong class="is-uppercase">${station.name}</strong>
                (<a href="/${station.name}/status">status</a>, <a href="/${station.name}/listen.ogg">link</a>)

                <p class="has-text-grey">${station.description}</p>
                <hr/>
                <audio style="width:100%" controls src="/${station.name}/listen.ogg"/>
              </div>
            </div>'') theStations
          }
        </div>
      </main>
    </body>
  </html>
''
