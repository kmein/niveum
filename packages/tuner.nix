{ pkgs, playlists }:
let
  inherit (pkgs) lib;
  trackHTML = {url, provider, name}: ''
    <tr>
      <th>${name}</th>
      <td><small>${provider}</small></td>
      <td>
        <audio controls>
          <source src="${url}">
          Your browser does not support the audio tag.
        </audio>
      </td>
    </tr>
  '';
  playlistSection = name: {description, tracks}: ''
    <section>
      <header>
      <h2 id="${lib.strings.sanitizeDerivationName name}">${name}</h2>
      <p>${description}</p>
      </header>
      <table>
        <thead>
          <th>Name</th>
          <th>Provider</th>
          <th>Player</th>
        </thead>
        ${builtins.concatStringsSep "\n\n" (map trackHTML tracks)}
      </table>
    </section>
  '';
in pkgs.writeText "tuner.html" ''
  <!doctype html>
  <html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <title>Tuner</title>
      <link rel="stylesheet" href="https://unpkg.com/mvp.css">
    </head>
    <body>
      <header>
        <nav>
            <a href="#">Tuner</a>
            <small>kmein's webradio hub</small>
            <ul>
              <li>
                Playlists
                <ul>
                  ${builtins.concatStringsSep "\n" (lib.mapAttrsToList (name: {...}: ''<li><a href="#${lib.strings.sanitizeDerivationName name}">${name}</a></li>'') playlists)}
                </ul>
              </li>
            </ul>
        </nav>
      </header>
      <main>
        ${builtins.concatStringsSep "\n<hr/>\n" (lib.mapAttrsToList playlistSection playlists)}
      </main>
      <footer>
        <p><small>&copy; kmein 2020</small></p>
      </footer>
    </body>
  </html>
''
