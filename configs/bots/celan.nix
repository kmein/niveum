{
  pkgs,
  lib,
  config,
  ...
}: {
  niveum.bots.celan = {
    enable = true;
    time = "08:00";
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = ["@PaulCelan"];
    };
    mastodon = {
      enable = true;
      tokenFile = config.age.secrets.mastodon-token-celan.path;
      language = "de";
    };
    command = toString (pkgs.writers.writePython3 "random-celan.py" { libraries = [pkgs.python3Packages.lxml]; } ''
      from lxml import etree
      import random


      def xml_text(elements):
          return "".join("".join(t.itertext()) for t in elements).strip()


      tree = etree.parse('${pkgs.fetchurl {
        url = "http://c.krebsco.de/celan.tei.xml";
        hash = "sha256-HgNmJYfhuwyfm+FcNtnnYWpJpIIU1ElHLeLiIFjF9mE=";
      }}')
      root = tree.getroot()

      tei = {"tei": "http://www.tei-c.org/ns/1.0"}

      poems = root.xpath(".//tei:lg[@type='poem']", namespaces=tei)

      poem = random.choice(poems)

      for stanza in poem.xpath("./tei:lg[@type='stanza']", namespaces=tei):
          for line in stanza.xpath('./tei:l', namespaces=tei):
              if line.text:
                  print(line.text.strip())
          print()

      current_element = poem
      while current_element is not None:
          if current_element.tag == "{http://www.tei-c.org/ns/1.0}text":
              text_element = current_element

              title = xml_text(text_element.xpath("./tei:front/tei:docTitle",
                               namespaces=tei))
              print(f"Aus: #{title.replace(" ", "_")}", end=" ")

              if date := xml_text(text_element.xpath("./tei:front/tei:docDate",
                                  namespaces=tei)):
                  print(f"({date})")
              break
          current_element = current_element.getparent()

      print("\n\n#PaulCelan #Celan #Lyrik #poetry")
    '');
  };

  age.secrets = {
    mastodon-token-celan.file = ../../secrets/mastodon-token-celan.age;
  };

  systemd.timers.bot-celan.timerConfig.RandomizedDelaySec = "10h";

  niveum.passport.services = [
    {
      title = "Paul Celan Bot";
      description = "sends a random poem by Paul Celan to Telegram.";
      link = "https://t.me/PaulCelan";
    }
  ];
}
