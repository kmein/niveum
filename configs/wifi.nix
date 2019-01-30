{ config, ... }:
{
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
      EasyBox-927376 = { pskRaw = "dbd490ab69b39bd67cfa06daf70fc3ef3ee90f482972a668ed758f90f5577c22"; };
      "Asoziales Netzwerk" = { pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6"; };
      c-base-public = {};
    };
  };
}
