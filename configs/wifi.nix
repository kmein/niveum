{ pkgs, ... }:
{
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      "Aether" = {
        pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
        priority = 10;
      };
      "Asoziales Netzwerk" = {
        pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6";
        priority = 10;
      };
      "Libertarian WiFi" = {
        pskRaw = "e9beaae6ffa55d10e80b8a2e7d997411d676a3cc6f1f29d0b080391f04555050";
        priority = 9;
      };
      "EasyBox-927376".pskRaw = "dbd490ab69b39bd67cfa06daf70fc3ef3ee90f482972a668ed758f90f5577c22";
      "FlixBus Wi-Fi" = { };
      "FlixBus" = { };
      "FlixTrain" = { };
      "BVG Wi-Fi" = { };
      "wannseeforum" = { }; # login via curl -XPOST http://WannseeLancom.intern.:80/authen/login/ -d userid=$USER_ID -d password=$PASSWORD
      "Hotel_Krone" = { }; # login: http://192.168.10.1/
      "Ni/Schukajlow".pskRaw = "ffc47f6829da59c48aea878a32252223303f5c47a3859edc90971ffc63346781";
      "WIFIonICE" = { }; # login: http://10.101.64.10/
      "WLAN-914742".psk = "67647139648174545446";
      "KDG-CEAA4".psk = "PBkBSmejcvM4";
      "KDG-4ECF7".psk = "Gdbwh7afw2Bx";
      "WLAN-XVMU6T".pskRaw =
      "46ea807283255a3d7029233bd79c18837df582666c007c86a8d591f65fae17cc";
      "c-base-public" = { };
      "discord".psk = "baraustrinken";
      "GoOnline".psk = "airbnbguest";
      "security-by-obscurity".psk = "44629828256481964386";
      "Mayflower".psk = "Fr31EsLan";
      "Born11".psk = "56LMVLbw840EGNWk0RYRqvgicx3FSO";
      "FactoryCommunityGuest".psk = "Factory4ever";
      "krebs".psk = "aidsballs";
    };
  };

  environment.systemPackages = [ pkgs.wpa_supplicant_gui ];
}
