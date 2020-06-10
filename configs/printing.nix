{ pkgs, ... }:
let hp-driver = pkgs.hplipWithPlugin;
in {
  services.printing = {
    enable = true;
    drivers = [ hp-driver ];
  };

  hardware.sane = {
    enable = true;
    extraBackends = [ hp-driver ];
  };

  users.users.me.extraGroups = [ "scanner" ];

  hardware.printers.ensurePrinters = [{
    name = "OfficeJet";
    location = "Zimmer";
    deviceUri = "https://192.168.178.27";
    model = "drv:///hp/hpcups.drv/hp-officejet_4650_series.ppd";
    ppdOptions = {
      Duplex = "DuplexNoTumble"; # DuplexNoTumble DuplexTumble None
      PageSize = "A4"; # A4 A4.FB A4.Duplex
      MediaType = "Plain";
      OutputMode = "Normal";
      ColorModel = "KGray"; # RGB CMYGray KGray
    };
  }];
}

/* HP/hp-officejet_4650_series.ppd.gz
   drv:///hp/hpcups.drv/hp-officejet_4650_series.ppd
*/
