{ pkgs, lib, ... }:
let
  hp-driver = pkgs.hplip;
in
{
  services.printing = {
    enable = true;
    drivers = [ hp-driver ];
  };

  environment.systemPackages = [
    pkgs.system-config-printer
  ];

  # allow connecting to .local printers
  services.avahi.nssmdns4 = true;

  hardware.printers.ensurePrinters = [
    {
      name = "OfficeJet";
      location = "Zimmer";
      deviceUri = "https://${pkgs.lib.niveum.localAddresses.officejet}";
      model = "drv:///hp/hpcups.drv/hp-officejet_4650_series.ppd";
      ppdOptions = {
        Duplex = "DuplexNoTumble"; # DuplexNoTumble DuplexTumble None
        PageSize = "A4"; # A4 A4.FB A4.Duplex
        MediaType = "Plain";
        OutputMode = "Normal";
        ColorModel = "KGray"; # RGB CMYGray KGray
      };
    }
  ];
}
/*
  HP/hp-officejet_4650_series.ppd.gz
  drv:///hp/hpcups.drv/hp-officejet_4650_series.ppd
*/
