{
  config,
  pkgs,
  ...
}:
{
  boot.extraModulePackages = with config.boot.kernelPackages; [
    tp_smapi
    acpi_call
  ];
  boot.kernelModules = [
    "tp_smapi"
    "acpi_call"
  ];
  environment.systemPackages = [
    pkgs.tpacpi-bat
    pkgs.powertop
  ];

  services.tlp = {
    enable = true;
    settings = {
      START_CHARGE_THRESH_BAT0 = 80;
      STOP_CHARGE_THRESH_BAT0 = 95;
    };
  };
}
