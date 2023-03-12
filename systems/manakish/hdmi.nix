{pkgs, ...}: let
  pulseaudioCard = "alsa_card.pci-0000_00_1b.0";
  fingerprint = {
    HDMI-1 = "00ffffffffffff004c2d320d48435030181a0103803c22782a5295a556549d250e5054bb8c00b30081c0810081809500a9c001010101023a801871382d40582c450056502100001e000000fd0032481e5111000a202020202020000000fc00433237463339300a2020202020000000ff00485451483630323132390a202001e402031af14690041f131203230907078301000066030c00100080011d00bc52d01e20b828554056502100001e8c0ad090204031200c4055005650210000188c0ad08a20e02d10103e9600565021000018000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000061";
    LVDS-1 = "00ffffffffffff0006af6c100000000000140104901c10780220e5925554922825505400000001010101010101010101010101010101121b56585000193030203600159c100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231323558573031205630200a00ec";
  };
in {
  home-manager.users.me = {
    programs.autorandr = {
      enable = true;
      hooks.postswitch.wallpaper = "systemctl --user restart wallpaper";
      profiles = {
        single = {
          hooks.postswitch = "${pkgs.pulseaudio}/bin/pactl set-card-profile ${pulseaudioCard} alsa_output.pci-0000_00_1b.0.analog-stereo";
          fingerprint = {inherit (fingerprint) LVDS-1;};
          config = {
            HDMI-1.enable = false;
            LVDS-1 = {
              enable = true;
              crtc = 0;
              mode = "1366x768";
              position = "277x1080";
              primary = true;
              rate = "60.10";
            };
          };
        };
        desk = {
          hooks.postswitch = "${pkgs.pulseaudio}/bin/pactl set-card-profile ${pulseaudioCard} output:hdmi-stereo+input:analog-stereo";
          fingerprint = {inherit (fingerprint) HDMI-1 LVDS-1;};
          config = {
            HDMI-1 = {
              enable = true;
              crtc = 1;
              mode = "1920x1080";
              position = "0x0";
              primary = true;
              rate = "60.00";
            };
            LVDS-1 = {
              enable = true;
              crtc = 0;
              mode = "1366x768";
              position = "277x1080";
              rate = "60.10";
            };
          };
        };
      };
    };
  };
}
