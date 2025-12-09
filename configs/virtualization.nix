{ pkgs, ... }:
{
  users.users.me.extraGroups = [ "libvirtd" ];
  virtualisation.libvirtd.enable = true;

  # Enable TPM support for VMs
  virtualisation.libvirtd.qemu = {
    # swtpm.enable = true;
  };

  environment.systemPackages = with pkgs; [
    virt-manager
  ];
}
