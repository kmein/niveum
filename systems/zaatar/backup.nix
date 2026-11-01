{
  # the repository from before the move to the storagebox, read-only from here
  # on; it opens with the old restic.age password
  fileSystems."/backup" = {
    device = "/dev/disk/by-id/ata-WDC_WD10JPVX-22JC3T0_WD-WXD1E5510MKW";
    fsType = "ext4";
    options = [ "ro" ];
  };
}
