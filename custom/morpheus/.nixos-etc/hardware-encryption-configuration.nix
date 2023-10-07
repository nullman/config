{ config, lib, pkgs, modulesPath, ... }:

{
  # luks
  boot.initrd.luks.devices = {
    root0 = {
      device = "/dev/disk/by-uuid/51cec6b1-cd69-45fa-a4a6-38dbf790807e";
      #device = "/dev/disk/by-label/";
      preLVM = true;
      allowDiscards = true;
    };
    root1 = {
      device = "/dev/disk/by-uuid/db28edc0-7ef5-449d-bcd9-df5f354c1ff9";
      #device = "/dev/disk/by-label/";
      preLVM = true;
      allowDiscards = true;
    };
  };
}
