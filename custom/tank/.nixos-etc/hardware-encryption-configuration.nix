{ config, lib, pkgs, modulesPath, ... }:

{
  # luks
  boot.initrd.luks.devices = {
    root0 = {
      device = "/dev/disk/by-uuid/9e8cc39a-8e3b-4da3-9a78-e5f5b483265b";
      preLVM = true;
      allowDiscards = true;
    };
    root1 = {
      device = "/dev/disk/by-uuid/e906b897-959c-47bb-aea8-c922cbed83ae";
      preLVM = true;
      allowDiscards = true;
    };
  };
}
