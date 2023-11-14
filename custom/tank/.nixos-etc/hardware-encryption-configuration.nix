{ config, lib, pkgs, modulesPath, ... }:

{
  # # mdadm
  # boot.initrd.services.swraid = {
  #   enable = true;
  #   mdadmConf = ''
  #     ARRAY /dev/md0 metadata=1.0 name=archiso:0 UUID=764b3bea:1aad3e88:5543f650:bd7314d2
  #     ARRAY /dev/md1 metadata=1.2 name=archiso:1 UUID=dc0e0617:70216ae6:c8af2052:5e730003
  #   '';
  # };

  # luks
  boot.initrd.luks.devices = {
    root0 = {
      device = "/dev/disk/by-uuid/9e8cc39a-8e3b-4da3-9a78-e5f5b483265b";
      #device = "/dev/disk/by-label/";
      preLVM = true;
      allowDiscards = true;
    };
    root1 = {
      device = "/dev/disk/by-uuid/e906b897-959c-47bb-aea8-c922cbed83ae";
      #device = "/dev/disk/by-label/";
      preLVM = true;
      allowDiscards = true;
    };
  };
}
