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
      device = "/dev/disk/by-uuid/0bbb9f6c-b91c-48e5-aaef-9bfb3785dc0e";
      preLVM = true;
      allowDiscards = true;
    };
    root1 = {
      device = "/dev/disk/by-uuid/62aeffa6-8f7e-4c4d-889b-15b9ea2152db";
      preLVM = true;
      allowDiscards = true;
    };
  };
}
