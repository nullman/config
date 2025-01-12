#===============================================================================
# NixOS Configuration
#
# Reference: https://nix.dev/
# Manual: nixos-help
# Man page: configuration.nix(5)
#===============================================================================

{ config, pkgs, ... }:

{
  # imports
  imports = [
    ./hardware-configuration.nix
    #<home-manager/nixos>
  ];
  ## imports
  #imports = [
  #  ./hardware-configuration.nix
  #  ./hardware-encryption-configuration.nix
  #  #<home-manager/nixos>
  #];

  # systemd-boot EFI boot loader
  boot.loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 20;
    };
    efi.canTouchEfiVariables = true;
  };
  fileSystems."/".options = [ "relatime" ];

  # add windows boot option
  boot.loader.systemd-boot = {
    extraFiles."efi/shell.efi" = "${pkgs.edk2-uefi-shell}/shell.efi";
    extraEntries = {
      "windows.conf" =
        let
          # To determine the name of the windows boot drive, boot into edk2 first, then run
          # `map -c` to get drive aliases, and try out running `FS1:`, then `ls EFI` to check
          # which alias corresponds to which EFI partition.
          boot-drive = "HD1c65535a2";
        in
        ''
          title Windows Bootloader
          efi /efi/shell.efi
          options -nointerrupt -nomap -noversion ${boot-drive}:EFI\Microsoft\Boot\Bootmgfw.efi
          #options -nointerrupt -nomap -noversion ${boot-drive}:ESD\Windows\efi\boot\bootx64.efi
          sort-key y_windows
        '';
      "edk2-uefi-shell.conf" = ''
        title EDK2 UEFI Shell
        efi /efi/shell.efi
        sort-key z_edk2
      '';
    };
  };

  swapDevices = [
    {
      device = "/home/swapfile";
      size = 32768;                     # 32 GB
    }
  ];

  # networking
  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };
  networking.hostName = "dozer";

  # open firewall ports
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 515 631 9100 ];
    #allowedUDPPorts = [ 515 631 9100 ];
    allowedTCPPortRanges = [
      { from = 1714; to = 1764; }
      { from = 1714; to = 1764; }
    ];
    #allowedUDPPortRanges = [
    #  { from = 1714; to = 1764; }
    #  { from = 1714; to = 1764; }
    #];
    checkReversePath = "loose";
  };
  networking.firewall.allowedTCPPorts = [ 24800 ]; # Barrier
  #networking.firewall.allowedTCPPorts = [ 30000 ]; # Foundry VTT

  # openssh server
  services.openssh.enable = true;

  # standard settings

  # sysctl settings
  boot.kernel.sysctl = {
    #"fs.file-max" = 524288;        # open file descriptors limit (star citizen)
    #"vm.max_map_count" = 16777216; # maxmimum number of "memory map areas" a process can have (star citizen)
    "vm.swappiness" = 0;           # only use swap if needed; cat /proc/sys/vm/swappiness
    "vm.vfs_cache_pressure" = 50;  # cache inode and dentry information; cat /proc/sys/vm/vfs_cache_pressure
  };

  # nix settings
  nix.settings = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];
  };
  # nix.extraOptions = "experimental-features = flakes nix-command";

  # package settings
  nixpkgs.config = {
    allowUnfree = true;
    nvidia.acceptLicense = true;
    joypixels.acceptLicense = true;
    permittedInsecurePackages = [
      "betterbird-115.9.0"
      "betterbird-unwrapped-115.9.0"
      "electron-12.2.3"
      "electron-24.8.6"
      "olm-3.2.16"
      "openssl-1.1.1w"
    ];
    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
      # ruby = pkgs.ruby.withPackages (p: with p; [ nokogiri ]) {
      #   inherit pkgs;
      # };
    };
  };
  nixpkgs.overlays = [
    (final: prev: {
      binutils-ia16 = prev.callPackage /home/user/.nixos/pkgs/binutils-ia16 {};
      crexx = prev.callPackage /home/user/.nixos/pkgs/crexx {};
      djgpp_i586 = prev.callPackage /home/user/.nixos/pkgs/djgpp { targetArchitecture = "i586"; };
      djgpp_i686 = prev.callPackage /home/user/.nixos/pkgs/djgpp { targetArchitecture = "i686"; };
      gcc-ia16 = prev.callPackage /home/user/.nixos/pkgs/gcc-ia16 {};
      #mtkclient = prev.callPackage /home/user/.nixos/pkgs/mtkclient {};
      play = prev.callPackage /home/user/.nixos/pkgs/play {};
      #stow = prev.callPackage /home/user/.nixos/pkgs/stow {};
      syncterm = prev.callPackage /home/user/.nixos/pkgs/syncterm {};
      #x48 = prev.callPackage /home/user/.nixos/pkgs/x48 {};
      #bspwm = prev.callPackage /home/user/code/github-nullman/bspwm {};
      #services.xserver.windowManager.bspwm = prev.callPackage /home/user/code/nixpkgs/pkgs/applications/window-managers/bspwm.nix {};
    })
  ];

  # time zone
  time.timeZone = "US/Pacific";
  #time.timeZone = "US/Mountain";
  #time.timeZone = "US/Central";
  #time.timeZone = "US/Eastern";

  # internationalization
  i18n.defaultLocale = "en_US.UTF-8";

  # console
  console = {
    packages = [ pkgs.terminus_font ];
    font = "${pkgs.terminus_font}/share/consolefonts/ter-d20b.psf.gz";
    useXkbConfig = true;
  };

  ## graphics
  #hardware.graphics = {
  #  enable = true;
  #  enable32Bit = true;
  #};

  # dbus
  services.dbus.enable = true;

  # dconf
  programs.dconf.enable = true;

  # acpi
  services.acpid.enable = true;

  # xdg
  xdg.portal = {
    enable = true;
    # wlr.enable = true;
    # gtk portal needed to make gtk apps happy
    #extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config = {
      common = {
        default = [
          "gtk"
        ];
      };
    };
  };

  # environment
  environment.variables = {
    GTK_DATA_PREFIX = "/run/current-system/sw";
  };

  # gnome settings
  services.gnome.gnome-settings-daemon.enable = true;

  # gnome policy kit
  security.polkit = {
    enable = true;
    extraConfig = ''
      polkit.addRule(function(action, subject) {
          if (action.id.indexOf("org.freedesktop.udisks2.") == 0 &&
              subject.isInGroup("wheel")) {
              return polkit.Result.YES;
          }
      });

      polkit.addRule(function(action, subject) {
          if ((action.id == "org.freedesktop.login1.power-off" ||
               action.id == "org.freedesktop.login1.power-off-multiple-sessions" ||
               action.id == "org.freedesktop.login1.reboot" ||
               action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
               action.id == "org.freedesktop.login1.hibernate" ||
               action.id == "org.freedesktop.login1.suspend") &&
              subject.isInGroup("users")) {
              return polkit.Result.YES;
          }
      });

      polkit.addRule(function(action, subject) {
          if ((action.id == "org.freedesktop.upower.hibernate" ||
               action.id == "org.freedesktop.upower.suspend") &&
              subject.isInGroup("users")) {
              return polkit.Result.YES;
          }
      });

      /* allow users of network group to use blueman feature requiring root without authentication */
      polkit.addRule(function(action, subject) {
          if ((action.id == "org.blueman.network.setup" ||
               action.id == "org.blueman.dhcp.client" ||
               action.id == "org.blueman.rfkill.setstate" ||
               action.id == "org.blueman.pppd.pppconnect") &&
              subject.isInGroup("network")) {
              return polkit.Result.YES;
          }
      });
    '';
  };
  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 30;
      };
    };
    extraConfig = ''
      DefaultTimeoutStopSec=30s
    '';
  };

  # gnome keyring
  services.gnome.gnome-keyring.enable = true;

  # # gnome file services: mount, trash, and other functionality
  # services.gvfs = {
  #   enable = true;
  #   package = lib.mkForce pkgs.gnome3.gvfs;
  # };

  # sudo
  security.sudo = {
    extraConfig = ''
      Defaults env_reset, timestamp_timeout=60
    '';
  };

  # suid wrappers
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # # udev
  # services.udev.extraRules = ''
  #   ACTION=="add", SUBSYSTEMS=="usb", SUBSYSTEM=="block", ENV{ID_FS_USAGE}=="filesystem", RUN{program}+="${pkgs.systemd}/bin/systemd-mount --no-block --automount=yes --collect $devnode /media"
  # '';

  # shells
  programs.zsh.enable = true;

  # adb
  programs.adb.enable = true;

  # ld (libraries in standard paths)
  #programs.nix-ld.enable = true;

  # steam
  programs.steam.enable = true;

  # locate
  services.locate = {
    enable = true;
    package = pkgs.mlocate;
    localuser = null;
    pruneBindMounts = true;
    prunePaths = [
      "/dev"
      "/lost+found"
      "/media"
      "/mnt"
      #"/nix/store"
      "/nix/var/log/nix"
      "/proc"
      "/srv"
      "/sys"
      "/tmp"
      "/var/cache"
      "/var/lock"
      "/var/run"
      "/var/spool"
      "/var/tmp"
    ];
  };

  # nvidia
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    # modesetting is needed for most wayland compositors
    modesetting.enable = true;
    # power management (experimental, and can cause sleep/suspend to fail)
    powerManagement.enable = false;
    # fine-grained power management; turns off GPU when not in use
    # (experimental, and only works on modern Nvidia GPUs -- Turing or newer)
    powerManagement.finegrained = false;
    # open source version of nvidia
    # Turing and later architectures only
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # only available on driver 515.43.04+
    open = false;
    # enable nvidia settings menu
    nvidiaSettings = true;
    # select appropriate driver version [optional]
    #package = config.boot.kernelPackages.nvidiaPackages.stable;
    #package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  };
  ## nvidia prime
  ## for laptops with intel and nvidia graphics cards
  #hardware.nvidia.prime = {
  #  # get bus id using: sudo lshw -c display
  #  intelBusId = "PCI:0:2:0";
  #  nvidiaBusId = "PCI:1:0:0";
  #  #sync.enable = true;
  #  reverseSync.enable = true;
  #  #offload = {
  #  #  enable = true;
  #  #  enableOffloadCmd = true;
  #  #};
  #};

  # use 470 driver for GeForce GTX 770
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;

  # x11
  services.xserver = {
    enable = true;
    xkb = {
      layout = "us";
      variant = "";
      options = "shift:both_capslock,caps:ctrl_modifier";
    };
  };

  ## wayland/hyprland
  #programs.hyprland = {
  #  enable = true;
  #  xwayland.enable = true;
  #};

  # display manager: lightdm
  #services.xserver.displayManager.lightdm.greeters.slick = {
  services.xserver.displayManager.lightdm.greeters.gtk = {
    enable = true;
    extraConfig = ''
      [Greeter]
      background=/etc/nixos/wallpapers/Fluorescence.jpg
    '';
  };
  ## display manager: sddm
  #services.displayManager.sddm.enable = true;

  # window manager: bspwm
  #services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager.bspwm.enable = true;
  services.displayManager.defaultSession = "none+bspwm";

  ## window manager: i3wm
  #services.xserver.windowManager.i3.enable = true;
  #services.xserver.windowManager.i3.package = pkgs.i3-gaps;

  # window manager: xfce
  services.xserver.desktopManager.xfce.enable = true;
  #services.displayManager.defaultSession = "xfce";

  #services.tlp = {
  #  enable = true;
  #  settings = {
  #    CPU_SCALING_GOVERNOR_ON_AC = "performance";
  #    CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
  #
  #    CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
  #    CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
  #
  #    CPU_MIN_PERF_ON_AC = 0;
  #    CPU_MAX_PERF_ON_AC = 100;
  #    CPU_MIN_PERF_ON_BAT = 0;
  #    CPU_MAX_PERF_ON_BAT = 20;
  #
  #    START_CHARGE_THRESH_BAT0 = 40;   # 40 and bellow battery starts to charge
  #    STOP_CHARGE_THRESH_BAT0 = 80;    # 80 and above battery stops charging
  #
  #    #USB_BLACKLIST_BTUSB = 1;         # make sure bluetooth still works
  #  };
  #};

  # compositor: picom
  services.picom.enable = true;

  ## pulse audio
  #security.rtkit.enable = true;
  #hardware.pulseaudio = {
  #  enable = true;
  #  support32Bit = true;
  #};
  #nixpkgs.config.pulseaudio = true;
  #hardware.pulseaudio.extraConfig = ''
  #  load-module module-combine-sink
  #  unload-module module-suspend-on-idle
  #  load-module module-switch-on-connect
  #  load-module module-bluetooth-policy
  #  load-module module-bluetooth-discover
  #'';
  ##hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  #hardware.bluetooth.hsphfpd.enable = true;

  # pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    wireplumber.extraConfig.bluetoothEnhancements = {
      "monitor.bluez.properties" = {
        "bluez5.enable-sbc-xq" = true;
        "bluez5.enable-msbc" = true;
        "bluez5.enable-hw-volume" = true;
        "bluez5.roles" = [ "a2dp_sink" "a2dp_source" "bap_sink" "bap_source" "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" ];
      };
    };
    # extraConfig.pipewire."92-low-latency" = {
    #   context.properties = {
    #     default.clock.rate = 48000;
    #     default.clock.quantum = 32;
    #     default.clock.min-quantum = 32;
    #     default.clock.max-quantum = 32;
    #   };
    # };
  };
  hardware.bluetooth.hsphfpd.enable = false;
  systemd.user.services.pipewire-pulse.path = [ pkgs.pulseaudio ];

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    #hsphfpd.enable = true;                # pulse audio
    #hsphfpd.enable = false;               # pipewire
  };
  services.blueman.enable = true;

  ## touchpad
  #services.libinput.enable = true;

  ## light
  #programs.light.enable = true;

  # fonts
  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      anonymousPro
      #cantarell-fonts
      corefonts
      #dejavu_fonts
      #dina-font
      fira-code
      fira-code-symbols
      font-awesome
      #freefont_ttf
      #google-fonts
      hack-font
      #joypixels
      #liberation_ttf
      #mononoki
      #mplus-outline-fonts.githubRelease
      nerdfonts
      noto-fonts
      noto-fonts-emoji
      #proggyfonts
      source-han-mono
      source-han-sans
      source-han-serif
      terminus-nerdfont
      terminus_font_ttf
      #ubuntu_font_family
      #unifont
      #xorg.fontadobe100dpi
      #xorg.fontadobe75dpi
      #xorg.fontbh100dpi
      #xorg.fontbh75dpi
      #xorg.fontbhtype1
      #xorg.fontmiscmisc
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        #monospace = ["Hack LG M Regular Nerd Font Complete Mono" ];
        #monospace = ["Meslo LG M Regular Nerd Font Complete Mono" ];
        #monospace = ["Hack Nerd Font Mono" ];
        monospace = ["MesloLGS Nerd Font Mono" ];
        serif = [ "Noto Serif" "Source Han Serif" ];
        sansSerif = [ "Noto Sans" "Source Han Sans" ];
      };
    };
  };

  # printing
  services = {
    printing = {
      enable = true;
      logLevel = "debug";
    };
  };
  # # printing avahi
  # services.avahi = {
  #   enable = true;
  #   nssmdns4 = true;
  #   openFirewall = true;
  # };
  hardware.printers = {
    ensurePrinters = [
      {
        name = "Canon-MF210";
        deviceUri = "ipp://printer/ipp/print";
        model = "everywhere";
      }
    ];
    ensureDefaultPrinter = "Canon-MF210";
  };

  # scanning
  hardware.sane = {
    enable = true;
    netConf = "printer";
    extraBackends = [ pkgs.cnijfilter2 ];
  };

  # virtualization
  virtualisation.libvirtd.enable = true;

  # thunar
  programs.thunar.enable = true;
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-volman
  ];
  programs.xfconf.enable = true;         # save preferences
  services.gvfs.enable = true;           # mount, trash, and other functionality
  services.tumbler.enable = true;        # thumbnail support for images

  # firefox
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
      extraPolicies = {
        DisableTelemetry = true;
        DisableFirefoxStudies = true;
        EnableTrackingProtection = {
          Value= true;
          Locked = true;
          Cryptomining = true;
          Fingerprinting = true;
        };
      };
    };
    nativeMessagingHosts = {
      packages = [ pkgs.vdhcoapp ];
    };
  };
  # programs.firefox = {
  #   enable = true;
  #   package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
  #     #nativeMessagingHosts = with pkgs.nur.repos.wolfangaukang; [ vdhcoapp ];
  #     nativeMessagingHosts = with pkgs; [ vdhcoapp ];
  #   };
  # };

  virtualisation.docker.enable = true;

  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  # mpd
  services.mpd = {
    enable = true;
    user = "user";
    musicDirectory = "/home/data/media/Audio/MPD";
    playlistDirectory = "/home/data/media/Audio/Playlists";
    extraConfig = ''
      # audio_output {
      #   type "pulse"
      #   name "PulseAudio Output"
      # }
      audio_output {
        type "pipewire"
        name "PipeWire Output"
      }
    '';
  };

  systemd.services.mpd.environment = {
    # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
    XDG_RUNTIME_DIR = "/run/user/1000";   # userid of above "user"
  };

  #services.dovecot2 = {
  #  enable = true;
  #  mailLocation = "maildir:~/Maildir";
  #};

  ## # gbar
  ## programs.gBar = {
  ##   enable = true;
  ##   # config = {
  ##   #   Location = "L";
  ##   #   EnableSNI = true;
  ##   #   SNIIconSize = {
  ##   #     Discord = 26;
  ##   #     OBS = 23;
  ##   #   };
  ##   #   WorkspaceSymbols = [ " " " " ];
  ##   # };
  ## };

  # user account
  users.groups.user = {
    name = "user";
    gid = 1000;
  };
  users.users.user = {
    group = "user";
    uid = 1000;
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/bash";
    #shell = "/run/current-system/sw/bin/zsh";
    initialPassword = "nixos";
    extraGroups = [
      "adbusers"
      "audio"
      "cdrom"
      "disk"
      "docker"
      "floppy"
      "input"
      "kvm"
      "libvirtd"
      "lp"
      "mlocate"
      "networkmanager"
      "scanner"
      "users"
      "video"
      "wheel"
    ];
  };

  # # home customizations
  # home-manager = {
  #   #useGlobalPkgs = true;
  #   #useUserPackages = true;
  #   users.user = { pkgs, ... }: {
  #     home.stateVersion = "23.05";        # same as system.stateVersion
  #     home.packages = with pkgs.nur.repos.wolfangaukang; [ vdhcoapp ];
  #   };
  # };

  # automounts
  services.rpcbind.enable = true;         # needed for NFS
  systemd.mounts = let commonMountOptions = {
    mountConfig = {
      Options = "vers=3.0,credentials=/home/user/.synology-mount-credentials,uid=1000,gid=1000,iocharset=utf8,sec=ntlmv2,rw,noatime,nofail";
      #Options = "vers=3.0,credentials=/home/user/.synology-mount-credentials,uid=1000,gid=1000,iocharset=utf8,sec=ntlmv2,rw,noatime,nofail,x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
      TimeoutSec = 30;
      Type = "cifs";
    };
  };

  in [
    (commonMountOptions // {
      what = "//synology/adult";
      where = "/mnt/synology/adult";
    })

    (commonMountOptions // {
      what = "//synology/media";
      where = "/mnt/synology/media";
    })

    (commonMountOptions // {
      what = "//synology/music";
      where = "/mnt/synology/music";
    })

    (commonMountOptions // {
      what = "//synology/photo";
      where = "/mnt/synology/photo";
    })

    (commonMountOptions // {
      what = "//synology/video";
      where = "/mnt/synology/video";
    })
  ];

  systemd.automounts = let commonAutoMountOptions = {
    wantedBy = [ "multi-user.target" ];
    # automountConfig = {
    #   TimeoutIdleSec = "600";
    # };
  };

  in [
    (commonAutoMountOptions // { where = "/mnt/synology/adult"; })
    (commonAutoMountOptions // { where = "/mnt/synology/media"; })
    (commonAutoMountOptions // { where = "/mnt/synology/music"; })
    (commonAutoMountOptions // { where = "/mnt/synology/photo"; })
    (commonAutoMountOptions // { where = "/mnt/synology/video"; })
  ];

  ## packages
  #environment.systemPackages = with pkgs; [
  #  curl
  #  wget
  #  vim
  #];

  # packages
  environment.systemPackages = with pkgs; [
    # system
    acpi                                    # Show battery status and other ACPI information
    aha                                     # ANSI HTML Adapter
    alacritty                               # Cross-platform, GPU-accelerated terminal emulator
    arandr                                  # Simple visual front end for XRandR
    aspell                                  # Spell checker for many languages
    aspellDicts.en                          # Aspell dictionary for English
    auto-cpufreq                            # Automatic CPU speed & power optimizer for Linux
    bandwhich                               # CLI utility for displaying current network utilization
    barrier                                 # Open-source KVM software
    bluez                                   # Official Linux Bluetooth protocol stack
    bluez-tools                             # Set of tools to manage bluetooth devices for linux
    #binsider                                # Analyzer of executables using a terminal user interface
    brightnessctl                           # This program allows you read and control device brightness
    bzip2                                   # High-quality data compression program
    clipmenu                                # Clipboard management using dmenu
    cifs-utils                              # Tools for managing Linux CIFS client filesystems
    comma                                   # Runs programs without installing them
    coolercontrol.coolercontrol-gui         # Monitor and control your cooling devices (GUI)
    coreutils                               # GNU Core Utilities
    curl                                    # Command line tool for transferring files with URL syntax
    direnv                                  # Shell extension that manages your environment
    dos2unix                                # Convert text files with DOS or Mac line breaks to Unix line breaks and vice versa
    dosfstools                              # Utilities for creating and checking FAT and VFAT file systems
    duf                                     # Disk Usage/Free Utility
    efibootmgr                              # Linux user-space application to modify the Intel Extensible Firmware Interfac...
    emacs                                   # Extensible, customizable GNU text editor
    exfat                                   # Free exFAT file system implementation
    exfatprogs                              # exFAT filesystem userspace utilities
    fastfetch                               # Like neofetch, but much faster because written in C
    file                                    # Program that shows the type of files
    findutils                               # GNU Find Utilities, the basic directory searching utilities of the GNU operating system
    fortune                                 # Program that displays a pseudorandom message from a database of quotations
    fzf                                     # Command-line fuzzy finder written in Go
    gawk                                    # GNU implementation of the Awk programming language
    git                                     # Distributed version control system
    gnugrep                                 # GNU implementation of the Unix grep command
    gnupg                                   # Modern release of the GNU Privacy Guard, a GPL OpenPGP implementation
    gnused                                  # GNU sed, a batch stream editor
    gnutar                                  # GNU implementation of the `tar' archiver
    gzip                                    # GNU zip compression program
    hdparm                                  # Tool to get/set ATA/SATA drive parameters under Linux
    helvum                                  # GTK patchbay for pipewire
    hfsprogs                                # HFS/HFS+ user space utils
    imagemagick                             # Software suite to create, edit, compose, or convert bitmap images
    imwheel                                 # Mouse wheel configuration tool for XFree86/Xorg
    inxi                                    # Full featured CLI system information tool
    iperf                                   # Tool to measure IP bandwidth using UDP or TCP
    ispell                                  # Interactive spell-checking program for Unix
    kitty                                   # Modern, hackable, featureful, OpenGL based terminal emulator
    languagetool                            # Proofreading program for English, French German, Polish, and more
    libnotify                               # Library that sends desktop notifications to a notification daemon
    libxslt                                 # C library and tools to do XSL transformations
    light                                   # GNU/Linux application to control backlights
    lm_sensors                              # Tools for reading hardware sensors
    lsb-release                             # Prints certain LSB (Linux Standard Base) and Distribution information
    lshw                                    # Provide detailed information on the hardware configuration of the machine
    lsof                                    # Tool to list open files
    lynx                                    # Text-mode web browser
    man                                     # Implementation of the standard Unix documentation system accessed using the man command
    man-db                                  # Implementation of the standard Unix documentation system accessed using the man command
    man-pages                               # Linux development manual pages
    menumaker                               # Heuristics-driven menu generator for several window managers
    micro                                   # Modern and intuitive terminal-based text editor
    moreutils                               # Growing collection of the unix tools that nobody thought to write long ago wh...
    mtools                                  # Utilities to access MS-DOS disks
    multitail                               # tail on Steroids
    neofetch                                # Fast, highly customizable system info script
    nettools                                # Set of tools for controlling the network subsystem in Linux
    nfs-utils                               # Linux user-space NFS utilities
    nitch                                   # Incredibly fast system fetch written in nim
    nix-index                               # Files database for nixpkgs
    nmap                                    # Free and open source utility for network discovery and security auditing
    ntfsprogs                               # FUSE-based NTFS driver with full write support
    ntp                                     # Implementation of the Network Time Protocol
    openssl                                 # Cryptographic library that implements the SSL and TLS protocols
    p7zip                                   # New p7zip fork with additional codecs and improvements (forked from https://sourceforge.net/projects/p7zip/)
    parted                                  # Create, destroy, resize, check, and copy partitions
    #pavucontrol                             # PulseAudio Volume Control
    pciutils                                # Collection of programs for inspecting and manipulating configuration of PCI devices
    pkg-config                              # Tool that allows packages to find out information about other packages (wrapper script)
    #pipewire                                # Server and user space API to deal with multimedia pipelines
    psmisc                                  # Set of small useful utilities that use the proc filesystem (such as fuser, killall and pstree)
    #pulseaudioFull                          # Sound server for POSIX and Win32 systems
    #pulseaudio                              # Sound server for POSIX and Win32 systems
    #pulseaudio-ctl                          # Control pulseaudio volume from the shell or mapped to keyboard shortcuts. No need for alsa-utils
    ripgrep                                 # Utility that combines the usability of The Silver Searcher with the raw speed of grep
    rsync                                   # Fast incremental file transfer utility
    samba                                   # Standard Windows interoperability suite of programs for Linux and Unix
    screen                                  # Window manager that multiplexes a physical terminal
    scrub                                   # Disk overwrite utility
    shc                                     # Shell Script Compiler
    silver-searcher                         # Code-searching tool similar to ack, but faster
    stow                                    # Tool for managing the installation of multiple software packages in the same run-time directory tree
    texinfo                                 # GNU documentation system
    tldr                                    # Simplified and community-driven man pages
    tlp                                     # Advanced Power Management for Linux
    tmux                                    # Terminal multiplexer
    traceroute                              # Tracks the route taken by packets over an IP network
    trash-cli                               # Command line interface to the freedesktop.org trashcan
    tree                                    # Command to produce a depth indented directory listing
    ufiformat                               # Low-level disk formatting utility for USB floppy drives
    unipicker                               # CLI utility for searching unicode characters by description and optionally co...
    unison                                  # Bidirectional file synchronizer
    unzip                                   # Extraction utility for archives compressed in .zip format
    usbutils                                # Tools for working with USB devices, such as lsusb
    veracrypt                               # Free Open-Source filesystem on-the-fly encryption
    vim                                     # Most popular clone of the VI editor
    wget                                    # Tool for retrieving files using HTTP, HTTPS, and FTP
    woeusb                                  # Create bootable USB disks from Windows ISO images
    xz                                      # General-purpose data compression software, successor of LZMA
    ydotool                                 # Generic Linux command-line automation tool
    zip                                     # Compressor/archiver for creating and modifying zipfiles

    # x11
    x2x                                     # Allows the keyboard, mouse on one X display to be used to control another X display
    xclip                                   # Tool to access the X clipboard from a console application
    xdo                                     # Small X utility to perform elementary actions on windows
    xorg.libX11                             #
    xorg.libX11.dev                         #
    xorg.libXft                             #
    xorg.libXinerama                        #
    xorg.libxcb                             #
    xorg.xbacklight                         #
    xorg.xev                                #
    xorg.xf86inputsynaptics                 #
    xorg.xinit                              #
    xorg.xinput                             #
    xorg.xkill                              #
    xorg.xprop                              #
    xorg.xwininfo                           #
    xsel                                    # Command-line program for getting and setting the contents of the X selection
    xvkbd                                   # Virtual keyboard for X window system

    ## wayland
    #clipman                                 # Simple clipboard manager for Wayland
    #hyprland                                # Dynamic tiling Wayland compositor that doesn't sacrifice on its looks
    #hyprpaper                               # Blazing fast wayland wallpaper utility
    #hyprpicker                              # Wlroots-compatible Wayland color picker that does not suck
    #qt5.qtwayland                           # Cross-platform application framework for C++
    #qt6.qtwayland                           # Cross-platform application framework for C++
    #rofi-wayland                            # Window switcher, run dialog and dmenu replacement for Wayland
    #swww                                    # Efficient animated wallpaper daemon for wayland, controlled at runtime
    #waybar                                  # Highly customizable Wayland bar for Sway and Wlroots based compositors
    #wayland-protocols                       # Wayland protocol extensions
    #wayland-utils                           # Wayland utilities (wayland-info)
    #wl-clipboard                            # Command-line copy/paste utilities for Wayland
    #wl-color-picker                         # Wayland color picker that also works on wlroots
    #wlroots                                 # Modular Wayland compositor library
    #wofi                                    # Launcher/menu program for wlroots based wayland compositors such as sway
    #xdg-desktop-portal-hyprland             # xdg-desktop-portal backend for Hyprland
    #xwayland                                # X server for interfacing X11 apps with the Wayland protocol

    # window manager
    adapta-gtk-theme                        # Adaptive GTK theme based on Material Design Guidelines
    adw-gtk3                                # Theme from libadwaita ported to GTK-3
    adwaita-icon-theme-legacy               # Fullcolor icon theme providing fallback for legacy apps
    adwaita-qt adwaita-qt6                  # Style to bend Qt applications to look like they belong into GNOME Shell
    blueman                                 # GTK-based Bluetooth Manager
    bsp-layout                              # Manage layouts in bspwm
    bspwm                                   # Tiling window manager based on binary space partitioning
    clipit                                  # Lightweight GTK Clipboard Manager
    conky                                   # Advanced, highly configurable system monitor based on torsmo
    dconf                                   #
    dialog                                  # Display dialog boxes from shell
    dmenu                                   # Generic, highly customizable, and efficient menu for the X Window System
    dunst                                   # Lightweight and customizable notification daemon
    ##eog                                     # GNOME image viewer
    evtest                                  # Simple tool for input event debugging
    eww                                     # Widget system made in Rust to create widgets for any WM
    gnome.eog                               # GNOME image viewer
    gnome.gnome-keyring                     # Collection of components in GNOME that store secrets, passwords, keys, certificates and make them available to applications
    gnome.gnome-settings-daemon             #
    gnome.gvfs                              # Virtual Filesystem support library (full GNOME support)
    gnome.zenity                            # Tool to display dialogs from the commandline and shell scripts
    ##gnome-keyring                           # Collection of components in GNOME that store secrets, passwords, keys, certificates and make them available to applications
    ##gnome-settings-daemon                   #
    gnome-icon-theme                        # Collection of icons for the GNOME 2 desktop
    gnome-themes-extra                      #
    gsimplecal                              # Lightweight calendar application written in C++ using GTK
    gtk3                                    # Multi-platform toolkit for creating graphical user interfaces
    gxmessage                               # GTK enabled dropin replacement for xmessage
    hicolor-icon-theme                      # Default fallback theme used by implementations of the icon theme specification
    i3-gaps                                 # Tiling window manager
    iwgtk                                   # Lightweight, graphical wifi management utility for Linux
    keychain                                # Keychain management tool
    #libsForQt5.qt5.qttools                  # Cross-platform application framework for C++
    lightdm                                 # Cross-desktop display manager
    lightdm-slick-greeter                   # Slick-looking LightDM greeter
    lxappearance                            # Lightweight program for configuring the theme and fonts of gtk applications
    mesa-demos                              # Collection of demos and test programs for OpenGL and Mesa
    mission-center                          # Monitor your CPU, Memory, Disk, Network and GPU usage
    ncpamixer                               # Terminal mixer for PulseAudio inspired by pavucontrol
    networkmanager                          # Network configuration and management tool
    networkmanagerapplet                    # NetworkManager control applet for GNOME
    nitrogen                                # Wallpaper browser and setter for X11
    pa_applet                               #
    pamixer                                 # Pulseaudio command line mixer
    paprefs                                 # PulseAudio Preferences
    pasystray                               # PulseAudio system tray
    pavucontrol                             # PulseAudio Volume Control
    picom                                   # Fork of XCompMgr, a sample compositing manager for X servers
    pmount                                  # Mount removable devices as normal user
    polkit_gnome                            # Dbus session bus service that is used to bring up authentication dialogs
    psensor                                 # Graphical hardware monitoring application for Linux
    qt6.qmake                               #
    rofi                                    # Window switcher, run dialog and dmenu replacement
    rofimoji                                # Simple emoji and character picker for rofi
    scrot                                   # Command-line screen capture utility
    sxhkd                                   # Simple X hotkey daemon
    system-config-printer                   #
    tint2                                   # Simple panel/taskbar unintrusive and light (memory, cpu, aestetic)
    unclutter                               # Hides mouse pointer while not in use
    wmname                                  # Prints or set the window manager name property of the root window
    wmctrl                                  # CLI tool to interact with EWMH/NetWM compatible X Window Managers
    xfce.exo                                # Application library for Xfce
    xfce.libxfce4ui                         # Widgets library for Xfce
    xfce.libxfce4util                       # Extension library for Xfce
    xfce.xfce4-appfinder                    # Appfinder for the Xfce4 Desktop Environment
    xfce.xfce4-battery-plugin               # Battery plugin for Xfce panel
    xfce.xfce4-clipman-plugin               # Clipboard manager for Xfce panel
    xfce.xfce4-cpufreq-plugin               # CPU Freq load plugin for Xfce panel
    xfce.xfce4-cpugraph-plugin              # CPU graph show for Xfce panel
    xfce.xfce4-datetime-plugin              # Shows the date and time in the panel, and a calendar appears when you left-cl...
    xfce.xfce4-dict                         # Dictionary Client for the Xfce desktop environment
    xfce.xfce4-fsguard-plugin               # Filesystem usage monitor plugin for the Xfce panel
    xfce.xfce4-genmon-plugin                # Generic monitor plugin for the Xfce panel
    xfce.xfce4-mailwatch-plugin             # Mail watcher plugin for Xfce panel
    xfce.xfce4-mpc-plugin                   # MPD plugin for Xfce panel
    xfce.xfce4-netload-plugin               # Internet load speed plugin for Xfce4 panel
    xfce.xfce4-notes-plugin                 # Sticky notes plugin for Xfce panel
    xfce.xfce4-notifyd                      # Simple notification daemon for Xfce
    xfce.xfce4-panel                        # Panel for the Xfce desktop environment
    xfce.xfce4-power-manager                # Power manager for the Xfce Desktop Environment
    xfce.xfce4-pulseaudio-plugin            # Adjust the audio volume of the PulseAudio sound system
    xfce.xfce4-sensors-plugin               # Panel plug-in for different sensors using acpi, lm_sensors and hddtemp
    xfce.xfce4-settings                     # Settings manager for Xfce
    xfce.xfce4-systemload-plugin            # System load plugin for Xfce panel
    xfce.xfce4-taskmanager                  # Easy to use task manager for Xfce
    xfce.xfce4-time-out-plugin              # Panel plug-in to take periodical breaks from the computer
    xfce.xfce4-timer-plugin                 # Simple countdown and alarm plugin for the Xfce panel
    xfce.xfce4-volumed-pulse                # Volume keys control daemon for Xfce using pulseaudio
    xfce.xfce4-weather-plugin               # Weather plugin for the Xfce desktop environment
    xfce.xfce4-whiskermenu-plugin           # Alternate application launcher for Xfce
    xfce.xfce4-windowck-plugin              # Xfce panel plugin for displaying window title and buttons
    xfce.xfce4-xkb-plugin                   # Allows you to setup and use multiple keyboard layouts
    xfce.xfconf                             # Simple client-server configuration storage and query system for Xfce
    xdg-desktop-portal-gtk                  # Desktop integration portals for sandboxed apps
    xdg-utils                               # Set of command line tools that assist applications with a variety of desktop integration tasks
    yad                                     # GUI dialog tool for shell scripts
    ##zenity                                  # Tool to display dialogs from the commandline and shell scripts

    ## xfce panel
    #xfce.xfce4-panel                        # Panel for the Xfce desktop environment
    #xfce.libxfce4ui                         # Widgets library for Xfce
    #xfce.libxfce4util                       # Extension library for Xfce
    #xfce.xfce4-appfinder                    # Appfinder for the Xfce4 Desktop Environment
    #xfce.xfce4-battery-plugin               # Battery plugin for Xfce panel
    #xfce.xfce4-clipman-plugin               # Clipboard manager for Xfce panel
    #xfce.xfce4-cpufreq-plugin               # CPU Freq load plugin for Xfce panel
    #xfce.xfce4-cpugraph-plugin              # CPU graph show for Xfce panel
    #xfce.xfce4-datetime-plugin              # Shows the date and time in the panel, and a calendar appears when you left-cl...
    #xfce.xfce4-dict                         # Dictionary Client for the Xfce desktop environment
    #xfce.xfce4-fsguard-plugin               # Filesystem usage monitor plugin for the Xfce panel
    #xfce.xfce4-genmon-plugin                # Generic monitor plugin for the Xfce panel
    #xfce.xfce4-mailwatch-plugin             # Mail watcher plugin for Xfce panel
    #xfce.xfce4-mpc-plugin                   # MPD plugin for Xfce panel
    #xfce.xfce4-netload-plugin               # Internet load speed plugin for Xfce4 panel
    #xfce.xfce4-notes-plugin                 # Sticky notes plugin for Xfce panel
    #xfce.xfce4-notifyd                      # Simple notification daemon for Xfce
    #xfce.xfce4-pulseaudio-plugin            # Adjust the audio volume of the PulseAudio sound system
    #xfce.xfce4-sensors-plugin               # Panel plug-in for different sensors using acpi, lm_sensors and hddtemp
    #xfce.xfce4-systemload-plugin            # System load plugin for Xfce panel
    #xfce.xfce4-taskmanager                  # Easy to use task manager for Xfce
    #xfce.xfce4-time-out-plugin              # Panel plug-in to take periodical breaks from the computer
    #xfce.xfce4-timer-plugin                 # Simple countdown and alarm plugin for the Xfce panel
    #xfce.xfce4-volumed-pulse                # Volume keys control daemon for Xfce using pulseaudio
    #xfce.xfce4-weather-plugin               # Weather plugin for the Xfce desktop environment
    #xfce.xfce4-whiskermenu-plugin           # Alternate application launcher for Xfce
    #xfce.xfce4-windowck-plugin              # Xfce panel plugin for displaying window title and buttons
    #xfce.xfce4-xkb-plugin                   # Allows you to setup and use multiple keyboard layouts

    # applications
    ardour                                  # Multi-track hard disk recording software
    audacity                                # Sound editor with graphical UI
    blender                                 # 3D Creation/Animation/Publishing System
    calibre                                 # Comprehensive e-book software
    cheesecutter                            # Tracker program for composing music for the SID chip
    cider                                   # New look into listening and enjoying Apple Music in style and performance
    electrum                                # Lightweight Bitcoin wallet
    freecad                                 # General purpose Open Source 3D CAD/MCAD/CAx/CAE/PLM modeler
    freetube                                # Open Source YouTube app for privacy
    furnace                                 # Multi-system chiptune tracker compatible with DefleMask modules
    #goattracker                             # Crossplatform music editor for creating Commodore 64 music. Uses reSID librar...
    goattracker-stereo                      # Crossplatform music editor for creating Commodore 64 music. Uses reSID librar...
    gimp                                    # GNU Image Manipulation Program
    gphoto2                                 # Ready to use set of digital camera software applications
    gphoto2fs                               # Fuse FS to mount a digital camera
    inkscape                                # Vector graphics editor
    kdenlive glaxnimate                     # Video editor
    kicad ngspice                           # Open Source Electronics Design Automation suite
    libreoffice                             # Comprehensive, professional-quality productivity suite, a variant of openoffice.org
    lmms                                    # DAW similar to FL Studio (music production software)
    obs-studio                              # Free and open source software for video recording and live streaming
    (pkgs.wrapOBS { plugins = with pkgs.obs-studio-plugins; [ wlrobs ]; })
    reaper                                  # Digital audio workstation
    spotify                                 # Play music from the Spotify music service

    # utilities
    appimage-run                            #
    audacious                               # Lightweight and versatile audio player
    bitwarden                               # A secure and free password manager for all of your devices
    celluloid                               # Simple GTK frontend for the mpv video player
    cherrytree                              # Hierarchical note taking application
    czkawka                                 # Simple, fast and easy to use app to remove unnecessary files from your computer
    dmg2img                                 # Apple's compressed dmg to standard (hfsplus) image disk file convert tool
    easytag                                 # View and edit tags for various audio files
    evince                                  # GNOME's document viewer
    ffmpeg_6-full                           # Complete, cross-platform solution to record, convert and stream audio and video
    ##file-roller                             # Archive manager for the GNOME desktop environment
    flameshot                               # Powerful yet simple to use screenshot software
    font-manager                            # Simple font management for GTK desktop environments
    fontpreview                             # Highly customizable and minimal font previewer written in bash
    fontforge-gtk                           # Font editor
    gcal                                    # Program for calculating and printing calendars
    glmark2                                 # OpenGL (ES) 2.0 benchmark
    #gnome.file-roller                       # Archive manager for the GNOME desktop environment
    gnome-frog                              # Intuitive text extraction tool (OCR) for GNOME desktop
    goldendict-ng                           # Advanced multi-dictionary lookup program
    gparted                                 # Graphical disk partitioning tool
    grip id3lib                             # GTK-based audio CD player/ripper
    gtkimageview                            # Image viewer widget for GTK
    handbrake libdvdcss libaacs libbluray   # Tool for converting video files and ripping DVDs
    kdeconnect                              # KDE Connect provides several features to integrate your phone and your computer
    libation                                # Audible audiobook manager
    livecaptions                            # Linux Desktop application that provides live captioning
    makemkv                                 # Convert blu-ray and dvd to mkv
    mate.engrampa                           # Archive Manager for MATE
    mediawriter                             # Tool to write images files to portable media
    meld                                    # Visual diff and merge tool
    mission-center                          # Monitor your CPU, Memory, Disk, Network and GPU usage
    mpv                                     # General-purpose media player, fork of MPlayer and mplayer2
    #mtkclient                               #
    mupdf                                   # Lightweight PDF, XPS, and E-book viewer and toolkit written in portable C
    notepadqq                               # Notepad++-like editor for the Linux desktop
    ##plasma5Packages.kdeconnect-kde          # KDE Connect provides several features to integrate your phone and your computer
    poppler_utils                           # PDF rendering library
    qmplay2                                 # Qt-based Multimedia player
    scrcpy                                  # Display and control Android devices over USB or TCP/IP
    simplescreenrecorder                    # Screen recorder for Linux
    upscayl                                 # Free and Open Source AI Image Upscaler
    vlc                                     # Cross-platform media player and streaming server
    virt-viewer                             # Viewer for remote virtual machines
    #x48                                     #
    xarchiver                               # GTK frontend to 7z,zip,rar,tar,bzip2, gzip,arj, lha, rpm and deb (open and ex...
    ymuse                                   # GTK client for Music Player Daemon (MPD)
    zathura                                 # Highly customizable and functional PDF viewer

    # tui
    atuin                                   # Replacement for a shell history which records additional commands context with optional encrypted synchronization between machines
    bmon                                    # Network bandwidth monitor
    btop                                    # Monitor of resources
    calcurse                                # Calendar and scheduling application for the command line
    cmus                                    # Small, fast and powerful console music player for Linux and *BSD
    cointop                                 # Fastest and most interactive terminal based UI application for tracking cryptocurrencies
    du-dust                                 # du + rust = dust. Like du but more intuitive
    elinks                                  # Full-featured text-mode web browser
    gopher                                  # Ncurses gopher client
    #gomuks                                  # A terminal based Matrix client written in Go
    hexedit                                 # View and edit files in hexadecimal or in ASCII
    hexyl                                   # Command-line hex viewer
    htop                                    # Interactive process viewer
    iftop                                   # Display bandwidth usage on a network interface
    irssi                                   # Terminal based IRC client
    links2                                  # Small browser with some graphics support
    lynx                                    # Text-mode web browser
    mc                                      # File Manager and User Shell for the GNU Project, known as Midnight Commander
    mcabber                                 # Small Jabber console client
    mop                                     # Simple stock tracker implemented in go
    #play                                    # A TUI playground for your favorite programs, such as grep, sed and awk
    ncmpcpp                                 # Featureful ncurses based MPD client inspired by ncmpc
    orpie                                   # Curses-based RPN calculator
    phetch                                  # Quick lil gopher client for your terminal, written in rust
    qodem                                   # Re-implementation of the DOS-era Qmodem serial communications package
    ranger                                  # File manager with minimalistic curses interface
    s-tui                                   # Stress-Terminal UI monitoring tool
    tig                                     # Text-mode interface for git
    tty-clock                               # Digital clock in ncurses
    ugrep                                   # Ultra fast grep with interactive query UI
    w3m                                     # Text-mode web browser
    wavemon                                 # Ncurses-based monitoring application for wireless network devices
    wordgrinder                             # Text-based word processor
    x3270                                   # IBM 3270 terminal emulator for the X Window System
    xplr                                    # Hackable, minimal, fast TUI file explorer
    yazi                                    # Blazing fast terminal file manager written in Rust, based on async I/O
    zenith                                  # Sort of like top or htop but with zoom-able charts, network, and disk usage

    # console
    argc                                    # Command-line options, arguments and sub-commands parser for bash
    asciinema                               # Terminal session recorder and the best companion of asciinema.org
    bat                                     # Cat(1) clone with syntax highlighting and Git integration
    bc                                      # GNU software calculator
    bitwarden-cli                           # Secure and free password manager for all of your devices
    bitwarden-menu                          # Dmenu/Rofi frontend for managing Bitwarden vaults. Uses the Bitwarden CLI tool to interact with the Bitwarden database
    boxes                                   # Command line program which draws, removes, and repairs ASCII art boxes
    broot                                   # config-private/common/org loading ❯
    chezmoi                                 # Interactive tree view, a fuzzy search, a balanced BFS descent and customizable commands
    cool-retro-term                         # Manage your dotfiles across multiple machines, securely
    ddgr                                    # Terminal emulator which mimics the old cathode display
    dict                                    # Dict protocol server and client
    diskonaut                               # Terminal disk space navigator
    element                                 # Periodic table on the command line
    eza                                     # Modern, maintained replacement for ls
    f3                                      # Fight Flash Fraud
    fd                                      # Simple, fast and user-friendly alternative to find
    frogmouth                               # Markdown browser for your terminal
    gcalcli                                 # CLI for Google Calendar
    gdu                                     # Disk usage analyzer with console interface
    googler                                 # Google Search, Google Site Search, Google News from the terminal
    gpart                                   # Guess PC-type hard disk partitions
    hardinfo                                # Display information about your hardware and operating system
    hyperfine                               # Command-line benchmarking tool
    id3v2                                   # Command line editor for id3v2 tags
    inetutils                               # Collection of common network programs
    lsd                                     # Next gen ls command
    miller                                  # Like awk, sed, cut, join, and sort for data formats such as CSV, TSV, JSON, JSON Lines, and positionally-indexed
    minicom                                 # Modem control and terminal emulation program
    mpc-cli                                 # Minimalist command line interface to MPD
    mpd mpdris2                             # Flexible, powerful daemon for playing music
    mtr                                     # Network diagnostics tool
    navi                                    # Interactive cheatsheet tool for the command-line and application launchers
    ncdu                                    # Disk usage analyzer with an ncurses interface
    ookla-speedtest                         # Command line internet speedtest tool by Ookla
    powertop                                # Analyze power consumption on Intel-based laptops
    procs                                   # Modern replacement for ps written in Rust
    rar                                     # Utility for RAR archives
    remind                                  # Sophisticated calendar and alarm program for the console
    sad                                     # CLI tool to search and replace
    sd                                      # Intuitive find & replace CLI (sed alternative)
    sharutils                               # Tools for remote synchronization and `shell archives'
    smem                                    # Memory usage reporting tool that takes shared memory into account
    speedtest-cli                           # Command line interface for testing internet bandwidth using speedtest.net
    sysbench                                # Modular, cross-platform and multi-threaded benchmark tool
    sysstat                                 # Collection of performance monitoring tools for Linux (such as sar, iostat and pidstat)
    sysz                                    # Fzf terminal UI for systemctl
    testdisk                                # Data recovery utilities
    #textual-paint                           # TUI image editor inspired by MS Paint
    thefuck                                 # Magnificent app which corrects your previous console command
    translate-shell                         # Command-line translator using Google Translate, Bing Translator, Yandex.Translate, and Apertium
    units                                   # Unit conversion tool
    wipe                                    # Secure file wiping utility
    yt-dlp                                  # Command-line tool to download videos from YouTube.com and other sites (youtube-dl fork)
    zoxide                                  # Fast cd command that learns your habits

    # internet
    betterbird                              # Betterbird is a fine-tuned version of Mozilla Thunderbird, Thunderbird on steroids, if you will
    bore-cli                                # Rust tool to create TCP tunnels
    brave                                   # Privacy-oriented browser for Desktop and Laptop computers
    chromium                                # Open source web browser from Google
    cointop                                 # Fastest and most interactive terminal based UI application for tracking cryptocurrencies
    dino                                    # Modern Jabber/XMPP Client using GTK/Vala
    discord                                 # All-in-one cross-platform voice and text chat for gamers
    element-desktop                         # A feature-rich client for Matrix.org
    filezilla                               # Graphical FTP, FTPS and SFTP client
    firefox                                 # Web browser built from Firefox source tree
    freenet                                 # Decentralised and censorship-resistant network
    gajim                                   # Jabber client written in PyGTK
    google-chrome                           # Freeware web browser developed by Google
    kristall                                # Graphical small-internet client, supports gemini, http, https, gopher, finger
    magic-wormhole                          # Securely transfer data between computers
    mop                                     # Simple stock tracker implemented in go
    ncgopher                                # Gopher and gemini client for the modern internet
    nheko                                   # Desktop client for the Matrix protocol
    nyxt                                    # Infinitely extensible web-browser (with Lisp development files using WebKitGTK platform port)
    opensnitch opensnitch-ui                # Application firewall
    pidgin                                  # Multi-protocol instant messaging client
    slack                                   # Desktop client for Slack
    simplex-chat-desktop                    # Desktop application for SimpleX Chat
    sparkleshare                            # Share and collaborate by syncing with any Git repository instantly. Linux, macOS, and Windows
    syncterm                                # BBS terminal emulator
    telegram-desktop                        # Telegram Desktop messaging app
    ##thunderbird                             # Full-featured e-mail client
    transmission_4-gtk                      # Fast, easy and free BitTorrent client
    tribler                                 # Decentralised P2P filesharing client based on the Bittorrent protocol
    tuba                                    # Browse the Fediverse
    #ungoogled-chromium                      # Open source web browser from Google, with dependencies on Google web services removed
    vdhcoapp                                # Companion application for the Video DownloadHelper browser add-on
    webwormhole                             # Send files using peer authenticated WebRTC
    zoom-us                                 # zoom.us video conferencing application

    # emulators
    anbox                                   # Android in a box
    basiliskii                              # 68k Macintosh emulator
    dosbox                                  # DOS emulator
    dosbox-x                                # Cross-platform DOS emulator based on the DOSBox project
    gnome.gnome-boxes                       # Simple GNOME 3 application to access remote or virtual systems
    ##gnome-boxes                             # Simple GNOME 3 application to access remote or virtual systems
    mame                                    # Multi-purpose emulation framework
    pcem                                    # Emulator for IBM PC computers and clones
    rpcemu                                  # Risc PC Emulator
    qemu_kvm                                # Generic and open source machine emulator and virtualizer
    quickemu                                # Quickly create and run optimised Windows, macOS and Linux virtual machines
    quickgui                                # Flutter frontend for quickemu
    scummvm                                 # Program to run certain classic graphical point-and-click adventure games (suc...
    tiny8086                                # Open-source small 8086 emulator
    uae                                     # Ultimate/Unix/Unusable Amiga Emulator
    vice                                    # Emulators for a variety of 8-bit Commodore computers
    virt-manager                            # Desktop user interface for managing virtual machines
    virt-viewer                             # Viewer for remote virtual machines
    virtualbox                              # PC emulator
    wine                                    # Open Source implementation of the Windows API on top of X, OpenGL, and Unix
    wineWowPackages.stable                  # Open Source implementation of the Windows API on top of X, OpenGL, and Unix
    winetricks                              # Script to install DLLs needed to work around problems in Wine

    # development
    acme                                    # Multi-platform cross assembler for 6502/6510/65816 CPUs
    adb-sync                                # Tool to synchronise files between a PC and an Android devices using ADB (Android Debug Bridge)
    #amber-lang                              # Programming language compiled to bash
    android-studio                          # Official IDE for Android (stable channel)
    android-tools                           # Android SDK platform tools
    android-udev-rules                      # Android udev rules list aimed to be the most comprehensive on the net
    autoconf                                # Part of the GNU Build System
    automake                                # GNU standard-compliant makefile generator
    binutils                                # Tools for manipulating binaries (linker, assembler, etc.) (wrapper script)
    binutils-ia16                           #
    bison                                   # Yacc-compatible parser generator
    cc65                                    # C compiler for processors of 6502 family
    ccache                                  # Compiler cache for fast recompilation of C/C++ code
    clang                                   # C language family frontend for LLVM (wrapper script)
    cmake                                   # Cross-platform, open-source build system generator
    #crexx                                   #
    csvkit                                  # Suite of command-line tools for converting to and working with CSV
    ctags                                   # Tool for fast source code browsing (exuberant ctags)
    djgpp_i586                              # Complete 32-bit GNU-based development system for Intel x86 PCs running DOS
    djgpp_i686                              # Complete 32-bit GNU-based development system for Intel x86 PCs running DOS
    flex                                    # Fast lexical analyser generator
    fpc                                     # Free Pascal Compiler from a source distribution
    gcc                                     # GNU Compiler Collection, version 13.2.0 (wrapper script)
    gcc-ia16                                # xss gcc-ia16
    github-desktop                          # GUI for managing Git and GitHub
    glibc                                   # GNU C Library
    gmp                                     # GNU multiple precision arithmetic library
    gnumake                                 # Tool to control the generation of non-source files from sources
    gnuplot                                 # Portable command-line driven graphing utility for many platforms
    go                                      # Go Programming language
    gpp                                     # General-purpose preprocessor with customizable syntax
    graphviz                                # Graph visualization tools
    gradle                                  # Enterprise-grade build system
    htmlq                                   # Like jq, but for HTML
    jdk                                     # Open-source Java Development Kit
    #jdk11                                   # Open-source Java Development Kit
    #jdk8                                    # Open-source Java Development Kit
    jetbrains.idea-community                # Free Java, Kotlin, Groovy and Scala IDE from jetbrains (built from source)
    jre                                     # Open-source Java Development Kit
    jq                                      # Lightweight and flexible command-line JSON processor
    jqp                                     # TUI playground to experiment with jq
    kotlin                                  # General purpose programming language
    lazarus lazarus-qt                      # Graphical IDE for the FreePascal language
    lazygit                                 # Simple terminal UI for git commands
    #lazyjournal                             # TUI for journalctl, file system logs, as well as Docker and Podman containers
    libmpc                                  # Library for multiprecision complex arithmetic with exact rounding
    m4                                      # GNU M4, a macro processor
    mpfr                                    # Library for multiple-precision floating-point arithmetic
    nasm                                    # 80x86 and x86-64 assembler designed for portability and modularity
    node2nix                                # Generate Nix expressions to build NPM packages
    open-watcom-bin                         # Project to maintain and enhance the Watcom C, C++, and Fortran cross compilers and tools
    open-watcom-v2                          # V2 fork of the Open Watcom suite of compilers and tools
    pandoc                                  # Conversion between documentation formats
    patch                                   # GNU Patch, a program to apply differences to files
    plantuml                                # Draw UML diagrams using a simple and human readable text description
    python311                               # High-level dynamically-typed programming language
    python311Packages.pip                   # PyPA recommended tool for installing Python packages
    racket                                  # Programmable programming language
    regina                                  # REXX interpreter
    ruby                                    # Object-oriented language for quick and easy programming
    rubyPackages.nokogiri                   #
    rustc                                   # Safe, concurrent, practical language (wrapper script)
    shellcheck                              # Shell script analysis tool
    tokei                                   # Program that allows you to count your code, quickly
    vlang                                   # Simple, fast, safe, compiled language for developing maintainable software
    x16-emulator                            # Official emulator of CommanderX16 8-bit computer
    x16-rom                                 # ROM file for CommanderX16 8-bit computer
    yq                                      # Command-line YAML/XML/TOML processor - jq wrapper for YAML, XML, TOML documents
    zlib                                    # Lossless data-compression library

    # containers
    distrobox                               # Wrapper around podman or docker to create and start containers
    podman                                  # Program for managing pods, containers and container images

    # docker
    docker                                  # Open source project to pack, ship and run any application as a lightweight co...
    docker-compose                          # Docker CLI plugin to define and run multi-container applications with Docker

    # ai
    #alpaca                                  # Ollama client made with GTK4 and Adwaita
    imaginer                                # Imaginer with AI
    ollama                                  # Get up and running with large language models locally
    open-webui                              # Comprehensive suite for LLMs with a user-friendly WebUI
    tabby                                   # Self-hosted AI coding assistant
    upscayl                                 # Free and Open Source AI Image Upscaler

    # zsh
    antibody                                # Fastest shell plugin manager
    meslo-lgs-nf                            # Meslo Nerd Font patched for Powerlevel10k
    zsh                                     # Z shell

    # printing
    system-config-printer                   #

    # console hacks
    aalib                                   # ASCII art graphics library
    asciiquarium                            # Enjoy the mysteries of the sea from the safety of your own terminal!
    bb                                      # AA-lib demo
    bucklespring                            # Nostalgia bucklespring keyboard sound
    cbonsai                                 # Grow bonsai trees in your terminal
    cmatrix                                 # Simulates the falling characters theme from The Matrix movie
    cowsay                                  # Program which generates ASCII pictures of a cow with a message
    dwt1-shell-color-scripts                # Collection of shell color scripts collected by dt (Derek Taylor)
    figlet                                  # Program for making large letters out of ordinary text
    genact                                  # Nonsense activity generator
    hackertyper                             # C rewrite of hackertyper.net
    hollywood                               # Fill your console with Hollywood melodrama technobabble
    lolcat                                  # Rainbow version of cat
    rig                                     # Random identity generator
    sl                                      # Steam Locomotive runs across your terminal when you type 'sl'
    tty-clock                               # Digital clock in ncurses

    # latex
    texliveFull                             # TeX Live environment
    texlivePackages.scheme-full             # full scheme (everything)

    # xscreensaver
    xscreensaver                            # Set of screensavers

    # extra savers
    antsimulator                            # Simple Ants simulator
    xmountains                              # X11 based fractal landscape generator
    xplanet                                 # Renders an image of the earth or other planets into the X root window

    # games
    alephone                                # Aleph One is the open source continuation of Bungie’s Marathon 2 game engine
    #alephone-apotheosis-x                   # Total conversion for Marathon Infinity running on the Aleph One engine
    alephone-durandal                       # Second chapter of the Marathon trilogy
    alephone-eternal                        # Picking up from the end of the Marathon trilogy, you find yourself suddenly ninety-four years in the future, in the year 2905
    alephone-evil                           # First conversion for Marathon Infinity
    alephone-infinity                       # Third chapter of the Marathon trilogy
    alephone-marathon                       # First chapter of the Marathon trilogy
    alephone-pathways-into-darkness         # Port of the 1993 mac game "Pathways Into Darkness" by Bungie to the Aleph One engine
    alephone-pheonix                        # 35-level single player major Marathon conversion
    alephone-red                            # Survival horror-esque Marathon conversion
    alephone-rubicon-x                      # Unofficial forth chapter of the Marathon series
    #alephone-yuge                           # 30 level Marathon scenario, plus 225 secret levels for many extra hours of gameplay
    azimuth                                 # Metroidvania game using only vectorial graphic
    bsdgames                                # Ports of all the games from NetBSD-current that are free
    eidolon                                 # Single TUI-based registry for drm-free, wine and steam games on linux, accessed through a rofi launch menu
    endless-sky                             # Sandbox-style space exploration game similar to Elite, Escape Velocity, or Star Control
    flare                                   # Fantasy action RPG using the FLARE engine
    gamemode                                # Optimise Linux system performance on demand
    gemrb                                   # Reimplementation of the Infinity Engine, used by games such as Baldur's Gate
    gzdoom                                  # Modder-friendly OpenGL and Vulkan source port based on the DOOM engine
    heroic                                  # Native GOG, Epic, and Amazon Games Launcher for Linux, Windows and Mac
    lutris                                  # Open Source gaming platform for GNU/Linux
    openttd                                 # Open source clone of the Microprose game "Transport Tycoon Deluxe"
    pingus                                  # Puzzle game with mechanics similar to Lemmings
    playonlinux                             # GUI for managing Windows programs under linux
    proton-caller                           # Run Windows programs with Proton
    protontricks                            # Simple wrapper for running Winetricks commands for Proton-enabled games
    pysolfc                                 # A collection of more than 1000 solitaire card games
    shattered-pixel-dungeon                 # Traditional roguelike game with pixel-art graphics and simple interface
    steam                                   # Digital distribution platform
    steam-tui                               # Rust TUI client for steamcmd

    #(pkgs.retroarch.override {
    #  cores = with libretro; [
    #    atari800
    #    beetle-psx-hw
    #    dosbox
    #    genesis-plus-gx
    #    mame
    #    snes9x
    #  ];
    #})
  ];

  # copy nixos configuration on rebuild
  system.copySystemConfiguration = true;

  # nixos system version
  system.stateVersion = "24.05";

}

#===============================================================================
# End of File
#===============================================================================
