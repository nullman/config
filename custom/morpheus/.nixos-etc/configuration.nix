#===============================================================================
# NixOS Configuration
#
# Reference: https://nix.dev/
# Manual: nixos-help
# Man page: configuration.nix(5)
#===============================================================================

{ config, pkgs, ... }:

{
  ## imports
  #imports = [
  #  ./hardware-configuration.nix
  #  #<home-manager/nixos>
  #];

  # imports
  imports = [
    ./hardware-configuration.nix
    ./hardware-encryption-configuration.nix
    #<home-manager/nixos>
  ];

  # grub MBR boot loader
  boot.loader = {
    grub = {
      enable = true;
      enableCryptodisk = true;
      configurationLimit = 20;
      device = "/dev/sda";
      useOSProber = true;
    };
  };
  fileSystems."/".options = [ "noatime" ];

  # networking
  networking = {
    wireless.iwd.enable = true;
    #networkmanager = {
    #  enable = true;
    #  wifi.backend = "iwd";
    #};
  };
  networking.hostName = "morpheus";

  ## open firewall ports
  #networking.firewall = {
  #  enable = true;
  #  allowedTCPPorts = [ 515 631 9100 ];
  #  allowedUDPPorts = [ 515 631 9100 ];
  #  checkReversePath = "loose";
  #};

  # openssh server
  services.openssh.enable = true;

  # standard settings

  # sysctl settings
  boot.kernel.sysctl = {
    "fs.file-max" = 524288;        # open file descriptors limit (star citizen)
    "vm.max_map_count" = 16777216; # maxmimum number of "memory map areas" a process can have (star citizen)
    "vm.swappiness" = 0;           # only use swap if needed; cat /proc/sys/vm/swappiness
  };

  # nix settings
  nix.settings = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];
  };

  # package settings
  nixpkgs.config = {
    allowUnfree = true;
    nvidia.acceptLicense = true;
    joypixels.acceptLicense = true;
    permittedInsecurePackages = [
      "electron-12.2.3"
      "electron-19.1.9"
      "electron-24.8.6"
      "openssl-1.1.1u"
      "openssl-1.1.1v"
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
      binutils-ia16 = prev.callPackage /home/kyle/.nixos/pkgs/binutils-ia16 {};
      djgpp_i586 = prev.callPackage /home/kyle/.nixos/pkgs/djgpp { targetArchitecture = "i586"; };
      djgpp_i686 = prev.callPackage /home/kyle/.nixos/pkgs/djgpp { targetArchitecture = "i686"; };
      gcc-ia16 = prev.callPackage /home/kyle/.nixos/pkgs/gcc-ia16 {};
      #mtkclient = prev.callPackage /home/kyle/.nixos/pkgs/mtkclient {};
      syncterm = prev.callPackage /home/kyle/.nixos/pkgs/syncterm {};
      vdhcoapp = prev.callPackage /home/kyle/.nixos/pkgs/vdhcoapp {};
      bspwm = prev.callPackage /home/kyle/code/bspwm {};
      #services.xserver.windowManager.bspwm = prev.callPackage /home/kyle/code/nixpkgs/pkgs/applications/window-managers/bspwm.nix {};
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
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # environment
  environment.variables = {
    GTK_DATA_PREFIX = "/run/current-system/sw";
  };

  # gnome settings
  services.gnome.gnome-settings-daemon.enable = true;

  # gnome policy kit
  security.polkit.enable = true;
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

  # steam
  programs.steam.enable = true;

  # locate
  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
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

  # opengl
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  ## nvidia
  #services.xserver.videoDrivers = [ "nvidia" ];
  #hardware.nvidia = {
  #  # modesetting is needed for most wayland compositors
  #  modesetting.enable = true;
  #  # power management (experimental, and can cause sleep/suspend to fail)
  #  powerManagement.enable = false;
  #  # fine-grained power management; turns off GPU when not in use
  #  # (experimental, and only works on modern Nvidia GPUs -- Turing or newer)
  #  powerManagement.finegrained = false;
  #  # open source version of nvidia
  #  # Turing and later architectures only
  #  # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
  #  # only available on driver 515.43.04+
  #  open = false;
  #  # enable nvidia settings menu
  #  nvidiaSettings = true;
  #  # select appropriate driver version [optional]
  #  #package = config.boot.kernelPackages.nvidiaPackages.stable;
  #  #package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  #};
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

  # x11
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    xkbOptions = "shift:both_capslock,caps:ctrl_modifier";
  };

  # wayland/hyprland
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # display manager: lightdm
  services.xserver.displayManager.lightdm.greeters.slick = {
    enable = true;
    extraConfig = ''
      [Greeter]
      background=/etc/nixos/wallpapers/Fluorescence.jpg
    '';
  };

  # window manager: bspwm
  #services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.displayManager.defaultSession = "none+bspwm";

  # window manager: i3wm
  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.i3.package = pkgs.i3-gaps;

  # compositor: picom
  services.picom.enable = true;

  # pulse audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  nixpkgs.config.pulseaudio = true;
  #hardware.pulseaudio.extraConfig = "load-module module-combine-sink";
  #hardware.pulseaudio.extraConfig = "unload-module module-suspend-on-idle";
  security.rtkit.enable = true;
  hardware.bluetooth.hsphfpd.enable = true;

  ## pipewire
  #sound.enable = true;
  #security.rtkit.enable = true;
  #services.pipewire = {
  #  enable = true;
  #  alsa.enable = true;
  #  alsa.support32Bit = true;
  #  pulse.enable = true;
  #  jack.enable = true;
  #};

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    #hsphfpd.enable = true;                # pulse audio
    #hsphfpd.enable = false;               # pipewire
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  services.blueman.enable = true;

  # touchpad
  services.xserver.libinput.enable = true;

  # light
  programs.light.enable = true;

  # fonts
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
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
      #logLevel = "debug";
    };
  };
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

  # virtualization
  virtualisation.libvirtd.enable = true;

  # thunar
  programs.thunar.enable = true;
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-volman
  ];
  services.gvfs.enable = true;         # mount, trash, and other functionalities
  services.tumbler.enable = true;      # thumbnail support for images

  # firefox
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
      #extraNativeMessagingHosts = with pkgs.nur.repos.wolfangaukang; [ vdhcoapp ];
      extraNativeMessagingHosts = with pkgs; [ vdhcoapp ];
    };
  };

  virtualisation.docker.enable = true;

  # mpd
  services.mpd = {
    enable = true;
    user = "kyle";
    musicDirectory = "/home/data/media/Audio/MPD";
    playlistDirectory = "/home/data/media/Audio/Playlists";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "PulseAudio Output"
      }
      # audio_output {
      #   type "pipewire"
      #   name "PipeWire Output"
      # }
    '';
  };

  systemd.services.mpd.environment = {
    # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
    XDG_RUNTIME_DIR = "/run/user/1000";   # userid of above "user"
  };

  services.dovecot2 = {
    enable = true;
    mailLocation = "maildir:~/Maildir";
  };

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
  users.groups.kyle = {
    name = "kyle";
    gid = 1000;
  };
  users.users.kyle = {
    group = "kyle";
    uid = 1000;
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/zsh";
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
  #   users.kyle = { pkgs, ... }: {
  #     home.stateVersion = "23.05";        # same as system.stateVersion
  #     home.packages = with pkgs.nur.repos.wolfangaukang; [ vdhcoapp ];
  #   };
  # };

  # automounts
  services.rpcbind.enable = true;         # needed for NFS
  systemd.mounts = let commonMountOptions = {
    mountConfig = {
      Options = "vers=3.0,credentials=/home/kyle/.synology-mount-credentials,iocharset=utf8,rw,file_mode=0777,dir_mode=0777,noatime,nofail";
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
    acpi
    alacritty
    arandr
    aspell
    aspellDicts.en
    bluez
    bluez-tools
    brightnessctl
    #busybox                                 # non-standard ps, ls, etc
    bzip2
    clipmenu
    cifs-utils
    coreutils
    curl
    direnv
    dos2unix
    dosfstools
    duf
    emacs
    exfat
    exfatprogs
    file
    findutils
    fortune
    fzf
    gawk
    git
    gnugrep
    gnupg
    gnused
    gnutar
    gzip
    hdparm
    hfsprogs
    imagemagick
    inxi
    iperf
    ispell
    kitty
    libnotify
    light
    lm_sensors
    lsb-release
    lshw
    lsof
    lynx
    man
    man-db
    man-pages
    menumaker
    #mlocate
    mtools
    neofetch
    nettools
    nfs-utils
    nix-index
    ntfsprogs
    nmap
    parted
    pciutils
    pipewire
    pkg-config
    pulseaudio
    pulseaudio-ctl
    ripgrep
    rsync
    samba
    screen
    scrub
    shc
    silver-searcher
    stow
    texinfo
    tldr
    tlp
    tmux
    traceroute
    trash-cli
    ufiformat
    unzip
    usbutils
    vim
    wget
    woeusb
    xz
    ydotool

    # window manager
    adapta-gtk-theme
    blueman
    bspwm
    bsp-layout
    clipit
    conky
    dconf
    dialog
    dmenu
    dunst
    evtest
    eww
    eww-wayland
    gnome-icon-theme
    gnome.eog
    gnome.gnome-keyring
    gnome.gnome-settings-daemon
    gnome.gnome-themes-extra
    gnome.gvfs
    gnome.zenity
    gsimplecal
    gtk3
    gxmessage
    hicolor-icon-theme
    hyprland
    hyprpaper
    hyprpicker
    i3-gaps
    iwgtk
    keychain
    lightdm
    lightdm-slick-greeter
    lxappearance
    mesa-demos
    ncpamixer
    networkmanager
    networkmanagerapplet
    nitrogen
    pa_applet
    pamixer
    paprefs
    pasystray
    pavucontrol
    picom
    pmount
    polkit_gnome
    psensor
    qt5.qtwayland
    qt6.qmake
    qt6.qtwayland
    rofi
    rofi-wayland
    rofimoji
    scrot
    sxhkd
    system-config-printer
    tint2
    unclutter
    #waybar
    wl-color-picker
    wlroots
    wmname
    #wofi
    wmctrl
    xfce.exo
    xfce.xfce4-power-manager
    xfce.xfce4-settings
    xfce.xfconf
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    xdg-utils
    xwayland
    yad

    # x11
    x2x
    xclip
    xdo
    xorg.libX11
    xorg.libX11.dev
    xorg.libXft
    xorg.libXinerama
    xorg.libxcb
    xorg.xbacklight
    xorg.xev
    xorg.xinit
    xorg.xinput
    xorg.xkill
    xorg.xprop
    xorg.xwininfo
    xsel
    xvkbd

    # applications
    ardour
    audacious
    audacity
    blender
    calibre
    celluloid
    cider
    electrum
    evince
    freetube
    gimp
    gphoto2
    gphoto2fs
    inkscape
    #kdenlive
    libreoffice
    mpv
    mupdf
    notepadqq
    #obs-studio
    #(pkgs.wrapOBS {plugins = with pkgs.obs-studio-plugins; [ wlrobs ]; })
    reaper
    spotify
    vlc
    #virt-viewer
    #xfce.thunar
    #xfce.thunar-archive-plugin
    #xfce.thunar-media-tags-plugin
    #xfce.thunar-volman
    ymuse

    # utilities
    appimage-run
    authy
    bitwarden
    dmg2img
    easytag
    etcher
    ffmpeg_6-full
    flameshot
    font-manager
    fontpreview
    fontforge-gtk
    gcal
    glmark2
    gnome.file-roller
    gnome-frog
    gparted
    grip id3lib
    gtkimageview
    handbrake libdvdcss libaacs libbluray
    livecaptions
    makemkv
    #mediawriter
    meld
    #mtkclient
    #nur.repos.wolfangaukang.vdhcoapp
    scrcpy
    simplescreenrecorder
    #upscayl
    #vdhcoapp
    #x48

    # tui
    btop
    calcurse
    cmus
    cointop
    elinks
    gopher
    gomuks
    hexedit
    hexyl
    htop
    iftop
    irssi
    links2
    lynx
    mc
    mcabber
    mop
    orpie
    phetch
    qodem
    ranger
    s-tui
    tig
    ugrep
    w3m
    wordgrinder
    x3270
    xplr
    zenith

    # console
    argc
    asciinema
    bat
    bc
    bitwarden-cli
    bitwarden-menu
    boxes
    broot
    #carbonyl
    cool-retro-term
    ddgr
    element
    exa
    #eza
    f3
    fd
    frogmouth
    gcalcli
    gdu
    googler
    gpart
    hardinfo
    hyperfine
    id3v2
    inetutils
    lsd
    miller
    minicom
    mpc-cli
    mpd mpdris2
    mtr
    navi
    ncdu
    ookla-speedtest
    powertop
    procs
    rar
    remind
    sad
    sd
    sharutils
    smem
    speedtest-cli
    sysbench
    sysstat
    sysz
    testdisk
    #textual-paint
    thefuck
    translate-shell
    units
    wipe
    yt-dlp
    zoxide

    # internet
    betterbird
    bore-cli
    brave
    chromium
    cointop
    dino
    discord
    element-desktop
    filezilla
    firefox
    #firefox-wayland
    gajim
    google-chrome
    kristall
    magic-wormhole
    mop
    ncgopher
    nyxt
    pidgin
    #simplex-chat-desktop
    slack
    syncterm
    telegram-desktop
    transmission-gtk
    tuba

    # emulators
    basiliskii
    dosbox
    gnome.gnome-boxes
    mame
    qemu_kvm
    quickemu
    quickgui
    uae
    vice
    virt-manager
    virt-viewer
    virtualbox
    #wine
    wineWowPackages.stable
    winetricks

    # containers
    distrobox
    podman

    # development
    acme
    adb-sync
    android-studio
    android-tools
    android-udev-rules
    binutils
    binutils-ia16
    bison
    cc65
    ccache
    clang
    cmake
    ctags
    djgpp_i686
    flex
    fpc
    gcc
    gcc-ia16
    github-desktop
    glibc
    #gmp
    gnumake
    gnuplot
    gpp
    graphviz
    gradle
    htmlq
    jdk
    #jdk11
    #jdk8
    jetbrains.idea-community
    jre
    jq
    jqp
    kotlin
    lazygit
    #libmpc
    m4
    #mpfr
    nasm
    node2nix
    open-watcom-bin
    #open-watcom-v2
    pandoc
    patch
    plantuml
    python311
    python311Packages.pip
    racket
    regina
    rnix-lsp
    ruby
    rubyPackages.nokogiri
    rustc
    shellcheck
    tokei
    vlang
    x16-emulator
    x16-rom
    yq
    zlib

    # zsh
    antibody
    meslo-lgs-nf
    zsh

    # printing
    system-config-printer

    # console hacks
    aalib
    asciiquarium
    bb
    bucklespring
    cbonsai
    cmatrix
    cowsay
    dwt1-shell-color-scripts
    figlet
    genact
    hackertyper
    hollywood
    lolcat
    rig
    sl
    tty-clock

    # latex
    texlive.combined.scheme-full

    # xscreensaver
    xscreensaver
    # extra savers
    antsimulator
    xmountains
    xplanet

    # games
    alephone
    #alephone-apotheosis-x
    alephone-durandal
    alephone-eternal
    alephone-evil
    alephone-infinity
    alephone-marathon
    alephone-pathways-into-darkness
    alephone-pheonix
    alephone-red
    alephone-rubicon-x
    #alephone-yuge
    azimuth
    bsdgames
    eidolon
    flare
    gamemode
    gemrb
    lutris
    pingus
    playonlinux
    proton-caller
    protontricks
    pysolfc
    shattered-pixel-dungeon
    steam
    steam-tui
  ];

  # copy nixos configuration on rebuild
  system.copySystemConfiguration = true;

  # nixos system version
  system.stateVersion = "23.05";
}

#===============================================================================
# End of File
#===============================================================================
