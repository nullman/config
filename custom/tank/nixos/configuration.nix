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
    ./hardware-encryption-configuration.nix
  ];

  # systemd-boot EFI boot loader
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  # # mdadm.conf
  # environment.etc."mdadm.conf".text = ''
  #   ARRAY /dev/md0 metadata=1.0 name=archiso:0 UUID=764b3bea:1aad3e88:5543f650:bd7314d2
  #   ARRAY /dev/md1 metadata=1.2 name=archiso:1 UUID=dc0e0617:70216ae6:c8af2052:5e730003
  # '';

  # networking
  networking.hostName = "tank";
  networking.networkmanager.enable = true;

  # standard settings

  # package settings
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.joypixels.acceptLicense = true;
  nixpkgs.config.permittedInsecurePackages = [
    "electron-12.2.3"
  ];

  # time zone
  time.timeZone = "US/Central";

  # internationalization
  i18n.defaultLocale = "en_US.UTF-8";

  # console
  # console = {
  #   font = "Lat2-Terminus16";
  #   # keyMap = "us";
  #   useXkbConfig = true;
  # };
  console = {
    packages = [ pkgs.terminus_font ];
    font = "${pkgs.terminus_font}/share/consolefonts/ter-d20b.psf.gz";
    useXkbConfig = true;
  };

  # gnome keyring
  services.gnome.gnome-keyring.enable = true;

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

  # policy kit
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
        TimeoutStopSec = 10;
      };
    };
    extraConfig = ''
      DefaultTimeoutStopSec=10s
    '';
  };

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

  # udev
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEMS=="usb", SUBSYSTEM=="block", ENV{ID_FS_USAGE}=="filesystem", RUN{program}+="${pkgs.systemd}/bin/systemd-mount --no-block --automount=yes --collect $devnode /media"
  '';

  # locate
  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
    localuser = null;
  };

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
    xwayland = {
      enable = true;
      hidpi = true;
    };
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
  services.xserver.windowManager.bspwm.enable = true;

  # compositor: picom
  services.picom.enable = true;

  # pipewire
  sound.enable = true;
  # hardware.pulseaudio.enable = true;
  # hardware.pulseaudio.support32Bit = true;
  # nixpkgs.config.pulseaudio = true;
  # #hardware.pulseaudio.extraConfig = "load-module module-combine-sink";
  # #hardware.pulseaudio.extraConfig = "unload-module module-suspend-on-idle";
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
    General = {
      Enable = "Source,Sink,Media,Socket";
    };
  };
  services.blueman.enable = true;

  # touchpad
  services.xserver.libinput.enable = true;

  # light
  programs.light.enable = true;

  # printing
  services.printing.enable = true;

  # fonts
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      anonymousPro
      #cantarell-fonts
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
        monospace = ["Hack LG M Regular Nerd Font Complete Mono" ];
        serif = [ "Noto Serif" "Source Han Serif" ];
        sansSerif = [ "Noto Sans" "Source Han Sans" ];
      };
    };
  };

  # opengl
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
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

  # openssh
  services.openssh.enable = true;

  # user account
  users.groups.kyle = {
    name = "kyle";
    gid = 1000;
  };
  users.users.kyle = {
    group = "kyle";
    uid = 1000;
    isNormalUser = true;
    #shell = "/run/current-system/sw/bin/zsh";
    initialPassword = "nixos";
    extraGroups = [
      "audio"
      "cdrom"
      "disk"
      "floppy"
      "input"
      "kvm"
      "lp"
      "mlocate"
      "networkmanager"
      "users"
      "video"
      "wheel"
    ];
  };

  # mpd
  services.mpd = {
    enable = true;
    user = "kyle";
    musicDirectory = "/home/data/media/Audio/Music";
    playlistDirectory = "/home/data/media/Audio/Playlists";
    extraConfig = ''
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
    clipmenu
    cifs-utils
    curl
    dosfstools
    duf
    emacs
    findutils
    fortune
    fzf
    git
    gnugrep
    gnupg
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
    lsof
    lynx
    man
    man-db
    man-pages
    menumaker
    mlocate
    mtools
    neofetch
    nettools
    nix-index
    nmap
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
    gnome-icon-theme
    gnome.eog
    gnome.gnome-keyring
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
    tint2
    unclutter
    #waybar
    wl-color-picker
    wlroots
    wmname
    #wofi
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    xdg-utils
    xwayland
    yad

    # x11
    x2x
    xclip
    xdo
    xfce.xfconf
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
    audacious
    audacity
    evince
    gimp
    gphoto2
    gphoto2fs
    #kdenlive
    libreoffice
    mpc-qt
    mpv
    mupdf
    notepadqq
    #obs-studio
    #(pkgs.wrapOBS {plugins = with pkgs.obs-studio-plugins; [ wlrobs ]; })
    #virt-viewer
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-media-tags-plugin
    xfce.thunar-volman

    # utilities
    bitwarden
    etcher
    flameshot
    font-manager
    fontpreview
    fontforge-gtk
    gcal
    gnome.file-roller
    gparted
    gtkimageview
    meld
    simplescreenrecorder
    #x48
    xscreensaver

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
    xplr
    zenith

    # console
    bat
    bc
    bitwarden-cli
    bitwarden-menu
    broot
    ddgr
    element
    exa
    f3
    fd
    frogmouth
    gcalcli
    gdu
    googler
    gpart
    hardinfo
    hyperfine
    inetutils
    lsd
    miller
    mpc-cli
    mpd mpdris2
    mtr
    navi
    ncdu
    powertop
    procs
    rar
    remind
    sad
    sd
    sharutils
    smem
    sysbench
    sysstat
    sysz
    testdisk
    thefuck
    translate-shell
    units
    wipe
    yt-dlp
    zoxide

    # internet
    betterbird
    chromium
    discord
    element-desktop
    filezilla
    firefox
    #firefox-wayland
    gajim
    kristall
    ncgopher
    nyxt
    slack
    #syncterm
    tuba

    # emulation
    dosbox
    qemu_kvm

    # development
    adb-sync
    android-tools
    android-udev-rules
    cmake
    gcc
    glibc
    gnumake
    htmlq
    jq
    python311
    python311Packages.pip
    rustup
    tokei
    yq

    # zsh
    fzf-zsh
    meslo-lgs-nf
    zsh
    zsh-autosuggestions
    zsh-history-substring-search
    zsh-powerlevel10k
    zsh-syntax-highlighting

    ## fonts
    #anonymousPro
    ##cantarell-fonts
    ##dejavu_fonts
    ##dina-font
    #fira-code
    #fira-code-symbols
    #font-awesome
    ##freefont_ttf
    ##google-fonts
    #hack-font
    ##joypixels
    ##liberation_ttf
    ##mononoki
    ##mplus-outline-fonts.githubRelease
    #nerdfonts
    #noto-fonts
    #noto-fonts-emoji
    ##proggyfonts
    #terminus-nerdfont
    #terminus_font_ttf
    ##ubuntu_font_family
    ##unifont
    ##xorg.fontadobe100dpi
    ##xorg.fontadobe75dpi
    ##xorg.fontbh100dpi
    ##xorg.fontbh75dpi
    ##xorg.fontbhtype1
    ##xorg.fontmiscmisc

    # printer
    system-config-printer
    canon-cups-ufr2
  ];

  # copy nixos configuration on rebuild
  system.copySystemConfiguration = true;

  # nixos system version
  system.stateVersion = "23.05";
}

#===============================================================================
# End of File
#===============================================================================
