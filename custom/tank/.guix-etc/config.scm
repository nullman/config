;;==============================================================================
;; Guix OS Configuration
;;
;; Reference: https://guix.gnu.org/en/manual/devel/en/html_node/
;; Manual: info guix
;; Man page: guix(1)
;;==============================================================================

(use-modules
 (gnu)
 (gnu packages gl)
 (gnu packages gnome)
 (gnu packages gtk)
 (gnu packages lxde)
 (gnu packages mail)
 (gnu packages package-management)
 (gnu packages rsync)
 (gnu packages search)
 (gnu packages tls)
 (gnu packages xorg)
 (gnu services xorg)
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (nongnu packages nvidia)
 (nongnu services nvidia))

(use-service-modules desktop networking ssh xorg)

(use-package-modules
 admin
 algebra
 aspell
 ;;betterbird
 bootloaders
 certs
 commencement
 conky
 curl
 emacs
 emacs-xyz
 ;;gnome
 gnuzilla
 gxmessage
 librewolf
 ncurses
 screen
 ssh
 suckless
 terminals
 version-control
 vim
 w3m
 wget
 wm
 xdisorg
 xfce)

(operating-system
 (kernel linux)
 (kernel-arguments
  (list
   "nvidia_drm.modeset=1"
   "rd.driver.blacklist=nouveau"
   "modprobe.blacklist=nouveau"))
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

 (host-name "tank")
 (timezone "America/Los_Angeles")
 (locale "en_US.utf8")
 (keyboard-layout (keyboard-layout "us" #:options '("shift:both_capslock" "caps:ctrl_modifier")))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot"))
   (keyboard-layout keyboard-layout)))

 ;; (bootloader (bootloader-configuration
 ;;              (bootloader grub-bootloader)
 ;;              (targets '("/dev/DEVICE"))
 ;;              (keyboard-layout keyboard-layout)))

 (mapped-devices
  (list
   (mapped-device
    (source (uuid "c0997efd-36fa-4bb7-9922-8a7cc6832837"))
    ;;(source "/dev/nvme0n1p2")
    (target "luks-root")
    (type luks-device-mapping))
   (mapped-device
    (source (uuid "5c93d084-63b5-4298-b9ee-a879aa7cc65e"))
    ;;(source "/dev/nvme0n1p1")
    (target "luks-home")
    (type luks-device-mapping))))

 (file-systems
  (cons*
   (file-system
    (device (uuid "f9ef2e21-bcac-4281-aa0c-eb1b64212e25"))
    ;;(device "/dev/mapper/luks-root")
    (mount-point "/")
    (type "ext4")
    (flags '(no-atime))
    (dependencies mapped-devices))
   (file-system
    (device (uuid "17A9-4856" 'fat))
    ;;(device "/dev/nvme0n1p1")
    (mount-point "/boot")
    ;;(mount-point "/boot/efi")
    (type "vfat"))
   (file-system
    (device (uuid "1865f4f0-2ac7-4659-8688-1d3af93bb5e7"))
    ;;(device "/dev/mapper/luks-home")
    (mount-point "/home")
    (type "ext4")
    (flags '(no-atime))
    (dependencies mapped-devices))
   %base-file-systems))

 (users
  (cons*
   (user-account
    (name "user")
    (comment "User")
    (password (crypt "user" "user"))
    (group "users")
    (supplementary-groups
     (list
      ;;"adbusers"
      "audio"
      "cdrom"
      "disk"
      ;;"docker"
      "floppy"
      "input"
      "kvm"
      ;;"libvirtd"
      "lp"
      ;;"mlocate"
      "netdev"
      ;;"networkmanager"
      "scanner"
      "users"
      "video"
      "wheel")))
   %base-user-accounts))

 (packages
  (append
   (list
    adwaita-icon-theme
    alacritty
    arandr
    aspell
    aspell-dict-en
    ;;betterbird
    bc
    bspwm
    btop
    clipmenu
    conky
    curl
    dialog
    dmenu
    dovecot
    emacs
    eog
    fastfetch
    ;;firefox
    ;;fortunes-jkirchartz
    gcc-toolchain
    git
    gnome-themes-extra
    gvfs
    gxmessage
    htop
    inxi
    ispell
    librewolf
    lxappearance
    neofetch
    nitrogen
    nmap
    openssh
    openssl
    plocate
    rofi
    rsync
    screen
    scrot
    stow
    system-config-printer
    sxhkd
    thunar
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-vcs-plugin
    thunar-volman
    unclutter
    vim
    w3m
    wget
    wmctrl
    xclip
    xdo
    xdotool
    xclip
    xfce4-panel
    xkill
    xprop
    xrandr
    xscreensaver
    xsel
    yad
    zenity)
   (list (replace-mesa glmark2))
   %base-packages))

 (services
  (cons*
   (service nvidia-service-type)
   ;;(service gnome-desktop-service-type
   ;;         (gnome-desktop-configuration (gnome (replace-mesa gnome))))
   (service xfce-desktop-service-type)
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout keyboard-layout)
     (modules (cons nvda %default-xorg-modules))
     (drivers '("nvidia"))))
   %desktop-services))

 (name-service-switch %mdns-host-lookup-nss))

;;==============================================================================
;; End of File
;;==============================================================================
