(use-modules (gnu)(nongnu packages linux)(gnu services docker )(gnu services virtualization))
(use-service-modules vpn cups desktop networking ssh xorg)
(use-package-modules vpn)


(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Moscow")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "username")

  (users (cons* (user-account
                  (name "username")
                  (comment "Username")
                  (group "users")
                  (home-directory "/home/username")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  (packages (append (list (specification->package "i3-wm")
                          (specification->package "i3status")
                          (specification->package "dmenu")
                          (specification->package "st")
                          (specification->package "emacs")
                          (specification->package "emacs-exwm")
                          (specification->package
                          "emacs-desktop-environment")
                          (specification->package "nss-certs")
			  (specification->package "wireguard-tools"))
		    %base-packages))

  (services (append (list 
		     (service gnome-desktop-service-type)
			 (service bluetooth-service-type)

			 (service docker-service-type)

		     (set-xorg-configuration
                       (xorg-configuration (keyboard-layout keyboard-layout))) 

		    (service libvirt-service-type
		              (libvirt-configuration
				        (unix-sock-group "libvirt")
					    (tls-port "16555")))

		     (service wireguard-service-type
				(wireguard-configuration
	                 (port 51828)
	                 (addresses '("10.0.0.14/32"))
			         (dns '("8.8.8.8"))
			 (peers
			  (list
			   (wireguard-peer
			    (name "server")
			    (endpoint "159.223.237.128:51820")
		    	    (public-key "CQ4UnP5p43gc3JT3gD0ShiwSS08LIOjoi/RWH43+Jkw=")
		            (allowed-ips '("0.0.0.0/0")))))
		         (private-key "/srv/wg/GuixX220.key"))))

		      %desktop-services))

  ;;     		     (guix-service-type config =>
  ;; 			(guix-configuration
  ;;                         (inherit config)
  ;;                         (substitute-urls
  ;;                           (append (list "https://bordeaux.guix.gnu.org")
  ;;                           %default-substitute-urls))))


  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sdb"))
                (keyboard-layout keyboard-layout)))
  
  (mapped-devices (list (mapped-device
                          (source (uuid "79631668-1d3c-4d09-b505-2cc9c611d6a9"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems)))

