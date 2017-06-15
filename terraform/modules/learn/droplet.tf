resource "digitalocean_droplet" "learn" {

  ### droplet
  name               = "learn${count.index + 1}"
  ssh_keys           = "${var.do_ssh_keys}"
  image              = "${var.settings["image"]}"
  region             = "${var.settings["region"]}"
  size               = "${var.settings["size"]}"
  resize_disk        = "${var.settings["resize_disk"]}"
  private_networking = false

  ### how many
  count              = "${var.num_instances}"

  ### tags
  tags = "${var.taglist}"

  ### provisioning
  connection {
    type        = "ssh"
    user        = "root"
    timeout     = "2m"
  }

    # firewall
    provisioner "file" {
        source = "files/iptables.conf"
        destination = "/etc/network/iptables.conf"
    }

    provisioner "file" {
        source = "files/ip6tables.conf"
        destination = "/etc/network/ip6tables.conf"
    }
    provisioner "file" {
        source = "files/loadiptables"
        destination = "/etc/network/if-pre-up.d/loadiptables"
    }
    provisioner "remote-exec" {
        inline = [
            "chmod a+x /etc/network/if-pre-up.d/loadiptables",
            "IFACE=lo /etc/network/if-pre-up.d/loadiptables",
        ]
    }

    # fail2ban
    # see: https://www.digitalocean.com/community/tutorials/how-to-protect-ssh-with-fail2ban-on-ubuntu-14-04
    provisioner "remote-exec" {
        inline = [
            "apt-get --assume-yes install fail2ban"
        ]
    }


    # logging to papertrail
    # see https://papertrailapp.com/systems/setup, http://help.papertrailapp.com/kb/configuration/advanced-unix-logging-tips/
    provisioner "local-exec" {
        command = "curl -sS https://papertrailapp.com/tools/papertrail-bundle.pem > /tmp/papertrail-bundle.pem"
    }

    provisioner "file" {
        source = "/tmp/papertrail-bundle.pem"
        destination = "/etc/papertrail-bundle.pem"
    }
    provisioner "file" {
        source = "files/90-papertrail.conf"
        destination = "/etc/rsyslog.d/90-papertrail.conf"
    }
    provisioner "remote-exec" {
        script = "scripts/papertrail.sh"
    }

    # digitalocean agent
    # todo: https://www.digitalocean.com/community/tutorials/how-to-install-and-use-the-digitalocean-agent-for-additional-droplet-graphs
}
