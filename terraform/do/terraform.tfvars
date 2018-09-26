learn_settings = {
    image = "ubuntu-16-10-x64"
    region = "fra1"
    size = "512mb"
    resize_disk = false
}

do_ssh_keys = ["${digitalocean_ssh_key.petrem_rsa.id}", "${digitalocean_ssh_key.petrem_dsa.id}", "${digitalocean_ssh_key.pmierlutiu.id}"]
