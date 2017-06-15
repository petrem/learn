module "learn_droplets" {
  source = "./modules/learn"

  settings = "${var.learn_settings}"
  num_instances = "2"
  do_ssh_keys = "${var.do_ssh_keys}"
  taglist = ["${digitalocean_tag.learn.name}"]
}

resource "digitalocean_tag" "learn" {
         name = "learn"
}

