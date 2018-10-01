module "learn_things" {
  source = "./modules/learn"

  settings = "${var.learn_settings}"
  num_instances = "1"
  ssh_keys = [
    "ae:fe:84:b1:d2:d7:c2:13:75:b4:8d:75:2a:2b:2e:01",
    "ba:d9:3a:00:d4:95:02:6c:39:67:93:1c:5b:88:20:eb"
  ]
  taglist = ["${digitalocean_tag.learn.id}", "${digitalocean_tag.learn2.id}"]
}

resource "digitalocean_tag" "learn" {
         name = "learn"
}
resource "digitalocean_tag" "learn2" {
         name = "learn2"
}

output "droplet_ip" {
  value = "${module.learn_things.ip_addresses}"
}

output "taglist" {
  value = "${module.learn_things.response}"
}
