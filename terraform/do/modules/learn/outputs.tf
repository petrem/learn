output "learn_ips" {
  value = ["${digitalocean_droplet.learn.*.ipv4_address}"]
}
