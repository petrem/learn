module "learn" {
  source = "./modules/learn"
  region = "us-east-1"
  connection_key = "${file("../pk-learn.pem")}"
  user_key = "${var.petrem_public_key}"
  play_thing = ["foo", "bar"]
}

output "instance_ip" {
  value = "${module.learn.ip}"
}

output "module_play_thing" {
  value = "${module.learn.received}"
}
