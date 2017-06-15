variable "settings" {
  default = {
    image = "ubuntu-16-04-x64"
    region = "fra1"
    size = "512mb"
    resize_disk = true
  }
}

variable "num_instances" {}

variable "do_ssh_keys" { type="list" }

variable "taglist" {type="list"}
