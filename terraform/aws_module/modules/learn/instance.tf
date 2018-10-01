resource "aws_instance" "learn" {
  #  ami           = "ami-2757f631"
  ami           = "${lookup(var.aws_amis, var.region)}"
  instance_type = "t2.micro"

  key_name = "learn"

  connection {
    type     = "ssh"
    user     = "ubuntu"
    private_key = "${var.connection_key}"
  }

  provisioner "remote-exec" {
    inline = [
      "echo ${var.user_key} >> .ssh/authorized_keys"
    ]
  }
}

variable "region" {}

variable "connection_key" {}

variable "user_key" {}

variable "play_thing" { type = "list" }

output "ip" {
  value = "${aws_instance.learn.public_ip}"
}

output "received" {
  value = "${var.play_thing}"
}
