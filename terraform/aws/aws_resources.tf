resource "aws_instance" "example" {
  #  ami           = "ami-2757f631"
  ami           = "${lookup(var.aws_amis, var.aws_region)}"
  instance_type = "t2.micro"
  depends_on = ["aws_s3_bucket.example"]

  key_name = "learn"

  connection {
    type     = "ssh"
    user     = "ubuntu"
    private_key = "${file("../pk-learn.pem")}"
  }

  provisioner "remote-exec" {
    inline = [
      "echo ${var.petrem_public_key} >> .ssh/authorized_keys"
    ]
  }
}

# resource "aws_instance" "another" {
#   ami           = "ami-b374d5a5"
#   instance_type = "t2.micro"
# }

resource "aws_eip" "ip" {
  instance = "${aws_instance.example.id}"
}

resource "aws_s3_bucket" "example" {
  # NOTE: S3 bucket names must be unique across _all_ AWS accounts, so
  # this name must be changed before applying this example to avoid naming
  # conflicts.
  bucket = "petrem-learn-tf-getting-started-guide"
  acl    = "private"
}

output "ip" {
  value = "${aws_eip.ip.public_ip}"
}
