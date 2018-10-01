variable "aws_amis" {
  type = "map"
  default = {
    #"us-east-1" = "ami-b374d5a5"
    "us-east-1" = "ami-0ac019f4fcb7cb7e6"
    "us-west-2" = "ami-4b32be2b"
  }
}
