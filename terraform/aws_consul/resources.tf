module "consul" {
  source = "hashicorp/consul/aws"

  num_servers = "3"
}
