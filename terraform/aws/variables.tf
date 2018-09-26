variable "aws_access_key" {}
variable "aws_secret_key" {}
variable "aws_region" {
  default = "us-east-1"
}

variable "aws_amis" {
  type = "map"
  default = {
    #"us-east-1" = "ami-b374d5a5"
    "us-east-1" = "ami-0ac019f4fcb7cb7e6"
    "us-west-2" = "ami-4b32be2b"
  }
}

variable "petrem_public_key" {
  default = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCx0qFtjyvk4KafnfX5t6QpHIIj6wOTcc2NBrMeMGu6EslcX3dpbpLW2oHvNp9k+JQC4ZfQjHqEsDpOoovU0fw8VpOL5Xm8ndROmj0yry0P59+/hhiU2rpBGifTuuCru+7Y6EW47hg/BH93ViBARrYsrqmWQpbhGqu2mjB8v614x1sHl9yHBeVtp3z9RgB6JvhqKX5xzOS+XDJqT3sSwSzuETg6jCNpxhEB7dCeiBnnXI7iJ9yjy9QSeJovK9FpsU/ALHUOvghyvqAReciA9f80FxDK16w4mYL3Fqmo5q3292NAEuidyZ8ctxOdDnbLf33RYI/QoqO9dzx5ky4ZijoJ"
}
