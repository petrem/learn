resource "digitalocean_ssh_key" "petrem_rsa" {
  name = "petrem_rsa"
  public_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCx0qFtjyvk4KafnfX5t6QpHIIj6wOTcc2NBrMeMGu6EslcX3dpbpLW2oHvNp9k+JQC4ZfQjHqEsDpOoovU0fw8VpOL5Xm8ndROmj0yry0P59+/hhiU2rpBGifTuuCru+7Y6EW47hg/BH93ViBARrYsrqmWQpbhGqu2mjB8v614x1sHl9yHBeVtp3z9RgB6JvhqKX5xzOS+XDJqT3sSwSzuETg6jCNpxhEB7dCeiBnnXI7iJ9yjy9QSeJovK9FpsU/ALHUOvghyvqAReciA9f80FxDK16w4mYL3Fqmo5q3292NAEuidyZ8ctxOdDnbLf33RYI/QoqO9dzx5ky4ZijoJ"
  lifecycle = {
    prevent_destroy = true
  }
}

resource "digitalocean_ssh_key" "petrem_dsa" {
  name = "petrem_dsa"
  public_key = "ssh-dss AAAAB3NzaC1kc3MAAACBAJaUvu9fThGAyPrMEYQKerpjTBicYIKf/8i5eHLLSSAeTTqlx9m8+KKWDh9oTbH+lCETK3wYa4t1IlogqXADwCWOkBrXSOzk6fsCaIzcuQFi1h8kcraHMXhgDsCM/ociS6A9SBkN1B5QMsClp257Is7Dw9Yvt+R7NV7q4n8rQRvNAAAAFQDHJDVF7pS2LW9s5oSCs4Z2VlhdGQAAAIA69IVY3phIIiQ0TjpANoLQ8TKUcpa6RfWOEkfYqGUzElQ0A/Ves9aeuxuWO65+zB2VgKCZd4Q06Ky9rfoc9PU3e/eO3oAn/NMBYCwMYJBzS7mG6xFxFBlVM8OaSJ7IwXmoYz2CIRE2Cg7/sCluMp1s9TQjR3uqNOl86qLv0FqXuQAAAIB3gKN2YzMVPa6ZJvLeFd95Rl5FbBNYFsQJr3FxR2Qyb82ryzTVNMiLzy2Fe60L4LRrUOyc0EoU9rHHVWlvR8Fv41gCZ7/Q50YoRTHg3HGCQGzvP11RsYxrSBV9XGWp6DdkrNHr++uWI9RM5eWAqIEHJDscp2AcJCPPMBAXeTrm7A=="
  lifecycle = {
    prevent_destroy = true
  }
}

resource "digitalocean_ssh_key" "pmierlutiu" {
  name = "pmierlutiu"
  public_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAAgQDLVr6SrwSsy43qBv3aSHh8RkTGBp1PqLs/MU4ef55erAg+Z0yi82yp+S+vKtzjIZQwh24iubj5pxtes/6dmzGSu+wr+l1XvwbnJpDhq+mrJeS0Iwsw/3dUsSPtp+OrwvKbA92b23N2iaE2EKZZW/zojuKDpDUrj8GTZbxMpVamnw=="
  lifecycle = {
    prevent_destroy = true
  }
}

