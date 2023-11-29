library(glue)
library(magrittr)
library(paws.storage)

s3 <- paws.storage::s3(config(signature_version = "v4", region = "us-east-1"))
report_url <- s3$generate_presigned_url(
  client_method = "get_object",
  params = list(
    Bucket = "ccao-data-public-us-east-1",
    Key = "reporting/old_ward.parquet"
  ),
  expires_in = 3600
)
glue::glue("{report_url}") %>% message()
