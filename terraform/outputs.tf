# Define output variables that can be used by Terraform callers to access
# attributes of resources created by the config or input values that were
# passed into the config at build time

output "batch_job_definition_arn" {
  description = "ARN of the Batch job definition"
  value       = aws_batch_job_definition.main.arn
}

output "batch_job_queue_arn" {
  description = "ARN of the Batch job queue"
  value       = aws_batch_job_queue.main.arn
}

output "batch_job_name" {
  description = "Name of the Batch job"
  value       = var.batch_job_name
}

output "batch_container_image_name" {
  description = "Name of the container image to use for the Batch job"
  value       = var.batch_container_image_name
}
