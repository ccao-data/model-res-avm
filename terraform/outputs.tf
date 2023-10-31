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
