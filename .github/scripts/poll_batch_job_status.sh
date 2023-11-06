#!/usr/bin/env bash
# Poll an AWS Batch job to check its status.
#
# Takes two positional arguments:
#
#   1. (required) The ID of the Batch job to poll
#   2. (optional) An enum indicating which type of poll to run: `startup` or
#      `completion`. Defaults to `completion`. If the value of the argument is
#      `startup`, the script will treat the `RUNNING` status as a terminal
#      success status. Otherwise, only `SUCCESS` will count as a terminal
#      success status, and a status of `RUNNING` will cause the script to
#      continue polling.
#
# Example usage:
#
#   ./.github/scripts/poll_batch_job_status.sh 12345 startup
set -euo pipefail

BATCH_JOB_LOG_URL_PREFIX="https://us-east-1.console.aws.amazon.com/cloudwatch/home?region=us-east-1#logsV2:log-groups/log-group/%2Faws%2Fbatch%2Fjob/log-events"
# How many times to poll AWS Batch job status while it's starting up before
# deciding to raise an error. Multiply by BATCH_JOB_POLL_INTERVAL_SECONDS to
# derive a timeout in second units. There is no equivalent timeout for running
# jobs, because those timeouts can be set on the Batch level, whereas startup
# timeouts are not controllable by Batch
BATCH_JOB_POLL_STARTUP_MAX_RETRIES=60

if [ -z "$1" ]; then
    echo "Missing Batch job ID"
    exit 1
fi

BATCH_JOB_ID="$1"

POLL_TYPE="completion"
  # How long to wait between queries when polling
BATCH_JOB_POLL_INTERVAL_SECONDS=300  # 5 minutes

if [ -n "$2" ]; then
    case "$2" in
        "startup")
            POLL_TYPE="startup"
            BATCH_JOB_POLL_INTERVAL_SECONDS=60
            ;;

        "completion")
            ;;

        *)
            echo "Positional argument must be one of 'startup' or 'completion', "
            echo "got: '$2'"
            exit 1
            ;;
    esac
fi

echo "Polling for status of Batch job $BATCH_JOB_ID, waiting for $POLL_TYPE"


LOOP_COUNTER=0
while true; do
    echo "Getting status of job $BATCH_JOB_ID"
    JOB_DESCRIPTIONS=$(aws batch describe-jobs --jobs "$BATCH_JOB_ID")

    JOB_LIST=$(echo "$JOB_DESCRIPTIONS" | jq -r '.jobs')
    if [[ "$JOB_LIST" == "[]" ]]; then
        echo "Unexpected empty response from aws batch describe-jobs"
        exit 1
    fi

    JOB_STATUS=$(echo "$JOB_DESCRIPTIONS" | jq -r '.jobs[0].status')
    echo "Job status is $JOB_STATUS"

    JOB_LOG_STREAM_NAME=$(\
        echo "$JOB_DESCRIPTIONS" | \
        jq -r '.jobs[0].container.logStreamName' \
    )
    # Any slashes in the log stream name need to be urlencoded
    JOB_LOG_URL="${BATCH_JOB_LOG_URL_PREFIX}/${JOB_LOG_STREAM_NAME//\//%2F}"

    case "$JOB_STATUS" in
        "RUNNING")
        if [[ "$POLL_TYPE" == "startup" ]]; then
            echo "Job has started! See logs: $JOB_LOG_URL"
            exit 0
        fi
        ;;

        "SUCCEEDED")
        echo "Job succeeded!"
        exit 0
        ;;

        "FAILED")
        echo "Job failed :( See logs: $JOB_LOG_URL"
        echo "More logs and container metrics can also be found on the "
        echo "job detail page in the AWS Batch console"
        exit 1
        ;;

        *)
        if [[ "$LOOP_COUNTER" == "$BATCH_JOB_POLL_STARTUP_MAX_RETRIES" ]]; then
            echo "Failing workflow due to job startup timeout. This means "
            echo "that the job did not enter a RUNNING state within a "
            echo "reasonable amount of time. This usually indicates a "
            echo "problem in the underlying ECS or EC2 backend that can "
            echo "be debugged by checking cluster/instance logs in the "
            echo "AWS console."
            exit 1
        fi
        ;;
    esac

    echo "Sleeping ${BATCH_JOB_POLL_INTERVAL_SECONDS}s until next status check"
    sleep "$BATCH_JOB_POLL_INTERVAL_SECONDS"

    LOOP_COUNTER=$((LOOP_COUNTER + 1))
    echo "Starting status check #$LOOP_COUNTER"

done
