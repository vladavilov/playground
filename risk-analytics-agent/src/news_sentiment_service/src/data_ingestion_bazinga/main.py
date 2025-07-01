"""DIS-Bazinga – Service Entrypoint

This is the cron-driven micro-service responsible for fetching raw news
articles from the *Bazinga* provider and queuing them for downstream
processing.

Specification reference (NewsSentimentService.md):
  • RTN-FR-01 – periodic ingestion trigger
  • RTN-FR-01a – deduplication via article_hash
  • RTN-FR-01b – watermarking by last-run timestamp
"""

import logging
import sys
from typing import NoReturn

from .orchestrator import run_ingestion_cycle

SERVICE_NAME = "data_ingestion_bazinga"


def main() -> NoReturn:  # pragma: no cover
    """Bootstrap the scheduled ingestion loop."""
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s — %(name)s — %(levelname)s — %(message)s",
    )
    logger = logging.getLogger(SERVICE_NAME)

    try:
        logger.info("Starting %s service …", SERVICE_NAME)

        # NOTE: In production this will likely be called by an external
        # scheduler (K8s CronJob, Airflow, etc.). For local development
        # we invoke it directly.
        run_ingestion_cycle()

        logger.info("%s service completed successfully.", SERVICE_NAME)
    except KeyboardInterrupt:  # pragma: no cover
        logger.warning("%s interrupted by user.", SERVICE_NAME)
        sys.exit(130)
    except Exception as exc:  # pragma: no cover
        logger.exception(
            "%s crashed due to an unhandled exception: %s", SERVICE_NAME, exc
        )
        sys.exit(1)


if __name__ == "__main__":  # pragma: no cover
    main()
