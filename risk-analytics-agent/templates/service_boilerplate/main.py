import logging
from typing import NoReturn


def main() -> NoReturn:  # pragma: no cover
    service_name = "<YOUR_SERVICE_NAME>"

    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s — %(name)s — %(levelname)s — %(message)s",
    )
    logger = logging.getLogger(service_name)

    logger.info("Starting %s …", service_name)

    # TODO: Implement service-specific logic here.

    logger.info("%s has finished execution.", service_name)


if __name__ == "__main__":  # pragma: no cover
    main() 