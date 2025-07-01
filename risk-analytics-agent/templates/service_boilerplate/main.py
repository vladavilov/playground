"""Standard Service Entrypoint

This file serves as the canonical example of a minimal runnable Python
micro-service within the Risk Analytics News Sentiment platform. Future
services should copy this file into their dedicated directory, rename the
`service_name` variable, and extend the `main` function with the desired
behaviour.
"""

import logging
from typing import NoReturn


def main() -> NoReturn:  # pragma: no cover
    """Bootstraps the service.

    Replace the `pass` statement with the service-specific start-up logic.
    """
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