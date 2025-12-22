"""OpenTelemetry tracing helpers (Task 21).

We keep tracing optional and lightweight: if an exporter isn't configured,
spans will still be created in-process for local debugging.
"""

from __future__ import annotations

from functools import lru_cache

from opentelemetry import trace
from opentelemetry.sdk.resources import Resource
from opentelemetry.sdk.trace import TracerProvider


@lru_cache()
def init_tracing(service_name: str) -> None:
    # Avoid overriding an existing provider configured by the environment.
    provider = trace.get_tracer_provider()
    if isinstance(provider, TracerProvider):
        return
    trace.set_tracer_provider(
        TracerProvider(resource=Resource.create({"service.name": service_name}))
    )


def get_tracer(name: str):
    return trace.get_tracer(name)



