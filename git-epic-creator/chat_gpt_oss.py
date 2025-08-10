#!/usr/bin/env python3
"""
Simple client to chat with the local gpt-oss service (Ollama, OpenAI-compatible).

Defaults:
- Base URL: http://localhost:8010/v1
- API key:  EMPTY
- Model:    gpt-oss:20b

Usage examples:
  python chat_gpt_oss.py -m "Hello!"
  python chat_gpt_oss.py -s "You are helpful" -m "Explain MXFP4 quantization."

Environment overrides:
  OPENAI_BASE_URL, OPENAI_API_KEY, GPT_OSS_MODEL
"""

from __future__ import annotations

import argparse
import os
import sys
from typing import Optional

try:
    from openai import OpenAI
except Exception as exc:  # pragma: no cover
    print("ERROR: Missing dependency 'openai'. Install via: pip install openai", file=sys.stderr)
    raise


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Chat with local gpt-oss service")
    parser.add_argument("-m", "--message", help="User message. If omitted, you'll be prompted interactively.")
    parser.add_argument("-s", "--system", default="You are a helpful assistant.", help="System prompt")
    parser.add_argument("--model", default=os.getenv("GPT_OSS_MODEL", "gpt-oss:20b"), help="Model name")
    parser.add_argument(
        "--base-url",
        default=os.getenv("OPENAI_BASE_URL", "http://localhost:8010/v1"),
        help="OpenAI-compatible base URL",
    )
    parser.add_argument(
        "--api-key",
        default=os.getenv("OPENAI_API_KEY", "EMPTY"),
        help="API key (optional; service may accept EMPTY)",
    )
    return parser.parse_args(argv)


def main(argv: Optional[list[str]] = None) -> int:
    args = parse_args(argv)

    user_message = args.message
    if not user_message:
        try:
            user_message = input("You: ").strip()
        except KeyboardInterrupt:
            return 130

    client = OpenAI(base_url=args.base_url, api_key=args.api_key)

    try:
        response = client.chat.completions.create(
            model=args.model,
            messages=[
                {"role": "system", "content": args.system},
                {"role": "user", "content": user_message},
            ],
        )
    except Exception as exc:  # pragma: no cover
        print(f"ERROR: request failed: {exc}", file=sys.stderr)
        return 1

    try:
        content = response.choices[0].message.content
    except Exception:  # pragma: no cover
        print("ERROR: malformed response", file=sys.stderr)
        print(response, file=sys.stderr)
        return 1

    print(content)
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())




