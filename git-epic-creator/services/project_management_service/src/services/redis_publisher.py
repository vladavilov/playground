from utils.redis_abstractions import BaseRedisPublisher, RedisChannelConfig, RedisMode


class SimpleRedisPublisher(BaseRedisPublisher):
    """Minimal generic Redis publisher with configurable prefix/name/mode."""

    def __init__(self, redis_client, prefix: str, default_name: str, mode: RedisMode):
        super().__init__(redis_client, RedisChannelConfig(prefix, ":"), mode=mode)
        self._default_name = default_name

    def get_default_channel(self) -> str:
        return self.channel_config.get_channel_name(self._default_name)

    def get_default_stream(self) -> str:
        return self.channel_config.get_channel_name(self._default_name)


