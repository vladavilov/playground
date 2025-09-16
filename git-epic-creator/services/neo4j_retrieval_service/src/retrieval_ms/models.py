from typing import Dict, List, TypedDict, Any


class PrimerResult(TypedDict):
    communities: List[int]
    sampled: Dict[int, List[int]]
    primer: Dict[str, Any]



