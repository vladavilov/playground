from workflow_models.agent_models import PromptAnalysis, RetrievedContext
from models.retrieval import RetrievalPlan
from orchestrator.experts.clients.graphrag_client import GraphRAGClient


class ContextRetriever:
    async def retrieve(self, analysis: PromptAnalysis) -> RetrievedContext:
        plan = RetrievalPlan(query=analysis.prompt, intents=analysis.intents)
        pack = await GraphRAGClient().retrieve(plan)
        return RetrievedContext(citations=pack.citations, notes=None)




