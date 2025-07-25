import os
import io
import requests
from fastapi import FastAPI, Query, HTTPException
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse
from neo4j import GraphDatabase
from PyPDF2 import PdfReader

from neo4j_graphrag.embeddings import OpenAIEmbeddings
from neo4j_graphrag.retrievers import VectorRetriever
from neo4j_graphrag.llm import OpenAILLM
from neo4j_graphrag.generation import GraphRAG
from neo4j_graphrag.experimental.pipeline.kg_builder import SimpleKGPipeline

app = FastAPI()
app.mount("/static", StaticFiles(directory="static"), name="static")

NEO4J_URI = os.getenv("NEO4J_URI", "neo4j://localhost:7687")
NEO4J_USER = os.getenv("NEO4J_USER", "neo4j")
NEO4J_PASS = os.getenv("NEO4J_PASS", "test")
OPENAI_API_KEY = os.getenv("OAI_KEY")
OPENAI_MODEL = os.getenv("OAI_MODEL", "gpt‑4o")

driver = GraphDatabase.driver(NEO4J_URI, auth=(NEO4J_USER, NEO4J_PASS))

embedder = OpenAIEmbeddings(model="text‑embedding‑3‑large", api_key=OPENAI_API_KEY)
INDEX_NAME = "graphrag_index"
retriever = VectorRetriever(driver, INDEX_NAME, embedder)

llm = OpenAILLM(model_name=OPENAI_MODEL, model_params={"temperature": 0}, api_key=OPENAI_API_KEY)

rag = GraphRAG(retriever=retriever, llm=llm)

@app.get("/version")
def neo4j_version():
    info = driver.get_server_info()
    return {"neo4j_version": info.agent}

@app.post("/process")
async def process_pdf(url: str = Query(...)):
    if url.startswith(("http://", "https://")):
        resp = requests.get(url)
        data = resp.content
    else:
        with open(url, "rb") as f:
            data = f.read()
    reader = PdfReader(io.BytesIO(data))
    text = "\n".join(page.extract_text() or "" for page in reader.pages)
    if not text:
        raise HTTPException(status_code=400, detail="No text found in PDF")

    kg_builder = SimpleKGPipeline(
        llm=llm,
        driver=driver,
        embedder=embedder,
        from_pdf=False
    )
    await kg_builder.run_async(text=text)

    return {"message": "PDF processed and knowledge graph built in Neo4j."}

@app.get("/visualize")
def visualize():
    with driver.session() as session:
        res = session.run("MATCH (n)-[r]->(m) RETURN n, r, m LIMIT 100")
        graph = []
        for rec in res:
            graph.append({
                "source": rec["n"].id,
                "relationship": rec["r"].type,
                "target": rec["m"].id,
                "source_label": list(rec["n"].labels),
                "target_label": list(rec["m"].labels),
            })
    return {"graph": graph}

@app.get("/query")
def query(prompt: str = Query(...), top_k: int = Query(5)):
    result = rag.search(query_text=prompt, retriever_config={"top_k": top_k})
    return {"result": result.answer, "context": result.context}

@app.get("/", response_class=FileResponse)
def root():
    return "static/visualize.html"

@app.get("/visualize-ui", response_class=FileResponse)
def visualize_ui():
    return "static/visualize.html"

@app.get("/process-ui", response_class=FileResponse)
def process_ui():
    return "static/process.html"

@app.get("/query-ui", response_class=FileResponse)
def query_ui():
    return "static/query.html"
