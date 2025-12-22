CREATE INDEX document_title_index IF NOT EXISTS
FOR (d:`__Document__`)
ON (d.title)
