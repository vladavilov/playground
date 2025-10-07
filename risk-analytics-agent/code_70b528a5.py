# Let's create a simple test to understand what the SHOW CONSTRAINTS query returns
import json

# Simulate the constraint data structure based on the logs
constraints_data = [
    {
        "name": "entity_id_unique_underscored",
        "entityType": "NODE",
        "labelsOrTypes": ["__Entity__"],
        "properties": ["id"],
        "type": "UNIQUENESS"
    },
    {
        "name": "document_id_unique_underscored", 
        "entityType": "NODE",
        "labelsOrTypes": ["__Document__"],
        "properties": ["id"],
        "type": "UNIQUENESS"
    }
]

def has_node_unique(label: str) -> bool:
    for rec in constraints_data:
        if (rec.get("entityType") == "NODE"
            and label in (rec.get("labelsOrTypes") or [])
            and "id" in (rec.get("properties") or [])
            and "UNIQUE" in (rec.get("type") or "")):
            return True
    return False

# Test the function
print("Testing has_node_unique function:")
print(f"has_node_unique('__Entity__'): {has_node_unique('__Entity__')}")
print(f"has_node_unique('__Document__'): {has_node_unique('__Document__')}")

# Let's also check what the actual constraint type is
print("\nConstraint types in data:")
for rec in constraints_data:
    print(f"Name: {rec['name']}, Type: {rec['type']}")