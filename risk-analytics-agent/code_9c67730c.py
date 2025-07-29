from uuid import uuid4, UUID
import json

# Generate a valid UUID
valid_uuid = uuid4()
print(f"Valid UUID: {valid_uuid}")
print(f"Valid UUID type: {type(valid_uuid)}")

# Try to create UUID from the test string
test_string = "non-existent-project-id"
print(f"\nTest string: {test_string}")

try:
    invalid_uuid = UUID(test_string)
    print(f"Successfully created UUID: {invalid_uuid}")
except ValueError as e:
    print(f"UUID validation failed: {e}")

# Try with a valid UUID string format but non-existent project
fake_valid_uuid = "123e4567-e89b-12d3-a456-426614174000"
try:
    fake_uuid = UUID(fake_valid_uuid)
    print(f"\nFake valid UUID: {fake_uuid}")
    print(f"This should pass UUID validation but fail project existence check")
except ValueError as e:
    print(f"Even fake UUID failed: {e}")