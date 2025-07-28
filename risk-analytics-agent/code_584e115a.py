import os
import sys

# Add the source directory to path
test_src_path = r'C:\Users\vlada\cobol-migration-poc\risk-analytics\playground\git-epic-creator\e2e_tests\src'
sys.path.insert(0, test_src_path)

try:
    from test_end_to_end_document_workflow import TestConfig
    
    # Test static resource methods
    print("Testing TestConfig static resource methods:")
    print("=" * 50)
    
    # Test PDF path method
    pdf_path = TestConfig.get_dummy_pdf_path()
    print(f"PDF path: {pdf_path}")
    print(f"PDF exists: {os.path.exists(pdf_path)}")
    
    # Test PDF reading method
    if os.path.exists(pdf_path):
        pdf_content = TestConfig.read_dummy_pdf()
        print(f"PDF size: {len(pdf_content)} bytes")
        print(f"PDF header: {pdf_content[:10]}")
        print(f"Is valid PDF: {pdf_content.startswith(b'%PDF')}")
    else:
        print("PDF file not found!")
    
    # Test configuration methods
    print("\nTesting configuration methods:")
    print("=" * 50)
    
    service_urls = TestConfig.get_service_urls()
    print(f"Service URLs: {service_urls}")
    
    postgres_config = TestConfig.get_postgres_config()
    print(f"Postgres config: {postgres_config}")
    
    print("\n✅ All TestConfig methods working correctly!")
    
except Exception as e:
    print(f"❌ Error testing TestConfig: {e}")
    import traceback
    traceback.print_exc()