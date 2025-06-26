import blpapi

# MUNIFLOW Index
SECURITY = 'MUNIFLOW Index'
FIELDS = ['FUND_FLOW_NET']

def main():
    """
    Main function to connect to Bloomberg, retrieve data, and print it.
    """
    # Fill SessionOptions
    session_options = blpapi.SessionOptions()
    session_options.setServerHost("localhost")
    session_options.setServerPort(8194)

    print("Connecting to Bloomberg...")
    # Create a Session
    session = blpapi.Session(session_options)

    # Start a Session
    if not session.start():
        print("Failed to start session.")
        return

    # Open the reference data service
    if not session.openService("//blp/refdata"):
        print("Failed to open //blp/refdata service.")
        session.stop()
        return

    # Obtain previously opened service
    ref_data_service = session.getService("//blp/refdata")

    # Create and fill the request
    request = ref_data_service.createRequest("HistoricalDataRequest")
    request.append("securities", SECURITY)

    for field in FIELDS:
        request.append("fields", field)

    request.set("startDate", "20231001")
    request.set("endDate", "20231012")
    request.set("periodicitySelection", "WEEKLY")

    print(f"Sending Request: {request}")
    # Send the request
    session.sendRequest(request)

    # Process received events
    try:
        while True:
            # We provide a timeout to give the chance for Ctrl+C handling
            event = session.nextEvent(500)
            for msg in event:
                if msg.correlationIds()[0].value() == request.correlationId().value():
                    print("\n--- MOVE Index Data ---")
                    security_data = msg.getElement("securityData").getValueAsElement(0)
                    security_name = security_data.getElementAsString("security")
                    field_data = security_data.getElement("fieldData")
                    
                    print(f"Ticker: {security_name}")
                    for field_str in FIELDS:
                        if field_data.hasElement(field_str):
                            print(f"{field_str}: {field_data.getElement(field_str).getValueAsString()}")
                        else:
                            print(f"{field_str}: Not Available")
                    print("-----------------------")

            # Response completely received, so we could exit
            if event.eventType() == blpapi.Event.RESPONSE:
                break
    finally:
        # Stop the session
        session.stop()
        print("Session stopped.")

if __name__ == "__main__":
    main()