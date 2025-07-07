# UI Refactoring Summary

The UI is composed of the following files located in the `playground/risk-analytics-agent/ui/` directory:

-   `index.html`: The main HTML file containing the UI's structure.
-   `style.css`: The stylesheet for the UI.
-   `app.js`: The main JavaScript entry point that initializes and connects the modules.
-   `modules/`: A directory containing the individual JavaScript modules.
    -   `dataProvider.js`: Responsible for creating and providing mock data.
    -   `gridController.js`: Manages all AG-Grid logic, including setup, data streaming, and user interactions with the grids.
    -   `uiController.js`: Controls the dynamic parts of the UI, such as the detail panel, alerts, and tab navigation.
-   `refactoring_summary.md`: This documentation file.

## File Breakdown

### 1. `index.html`

-   **Purpose**: Defines the complete DOM structure of the web application.
-   **Contents**:
    -   All HTML tags that create the layout, including the main grid view, the detail view, and various panels.
    -   `<link>` tag in the `<head>` to include `style.css`.
    -   `<script>` tags to include external libraries (Tailwind CSS, AG Grid) and the main application logic from `script.js`.
-   **Changes**: All inline `<style>` and `<script>` blocks were removed and replaced with links to external files. The `<script>` tag was updated to load `app.js` with `type="module"`, enabling the new modular JavaScript structure.

### 2. `style.css`

-   **Purpose**: Contains all the custom CSS rules for styling the application.
-   **Contents**:
    -   Styles for the explainability tooltips (`.explain-tooltip`).
    -   Customizations for the AG Grid theme (`.ag-theme-alpine-dark`).
    -   Styles for the toggle switch, tabs, and keyframe animations (`@keyframes`).
-   **Changes**: This is a new file containing all the CSS previously located inside the `<style>` tags of the original HTML file.

### 3. `app.js` (Main Entry Point)

-   **Purpose**: Initializes the application by orchestrating the different modules.
-   **Contents**:
    -   Imports the `initGrid` and `initUIManager` functions from the controller modules.
    -   Sets up a `DOMContentLoaded` listener to start the application.
    -   Initializes the UI and Grid controllers, passing callback functions to connect them, which keeps the modules decoupled.

### 4. `modules/dataProvider.js`

-   **Purpose**: To generate and export mock data for the application.
-   **Exports**:
    -   `createMockData()`: A function that returns an array of mock instrument data.

### 5. `modules/gridController.js`

-   **Purpose**: To encapsulate all logic related to AG-Grid.
-   **Contents**:
    -   Grid column definitions and configuration options.
    -   Logic for filtering risky rows (`isRowRisky`).
    -   The simulated real-time data stream (`startDataStream`) that adds new data to the grids.
    -   Event listener for the "Hide Risky" toggle.
-   **Exports**:
    -   `initGrid(onRowClicked, uiManager)`: Initializes the triage and screened grids. It takes a callback function to handle row clicks and the UI manager instance to interact with UI elements like tabs and alerts.

### 6. `modules/uiController.js`

-   **Purpose**: To manage all DOM manipulation and UI-specific event listeners outside of the grid.
-   **Contents**:
    -   Functions to populate the detailed instrument view (`populateDetailView`, `setupValuationIntelligence`).
    -   Functions to create and manage alerts and notifications (`createAlert`, `createSubmissionAlert`).
    -   Event listeners for the "Back to Grids" button and tab navigation.
-   **Exports**:
    -   `initUIManager(onRowClickCallback)`: Sets up the UI event listeners. It returns an object with methods that the grid controller can use to interact with the UI, such as `populateDetailView`, `createAlert`, and `showTab`. 