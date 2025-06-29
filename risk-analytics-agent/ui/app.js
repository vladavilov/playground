import { initGrid } from './modules/gridController.js';
import { initUIManager } from './modules/uiController.js';

document.addEventListener('DOMContentLoaded', () => {
    const uiManager = initUIManager(handleRowClick);
    
    function handleRowClick(event) {
        uiManager.populateDetailView(event.data);
    }

    initGrid(handleRowClick, uiManager);
}); 