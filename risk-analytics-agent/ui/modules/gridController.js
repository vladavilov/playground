import { createMockData } from './dataProvider.js';

let triageGridApi;
let screenedGridApi;
let allRowData = [];
let isRiskyFilterActive = false;

const columnDefs = [
    { headerName: "CUSIP", field: "cusip", width: 120, sortable: true, filter: true },
    { headerName: "Issuer", field: "security_details.issuer_name", sortable: true, filter: true, flex: 2 },
    { headerName: "Rating", field: "security_details.rating", width: 100, sortable: true, filter: true },
    { 
        headerName: "Illiquid", field: "liquidity.is_illiquid_flag", width: 100, sortable: true, filter: true,
        cellClassRules: {
            'bg-yellow-200 text-yellow-800': params => params.value === true,
        },
        cellRenderer: params => params.value ? 'Yes' : 'No'
    },
    { 
        headerName: "1D Bid/Ask %", field: "forecasted_values.1-day.bid_ask_spread_pct", width: 150, sortable: true, filter: 'agNumberColumnFilter',
        valueFormatter: params => parseFloat(params.value).toFixed(4)
    },
    { 
        headerName: "Neg. News Prob %", field: "forecasted_values.1-day.probability_negative_news_pct", width: 180, sortable: true, filter: 'agNumberColumnFilter',
        cellClassRules: {
            'bg-red-200 text-red-800': params => Number(params.value) > 15,
        },
        valueFormatter: params => parseFloat(params.value).toFixed(2)
    },
    { headerName: "RV vs Peers (bps)", field: "relative_value.vs_peers_bps", width: 180, sortable: true, filter: 'agNumberColumnFilter',
        valueFormatter: params => parseFloat(params.value).toFixed(2)
    },
    { 
        headerName: "1D OAS Δ (bps)", field: "forecasted_oas_change_1d_bps", width: 160, sortable: true, filter: 'agNumberColumnFilter',
        cellClassRules: {
            'bg-red-200 text-red-800': params => Number(params.value) > 5,
            'bg-green-200 text-green-800': params => Number(params.value) < -5
        },
        valueFormatter: params => parseFloat(params.value).toFixed(2)
    },
];

function isRowRisky(row) {
    return row.liquidity.is_illiquid_flag === true ||
            Number(row.forecasted_values['1-day'].probability_negative_news_pct) > 15 ||
            row.ownership.is_concentrated_flag === true ||
            Number(row.forecasted_oas_change_1d_bps) > 5;
}

function isHighConviction(row) {
    return Number(row.forecasted_oas_change_1d_bps) < -5 &&
            row.liquidity.is_illiquid_flag === false &&
            Number(row.forecasted_values['1-day'].probability_negative_news_pct) < 5;
}

function checkForOpportunities(data, uiManager) {
    data.forEach(row => {
        if (isHighConviction(row)) {
            uiManager.createAlert(row);
        }
    });
}

function startDataStream(uiManager) {
    let nextLowRiskTime = Date.now() + 60000; // Schedule first low-risk for 1 minute from now

    setInterval(() => {
        const currentTime = Date.now();
        let newData;

        if (currentTime >= nextLowRiskTime) {
            // Time to generate a low-risk / high-conviction bid
            newData = createMockData()[0]; // Start with a random base
            newData.is_new = true;
            newData.liquidity.is_illiquid_flag = false;
            newData.ownership.is_concentrated_flag = false; // Ensure this is not risky
            newData.forecasted_values['1-day'].probability_negative_news_pct = (Math.random() * 4).toFixed(2); // < 5
            
            // Correctly derive the change to ensure data consistency
            const change_bps = -(Math.random() * 10 + 6); // e.g., -12.5
            const current_oas = parseFloat(newData.calculated_risk_metrics.option_adjusted_spread_bps);
            const new_forecast_oas = current_oas + change_bps;

            newData.forecasted_oas_change_1d_bps = change_bps.toFixed(2);
            newData.forecasted_values['1-day'].credit_spread_oas_bps = new_forecast_oas.toFixed(2);

            // Set the next time for a low-risk bid
            nextLowRiskTime = currentTime + 60000; // 1 minute
        } else {
            // Generate a standard, random bid
            newData = createMockData()[0];
            newData.is_new = true;
        }

        allRowData.unshift(newData);
        
        if (!isRowRisky(newData)) {
            // It's NOT risky, so it ALWAYS goes to the main triage grid.
            triageGridApi.applyTransaction({ add: [newData] });
            // And ONLY non-risky items are checked for opportunities.
            checkForOpportunities([newData], uiManager);
        } else {
            // It IS risky. Add it to the correct grid based on the filter state.
            if (isRiskyFilterActive) {
                screenedGridApi.applyTransaction({ add: [newData] });
            } else {
                triageGridApi.applyTransaction({ add: [newData] });
            }
        }

        setTimeout(() => {
            if (!triageGridApi || !screenedGridApi) return;
            
            let rowNode = triageGridApi.getRowNode(newData.cusip);
            let targetApi = triageGridApi;

            if (!rowNode) {
                rowNode = screenedGridApi.getRowNode(newData.cusip);
                targetApi = screenedGridApi;
            }
            
            if (rowNode) {
                const updatedData = { ...rowNode.data };
                delete updatedData.is_new;
                targetApi.applyTransaction({ update: [updatedData] });
            }
        }, 5000);

    }, 30000); // Add a new bid every 30 seconds
}


export function initGrid(onRowClicked, uiManager) {
    function onGridsReady() {
        if (triageGridApi && screenedGridApi) {
            allRowData = createMockData();
            triageGridApi.setGridOption('rowData', allRowData);
            checkForOpportunities(allRowData.slice(0, 5), uiManager); // Check for initial opportunities
            startDataStream(uiManager);
        }
    }

    const gridOptions = {
        columnDefs: columnDefs,
        rowData: null,
        pagination: true,
        paginationPageSize: 50,
        onRowClicked: onRowClicked,
        domLayout: 'autoHeight',
        rowHeight: 28,
        headerHeight: 30,
        theme: 'ag-theme-quartz-dark',
        getRowId: params => params.data.cusip,
        onGridReady: (params) => {
            triageGridApi = params.api;
            onGridsReady();
        },
            rowClassRules: {
            'bg-green-100/10 animate-pulse': params => params.data.is_new === true,
        }
    };

    const screenedGridOptions = {
        columnDefs: columnDefs,
        rowData: [],
        onRowClicked: onRowClicked,
        domLayout: 'autoHeight',
        rowHeight: 28,
        headerHeight: 30,
        theme: 'ag-theme-quartz-dark',
        getRowId: params => params.data.cusip,
        onGridReady: (params) => {
            screenedGridApi = params.api;
            onGridsReady();
        },
    };

    const gridDiv = document.querySelector('#triage-grid');
    agGrid.createGrid(gridDiv, gridOptions);

    const screenedGridDiv = document.querySelector('#screened-grid');
    agGrid.createGrid(screenedGridDiv, screenedGridOptions);

    const hideRiskyToggle = document.querySelector('#hide-risky-toggle');
    hideRiskyToggle.addEventListener('change', (event) => {
        if (!triageGridApi || !screenedGridApi) return;
        isRiskyFilterActive = event.target.checked;

        if (isRiskyFilterActive) {
            document.querySelector('#screened-tab').classList.remove('hidden');
            const rowsToMove = [];
            triageGridApi.forEachNode(node => {
                if (isRowRisky(node.data)) {
                    rowsToMove.push(node.data);
                }
            });
            if(rowsToMove.length > 0) {
                triageGridApi.applyTransaction({ remove: rowsToMove });
                screenedGridApi.applyTransaction({ add: rowsToMove });
            }
        } else {
            document.querySelector('#screened-tab').classList.add('hidden');
            uiManager.showTab('triage-view'); 
            const rowsToMove = [];
            screenedGridApi.forEachNode(node => rowsToMove.push(node.data));
            if (rowsToMove.length > 0) {
                screenedGridApi.applyTransaction({ remove: rowsToMove });
                triageGridApi.applyTransaction({ add: rowsToMove });
            }
        }
    });
} 