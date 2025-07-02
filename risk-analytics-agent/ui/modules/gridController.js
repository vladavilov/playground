import { createMockData } from './dataProvider.js';

let triageGridApi;
let screenedGridApi;
let allRowData = [];
let isRiskyFilterActive = false;

// Helper to safely access nested properties
const get = (p, o) => p.reduce((xs, x) => (xs && xs[x]) ? xs[x] : null, o);

/**
 * Creates a comprehensive risk score adjustment based on multiple factors.
 * @param {object} data The full instrument data object.
 * @returns {number} A numeric score where higher values indicate higher risk.
 */
const calculateRiskScore = (data) => {
    let score = 0;
    // 1. Market Regime
    const regime = get(['market_regime_context', 'contextual_regime', 'label'], data);
    if (regime && (regime.includes('Bear') || regime.includes('Volatile'))) score += 2;
    // 2. Liquidity Score
    const liquidityScore = get(['liquidity', 'composite_score'], data);
    if (liquidityScore && liquidityScore < 40) score += 3;
    // 3. Concentrated Ownership
    if (get(['ownership', 'is_concentrated_flag'], data) === true) score += 4;
    // 4. Idiosyncratic Risk (Negative News)
    const negNewsProb = get(['forecasted_values', '5-day', 'probability_negative_news_pct'], data);
    if (negNewsProb && negNewsProb > 15) score += 3;
    // 5. Downside Volatility Risk
    const downsideVol = get(['forecasted_values', '5-day', 'downside_price_volatility', 'value'], data);
    if (downsideVol && downsideVol > 1.5) score += 2;
    // 6. State Fiscal Health Risk
    const budgetDeficit = get(['state_fiscal_health', 'budget_surplus_deficit_pct_gsp'], data);
    if (budgetDeficit && budgetDeficit < -0.5) score += 1;
    // 7. Rich Valuation
    const vsPeers = get(['relative_value', 'vs_peers_bps'], data);
    if (vsPeers && vsPeers < -10) score += 2; // Richly valued
    return score;
};

const columnDefs = [
    { headerName: "CUSIP", field: "cusip", width: 120, sortable: true, filter: true },
    { headerName: "Rating", field: "security_details.rating", width: 100, sortable: true, filter: true },
    { headerName: "Maturity", field: "security_details.maturity_date", width: 120, sortable: true, filter: true },
    { headerName: "Coupon", field: "security_details.coupon_rate", width: 100, sortable: true, filter: 'agNumberColumnFilter', valueFormatter: params => params.value ? `${parseFloat(params.value).toFixed(3)}%` : '' },
    { 
        headerName: "Next Call Date", 
        field: "security_details.call_features.next_call_date", 
        width: 140, 
        sortable: true, 
        filter: true,
        valueGetter: params => params.data.security_details.call_features.is_callable ? params.data.security_details.call_features.next_call_date : null
    },
    { 
        headerName: "Next Call Price", 
        field: "security_details.call_features.next_call_price", 
        width: 150, 
        sortable: true, 
        filter: 'agNumberColumnFilter',
        valueGetter: params => params.data.security_details.call_features.is_callable ? params.data.security_details.call_features.next_call_price : null
    },
    { headerName: "YTW", field: "calculated_risk_metrics.yield_to_worst", width: 100, sortable: true, filter: 'agNumberColumnFilter', valueFormatter: params => params.value ? `${parseFloat(params.value).toFixed(3)}%` : '' },
    { 
        headerName: "Illiquid", field: "liquidity.is_illiquid_flag", width: 100, sortable: true, filter: true,
        cellClassRules: {
            'bg-yellow-900/50 text-yellow-300': params => params.value === true,
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
            'bg-red-900/50 text-red-300': params => Number(params.value) > 15,
        },
        valueFormatter: params => parseFloat(params.value).toFixed(2)
    },
    { headerName: "RV vs Peers (bps)", field: "relative_value.vs_peers_bps", width: 180, sortable: true, filter: 'agNumberColumnFilter',
        valueFormatter: params => parseFloat(params.value).toFixed(2)
    },
    { 
        headerName: "1D YTW Δ (bps)", field: "forecasted_ytw_change_1d_bps", width: 160, sortable: true, filter: 'agNumberColumnFilter',
        cellClassRules: {
            'bg-red-900/50 text-red-300': params => Number(params.value) > 5,
            'bg-green-900/50 text-green-300': params => Number(params.value) < -5
        },
        valueFormatter: params => parseFloat(params.value).toFixed(2)
    },
    {
        headerName: "Risk Score",
        width: 120,
        sortable: true,
        filter: 'agNumberColumnFilter',
        valueGetter: params => calculateRiskScore(params.data),
        cellClassRules: {
            'bg-red-900/50 text-red-300': params => params.value > 4,
            'bg-yellow-900/50 text-yellow-300': params => params.value > 2 && params.value <= 4,
        }
    }
];

function isRowRisky(row) {
    // A row is risky if its composite risk score is high.
    // We set the threshold at 4. This corresponds to having one major risk factor (e.g. Concentrated Ownership)
    // or a combination of minor ones.
    return calculateRiskScore(row) > 4;
}

function isHighConviction(row) {
    // A high-conviction opportunity has a strong positive forecast AND a low risk score.
    const hasStrongForecast = Number(row.forecasted_ytw_change_1d_bps) < -5;
    const hasLowRisk = calculateRiskScore(row) <= 2;
    return hasStrongForecast && hasLowRisk;
}

function checkForOpportunities(data, uiManager) {
    data.forEach(row => {
        if (isHighConviction(row)) {
            uiManager.createHighConvictionAlert(row);
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

            newData.forecasted_ytw_change_1d_bps = change_bps.toFixed(2);
            newData.forecasted_values['1-day'].credit_spread_ytw_bps = new_forecast_oas.toFixed(2);

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