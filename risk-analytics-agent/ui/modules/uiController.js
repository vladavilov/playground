function createSubmissionAlert(message) {
    const alertContainer = document.getElementById('alert-container');
    const alertId = `alert-submit-${Date.now()}`;

    const alertEl = document.createElement('div');
    alertEl.id = alertId;
    alertEl.className = 'bg-slate-700 border border-slate-600 rounded-md shadow-lg p-3 animate-fade-in-right flex items-center space-x-3';
    alertEl.innerHTML = `
        <div class="flex-shrink-0">
            <svg class="h-5 w-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                <path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clip-rule="evenodd" />
            </svg>
        </div>
        <div class="flex-1">
            <p class="text-sm font-medium text-slate-100">${message}</p>
        </div>
    `;

    alertContainer.appendChild(alertEl);

    setTimeout(() => {
        alertEl.classList.add('animate-fade-out-right');
        setTimeout(() => alertEl.remove(), 300);
    }, 3000); // Remove after 3 seconds
}

function handleAction(type, size, price) {
    createSubmissionAlert(`Your ${type} for ${size} @ ${price} has been submitted.`);
    const detailView = document.querySelector('#detail-view');
    const mainView = document.querySelector('#main-view');
    detailView.classList.add('hidden');
    mainView.classList.remove('hidden');
}

function createForecastLine(elementId, label, value, unit, attributions, parentDivId, tooltipAlign) {
    let container = document.getElementById(elementId);
    // If the element doesn't exist, create it and append it to a parent
    if (!container && parentDivId) {
        const parent = document.querySelector(`#${parentDivId}`);
        if (parent) {
            container = document.createElement('div');
            container.id = elementId;
            container.className = 'explain-container relative text-xs';
            parent.appendChild(container);
        }
    }
    if (!container) return; // Exit if container still not found

    let infoIcon = '';
    if (attributions && attributions.length > 0) {
        const tooltipText = attributions.map(a => `<div>${a.feature}: <span class='font-mono'>${a.attribution}</span></div>`).join('');
        const alignClass = tooltipAlign === 'right' ? 'tooltip-right' : '';
        infoIcon = `
            <span class="has-tooltip relative ml-2 text-slate-500 cursor-pointer">
                <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3 inline" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                <div class="explain-tooltip font-sans ${alignClass}">${tooltipText}</div>
            </span>`;
    }

    container.innerHTML = `
        <div class="flex justify-between">
            <span>${label}:</span>
            <div>
                <span class="font-semibold font-mono">${value} ${unit}</span>
                ${infoIcon}
            </div>
        </div>`;
}

function populateMarketRegime(elementId, label, value, drivers) {
        const container = document.getElementById(elementId);
        if (!container) return;

    let infoIcon = '';
    if (drivers && drivers.length > 0) {
        const tooltipText = drivers.map(d => `<div>${d.feature}: <span class='font-mono'>${d.attribution}</span></div>`).join('');
        infoIcon = `
            <span class="has-tooltip relative ml-1 text-slate-500 cursor-pointer">
                <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3 inline" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                <div class="explain-tooltip tooltip-right font-sans">${tooltipText}</div>
            </span>`;
    }
    
    const formattedValue = value.replace(/_/g, ' ');

    container.innerHTML = `
        <div class="text-xs text-slate-400">${label}</div>
        <div class="text-base font-semibold text-slate-200 flex items-center justify-end">
            <span>${formattedValue}</span>
            ${infoIcon}
        </div>
    `;
}

function setupValuationIntelligence(data) {
    const sliderContainer = document.getElementById('weight-sliders');
    const suggestedPriceInput = document.getElementById('suggested-price-input');
    const explanationEl = document.getElementById('suggested-price-explanation');
    const scenarioContainer = document.getElementById('scenario-buttons');
    const valuationActionContainer = document.getElementById('valuation-action-buttons');

    sliderContainer.innerHTML = ''; // Clear old sliders
    valuationActionContainer.innerHTML = ''; // Clear old buttons
    // Clear all but the first child (the label)
    while (scenarioContainer.children.length > 1) {
        scenarioContainer.removeChild(scenarioContainer.lastChild);
    }


    const fv = data.forecasted_values;
    const crm = data.calculated_risk_metrics;
    const sd = data.security_details;
    const marketRegime = data.market_context?.regime;
    const negNewsProb = parseFloat(fv['1-day'].probability_negative_news_pct);

    const SCENARIO_WEIGHTS = {
        'Base Case': { current: 40, d1: 30, d5: 20, d20: 10 },
        'Defensive': { current: 80, d1: 10, d5: 5, d20: 5 },
        'Aggressive Alpha': { current: 10, d1: 50, d5: 30, d20: 10 },
        'Strategic Value': { current: 10, d1: 10, d5: 30, d20: 50 }
    };
    let activeScenario = 'Base Case';

    const oas = {
        current: parseFloat(crm.option_adjusted_spread_bps),
        d1: parseFloat(fv['1-day'].credit_spread_oas_bps),
        d5: parseFloat(fv['5-day'].credit_spread_oas_bps),
        d20: parseFloat(fv['20-day'].credit_spread_oas_bps),
    };

    const weights = { current: 50, d1: 25, d5: 15, d20: 10 }; 
    const sliderElements = {};
    
    const horizons = [
        { key: 'current', label: 'Current OAS' },
        { key: 'd1', label: '1-Day Forecast' },
        { key: 'd5', label: '5-Day Forecast' },
        { key: 'd20', label: '20-Day Forecast' },
    ];

    horizons.forEach(h => {
        const sliderWrapper = document.createElement('div');
        sliderWrapper.className = 'flex items-center space-x-2';
        sliderWrapper.innerHTML = `
            <label for="${h.key}-weight" class="text-xs w-28">${h.label}:</label>
            <input type="range" id="${h.key}-weight" min="0" max="100" value="${weights[h.key]}" class="w-full h-1 bg-slate-700 rounded-lg appearance-none cursor-pointer">
            <span id="${h.key}-value" class="text-xs w-8 text-right font-mono">${weights[h.key]}%</span>
        `;
        sliderContainer.appendChild(sliderWrapper);

        const input = sliderWrapper.querySelector('input');
        const valueSpan = sliderWrapper.querySelector(`#${h.key}-value`);
        sliderElements[h.key] = { input, valueSpan };

        input.addEventListener('input', (e) => {
            weights[h.key] = parseInt(e.target.value, 10);
            valueSpan.textContent = `${weights[h.key]}%`;
            // When user manually slides, deactivate scenario buttons
            activeScenario = null; 
            updateScenarioButtons();
            updateSuggestedPrice();
        });
    });
    
    const bwd = data.bid_wanted_details;
    let bidBtn, offerBtn;

    function updateButtonText() {
        if (bwd) {
            const formattedSize = bwd.size >= 1000000 ? `${bwd.size / 1000000}mm` : `${bwd.size / 1000}k`;
            const currentPrice = suggestedPriceInput.value;
            if (bidBtn) {
                bidBtn.textContent = `Bid ${formattedSize} @ ${currentPrice}`;
            }
            if (offerBtn) {
                const offerPrice = (parseFloat(currentPrice) * 1.0025).toFixed(3);
                offerBtn.textContent = `Offer ${formattedSize} @ ${offerPrice}`;
            }
        }
    }

    if (bwd) {
        const formattedSize = bwd.size >= 1000000 ? `${bwd.size/1000000}mm` : `${bwd.size/1000}k`;
        const primaryBtnClass = 'bg-blue-600 hover:bg-blue-700 text-white font-semibold py-1.5 px-3 rounded text-sm transition-all duration-200';
        const secondaryBtnClass = 'bg-slate-700 hover:bg-slate-600 text-slate-300 font-semibold py-1.5 px-3 rounded text-sm transition-all duration-200';

        bidBtn = document.createElement('button');
        bidBtn.className = bwd.side === 'BWIC' ? primaryBtnClass : secondaryBtnClass;
        bidBtn.addEventListener('click', () => handleAction('bid', formattedSize, suggestedPriceInput.value));

        offerBtn = document.createElement('button');
        offerBtn.className = bwd.side === 'OWIC' ? primaryBtnClass : secondaryBtnClass;
        offerBtn.addEventListener('click', () => {
                const offerPrice = (parseFloat(suggestedPriceInput.value) * 1.0025).toFixed(3);
                handleAction('offer', formattedSize, offerPrice);
        });
        
        valuationActionContainer.appendChild(bidBtn);
        valuationActionContainer.appendChild(offerBtn);
    }

    suggestedPriceInput.addEventListener('input', () => {
        activeScenario = null;
        updateScenarioButtons();
        updateButtonText();
    });
    
    function applyScenario(scenarioName) {
        activeScenario = scenarioName;
        const scenarioWeights = SCENARIO_WEIGHTS[scenarioName];
        
        for(const key in scenarioWeights) {
            weights[key] = scenarioWeights[key];
            sliderElements[key].input.value = weights[key];
            sliderElements[key].valueSpan.textContent = `${weights[key]}%`;
        }
        updateScenarioButtons();
        updateSuggestedPrice();
    }

    function updateScenarioButtons() {
            scenarioContainer.querySelectorAll('button').forEach(btn => {
            if (btn.textContent === activeScenario) {
                btn.className = 'text-xs bg-blue-600 text-white font-semibold py-1 px-2 rounded';
            } else {
                btn.className = 'text-xs bg-slate-700 hover:bg-slate-600 text-slate-300 font-semibold py-1 px-2 rounded';
            }
        });
    }
    
    Object.keys(SCENARIO_WEIGHTS).forEach(name => {
        const btn = document.createElement('button');
        btn.textContent = name;
        scenarioContainer.appendChild(btn);
        btn.addEventListener('click', () => applyScenario(name));
    });


    function updateSuggestedPrice() {
        let totalWeight = 0;
        let weightedOASSum = 0;

        for (const key in weights) {
            totalWeight += weights[key];
            weightedOASSum += oas[key] * weights[key];
        }

        if (totalWeight === 0) {
            suggestedPriceInput.value = '-';
            explanationEl.textContent = 'Adjust weights to see a suggestion.';
            return;
        }
        
        const oas_agg = weightedOASSum / totalWeight;
        const oas_change_bps = oas_agg - oas.current;
        const p0 = parseFloat(sd.price);
        const mod_dur = parseFloat(crm.modified_duration);
        const price_change = -mod_dur * (oas_change_bps / 100) * (p0 / 100);
        const p_sugg = p0 + price_change;
        
        suggestedPriceInput.value = p_sugg.toFixed(3);
        
        let explanation = activeScenario ? `[${activeScenario}] ` : '[Custom] ';
        explanation += `Based on an aggregated OAS of <b>${oas_agg.toFixed(2)}bps</b>`;
        if (Math.abs(oas_change_bps) > 0.1) {
            const direction = oas_change_bps < 0 ? 'tightening' : 'widening';
            explanation += `, implying a spread ${direction} of <b>${Math.abs(oas_change_bps).toFixed(2)}bps</b>.`;
        } else {
            explanation += `, effectively neutral to current spread.`;
        }
        explanationEl.innerHTML = explanation;
        updateButtonText();
    }
    
    const riskOffRegimes = ['Bear_Flattener', 'Bear_Steepener', 'Recession_Easing', 'Idiosyncratic_Distress'];

    // Determine initial scenario
    if (negNewsProb > 15 || riskOffRegimes.includes(marketRegime)) {
        applyScenario('Defensive');
    } else {
        applyScenario('Base Case');
    }
}

export function createAlert(row, onRowClickCallback) {
    const alertContainer = document.getElementById('alert-container');
    const alertId = `alert-${row.cusip}`;

    // Avoid duplicate alerts
    if (document.getElementById(alertId)) return;

    const drivers = row.forecast_explainability?.forecasted_oas_change_1d_bps?.feature_attributions
        ?.filter(a => a.attribution < 0) // Show positive drivers (negative attribution means tightening)
        .slice(0, 3)
        .map(d => `<li class="text-xs">${d.feature}: <span class="font-semibold font-mono">${d.attribution}</span></li>`)
        .join('') || '<li>No drivers available</li>';

    const alertEl = document.createElement('div');
    alertEl.id = alertId;
    alertEl.className = 'bg-slate-800 border border-slate-700 rounded-md shadow-lg p-3 animate-fade-in-right';
    alertEl.innerHTML = `
        <div class="flex items-start">
            <div class="flex-shrink-0">
                <svg class="h-5 w-5 text-green-500" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z" />
                </svg>
            </div>
            <div class="ml-3 w-0 flex-1">
                <p class="text-sm font-medium text-slate-100">
                    High-Conviction Opportunity
                </p>
                <p class="mt-1 text-sm text-slate-300">
                    <span class="font-bold font-mono">${row.cusip}</span> - ${row.security_details.issuer_name}
                </p>
                    <p class="mt-1 text-sm text-green-400">
                    Forecasted Tightening: <span class="font-bold font-mono">${row.forecasted_oas_change_1d_bps} bps</span>
                </p>
                <p class="mt-2 text-xs text-slate-400">Positive Drivers:</p>
                <ul class="list-disc list-inside ml-2">${drivers}</ul>
                <div class="mt-3 flex space-x-3">
                    <button class="view-details-btn bg-blue-600 text-white px-2 py-1 text-xs rounded hover:bg-blue-700">View Details</button>
                    <button class="dismiss-btn text-slate-400 hover:text-slate-200 text-xs">Dismiss</button>
                </div>
            </div>
                <div class="ml-4 flex-shrink-0 flex">
                <button class="close-btn inline-flex text-gray-500 rounded-md hover:text-gray-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-slate-800 focus:ring-indigo-500">
                    <span class="sr-only">Close</span>
                    <svg class="h-4 w-4" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true"><path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd" /></svg>
                </button>
            </div>
        </div>
    `;

    const close = () => {
        alertEl.classList.add('animate-fade-out-right');
        setTimeout(() => alertEl.remove(), 300);
    };

    alertEl.querySelector('.close-btn').addEventListener('click', close);
    alertEl.querySelector('.dismiss-btn').addEventListener('click', close);
    alertEl.querySelector('.view-details-btn').addEventListener('click', () => {
        onRowClickCallback({ data: row });
        close();
    });

    alertContainer.appendChild(alertEl);
}

function _populateDetailView(data) {
    // Helper to safely access nested properties
    const get = (p, o) => p.reduce((xs, x) => (xs && xs[x]) ? xs[x] : null, o);
    
    // Header
    document.getElementById('detail-issuer-name').textContent = get(['security_details', 'issuer_name'], data);
    document.getElementById('detail-cusip').textContent = `CUSIP: ${get(['cusip'], data)}`;
    document.getElementById('detail-rating').textContent = get(['security_details', 'rating'], data);

    // Action Buttons
    const actionContainer = document.getElementById('action-buttons');
    actionContainer.innerHTML = ''; // This section is now empty, buttons moved to valuation.

    // Valuation Intelligence
    setupValuationIntelligence(data);

    // Value Assessment
    document.getElementById('detail-current-oas').textContent = `${get(['calculated_risk_metrics', 'option_adjusted_spread_bps'], data)} bps`;
    document.getElementById('detail-vs-peers').textContent = `${get(['relative_value', 'vs_peers_bps'], data)} bps`;
    document.getElementById('detail-vs-mmd').textContent = `${get(['relative_value', 'vs_mmd_bps'], data)} bps`;
    document.getElementById('detail-vs-ust').textContent = `${get(['relative_value', 'vs_ust_bps'], data)} bps`;
    
    // Liquidity Assessment
    document.getElementById('detail-liquidity-score').textContent = get(['liquidity', 'composite_score'], data);
    const isConcentrated = get(['ownership', 'is_concentrated_flag'], data);
    const isConcentratedEl = document.getElementById('detail-is-concentrated');
    isConcentratedEl.textContent = isConcentrated ? 'Yes' : 'No';
    isConcentratedEl.className = `font-semibold ${isConcentrated ? 'text-red-600' : 'text-green-600'}`;


    document.getElementById('detail-trade-1d').textContent = `${(get(['trade_history_summary', 't1d', 'total_par_volume'], data) / 1e6).toFixed(1)}M / ${get(['trade_history_summary', 't1d', 'unique_dealer_count'], data)}`;
    document.getElementById('detail-trade-5d').textContent = `${(get(['trade_history_summary', 't5d', 'total_par_volume'], data) / 1e6).toFixed(1)}M / ${get(['trade_history_summary', 't5d', 'unique_dealer_count'], data)}`;

    // Market Context
    const mc = data.market_context;
    if(mc) {
        populateMarketRegime('detail-market-regime-container', 'Market Regime', mc.regime, mc.regime_drivers);
    }

    // Forecasts with Explainability
    const fv = data.forecasted_values;
    const fe = data.forecast_explainability;
    const ns = data.news_sentiment;
    
    createForecastLine('detail-credit-spread-1d', '1D Credit Spread (OAS)', get(['1-day', 'credit_spread_oas_bps'], fv), 'bps', get(['1-day.credit_spread_oas_bps', 'feature_attributions'], fe));
    createForecastLine('detail-credit-spread-5d', '5D Credit Spread (OAS)', get(['5-day', 'credit_spread_oas_bps'], fv), 'bps', get(['5-day.credit_spread_oas_bps', 'feature_attributions'], fe));
    createForecastLine('detail-credit-spread-20d', '20D Credit Spread (OAS)', get(['20-day', 'credit_spread_oas_bps'], fv), 'bps', get(['20-day.credit_spread_oas_bps', 'feature_attributions'], fe), 'value-assessment-panel');

    createForecastLine('detail-bid-ask-5d', '5D Bid/Ask Spread', get(['5-day', 'bid_ask_spread_pct'], fv), '%', get(['5-day.bid_ask_spread_pct', 'feature_attributions'], fe));
    
    const newsArticles = ns.top_articles.map(a => ({ feature: a.source, attribution: a.headline }));
    createForecastLine('detail-news-sentiment', 'News Sentiment', ns.score, '', newsArticles, null, 'right');
    createForecastLine('detail-neg-news-5d', '5D Neg. News Prob', get(['5-day', 'probability_negative_news_pct'], fv), '%', get(['5-day.probability_negative_news_pct', 'feature_attributions'], fe), null, 'right');
    createForecastLine('detail-downside-1d', '1D Downside Vol', get(['1-day', 'downside_price_volatility', 'value'], fv), '%', get(['1-day.downside_price_volatility.value', 'feature_attributions'], fe), null, 'right');
    createForecastLine('detail-downside-5d', '5D Downside Vol', get(['5-day', 'downside_price_volatility', 'value'], fv), '%', get(['1-day.downside_price_volatility.value', 'feature_attributions'], fe), null, 'right');
}

function showTab(tabId) {
    const tabPanes = document.querySelectorAll('.tab-pane');
    tabPanes.forEach(pane => {
        if (pane.id === tabId) {
            pane.classList.remove('hidden');
        } else {
            pane.classList.add('hidden');
        }
    });

    document.querySelectorAll('.tab-btn').forEach(btn => {
        if (btn.id === `${tabId.split('-')[0]}-tab`) {
            btn.classList.add('active');
        } else {
            btn.classList.remove('active');
        }
    });
}

export function initUIManager(onRowClickCallback) {
    const backButton = document.querySelector('#back-to-triage');
    const mainView = document.querySelector('#main-view');
    const detailView = document.querySelector('#detail-view');
    
    backButton.addEventListener('click', () => {
        detailView.classList.add('hidden');
        mainView.classList.remove('hidden');
    });

    const triageTab = document.querySelector('#triage-tab');
    const screenedTab = document.querySelector('#screened-tab');

    triageTab.addEventListener('click', () => showTab('triage-view'));
    screenedTab.addEventListener('click', () => showTab('screened-view'));

    return {
        populateDetailView: (data) => {
            _populateDetailView(data);
            detailView.classList.remove('hidden');
            mainView.classList.add('hidden');
        },
        createAlert: (row) => createAlert(row, onRowClickCallback),
        showTab,
    };
} 