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

function populateMarketRegime(elementId, label, regimeObject) {
    const container = document.getElementById(elementId);
    if (!container || !regimeObject) {
        if(container) container.innerHTML = '';
        return;
    }

    const { label: regimeLabel, evidence: regimeEvidence } = regimeObject;

    let infoIcon = '';
    if (regimeEvidence && regimeEvidence.length > 0) {
        const tooltipText = regimeEvidence.map(d => `<div>${d.name}: <span class='font-mono'>${d.value}</span></div>`).join('');
        infoIcon = `
            <span class="has-tooltip relative ml-1 text-slate-500 cursor-pointer">
                <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3 inline" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                <div class="explain-tooltip tooltip-right font-sans">${tooltipText}</div>
            </span>`;
    }
    
    const formattedValue = regimeLabel.replace(/_/g, ' ');

    container.innerHTML = `
        <div class="text-xs text-slate-400">${label}</div>
        <div class="text-base font-semibold text-slate-200 flex items-center justify-end">
            <span>${formattedValue}</span>
            ${infoIcon}
        </div>
    `;
}

function setupValuationIntelligence(data) {
    // --- Get DOM Elements ---
    const systemSpreadEl = document.getElementById('system-forecast-spread');
    const systemWeightSlider = document.getElementById('system-forecast-weight');
    const systemWeightDisplay = document.getElementById('system-weight-display');
    const systemExplainabilityEl = document.getElementById('system-forecast-explainability');
    
    const traderSpreadInput = document.getElementById('trader-input-spread');
    const traderWeightSlider = document.getElementById('trader-input-weight');
    const traderWeightDisplay = document.getElementById('trader-weight-display');

    const suggestedPriceInput = document.getElementById('suggested-price-input');
    const suggestedYieldInput = document.getElementById('suggested-yield-input');
    const explanationEl = document.getElementById('suggested-price-explanation');
    const valuationActionContainer = document.getElementById('valuation-action-buttons');
    const horizonSelectorEl = document.getElementById('horizon-selector');
    const baseYieldDisplayEl = document.getElementById('base-yield-display');

    // --- Get Data & Baseline Values ---
    const { 
        calculated_risk_metrics: crm, 
        security_details: sd,
        bid_wanted_details: bwd,
        forecasted_values: fv,
        market_context,
        forecast_explainability,
        relative_value,
        forecasted_ytw_change_1d_bps,
        forecasted_ytw_change_5d_bps
    } = data;

    const baselinePrice = parseFloat(sd.price);
    const baselineYield = parseFloat(crm.yield_to_worst); // YTW in percent
    const currentSpreadToMmd = parseFloat(relative_value.vs_mmd_bps);
    const modDuration = parseFloat(crm.modified_duration);
    
    // The MMD benchmark yield is the anchor. It's the bond's total yield minus its specific spread to the MMD curve.
    const benchmarkMmdYield = baselineYield - (currentSpreadToMmd / 100);

    // Calculate the forecasted YTWs first...
    const forecastYtw1d = baselineYield + (forecasted_ytw_change_1d_bps / 100);
    const forecastYtw5d = baselineYield + (forecasted_ytw_change_5d_bps / 100);
    
    // ...then calculate the forecasted spread to the MMD benchmark.
    const forecastSpreadToMmd1d = (forecastYtw1d - benchmarkMmdYield) * 100;
    const forecastSpreadToMmd5d = (forecastYtw5d - benchmarkMmdYield) * 100;

    const HORIZONS = {
        'Current': {
            spread: currentSpreadToMmd,
            explainability: []
        },
        '1D': {
            spread: forecastSpreadToMmd1d,
            explainability: forecast_explainability?.['forecasted_ytw_change_1d_bps']?.feature_attributions || []
        },
        '5D': {
            spread: forecastSpreadToMmd5d,
            explainability: forecast_explainability?.['forecasted_ytw_change_5d_bps']?.feature_attributions || []
        }
    };
    
    let isUpdating = false;

    // --- State ---
    const state = {
        systemWeight: 70,
        traderWeight: 30,
        traderSpread: HORIZONS['1D'].spread, // Default trader spread to 1D forecast
        activeHorizon: '1D'
    };

    // --- Smarter Defaults ---
    const riskOffRegimes = ['Bear_Flattener', 'Bear_Steepener', 'Recession_Easing', 'Idiosyncratic_Distress'];
    const negNewsProb = parseFloat(fv['1-day'].probability_negative_news_pct);
    if (negNewsProb > 15 || riskOffRegimes.includes(market_context?.regime)) {
        // Defensive posture: give more weight to trader input
        state.systemWeight = 40;
        state.traderWeight = 60;
    }

    // --- Helper Functions ---
    const updatePriceFromYield = (newYield) => {
        if (isNaN(newYield)) return baselinePrice;
        const yieldChange = (newYield - baselineYield) / 100; // Yields are in %, convert difference to decimal
        const priceChange = -yieldChange * modDuration * baselinePrice;
        return baselinePrice + priceChange;
    };

    const updateYieldFromPrice = (newPrice) => {
        if (isNaN(newPrice)) return baselineYield;
        const priceChangePct = (newPrice - baselinePrice) / baselinePrice;
        const yieldChange = -priceChangePct / modDuration;
        return baselineYield + (yieldChange * 100); // convert to %
    };

    const updateButtonText = (price) => {
        if (!bwd) return;
        const formattedSize = bwd.size >= 1000000 ? `${bwd.size / 1000000}mm` : `${bwd.size / 1000}k`;
        const primaryBtn = valuationActionContainer.querySelector('.btn-primary');
        const secondaryBtn = valuationActionContainer.querySelector('.btn-secondary');
        if (primaryBtn && primaryBtn.textContent.includes('Bid')) {
            primaryBtn.textContent = `Bid ${formattedSize} @ ${price}`;
            if(secondaryBtn) { const offerPrice = (parseFloat(price) * 1.0025).toFixed(3); secondaryBtn.textContent = `Offer ${formattedSize} @ ${offerPrice}`; }
        } else if (primaryBtn && primaryBtn.textContent.includes('Offer')) {
            primaryBtn.textContent = `Offer ${formattedSize} @ ${price}`;
            if(secondaryBtn) { secondaryBtn.textContent = `Bid ${formattedSize} @ ${price}`; }
        }
    };
    
    const updateExplainability = () => {
        const explainData = HORIZONS[state.activeHorizon].explainability
            .filter(a => Math.abs(parseFloat(a.attribution)) > 0.1) // Filter for non-trivial drivers
            .slice(0, 3);

        if (explainData.length === 0) {
            systemExplainabilityEl.innerHTML = '';
            return;
        }

        const tooltipText = explainData.map(a => `<div>${a.feature}: <span class='font-mono'>${parseFloat(a.attribution).toFixed(2)}</span></div>`).join('');
        systemExplainabilityEl.innerHTML = `
            <span class="has-tooltip relative ml-1 text-slate-500 cursor-pointer">
                <svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                <div class="explain-tooltip tooltip-right font-sans text-xs">${tooltipText}</div>
            </span>`;
    };

    const updateValuation = () => {
        isUpdating = true;
        
        const systemSpread = HORIZONS[state.activeHorizon].spread;

        const totalWeight = state.systemWeight + state.traderWeight;
        const blendedSpread = totalWeight > 0 ?
            (systemSpread * state.systemWeight + state.traderSpread * state.traderWeight) / totalWeight :
            currentSpreadToMmd;
        
        const finalYield = benchmarkMmdYield + (blendedSpread / 100);
        const suggestedPrice = updatePriceFromYield(finalYield);

        // Update UI
        suggestedYieldInput.value = finalYield.toFixed(3);
        suggestedPriceInput.value = suggestedPrice.toFixed(3);
        systemWeightDisplay.textContent = `${state.systemWeight}%`;
        traderWeightDisplay.textContent = `${state.traderWeight}%`;
        systemSpreadEl.textContent = systemSpread.toFixed(2);
        
        explanationEl.innerHTML = `Blended Spread-to-MMD of <b>${blendedSpread.toFixed(2)}bps</b> applied to base yield.`;
        
        updateButtonText(suggestedPrice.toFixed(3));
        isUpdating = false;
    };

    // --- Event Listeners ---
    horizonSelectorEl.addEventListener('click', (e) => {
        if (e.target.tagName !== 'BUTTON') return;
        state.activeHorizon = e.target.dataset.horizon;
        
        horizonSelectorEl.querySelectorAll('button').forEach(btn => btn.classList.remove('active'));
        e.target.classList.add('active');
        
        updateExplainability();
        updateValuation();
    });

    systemWeightSlider.addEventListener('input', (e) => {
        if(isUpdating) return;
        state.systemWeight = parseInt(e.target.value, 10);
        state.traderWeight = 100 - state.systemWeight;
        traderWeightSlider.value = state.traderWeight;
        updateValuation();
    });

    traderWeightSlider.addEventListener('input', (e) => {
        if(isUpdating) return;
        state.traderWeight = parseInt(e.target.value, 10);
        state.systemWeight = 100 - state.traderWeight;
        systemWeightSlider.value = state.systemWeight;
        updateValuation();
    });

    traderSpreadInput.addEventListener('input', (e) => {
        if(isUpdating) return;
        const newSpread = parseFloat(e.target.value);
        if (!isNaN(newSpread)) {
            state.traderSpread = newSpread;
            updateValuation();
        }
    });

    suggestedPriceInput.addEventListener('input', () => {
        if (isUpdating) return;
        const newPrice = parseFloat(suggestedPriceInput.value);
        if (isNaN(newPrice)) return;

        const newYield = updateYieldFromPrice(newPrice);
        const blendedSpreadBps = (newYield - benchmarkMmdYield) * 100;
        
        const totalWeight = state.systemWeight + state.traderWeight;
        const systemSpread = HORIZONS[state.activeHorizon].spread;

        if (totalWeight > 0 && state.traderWeight > 0) {
             const requiredTraderSpread = (blendedSpreadBps * totalWeight - systemSpread * state.systemWeight) / state.traderWeight;
             state.traderSpread = requiredTraderSpread;
             traderSpreadInput.value = requiredTraderSpread.toFixed(2);
        }
        
        isUpdating = true;
        suggestedYieldInput.value = newYield.toFixed(3);
        updateButtonText(newPrice.toFixed(3));
        isUpdating = false;
    });

    // --- Initialization ---
    valuationActionContainer.innerHTML = '';
    horizonSelectorEl.innerHTML = '';

    // Create Horizon Buttons
    Object.keys(HORIZONS).forEach(h => {
        const btn = document.createElement('button');
        btn.dataset.horizon = h;
        btn.textContent = h;
        btn.className = 'horizon-btn';
        if (h === state.activeHorizon) btn.classList.add('active');
        horizonSelectorEl.appendChild(btn);
    });

    if (bwd) {
        const formattedSize = bwd.size >= 1000000 ? `${bwd.size/1000000}mm` : `${bwd.size/1000}k`;
        const primaryBtnClass = 'btn-primary bg-blue-600 hover:bg-blue-700 text-white font-semibold py-1.5 px-3 rounded text-sm transition-all duration-200';
        const secondaryBtnClass = 'btn-secondary bg-slate-700 hover:bg-slate-600 text-slate-300 font-semibold py-1.5 px-3 rounded text-sm transition-all duration-200';
        const bidBtn = document.createElement('button');
        bidBtn.addEventListener('click', () => handleAction('bid', formattedSize, suggestedPriceInput.value));
        const offerBtn = document.createElement('button');
        offerBtn.addEventListener('click', () => { const offerPrice = (parseFloat(suggestedPriceInput.value) * 1.0025).toFixed(3); handleAction('offer', formattedSize, offerPrice); });
        if (bwd.side === 'BWIC') {
            bidBtn.className = primaryBtnClass; bidBtn.textContent = `Bid ${formattedSize}`;
            offerBtn.className = secondaryBtnClass; offerBtn.textContent = `Offer ${formattedSize}`;
        } else {
            bidBtn.className = secondaryBtnClass; bidBtn.textContent = `Bid ${formattedSize}`;
            offerBtn.className = primaryBtnClass; offerBtn.textContent = `Offer ${formattedSize}`;
        }
        valuationActionContainer.appendChild(bidBtn);
        valuationActionContainer.appendChild(offerBtn);
    }
    
    // Set initial UI state
    baseYieldDisplayEl.innerHTML = `Using MMD benchmark yield of <b>${benchmarkMmdYield.toFixed(3)}%</b> (Current YTW - Spread to MMD)`;
    traderSpreadInput.value = state.traderSpread.toFixed(2);
    systemWeightSlider.value = state.systemWeight;
    traderWeightSlider.value = state.traderWeight;

    // Trigger initial calculation & setup
    updateExplainability();
    updateValuation();
}

export function createAlert(row, onRowClickCallback) {
    const alertContainer = document.getElementById('alert-container');
    const alertId = `alert-${row.cusip}`;

    // Avoid duplicate alerts
    if (document.getElementById(alertId)) return;

    const drivers = row.forecast_explainability?.['forecasted_ytw_change_1d_bps']?.feature_attributions
        ?.filter(a => parseFloat(a.attribution) < 0) // Show positive drivers (negative attribution means tightening)
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
                    Forecasted Yield Change: <span class="font-bold font-mono">${row.forecasted_ytw_change_1d_bps} bps</span>
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

    // Peer Relative Value
    const rv = get(['relative_value'], data);
    if (rv) {
        document.getElementById('detail-vs-peers').textContent = `${rv.vs_peers_bps} bps`;
        document.getElementById('detail-vs-mmd').textContent = `${rv.vs_mmd_bps} bps`;
        document.getElementById('detail-vs-ust').textContent = `${rv.vs_ust_bps} bps`;

        const rvLabelEl = document.getElementById('detail-rv-label');
        const vsPeers = parseFloat(rv.vs_peers_bps);
        if (vsPeers > 5) {
            rvLabelEl.textContent = 'Cheap';
            rvLabelEl.className = 'text-center font-semibold text-lg mb-2 text-green-400';
        } else if (vsPeers < -5) {
            rvLabelEl.textContent = 'Rich';
            rvLabelEl.className = 'text-center font-semibold text-lg mb-2 text-red-400';
        } else {
            rvLabelEl.textContent = 'Fair';
            rvLabelEl.className = 'text-center font-semibold text-lg mb-2 text-slate-300';
        }

        const peerSizeEl = document.getElementById('detail-peer-group-size');
        const peerTooltipText = `
            <div class='text-left'>
                <div class='font-bold'>${rv.peer_selection_logic}</div>
                <ul class='list-disc list-inside mt-1'>
                    ${rv.peer_group_cusips.map(c => `<li>${c}</li>`).join('')}
                </ul>
            </div>`;
        peerSizeEl.innerHTML = `
            <span class="font-semibold font-mono">${rv.peer_group_size}</span>
            <span class="has-tooltip relative ml-1 text-slate-500 cursor-pointer">
                <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3 inline" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                <div class="explain-tooltip tooltip-right font-sans">${peerTooltipText}</div>
            </span>
        `;
    }
    
    // Liquidity Assessment
    document.getElementById('detail-liquidity-score').textContent = get(['liquidity', 'composite_score'], data);
    const isConcentrated = get(['ownership', 'is_concentrated_flag'], data);
    const isConcentratedEl = document.getElementById('detail-is-concentrated');
    isConcentratedEl.textContent = isConcentrated ? 'Yes' : 'No';
    isConcentratedEl.className = `font-semibold ${isConcentrated ? 'text-red-600' : 'text-green-600'}`;


    document.getElementById('detail-trade-1d').textContent = `${(get(['trade_history_summary', 't1d', 'total_par_volume'], data) / 1e6).toFixed(1)}M / ${get(['trade_history_summary', 't1d', 'unique_dealer_count'], data)}`;
    document.getElementById('detail-trade-5d').textContent = `${(get(['trade_history_summary', 't5d', 'total_par_volume'], data) / 1e6).toFixed(1)}M / ${get(['trade_history_summary', 't5d', 'unique_dealer_count'], data)}`;

    // Market Context
    const mrc = get(['market_regime_context'], data);
    if(mrc) {
        populateMarketRegime('detail-global-regime', 'Global Regime', get(['global_macro_regime'], mrc));
        populateMarketRegime('detail-contextual-regime', 'Muni Regime', get(['contextual_regime'], mrc));
    } else {
        // Clear if no context
        populateMarketRegime('detail-global-regime', 'Global Regime', null);
        populateMarketRegime('detail-contextual-regime', 'Muni Regime', null);
    }

    // Tax & Fiscal Profile
    const tp = get(['security_details', 'tax_profile'], data);
    const sfh = get(['state_fiscal_health'], data);
    if (tp) {
        document.getElementById('detail-tax-status').textContent = get(['tax_status'], tp).replace(/_/g, ' ');
        const isAmt = get(['is_amt'], tp);
        const isAmtEl = document.getElementById('detail-is-amt');
        isAmtEl.textContent = isAmt ? 'Yes' : 'No';
        isAmtEl.className = `font-semibold font-mono ${isAmt ? 'text-yellow-400' : 'text-slate-300'}`;
        document.getElementById('detail-in-state-exempt').textContent = get(['in_state_tax_exempt'], tp) ? 'Yes' : 'No';
    }
    if (sfh) {
        document.getElementById('detail-tax-receipts').textContent = `${get(['tax_receipts_yoy_growth'], sfh)}%`;
        document.getElementById('detail-budget-surplus').textContent = `${get(['budget_surplus_deficit_pct_gsp'], sfh)}%`;
    }

    // Forecasts with Explainability
    const fv = data.forecasted_values;
    const fe = data.forecast_explainability;
    const crm = data.calculated_risk_metrics;
    const ns = data.news_sentiment;

    // Yield Assessment Forecasts
    createForecastLine('detail-bid-ask-5d', '5D Bid/Ask Spread', get(['5-day', 'bid_ask_spread_pct'], fv), '%', get(['5-day.bid_ask_spread_pct', 'feature_attributions'], fe));
    
    const newsArticles = ns.top_articles.map(a => ({ feature: a.source, attribution: a.headline }));
    createForecastLine('detail-news-sentiment', 'News Sentiment', ns.score, '', newsArticles, null, 'right');
    createForecastLine('detail-neg-news-5d', '5D Neg. News Prob', get(['5-day', 'probability_negative_news_pct'], fv), '%', get(['5-day.probability_negative_news_pct', 'feature_attributions'], fe), null, 'right');
    createForecastLine('detail-downside-1d', '1D Downside Vol', get(['1-day', 'downside_price_volatility', 'value'], fv), '%', get(['1-day.downside_price_volatility.value', 'feature_attributions'], fe), null, 'right');
    createForecastLine('detail-downside-5d', '5D Downside Vol', get(['5-day', 'downside_price_volatility', 'value'], fv), '%', get(['5-day.downside_price_volatility.value', 'feature_attributions'], fe), null, 'right');
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