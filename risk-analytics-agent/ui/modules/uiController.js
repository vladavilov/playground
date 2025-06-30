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
    const horizonSelectorEl = document.getElementById('horizon-selector');
    const strategySelectorEl = document.getElementById('strategy-selector');

    const forecast1dEl = document.getElementById('1d-trend-forecast-spread');
    const forecast5dEl = document.getElementById('5d-trend-forecast-spread');
    const forecastPeerRVEl = document.getElementById('peer-rv-target-spread');
    
    const suggestedPriceInput = document.getElementById('suggested-price-input');
    const suggestedYieldInput = document.getElementById('suggested-yield-input');
    const explanationEl = document.getElementById('suggested-price-explanation');
    const valuationActionContainer = document.getElementById('valuation-action-buttons');
    const baseYieldDisplayEl = document.getElementById('base-yield-display');

    // --- Get Data & Baseline Values ---
    const { 
        calculated_risk_metrics: crm, 
        security_details: sd,
        bid_wanted_details: bwd,
        relative_value: rv,
        forecasted_ytw_change_1d_bps,
        forecasted_ytw_change_5d_bps,
        forecasted_ytw_change_20d_bps,
        peer_rv_spread_bps // Assuming this is a new field from the data
    } = data;

    const baselinePrice = parseFloat(sd.price) || 0;
    const baselineYield = parseFloat(crm.yield_to_worst) || 0; // YTW in percent
    const currentSpreadToMmd = parseFloat(rv.vs_mmd_bps) || 0;
    const durationForCalc = parseFloat(crm.effective_duration) || parseFloat(crm.modified_duration);
    
    const benchmarkMmdYield = baselineYield - (currentSpreadToMmd / 100);

    const forecastSpread1D = currentSpreadToMmd + (parseFloat(forecasted_ytw_change_1d_bps) || 0);
    const forecastSpread5D = currentSpreadToMmd + (parseFloat(forecasted_ytw_change_5d_bps) || 0);
    const forecastSpread20D = currentSpreadToMmd + (parseFloat(forecasted_ytw_change_20d_bps) || 0);
    const forecastPeerSpread = parseFloat(peer_rv_spread_bps) || parseFloat(rv.vs_peers_bps) || currentSpreadToMmd; 

    const FORECASTS = {
        '1D_TREND': forecastSpread1D,
        '5D_TREND': forecastSpread5D,
        '20D_TREND': forecastSpread20D,
        'PEER_RV': forecastPeerSpread
    };

    const STRATEGIES = {
        'FAIR_VALUE': { name: 'Fair Value', weights: { '1D_TREND': 0.2, '5D_TREND': 0.3, 'PEER_RV': 0.5 } },
        'TREND_FOLLOW': { name: 'Trend-Following', weights: { '1D_TREND': 0.6, '5D_TREND': 0.4, 'PEER_RV': 0.0 } },
        'RV_FOCUS': { name: 'Relative Value', weights: { '1D_TREND': 0.1, '5D_TREND': 0.1, 'PEER_RV': 0.8 } },
        'CAUTIOUS': { name: 'Cautious', weights: { '1D_TREND': 0.0, '5D_TREND': 0.0, 'PEER_RV': 1.0 } } // Defaults to peer value
    };
    
    const HORIZONS = ['1D', '5D', '10D'];

    let isUpdating = false;

    // --- State ---
    const state = {
        activeHorizon: '5D',
        activeStrategy: 'FAIR_VALUE'
    };
    
    const overrideState = {
        '1d-trend-forecast-spread': false,
        '5d-trend-forecast-spread': false,
        'peer-rv-target-spread': false,
    };

    // --- Helper Functions ---
    function createTooltip(elementId, text) {
        const iconContainer = document.getElementById(elementId);
        if (!iconContainer) return;

        iconContainer.classList.add('has-tooltip', 'relative', 'text-slate-500', 'cursor-pointer');
        
        const iconSvg = `<svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3 inline" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>`;
        const tooltipDiv = `<div class="explain-tooltip font-sans">${text}</div>`;

        iconContainer.innerHTML = iconSvg + tooltipDiv;
    }

    /**
     * Task 6: Creates the `Risk Premium Adjustment` by making the valuation model 
     * aware of key risk factors from other UI panels.
     * @param {object} data The full instrument data object.
     * @returns {number} A basis point adjustment value.
     */
    const calculateRiskPremiumAdjustment = (data) => {
        let adjustmentBps = 0;

        // 1. Market Regime Adjustment from `data.market_regime.contextual_regime`
        // Example: 'Bear_Steepener' -> +2bps
        if (data.market_regime && data.market_regime.contextual_regime) {
            const regime = data.market_regime.contextual_regime;
            if (regime === 'Bear_Steepener') {
                adjustmentBps += 2;
            }
        }

        // 2. Liquidity Score Adjustment from `data.liquidity.composite_score`
        // Example: score < 40 -> +3bps
        if (data.liquidity && typeof data.liquidity.composite_score !== 'undefined') {
            const score = data.liquidity.composite_score;
            if (score < 40) {
                adjustmentBps += 3;
            }
        }

        // 3. Idiosyncratic Risk from `data.forecasted_values['5-day'].probability_negative_news_pct`
        // Example: probability > 15% -> +2bps
        if (data.forecasted_values && data.forecasted_values['5-day'] && typeof data.forecasted_values['5-day'].probability_negative_news_pct !== 'undefined') {
            const prob = data.forecasted_values['5-day'].probability_negative_news_pct;
            if (prob > 15) {
                adjustmentBps += 2;
            }
        }

        return adjustmentBps;
    };

    const updatePriceFromYield = (newYield) => {
        if (isNaN(newYield) || !isFinite(durationForCalc) || durationForCalc === 0) return baselinePrice;
        const yieldChange = (newYield - baselineYield) / 100;
        const priceChange = -yieldChange * durationForCalc * baselinePrice;
        return baselinePrice + priceChange;
    };

    const updateYieldFromPrice = (newPrice) => {
        if (isNaN(newPrice) || !isFinite(durationForCalc) || durationForCalc === 0) return baselineYield;
        const priceChangePct = (newPrice - baselinePrice) / baselinePrice;
        const yieldChange = -priceChangePct / durationForCalc;
        return baselineYield + (yieldChange * 100);
    };

    const updateButtonText = (price) => {
        if (!bwd) return;
        const formattedSize = bwd.size >= 1000000 ? `${(bwd.size / 1000000).toFixed(1)}mm` : `${Math.round(bwd.size / 1000)}k`;
        const primaryBtn = valuationActionContainer.querySelector('.btn-primary');
        const secondaryBtn = valuationActionContainer.querySelector('.btn-secondary');
        const formattedPrice = parseFloat(price).toFixed(3);
        if (primaryBtn) {
            primaryBtn.textContent = `Bid ${formattedSize} @ ${formattedPrice}`;
             if(secondaryBtn) { 
                const offerPrice = (parseFloat(price) * 1.0025).toFixed(3);
                secondaryBtn.textContent = `Offer ${formattedSize} @ ${offerPrice}`; 
            }
        }
    };
    
    const updateValuation = () => {
        isUpdating = true;
        
        const strategy = STRATEGIES[state.activeStrategy];
        const weights = strategy.weights;

        const currentForecasts = {
             '1D_TREND': parseFloat(forecast1dEl.value) || 0,
             '5D_TREND': parseFloat(forecast5dEl.value) || 0,
             'PEER_RV': parseFloat(forecastPeerRVEl.value) || 0,
        };
        
        // Task 7: Implement 10-Day Forecast Interpolation (SL-04)
        // When the 10D horizon is selected, calculate a 10D trend value by interpolating
        // between the backend's 5D and 20D forecast values. This interpolated
        // value is then used for the 5D_TREND component in strategies.
        let trend5DValue = currentForecasts['5D_TREND'];
        if (state.activeHorizon === '10D') {
            const value5D = FORECASTS['5D_TREND'];
            const value20D = FORECASTS['20D_TREND'];
            // Using formula from requirements: Value_10D = Value_5D + (Value_20D - Value_5D) * ((10 - 5) / (20 - 5))
            trend5DValue = value5D + (value20D - value5D) * (5 / 15);
        }

        // Calculate the blended spread based on the chosen strategy weights
        const blendedSpread = (currentForecasts['1D_TREND'] * (weights['1D_TREND'] || 0)) +
                              (trend5DValue * (weights['5D_TREND'] || 0)) +
                              (currentForecasts['PEER_RV'] * (weights['PEER_RV'] || 0));

        // Task 4: Dynamic Horizon Adjustment
        // Replaces a static adjustment with a dynamic premium based on market volatility.
        // Assumes `data.market_volatility.move_index` is available.
        const horizonStep = HORIZONS.indexOf(state.activeHorizon); // 0, 1, or 2
        const moveIndex = data.market_volatility ? data.market_volatility.move_index : 100; // Default if data is missing
        const horizonPremium = (horizonStep * moveIndex) / 50; // As per requirements

        // Task 6: Integrate Risk Factors
        // Calculate a risk premium adjustment based on various factors.
        const riskPremiumAdjustment = calculateRiskPremiumAdjustment(data);
        
        let finalSpread;

        // Task 5: Refine "Cautious" Strategy Logic
        // The "Cautious" strategy should be 100% Peer RV + Risk Premium, without the horizon adjustment.
        if (state.activeStrategy === 'CAUTIOUS') {
            finalSpread = blendedSpread + riskPremiumAdjustment;
        } else {
            finalSpread = blendedSpread + horizonPremium + riskPremiumAdjustment;
        }

        const finalYield = benchmarkMmdYield + (finalSpread / 100);
        const suggestedPrice = updatePriceFromYield(finalYield);

        // Update UI
        suggestedYieldInput.value = finalYield.toFixed(3);
        suggestedPriceInput.value = suggestedPrice.toFixed(3);
        
        explanationEl.innerHTML = `<b>${strategy.name}</b> strategy projects <b>${finalSpread.toFixed(2)}bps</b> spread for a <b>${state.activeHorizon}</b> horizon.`;
        
        // Task 8: Update dynamic tooltip for fair value
        const fairValueTooltipText = `Calculated using <b>${strategy.name}</b> strategy with a <b>${riskPremiumAdjustment.toFixed(2)} bps</b> risk premium adjustment and <b>${horizonPremium.toFixed(2)} bps</b> horizon premium.`;
        createTooltip('fair-value-tooltip-icon', fairValueTooltipText);

        updateButtonText(suggestedPrice.toFixed(3));

        isUpdating = false;
    };

    const createResetIcon = (targetEl) => {
        let icon = targetEl.parentNode.querySelector('.reset-icon');
        if (!icon) {
            icon = document.createElement('span');
            icon.className = 'reset-icon absolute right-2 top-1/2 -translate-y-1/2 text-slate-500 hover:text-slate-300 cursor-pointer hidden';
            icon.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 0011.664 0l3.18-3.185m-4.993-4.992v4.992m0 0h-4.992m4.992 0l-3.182-3.182a8.25 8.25 0 00-11.664 0l-3.18 3.185" /></svg>`;
            targetEl.parentNode.appendChild(icon);
            icon.addEventListener('click', () => resetForecast(targetEl));
        }
        return icon;
    };
    
    const resetForecast = (inputEl) => {
        overrideState[inputEl.id] = false;
        inputEl.classList.remove('is-overridden');
        const icon = inputEl.parentNode.querySelector('.reset-icon');
        if(icon) icon.classList.add('hidden');
        
        // Reset to original calculated value
        if (inputEl.id === '1d-trend-forecast-spread') inputEl.value = FORECASTS['1D_TREND'].toFixed(2);
        if (inputEl.id === '5d-trend-forecast-spread') inputEl.value = FORECASTS['5D_TREND'].toFixed(2);
        if (inputEl.id === 'peer-rv-target-spread') inputEl.value = FORECASTS['PEER_RV'].toFixed(2);

        updateValuation();
    };

    const handleForecastInput = (e) => {
        const target = e.target;
        overrideState[target.id] = true;
        target.classList.add('is-overridden');
        createResetIcon(target).classList.remove('hidden');
        updateValuation();
    };

    // --- Event Handlers ---
    suggestedPriceInput.addEventListener('input', () => {
        if (isUpdating) return;
        const newPrice = parseFloat(suggestedPriceInput.value);
        if (!isNaN(newPrice)) {
            const newYield = updateYieldFromPrice(newPrice);
            isUpdating = true;
            suggestedYieldInput.value = newYield.toFixed(3);
            isUpdating = false;
            updateButtonText(newPrice.toFixed(3));
            explanationEl.textContent = 'Price manually adjusted.';
        }
    });

    [forecast1dEl, forecast5dEl, forecastPeerRVEl].forEach(el => {
        el.addEventListener('input', handleForecastInput);
        createResetIcon(el); // Create icons initially (hidden)
    });

    const createSelector = (container, items, stateKey, stateTarget) => {
        container.innerHTML = '';
        const isStrategy = Array.isArray(items);
        const keys = isStrategy ? items : Object.keys(items);

        keys.forEach(key => {
            const item = isStrategy ? key : items[key];
            const button = document.createElement('button');
            button.className = 'px-2 py-1 text-xs font-semibold rounded-md transition-colors duration-200';
            button.textContent = isStrategy ? item : item.name;
            button.dataset.key = isStrategy ? item : key;

            if (button.dataset.key === state[stateTarget]) {
                button.classList.add('bg-blue-600', 'text-white');
            } else {
                button.classList.add('bg-slate-800', 'hover:bg-slate-600', 'text-slate-300');
            }

            button.addEventListener('click', () => {
                state[stateTarget] = button.dataset.key;
                createSelector(container, items, stateKey, stateTarget); // Re-render to update styles
                updateValuation();
            });
            container.appendChild(button);
        });
    };
    
    // --- Initial Population & Event Listeners ---
    createTooltip('1d-trend-tooltip-icon', 'Forecast based on short-term (1-day) momentum models.');
    createTooltip('5d-trend-tooltip-icon', 'Forecast based on medium-term (5-day) momentum models.');
    createTooltip('peer-rv-tooltip-icon', 'Represents the current spread difference vs. a basket of comparable peer bonds.');

    // Populate initial values
    forecast1dEl.value = FORECASTS['1D_TREND'].toFixed(2);
    forecast5dEl.value = FORECASTS['5D_TREND'].toFixed(2);
    forecastPeerRVEl.value = FORECASTS['PEER_RV'].toFixed(2);

    createSelector(horizonSelectorEl, HORIZONS, 'horizons', 'activeHorizon');
    createSelector(strategySelectorEl, STRATEGIES, 'strategies', 'activeStrategy');

    baseYieldDisplayEl.innerHTML = `Baseline Yield: <b>${baselineYield.toFixed(3)}%</b> | Eff. Duration: <b>${durationForCalc.toFixed(2)}</b> | MMD Ref. Yield: <b>${benchmarkMmdYield.toFixed(3)}%</b>`;

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