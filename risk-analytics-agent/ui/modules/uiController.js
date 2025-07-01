// Helper to safely access nested properties
const get = (p, o) => p.reduce((xs, x) => (xs && xs[x]) ? xs[x] : null, o);

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
    const modeSelectorEl = document.getElementById('valuation-mode-selector');

    const forecast1dEl = document.getElementById('1d-trend-forecast-spread');
    const forecast5dEl = document.getElementById('5d-trend-forecast-spread');
    const forecastPeerRVEl = document.getElementById('peer-rv-target-spread');

    const weight1dEl = document.getElementById('1d-weight');
    const weight5dEl = document.getElementById('5d-weight');
    const weightRvEl = document.getElementById('rv-weight');

    const result1dEl = document.getElementById('1d-result');
    const result5dEl = document.getElementById('5d-result');
    const resultRvEl = document.getElementById('rv-result');

    const finalTargetSpreadEl = document.getElementById('final-target-spread');
    const strategyRationaleEl = document.getElementById('strategy-rationale');
    
    const suggestedPriceInput = document.getElementById('suggested-price-input');
    const suggestedYieldInput = document.getElementById('suggested-yield-input');
    const explanationEl = document.getElementById('suggested-price-explanation');
    const valuationActionContainer = document.getElementById('valuation-action-buttons');
    const baseYieldDisplayEl = document.getElementById('base-yield-display');

    const priceCalcBreakdownEl = document.getElementById('price-calculation-breakdown');
    const yieldCalcBreakdownEl = document.getElementById('yield-calculation-breakdown');

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
    const coupon = parseFloat(sd.coupon) || 0;
    const tenor = get(['security_details', 'tenor_years'], data) || (new Date(get(['security_details', 'maturity_date'], data)).getFullYear() - new Date().getFullYear());
    
    const benchmarkMmdYield = baselineYield - (currentSpreadToMmd / 100);

    const forecastSpread1D = currentSpreadToMmd + (parseFloat(forecasted_ytw_change_1d_bps) || 0);
    const forecastSpread5D = currentSpreadToMmd + (parseFloat(forecasted_ytw_change_5d_bps) || 0);
    const forecastSpread20D = currentSpreadToMmd + (parseFloat(forecasted_ytw_change_20d_bps) || 0);
    const forecastPeerSpread = parseFloat(peer_rv_spread_bps) || parseFloat(rv.vs_peers_bps) || currentSpreadToMmd; 

    // --- Configuration based on requirements ---
    const VALUATION_MODES = {
        'Trend-Based': { 
            name: 'Trend-Based', 
            weights: { '1d': 0.7, '5d': 0.2, 'rv': 0.1 },
            rationale: "Using <strong>Trend-Based</strong> mode, which prioritizes immediate momentum. The valuation is heavily influenced by the 1-day trend forecast to align with short-term tactical trading goals."
        },
        'Balanced': { 
            name: 'Balanced', 
            weights: { '1d': 0.1, '5d': 0.5, 'rv': 0.4 },
            rationale: "Using <strong>Balanced</strong> mode, which offers a view blending recent momentum with fundamental value. It's suitable for a multi-day holding period."
        },
        'Fair Value': { 
            name: 'Fair Value',
            weights: { '1d': 0.0, '5d': 0.1, 'rv': 0.9 },
            rationale: "Using <strong>Fair Value</strong> mode, which emphasizes fundamental analysis by basing the valuation almost entirely on the Peer RV spread. This is for strategic, long-term investment decisions."
        },
        'Cautious': {
            name: 'Cautious',
            weights: null, // Indicates special logic
            rationale: "Using <strong>Cautious</strong> mode. The valuation is based on the most conservative (highest) spread among the forecast inputs to create a defensive price."
        }
    };

    let isUpdating = false;

    // --- State ---
    const state = {
        activeMode: 'Balanced'
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
     * Creates a comprehensive risk score adjustment based on multiple factors.
     * Higher return values indicate higher risk.
     * @param {object} data The full instrument data object.
     * @returns {{adjustmentBps: number, breakdown: {name: string, value: string, impact: number}[]}} An object containing the total adjustment and a breakdown of contributing factors.
     */
    const calculateRiskPremiumAdjustment = (data) => {
        let adjustmentBps = 0;
        const breakdown = [];

        // 1. Market Regime
        const regime = get(['market_regime_context', 'contextual_regime', 'label'], data);
        if (regime && (regime.includes('Bear') || regime.includes('Volatile'))) {
            const impact = 2;
            adjustmentBps += impact;
            breakdown.push({ name: 'Market Regime', value: regime.replace(/_/g, ' '), impact });
        }

        // 2. Liquidity Score
        const liquidityScore = parseFloat(get(['liquidity', 'composite_score'], data));
        if (!isNaN(liquidityScore) && liquidityScore < 40) {
            const impact = 3;
            adjustmentBps += impact;
            breakdown.push({ name: 'Liquidity Score', value: liquidityScore, impact });
        }

        // 3. Concentrated Ownership
        if (get(['ownership', 'is_concentrated_flag'], data) === true) {
            const impact = 4;
            adjustmentBps += impact;
            breakdown.push({ name: 'Concentrated Ownership', value: 'Yes', impact });
        }

        // 4. Idiosyncratic Risk (Negative News)
        const negNewsProb = parseFloat(get(['forecasted_values', '5-day', 'probability_negative_news_pct'], data));
        if (!isNaN(negNewsProb) && negNewsProb > 15) {
            const impact = 3;
            adjustmentBps += impact;
            breakdown.push({ name: 'Neg. News Probability', value: `${negNewsProb.toFixed(1)}%`, impact });
        }
        
        // 5. Downside Volatility Risk
        const downsideVol = parseFloat(get(['forecasted_values', '5-day', 'downside_price_volatility', 'value'], data));
        if (!isNaN(downsideVol) && downsideVol > 1.5) { // Assuming 1.5% is a high threshold
             const impact = 2;
            adjustmentBps += impact;
            breakdown.push({ name: '5D Downside Vol', value: `${downsideVol.toFixed(2)}%`, impact });
        }
        
        // 6. State Fiscal Health Risk
        const budgetDeficit = parseFloat(get(['state_fiscal_health', 'budget_surplus_deficit_pct_gsp'], data));
        if (!isNaN(budgetDeficit) && budgetDeficit < -0.5) { // Deficit > 0.5% of GSP
            const impact = 1;
            adjustmentBps += impact;
            breakdown.push({ name: 'State Budget Deficit', value: `${budgetDeficit.toFixed(2)}%`, impact });
        }

        return { adjustmentBps, breakdown };
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
        if (isUpdating) return;
        isUpdating = true;

        const activeMode = VALUATION_MODES[state.activeMode];
        let finalSpread;

        const spread1d = parseFloat(forecast1dEl.value) || 0;
        const spread5d = parseFloat(forecast5dEl.value) || 0;
        const spreadRv = parseFloat(forecastPeerRVEl.value) || 0;

        if (activeMode.name === 'Cautious') {
            finalSpread = Math.max(spread1d, spread5d, spreadRv);
            
            // Visually update breakdown to reflect Cautious logic
            weight1dEl.textContent = 'n/a';
            weight5dEl.textContent = 'n/a';
            weightRvEl.textContent = 'n/a';
            result1dEl.textContent = spread1d.toFixed(1);
            result5dEl.textContent = spread5d.toFixed(1);
            resultRvEl.textContent = spreadRv.toFixed(1);

        } else {
            const weights = activeMode.weights;

            // Calculate weighted results
            const result1d = spread1d * weights['1d'];
            const result5d = spread5d * weights['5d'];
            const resultRv = spreadRv * weights['rv'];
            finalSpread = result1d + result5d + resultRv;

            // Update UI with weights and results
            weight1dEl.textContent = (weights['1d'] * 100).toFixed(0) + '%';
            weight5dEl.textContent = (weights['5d'] * 100).toFixed(0) + '%';
            weightRvEl.textContent = (weights['rv'] * 100).toFixed(0) + '%';
            
            result1dEl.textContent = result1d.toFixed(1);
            result5dEl.textContent = result5d.toFixed(1);
            resultRvEl.textContent = resultRv.toFixed(1);
        }

        strategyRationaleEl.innerHTML = activeMode.rationale;
        finalTargetSpreadEl.textContent = finalSpread.toFixed(1);
        
        // Calculate and display final price/yield
        const finalYield = benchmarkMmdYield + (finalSpread / 100);
        const finalPrice = updatePriceFromYield(finalYield);

        suggestedPriceInput.value = finalPrice.toFixed(4);
        suggestedYieldInput.value = finalYield.toFixed(4);

        if (yieldCalcBreakdownEl) {
            yieldCalcBreakdownEl.innerHTML = `<span class="font-mono">${benchmarkMmdYield.toFixed(2)}% + ${finalSpread.toFixed(1)}bps</span>`;
        }
        if (priceCalcBreakdownEl) {
            const tenorDisplay = !isNaN(tenor) ? `${tenor.toFixed(0)}Y` : 'N/A';
            priceCalcBreakdownEl.innerHTML = `<span class="font-mono" title="f(Yield, Coupon, Tenor)">f(${finalYield.toFixed(2)}%, ${coupon.toFixed(2)}%, ${tenorDisplay})</span>`;
        }

        updateButtonText(finalPrice);
        explanationEl.textContent = `Calculated from ${finalSpread.toFixed(1)} bps spread over benchmark.`;
        
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
        inputEl.classList.remove('override-active');
        
        // Restore original forecast value
        if(inputEl.id === '1d-trend-forecast-spread') inputEl.value = forecastSpread1D.toFixed(1);
        if(inputEl.id === '5d-trend-forecast-spread') inputEl.value = forecastSpread5D.toFixed(1);
        if(inputEl.id === 'peer-rv-target-spread') inputEl.value = forecastPeerSpread.toFixed(1);

        const resetIcon = inputEl.nextElementSibling;
        if(resetIcon) resetIcon.remove();
        
        updateValuation();
    };

    const handleForecastInput = (e) => {
        const inputEl = e.target;
        if (!overrideState[inputEl.id]) {
            overrideState[inputEl.id] = true;
            inputEl.classList.add('override-active');
            createResetIcon(inputEl);
        }
        updateValuation();
    };

    const createSelector = (container, items, activeItem, clickHandler) => {
        container.innerHTML = '';
        items.forEach(item => {
            const btn = document.createElement('button');
            const isActive = (item === activeItem);
            btn.className = `px-2 py-1 text-xs font-semibold rounded-md transition-colors duration-200 ${isActive ? 'bg-blue-600 text-white' : 'bg-slate-800 hover:bg-slate-600 text-slate-300'}`;
            btn.textContent = item;
            btn.onclick = () => clickHandler(item);
            container.appendChild(btn);
        });
    };

    const handleModeClick = (modeName) => {
        state.activeMode = modeName;
        createSelector(modeSelectorEl, Object.keys(VALUATION_MODES), state.activeMode, handleModeClick);
        updateValuation();
    };
    
    // --- Initialization ---
    baseYieldDisplayEl.textContent = `Baseline Yield: ${baselineYield.toFixed(3)}% | Benchmark (MMD): ${benchmarkMmdYield.toFixed(3)}% | Current Spread: ${currentSpreadToMmd.toFixed(1)} bps`;
    
    forecast1dEl.value = forecastSpread1D.toFixed(1);
    forecast5dEl.value = forecastSpread5D.toFixed(1);
    forecastPeerRVEl.value = forecastPeerSpread.toFixed(1);

    // Initial setup of selectors
    createSelector(modeSelectorEl, Object.keys(VALUATION_MODES), state.activeMode, handleModeClick);

    [forecast1dEl, forecast5dEl, forecastPeerRVEl].forEach(el => {
        el.addEventListener('input', handleForecastInput);
    });

    // Setup action buttons
    valuationActionContainer.innerHTML = ''; // Clear previous
    if(bwd) {
        const bidButton = document.createElement('button');
        bidButton.className = 'btn-primary bg-green-600 hover:bg-green-700 text-white font-bold py-2 px-4 rounded text-xs';
        valuationActionContainer.appendChild(bidButton);
        
        const offerButton = document.createElement('button');
        offerButton.className = 'btn-secondary bg-red-600 hover:bg-red-700 text-white font-bold py-2 px-4 rounded text-xs';
        valuationActionContainer.appendChild(offerButton);

        bidButton.onclick = () => handleAction('Bid', bwd.size, suggestedPriceInput.value);
        offerButton.onclick = () => {
             const offerPrice = (parseFloat(suggestedPriceInput.value) * 1.0025).toFixed(3);
             handleAction('Offer', bwd.size, offerPrice);
        };
    }
    
    // Initial calculation
    updateValuation();
    
    createTooltip('fair-value-tooltip-icon', 'This valuation is derived from a blend of forecast models. Adjust the mode to align the price with your trading goals.');
    
    const riskPremiumResult = calculateRiskPremiumAdjustment(data);
    const riskPremiumBps = riskPremiumResult.adjustmentBps;

    // Create a new element for the risk premium if it doesn't exist.
    let riskPremiumContainer = document.getElementById('risk-premium-container');
    if (!riskPremiumContainer) {
        riskPremiumContainer = document.createElement('div');
        riskPremiumContainer.id = 'risk-premium-container';
        riskPremiumContainer.className = 'mt-3 text-xs text-slate-400 bg-slate-900/50 border-l-2 border-red-500/50 p-2 rounded-r-md';
        // Insert it after the rationale
        const rationaleEl = document.getElementById('strategy-rationale');
        if (rationaleEl) {
            rationaleEl.parentNode.insertBefore(riskPremiumContainer, rationaleEl.nextSibling);
        }
    }

    if (riskPremiumBps > 0) {
        const breakdownHtml = riskPremiumResult.breakdown.map(item => `<li>${item.name} (${item.value}): <span class="font-mono text-red-400">+${item.impact}bps</span></li>`).join('');
        riskPremiumContainer.innerHTML = `
            <div class="font-semibold text-red-400">Risk Premium Adjustment: +${riskPremiumBps} bps</div>
            <ul class="list-disc list-inside mt-1">${breakdownHtml}</ul>
        `;
        riskPremiumContainer.classList.remove('hidden');
    } else {
        riskPremiumContainer.innerHTML = '';
        riskPremiumContainer.classList.add('hidden');
    }
}

export function createHighConvictionAlert(row, onRowClickCallback) {
    const alertContainer = document.getElementById('alert-container');
    const alertId = `alert-${row.cusip}`;

    // Avoid duplicate alerts
    if (document.getElementById(alertId)) return;

    const drivers = row.forecast_explainability?.['forecasted_ytw_change_1d_bps']?.feature_attributions
        ?.filter(a => parseFloat(a.attribution) < 0) // Show positive drivers (negative attribution means tightening)
        .slice(0, 3)
        .map(d => `<li class="text-xs">${d.feature}: <span class="font-bold font-mono">${d.attribution}</span></li>`)
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
        createHighConvictionAlert: (row) => createHighConvictionAlert(row, onRowClickCallback),
        showTab,
    };
}