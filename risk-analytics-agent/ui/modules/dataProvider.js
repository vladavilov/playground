// --- MOCK DATA ---
function generateCusip() {
    // Not a valid CUSIP, but good enough for a mock.
    return (Math.random().toString(36).substring(2, 7) + Math.random().toString(36).substring(2, 6)).toUpperCase();
}

export function createMockData() {
    const data = [];
    const issuers = ['State of California', 'New York City TFA', 'Commonwealth of Massachusetts', 'State of Texas', 'Chicago Board of Education'];
    const regimes = ['Bull_Steepener', 'Bear_Flattener', 'Bear_Steepener', 'Bull_Flattener', 'Recession_Easing', 'Idiosyncratic_Distress'];
    const contextual_regimes = ['Muni_Rich_Market', 'Muni_Cheap_Market', 'Muni_Neutral'];
    
    const regimeDrivers = {
        'Bull_Steepener': [
            { name: 'yield_curve_slope_10y2y', value: '+1.2 Z' },
            { name: 'investment_grade_credit_spread', value: '-1.1 Z' },
            { name: 'vix_index', value: '-0.9 Z' },
        ],
        'Bear_Flattener': [
            { name: 'yield_curve_slope_10y2y', value: '-1.3 Z' },
            { name: 'move_index', value: '+1.1 Z' },
        ],
        'Bear_Steepener': [
            { name: 'yield_curve_slope_10y2y', value: '+1.1 Z' },
            { name: 'tips_breakeven_5y', value: '+1.4 Z' },
            { name: 'vix_index', value: '+1.0 Z' },
        ],
        'Bull_Flattener': [
            { name: 'yield_curve_slope_10y2y', value: '-1.0 Z' },
            { name: 'high_yield_credit_spread', value: '+1.2 Z' },
            { name: 'vix_index', value: '+1.3 Z' },
        ],
        'Recession_Easing': [
            { name: 'yield_curve_slope_10y2y', value: '-1.6 Z' },
            { name: 'high_yield_credit_spread', value: '+1.8 Z' },
            { name: 'move_index', value: '+1.5 Z' },
        ],
        'Idiosyncratic_Distress': [
            { name: 'mmd_ust_ratio_10y', value: '+1.9 Z' },
            { name: 'muni_fund_flows_net', value: '-1.7 Z' },
            { name: 'HY Spread vs Muni Spread', value: '-1.5 Z Divergence' },
        ],
        'Muni_Rich_Market': [
            { name: 'MMD/UST Ratio (10Y)', value: '78%' },
            { name: 'Muni Fund Flows (4wk MovAvg)', value: '+$1.2B' }
        ],
        'Muni_Cheap_Market': [
            { name: 'MMD/UST Ratio (10Y)', value: '92%' },
            { name: 'Muni Fund Flows (4wk MovAvg)', value: '-$0.8B' }
        ],
        'Muni_Neutral': [
            { name: 'MMD/UST Ratio (10Y)', value: '86%' },
            { name: 'Muni Fund Flows (4wk MovAvg)', value: '+$0.2B' }
        ],
    };

    for (let i = 0; i < 15; i++) {
        const cusip = generateCusip();
        const current_oas = 50 + Math.random() * 50;
        const price = parseFloat((98 + Math.random() * 4).toFixed(3));
        const change_bps_1d = (Math.random() * 10 - 5);
        const forecast_1d_ytw = current_oas + change_bps_1d;
        const globalRegime = regimes[Math.floor(Math.random() * regimes.length)];
        const contextualRegime = contextual_regimes[Math.floor(Math.random() * contextual_regimes.length)];

        data.push({
            cusip: cusip,
            security_details: {
                issuer_name: issuers[i % issuers.length],
                rating: ['AAA', 'AA+', 'AA', 'A+', 'A'][Math.floor(Math.random() * 5)],
                price: price,
                coupon_rate: (Math.random() * 5 + 1).toFixed(3),
                maturity_date: '2034-08-15',
                call_features: {
                    is_callable: Math.random() > 0.5,
                    next_call_date: '2027-08-15',
                    next_call_price: 102.50
                },
                tax_profile: {
                    tax_status: 'TAX_EXEMPT_FEDERAL',
                    is_amt: Math.random() > 0.8,
                    in_state_tax_exempt: Math.random() > 0.3
                }
            },
            state_fiscal_health: {
                tax_receipts_yoy_growth: (Math.random() * 5 - 1).toFixed(2),
                budget_surplus_deficit_pct_gsp: (Math.random() * 3 - 1.5).toFixed(2)
            },
            liquidity: {
                is_illiquid_flag: Math.random() > 0.8,
                composite_score: (Math.random() * 100).toFixed(2),
            },
            ownership: {
                is_concentrated_flag: Math.random() > 0.7,
            },
            market_regime_context: {
                global_macro_regime: {
                    label: globalRegime,
                    evidence: regimeDrivers[globalRegime]
                },
                contextual_regime: {
                    label: contextualRegime,
                    evidence: regimeDrivers[contextualRegime]
                }
            },
            news_sentiment: {
                score: (Math.random() * 2 - 1).toFixed(2),
                top_articles: [
                    { headline: 'Issuer Announces Plans for New Infrastructure Projects', source: 'Local News Wire' },
                    { headline: 'State Pension Fund Reports Stable Outlook', source: 'Financial Times' },
                    { headline: 'Regional Economic Growth Exceeds Expectations', source: 'Economic Bulletin' },
                ]
            },
            bid_wanted_details: {
                side: 'BWIC',
                price: price.toFixed(3),
                size: [10, 25, 50, 100, 250][Math.floor(Math.random() * 5)] * 1000,
            },
            calculated_risk_metrics: {
                option_adjusted_spread_bps: current_oas.toFixed(2),
                yield_to_worst: (current_oas / 100 + 4.5).toFixed(3),
                modified_duration: (Math.random() * 5 + 2).toFixed(2),
            },
            forecasted_ytw_change_1d_bps: change_bps_1d.toFixed(2),
            forecasted_values: {
                '1-day': {
                    bid_ask_spread_pct: (Math.random() * 0.5).toFixed(4),
                    probability_negative_news_pct: (Math.random() * 20).toFixed(2),
                    credit_spread_ytw_bps: forecast_1d_ytw.toFixed(2),
                    yield_to_worst: (forecast_1d_ytw / 100 + 4.5).toFixed(3),
                    downside_price_volatility: { value: (Math.random() * 1.5).toFixed(2) },
                },
                '5-day': {
                    bid_ask_spread_pct: (Math.random() * 0.7).toFixed(4),
                    probability_negative_news_pct: (Math.random() * 30).toFixed(2),
                    credit_spread_ytw_bps: (current_oas + (Math.random() * 40 - 18)).toFixed(2),
                    yield_to_worst: ((current_oas + (Math.random() * 40 - 18)) / 100 + 4.5).toFixed(3),
                    downside_price_volatility: { value: (Math.random() * 2.5).toFixed(2) },
                },
                '20-day': {
                    credit_spread_ytw_bps: (current_oas + (Math.random() * 60 - 25)).toFixed(2),
                }
            },
            relative_value: {
                vs_peers_bps: (Math.random() * 20 - 10).toFixed(2),
                vs_mmd_bps: (Math.random() * 30 + 5).toFixed(2),
                vs_ust_bps: (Math.random() * 150 + 50).toFixed(2),
                peer_group_size: Math.floor(Math.random() * 15) + 5,
                peer_group_cusips: [generateCusip(), generateCusip(), generateCusip(), generateCusip(), generateCusip()],
                peer_selection_logic: 'Same State, Same Sector, Maturity +/- 2 Years'
            },
            trade_history_summary: {
                't1d': { total_par_volume: Math.floor(Math.random() * 5000000), unique_dealer_count: Math.floor(Math.random() * 5) + 1 },
                't5d': { total_par_volume: Math.floor(Math.random() * 25000000), unique_dealer_count: Math.floor(Math.random() * 10) + 2 },
            },
            forecast_explainability: {
                    'forecasted_ytw_change_1d_bps': {
                    feature_attributions: [
                        { feature: 'UST 10Y Yield Curve', attribution: (Math.random() * -5).toFixed(1) },
                        { feature: 'Similar Sector Spread Change', attribution: (Math.random() * -3).toFixed(1) },
                        { feature: 'State GOV Deficit Change', attribution: (Math.random() * 2).toFixed(1) }
                    ]
                },
                '1-day.credit_spread_ytw_bps': {
                    feature_attributions: [
                        { feature: 'UST 10Y Yield', attribution: (Math.random() * 0.5).toFixed(2) },
                        { feature: 'Market Volatility', attribution: (Math.random() * 0.3).toFixed(2) },
                        { feature: 'Issuer Rating Change', attribution: (Math.random() * 0.2).toFixed(2) }
                    ]
                },
                    '5-day.probability_negative_news_pct': {
                    feature_attributions: [
                        { feature: 'News Sentiment Score', attribution: (Math.random() * 0.6).toFixed(2) },
                        { feature: 'Similar Bond Price Action', attribution: (Math.random() * 0.4).toFixed(2) }
                    ]
                },
                    '5-day.bid_ask_spread_pct': {
                    feature_attributions: [
                            { feature: 'Recent Trade Volume', attribution: (Math.random() * -0.4).toFixed(2) },
                            { feature: 'Dealer Inventory Levels', attribution: (Math.random() * 0.2).toFixed(2) }
                    ]
                },
                '20-day.credit_spread_ytw_bps': {
                    feature_attributions: [
                        { feature: 'FOMC Rate Decision Probability', attribution: (Math.random() * 0.7).toFixed(2) },
                        { feature: '3M Inflation Swap', attribution: (Math.random() * 0.3).toFixed(2) }
                    ]
                },
                    '1-day.downside_price_volatility.value': {
                    feature_attributions: [
                            { feature: 'MOVE Index', attribution: (Math.random() * 0.8).toFixed(2) },
                            { feature: 'High Yield Spread Change', attribution: (Math.random() * 0.5).toFixed(2) }
                    ]
                },
                '5-day.credit_spread_ytw_bps': {
                    feature_attributions: [
                        { feature: '3M Sector Momentum', attribution: (Math.random() * 2.5).toFixed(2) },
                        { feature: '7D Fund Flows', attribution: (Math.random() * -1.5).toFixed(2) },
                        { feature: 'Dealer Inventory Pressure', attribution: (Math.random() * 1.0).toFixed(2) }
                    ]
                },
                '5-day.downside_price_volatility.value': {
                     feature_attributions: [
                            { feature: 'VIX Index', attribution: (Math.random() * 0.9).toFixed(2) },
                            { feature: 'Muni HY vs Corp HY Spread', attribution: (Math.random() * 0.6).toFixed(2) }
                    ]
                },
            }
        });
    }
    return data;
} 