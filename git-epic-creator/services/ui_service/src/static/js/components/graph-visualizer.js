/**
 * Graph Visualizer Component
 * 
 * Provides Neo4j graph visualization using Neovis.js for project knowledge graphs.
 * 
 * @module components/graph-visualizer
 */

'use strict';

export class GraphVisualizer {
  /**
   * Creates a new graph visualizer instance.
   * @param {Object} config - Configuration object
   * @param {string} config.containerId - DOM element ID for the graph container
   * @param {Function} config.onError - Error callback (optional)
   */
  constructor(config) {
    this.containerId = config.containerId;
    this.onError = config.onError || ((err) => console.error('Graph error:', err));
    this.viz = null;
    this.currentProjectId = null;
    
    // Check if Neovis is loaded
    if (typeof NeoVis === 'undefined') {
      throw new Error('Neovis.js library not loaded. Include <script src="https://unpkg.com/neovis.js@2.1.0"></script>');
    }
  }
  
  /**
   * Render graph from data.
   * @param {Object} graphData - Graph data from API
   * @param {Array} graphData.nodes - Array of node objects
   * @param {Array} graphData.relationships - Array of relationship objects
   * @param {Object} graphData.stats - Graph statistics
   */
  async renderFromData(graphData) {
    try {
      const container = document.getElementById(this.containerId);
      if (!container) {
        throw new Error(`Container element #${this.containerId} not found`);
      }
      
      // Clear container
      container.innerHTML = '';
      
      // Check if graph is empty
      if (!graphData.nodes || graphData.nodes.length === 0) {
        this._showEmptyState(container, graphData.stats?.message);
        return;
      }
      
      // Build vis.js compatible data
      const visData = this._buildVisData(graphData);
      
      // Configure visualization
      const config = {
        containerId: this.containerId,
        visConfig: {
          nodes: {
            shape: 'dot',
            size: 20,
            font: {
              size: 12,
              color: '#1e293b'
            },
            borderWidth: 2,
            shadow: true
          },
          edges: {
            arrows: {
              to: { enabled: true, scaleFactor: 0.5 }
            },
            smooth: {
              type: 'continuous'
            },
            font: {
              size: 10,
              align: 'middle'
            }
          },
          physics: {
            enabled: true,
            stabilization: {
              enabled: true,
              iterations: 200
            },
            barnesHut: {
              gravitationalConstant: -8000,
              centralGravity: 0.3,
              springLength: 150,
              springConstant: 0.04,
              damping: 0.09,
              avoidOverlap: 0.1
            }
          },
          interaction: {
            hover: true,
            tooltipDelay: 200,
            zoomView: true,
            dragView: true
          },
          layout: {
            improvedLayout: true,
            randomSeed: 42
          }
        },
        labels: this._buildLabelConfig(),
        relationships: this._buildRelationshipConfig()
      };
      
      // Create visualization without Neo4j connection (using provided data)
      // We'll manually populate the network
      const network = this._createNetworkFromData(visData, config.visConfig);
      
      // Add legend
      this._addLegend(container, graphData.stats);
      
      // Add controls
      this._addControls(container, network);
      
      console.log('Graph rendered successfully:', {
        nodes: graphData.nodes.length,
        relationships: graphData.relationships.length,
        types: graphData.stats.node_types
      });
      
    } catch (error) {
      console.error('Failed to render graph:', error);
      this.onError(error);
      throw error;
    }
  }
  
  /**
   * Build vis.js compatible data structure from API response.
   * @private
   */
  _buildVisData(graphData) {
    const nodes = graphData.nodes.map(node => ({
      id: node.id,
      label: this._getNodeLabel(node),
      group: node.label,
      title: this._buildNodeTooltip(node),
      ...this._getNodeStyle(node.label)
    }));
    
    const edges = graphData.relationships.map((rel, idx) => ({
      id: `edge-${idx}`,
      from: rel.source,
      to: rel.target,
      label: rel.type,
      title: rel.type,
      ...this._getEdgeStyle(rel.type)
    }));
    
    return { nodes, edges };
  }
  
  /**
   * Get display label for a node.
   * @private
   */
  _getNodeLabel(node) {
    const props = node.properties || {};
    
    // Try different property names based on node type
    if (props.name) return props.name;
    if (props.title) return props.title;
    if (props.text && props.text.length < 30) return props.text;
    
    // Fallback to type + truncated ID
    const type = node.label.replace(/__/g, '');
    const shortId = node.id.substring(0, 8);
    return `${type} ${shortId}`;
  }
  
  /**
   * Build tooltip HTML for a node.
   * @private
   */
  _buildNodeTooltip(node) {
    const props = node.properties || {};
    const type = node.label;
    
    let html = `<strong>${type}</strong><br/>`;
    html += `<strong>ID:</strong> ${node.id.substring(0, 16)}...<br/>`;
    
    // Add key properties based on type
    if (type === '__Project__') {
      if (props.name) html += `<strong>Name:</strong> ${props.name}<br/>`;
      if (props.description) html += `<strong>Description:</strong> ${props.description.substring(0, 100)}...<br/>`;
    } else if (type === '__Entity__') {
      if (props.title) html += `<strong>Title:</strong> ${props.title}<br/>`;
      if (props.type) html += `<strong>Type:</strong> ${props.type}<br/>`;
      if (props.description) html += `<strong>Description:</strong> ${props.description.substring(0, 100)}...<br/>`;
    } else if (type === '__Document__') {
      if (props.title) html += `<strong>Title:</strong> ${props.title}<br/>`;
      if (props.source) html += `<strong>Source:</strong> ${props.source}<br/>`;
    } else if (type === '__Chunk__') {
      if (props.text) html += `<strong>Text:</strong> ${props.text.substring(0, 100)}...<br/>`;
    } else if (type === '__Community__') {
      if (props.community !== undefined) html += `<strong>Community:</strong> ${props.community}<br/>`;
      if (props.level !== undefined) html += `<strong>Level:</strong> ${props.level}<br/>`;
      if (props.summary) html += `<strong>Summary:</strong> ${props.summary.substring(0, 100)}...<br/>`;
    }
    
    return html;
  }
  
  /**
   * Get node styling based on type.
   * @private
   */
  _getNodeStyle(label) {
    const styles = {
      '__Project__': { color: '#4f46e5', size: 30 },      // Indigo
      '__Entity__': { color: '#a855f7', size: 20 },       // Purple
      '__Document__': { color: '#10b981', size: 25 },     // Emerald
      '__Chunk__': { color: '#64748b', size: 15 },        // Slate
      '__Community__': { color: '#f59e0b', size: 22 }     // Amber
    };
    
    return styles[label] || { color: '#94a3b8', size: 18 };
  }
  
  /**
   * Get edge styling based on relationship type.
   * @private
   */
  _getEdgeStyle(type) {
    const styles = {
      'IN_PROJECT': { color: '#cbd5e1', width: 1, dashes: [5, 5] },
      'HAS_CHUNK': { color: '#6366f1', width: 2 },
      'HAS_ENTITY': { color: '#8b5cf6', width: 2 },
      'RELATED': { color: '#ec4899', width: 2 },
      'IN_COMMUNITY': { color: '#f97316', width: 1.5, dashes: [3, 3] }
    };
    
    return styles[type] || { color: '#94a3b8', width: 1 };
  }
  
  /**
   * Build label configuration for Neovis.
   * @private
   */
  _buildLabelConfig() {
    return {
      '__Project__': { label: 'name' },
      '__Entity__': { label: 'title' },
      '__Document__': { label: 'title' },
      '__Chunk__': { label: 'text' },
      '__Community__': { label: 'community' }
    };
  }
  
  /**
   * Build relationship configuration for Neovis.
   * @private
   */
  _buildRelationshipConfig() {
    return {
      'IN_PROJECT': {},
      'HAS_CHUNK': {},
      'HAS_ENTITY': {},
      'RELATED': {},
      'IN_COMMUNITY': {}
    };
  }
  
  /**
   * Create vis.js network from data.
   * @private
   */
  _createNetworkFromData(data, options) {
    const container = document.getElementById(this.containerId);
    
    // Import vis.js from NeoVis (it bundles vis-network)
    const { DataSet } = window.vis || window;
    
    if (!DataSet) {
      throw new Error('vis.js DataSet not found. Ensure Neovis.js is properly loaded.');
    }
    
    const nodes = new DataSet(data.nodes);
    const edges = new DataSet(data.edges);
    
    const network = new window.vis.Network(container, { nodes, edges }, options);
    
    // Add event listeners
    network.on('click', (params) => {
      if (params.nodes.length > 0) {
        console.log('Node clicked:', params.nodes[0]);
      }
    });
    
    network.on('stabilizationIterationsDone', () => {
      network.setOptions({ physics: false });
    });
    
    return network;
  }
  
  /**
   * Show empty state message.
   * @private
   */
  _showEmptyState(container, message) {
    const defaultMessage = 'No graph data available. Upload and process documents to see the knowledge graph.';
    container.innerHTML = `
      <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; color: #64748b; text-align: center; padding: 2rem;">
        <svg style="width: 64px; height: 64px; margin-bottom: 1rem; opacity: 0.5;" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path>
        </svg>
        <h3 style="font-size: 1.125rem; font-weight: 600; margin-bottom: 0.5rem;">No Graph Data</h3>
        <p style="max-width: 400px;">${message || defaultMessage}</p>
      </div>
    `;
  }
  
  /**
   * Add legend to the graph container.
   * @private
   */
  _addLegend(container, stats) {
    const legendHtml = `
      <div style="position: absolute; top: 10px; right: 10px; background: white; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); font-size: 12px; max-width: 250px; z-index: 10;">
        <div style="font-weight: 600; margin-bottom: 8px; color: #1e293b;">Graph Legend</div>
        <div style="display: flex; align-items: center; margin-bottom: 4px;">
          <div style="width: 12px; height: 12px; border-radius: 50%; background: #4f46e5; margin-right: 8px;"></div>
          <span>Project (${stats.node_types['__Project__'] || 0})</span>
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 4px;">
          <div style="width: 12px; height: 12px; border-radius: 50%; background: #10b981; margin-right: 8px;"></div>
          <span>Documents (${stats.node_types['__Document__'] || 0})</span>
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 4px;">
          <div style="width: 12px; height: 12px; border-radius: 50%; background: #64748b; margin-right: 8px;"></div>
          <span>Chunks (${stats.node_types['__Chunk__'] || 0})</span>
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 4px;">
          <div style="width: 12px; height: 12px; border-radius: 50%; background: #a855f7; margin-right: 8px;"></div>
          <span>Entities (${stats.node_types['__Entity__'] || 0})</span>
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 4px;">
          <div style="width: 12px; height: 12px; border-radius: 50%; background: #f59e0b; margin-right: 8px;"></div>
          <span>Communities (${stats.node_types['__Community__'] || 0})</span>
        </div>
        <div style="margin-top: 8px; padding-top: 8px; border-top: 1px solid #e2e8f0; color: #64748b;">
          <div>${stats.node_count} nodes, ${stats.relationship_count} edges</div>
        </div>
      </div>
    `;
    
    const legendDiv = document.createElement('div');
    legendDiv.innerHTML = legendHtml;
    container.appendChild(legendDiv.firstElementChild);
  }
  
  /**
   * Add control buttons to the graph.
   * @private
   */
  _addControls(container, network) {
    const controlsHtml = `
      <div style="position: absolute; bottom: 20px; left: 20px; display: flex; gap: 8px; z-index: 10;">
        <button id="graphZoomIn" class="px-3 py-1.5 bg-white border border-slate-300 rounded hover:bg-slate-50 text-sm font-medium shadow-sm transition-colors" title="Zoom In">🔍+</button>
        <button id="graphZoomOut" class="px-3 py-1.5 bg-white border border-slate-300 rounded hover:bg-slate-50 text-sm font-medium shadow-sm transition-colors" title="Zoom Out">🔍-</button>
        <button id="graphFit" class="px-3 py-1.5 bg-white border border-slate-300 rounded hover:bg-slate-50 text-sm font-medium shadow-sm transition-colors" title="Fit to View">⊡</button>
        <button id="graphPhysics" class="px-3 py-1.5 bg-white border border-slate-300 rounded hover:bg-slate-50 text-sm font-medium shadow-sm transition-colors" title="Toggle Physics">⚡</button>
      </div>
    `;
    
    const controlsDiv = document.createElement('div');
    controlsDiv.innerHTML = controlsHtml;
    container.appendChild(controlsDiv.firstElementChild);
    
    // Add event listeners
    let physicsEnabled = false;
    
    document.getElementById('graphZoomIn')?.addEventListener('click', () => {
      const scale = network.getScale();
      network.moveTo({ scale: scale * 1.2 });
    });
    
    document.getElementById('graphZoomOut')?.addEventListener('click', () => {
      const scale = network.getScale();
      network.moveTo({ scale: scale * 0.8 });
    });
    
    document.getElementById('graphFit')?.addEventListener('click', () => {
      network.fit({ animation: { duration: 500 } });
    });
    
    document.getElementById('graphPhysics')?.addEventListener('click', () => {
      physicsEnabled = !physicsEnabled;
      network.setOptions({ physics: physicsEnabled });
    });
  }
  
  /**
   * Destroy the visualization and clean up resources.
   */
  destroy() {
    if (this.viz) {
      // Clean up if needed
      this.viz = null;
    }
    this.currentProjectId = null;
  }
}

