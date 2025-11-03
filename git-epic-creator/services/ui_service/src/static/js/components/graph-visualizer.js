/**
 * Premium Graph Visualizer Component
 * 
 * High-UX graph visualization using D3.js for project knowledge graphs.
 * Features smooth animations, advanced interactions, and beautiful design.
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
    this.currentProjectId = null;
    
    // D3 visualization elements
    this.svg = null;
    this.simulation = null;
    this.nodes = [];
    this.links = [];
    this.selectedNode = null;
    this.searchTerm = '';
    
    // Check if D3 is loaded
    if (typeof d3 === 'undefined') {
      throw new Error('D3.js library not loaded. Include <script src="https://d3js.org/d3.v7.min.js"></script>');
    }
    
    // Color palette for node types
    this.colorPalette = {
      '__Project__': { primary: '#6366f1', secondary: '#818cf8', gradient: ['#4f46e5', '#6366f1'] },
      '__Document__': { primary: '#10b981', secondary: '#34d399', gradient: ['#059669', '#10b981'] },
      '__Chunk__': { primary: '#64748b', secondary: '#94a3b8', gradient: ['#475569', '#64748b'] },
      '__Entity__': { primary: '#a855f7', secondary: '#c084fc', gradient: ['#9333ea', '#a855f7'] },
      '__Community__': { primary: '#f59e0b', secondary: '#fbbf24', gradient: ['#d97706', '#f59e0b'] }
    };
    
    // Node sizes
    this.nodeSizes = {
      '__Project__': 40,
      '__Document__': 32,
      '__Chunk__': 20,
      '__Entity__': 28,
      '__Community__': 35
    };
  }
  
  /**
   * Render graph from data with premium UX.
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
      
      // Process data
      this.nodes = this._processNodes(graphData.nodes);
      this.links = this._processLinks(graphData.relationships, this.nodes);
      
      // Detect and assign communities for clustering
      this._detectCommunities();
      
      // Create visualization container
      this._createVisualization(container);
      
      // Add premium UI controls
      this._addPremiumControls(container, graphData.stats);
      
      // Initialize force simulation
      this._initializeSimulation();
      
      // Render graph
      this._renderGraph();
      
      console.log('Premium graph rendered:', {
        nodes: this.nodes.length,
        relationships: this.links.length,
        types: graphData.stats.node_types
      });
      
    } catch (error) {
      console.error('Failed to render graph:', error);
      this.onError(error);
      throw error;
    }
  }
  
  /**
   * Process nodes with enhanced data including community assignment.
   * @private
   */
  _processNodes(rawNodes) {
    return rawNodes.map(node => ({
      id: node.id,
      label: node.label,
      properties: node.properties || {},
      displayName: this._getNodeDisplayName(node),
      color: this.colorPalette[node.label]?.primary || '#94a3b8',
      gradient: this.colorPalette[node.label]?.gradient || ['#94a3b8', '#cbd5e1'],
      size: this.nodeSizes[node.label] || 24,
      // Community assignment for clustering (will be set later)
      community: null,
      // D3 force simulation properties
      x: Math.random() * 800,
      y: Math.random() * 600,
      vx: 0,
      vy: 0
    }));
  }
  
  /**
   * Process links with enhanced data.
   * @private
   */
  _processLinks(rawLinks, nodes) {
    const nodeMap = new Map(nodes.map(n => [n.id, n]));
    
    return rawLinks
      .filter(link => nodeMap.has(link.source) && nodeMap.has(link.target))
      .map(link => ({
        source: nodeMap.get(link.source),
        target: nodeMap.get(link.target),
        type: link.type,
        color: this._getLinkColor(link.type),
        width: this._getLinkWidth(link.type)
      }));
  }
  
  /**
   * Get display name for node.
   * @private
   */
  _getNodeDisplayName(node) {
    const props = node.properties || {};
    if (props.name) return props.name;
    if (props.title) return props.title;
    if (props.text && props.text.length < 40) return props.text;
    
    const type = node.label.replace(/__/g, '');
    return `${type} ${node.id.substring(0, 6)}`;
  }
  
  /**
   * Detect communities and assign nodes to clusters.
   * Uses IN_COMMUNITY relationships from Neo4j or falls back to connectivity-based clustering.
   * @private
   */
  _detectCommunities() {
    // Create a map of node ID to node object
    const nodeMap = new Map(this.nodes.map(n => [n.id, n]));
    
    // First, try to use explicit __Community__ nodes from Neo4j
    const communityNodes = this.nodes.filter(n => n.label === '__Community__');
    
    if (communityNodes.length > 0) {
      // Use Neo4j community assignments
      this.links.forEach(link => {
        if (link.type === 'IN_COMMUNITY') {
          const sourceNode = nodeMap.get(link.source.id || link.source);
          const targetNode = nodeMap.get(link.target.id || link.target);
          
          if (sourceNode && targetNode && targetNode.label === '__Community__') {
            // Assign community ID from the community node
            const communityId = targetNode.properties.community || targetNode.id;
            sourceNode.community = communityId;
          }
        }
      });
    } else {
      // Fallback: Use simple connectivity-based clustering
      this._assignConnectivityBasedCommunities();
    }
    
    // Assign default community to nodes without one
    this.nodes.forEach(node => {
      if (node.community === null) {
        // Assign based on node type for basic grouping
        node.community = node.label;
      }
    });
    
    console.log('Community detection complete:', {
      totalNodes: this.nodes.length,
      communitiesFound: new Set(this.nodes.map(n => n.community)).size
    });
  }
  
  /**
   * Assign communities based on graph connectivity (fallback method).
   * @private
   */
  _assignConnectivityBasedCommunities() {
    const visited = new Set();
    let communityId = 0;
    
    // Build adjacency list
    const adjacency = new Map();
    this.nodes.forEach(node => adjacency.set(node.id, []));
    
    this.links.forEach(link => {
      const sourceId = link.source.id || link.source;
      const targetId = link.target.id || link.target;
      
      if (adjacency.has(sourceId)) adjacency.get(sourceId).push(targetId);
      if (adjacency.has(targetId)) adjacency.get(targetId).push(sourceId);
    });
    
    // BFS to find connected components
    const bfs = (startId) => {
      const queue = [startId];
      const component = [];
      visited.add(startId);
      
      while (queue.length > 0) {
        const nodeId = queue.shift();
        component.push(nodeId);
        
        const neighbors = adjacency.get(nodeId) || [];
        neighbors.forEach(neighborId => {
          if (!visited.has(neighborId)) {
            visited.add(neighborId);
            queue.push(neighborId);
          }
        });
      }
      
      return component;
    };
    
    // Assign communities
    this.nodes.forEach(node => {
      if (!visited.has(node.id)) {
        const component = bfs(node.id);
        component.forEach(nodeId => {
          const n = this.nodes.find(x => x.id === nodeId);
          if (n) n.community = `cluster_${communityId}`;
        });
        communityId++;
      }
    });
  }
  
  /**
   * Get link color based on type.
   * @private
   */
  _getLinkColor(type) {
    const colors = {
      'IN_PROJECT': '#e2e8f0',
      'HAS_CHUNK': '#6366f1',
      'HAS_ENTITY': '#8b5cf6',
      'RELATED': '#ec4899',
      'IN_COMMUNITY': '#f97316'
    };
    return colors[type] || '#cbd5e1';
  }
  
  /**
   * Get link width based on type.
   * @private
   */
  _getLinkWidth(type) {
    const widths = {
      'IN_PROJECT': 1.5,
      'HAS_CHUNK': 2,
      'HAS_ENTITY': 2,
      'RELATED': 2.5,
      'IN_COMMUNITY': 2
    };
    return widths[type] || 1.5;
  }
  
  /**
   * Create SVG visualization container.
   * @private
   */
  _createVisualization(container) {
    const rect = container.getBoundingClientRect();
    const width = rect.width;
    const height = rect.height;
    
    // Create SVG with gradient definitions
    this.svg = d3.select(container)
      .append('svg')
      .attr('width', '100%')
      .attr('height', '100%')
      .attr('viewBox', `0 0 ${width} ${height}`)
      .style('background', 'linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%)');
    
    // Add gradient definitions
    const defs = this.svg.append('defs');
    
    Object.entries(this.colorPalette).forEach(([type, colors]) => {
      const gradient = defs.append('radialGradient')
        .attr('id', `gradient-${type}`)
        .attr('cx', '30%')
        .attr('cy', '30%');
      
      gradient.append('stop')
        .attr('offset', '0%')
        .attr('stop-color', colors.gradient[1])
        .attr('stop-opacity', 1);
      
      gradient.append('stop')
        .attr('offset', '100%')
        .attr('stop-color', colors.gradient[0])
        .attr('stop-opacity', 1);
    });
    
    // Add drop shadow filter
    const filter = defs.append('filter')
      .attr('id', 'drop-shadow')
      .attr('height', '130%');
    
    filter.append('feGaussianBlur')
      .attr('in', 'SourceAlpha')
      .attr('stdDeviation', 3);
    
    filter.append('feOffset')
      .attr('dx', 0)
      .attr('dy', 2)
      .attr('result', 'offsetblur');
    
    filter.append('feComponentTransfer')
      .append('feFuncA')
      .attr('type', 'linear')
      .attr('slope', 0.3);
    
    const feMerge = filter.append('feMerge');
    feMerge.append('feMergeNode');
    feMerge.append('feMergeNode').attr('in', 'SourceGraphic');
    
    // Create main group for zoom/pan
    this.mainGroup = this.svg.append('g').attr('class', 'main-group');
    
    // Add zoom behavior with smooth transitions
    const zoom = d3.zoom()
      .scaleExtent([0.1, 4])
      .on('zoom', (event) => {
        this.mainGroup.attr('transform', event.transform);
      });
    
    this.svg.call(zoom);
    
    // Store dimensions
    this.width = width;
    this.height = height;
  }
  
  /**
   * Initialize layout - static hierarchical or physics-based.
   * @private
   */
  _initializeSimulation() {
    // Use static layout by default (physics disabled)
    this.physicsEnabled = false;
    
    if (this.physicsEnabled) {
      this._initializePhysicsSimulation();
    } else {
      this._initializeStaticLayout();
    }
  }
  
  /**
   * Initialize static hierarchical layout (Neo4j browser style).
   * @private
   */
  _initializeStaticLayout() {
    // Create hierarchical layout based on node types
    const centerX = this.width / 2;
    const centerY = this.height / 2;
    
    // Group nodes by type and community
    const nodesByType = {
      '__Project__': [],
      '__Community__': [],
      '__Document__': [],
      '__Entity__': [],
      '__Chunk__': []
    };
    
    this.nodes.forEach(node => {
      if (nodesByType[node.label]) {
        nodesByType[node.label].push(node);
      }
    });
    
    // Layout strategy: Radial layout with hierarchy
    // Center: Project node
    // Inner ring: Communities
    // Middle ring: Documents and Entities
    // Outer ring: Chunks
    
    const projectNodes = nodesByType['__Project__'];
    const communityNodes = nodesByType['__Community__'];
    const documentNodes = nodesByType['__Document__'];
    const entityNodes = nodesByType['__Entity__'];
    const chunkNodes = nodesByType['__Chunk__'];
    
    // Position project node at center
    projectNodes.forEach(node => {
      node.x = centerX;
      node.y = centerY;
      node.fx = centerX;
      node.fy = centerY;
    });
    
    // Position communities in inner ring
    const communityRadius = Math.min(this.width, this.height) * 0.15;
    communityNodes.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / Math.max(communityNodes.length, 1);
      node.x = centerX + communityRadius * Math.cos(angle);
      node.y = centerY + communityRadius * Math.sin(angle);
      node.fx = node.x;
      node.fy = node.y;
    });
    
    // Group entities and chunks by their community
    const nodesByCommunity = new Map();
    
    // Find which nodes belong to which community via IN_COMMUNITY links
    this.links.forEach(link => {
      if (link.type === 'IN_COMMUNITY') {
        const sourceId = link.source.id || link.source;
        const targetId = link.target.id || link.target;
        const sourceNode = this.nodes.find(n => n.id === sourceId);
        const targetNode = this.nodes.find(n => n.id === targetId);
        
        if (sourceNode && targetNode && targetNode.label === '__Community__') {
          if (!nodesByCommunity.has(targetNode.id)) {
            nodesByCommunity.set(targetNode.id, []);
          }
          nodesByCommunity.get(targetNode.id).push(sourceNode);
        }
      }
    });
    
    // Position documents in middle ring
    const documentRadius = Math.min(this.width, this.height) * 0.28;
    documentNodes.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / Math.max(documentNodes.length, 1);
      node.x = centerX + documentRadius * Math.cos(angle);
      node.y = centerY + documentRadius * Math.sin(angle);
      node.fx = node.x;
      node.fy = node.y;
    });
    
    // Position entities around their communities or in middle ring
    const entityRadius = Math.min(this.width, this.height) * 0.32;
    const positionedEntities = new Set();
    
    // First, position entities that belong to communities
    communityNodes.forEach((commNode, commIndex) => {
      const communityMembers = nodesByCommunity.get(commNode.id) || [];
      const entities = communityMembers.filter(n => n.label === '__Entity__');
      
      if (entities.length > 0) {
        const commAngle = (2 * Math.PI * commIndex) / Math.max(communityNodes.length, 1);
        const localRadius = 80;
        
        entities.forEach((entity, i) => {
          const localAngle = commAngle + (2 * Math.PI * i) / entities.length;
          entity.x = commNode.x + localRadius * Math.cos(localAngle);
          entity.y = commNode.y + localRadius * Math.sin(localAngle);
          entity.fx = entity.x;
          entity.fy = entity.y;
          positionedEntities.add(entity.id);
        });
      }
    });
    
    // Position remaining entities without community
    const unpositionedEntities = entityNodes.filter(n => !positionedEntities.has(n.id));
    unpositionedEntities.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / Math.max(unpositionedEntities.length, 1);
      node.x = centerX + entityRadius * Math.cos(angle);
      node.y = centerY + entityRadius * Math.sin(angle);
      node.fx = node.x;
      node.fy = node.y;
    });
    
    // Position chunks in outer ring around their communities or documents
    const chunkRadius = Math.min(this.width, this.height) * 0.42;
    const positionedChunks = new Set();
    
    // Position chunks that belong to communities
    communityNodes.forEach((commNode, commIndex) => {
      const communityMembers = nodesByCommunity.get(commNode.id) || [];
      const chunks = communityMembers.filter(n => n.label === '__Chunk__');
      
      if (chunks.length > 0) {
        const commAngle = (2 * Math.PI * commIndex) / Math.max(communityNodes.length, 1);
        const localRadius = 120;
        
        chunks.forEach((chunk, i) => {
          const localAngle = commAngle + (2 * Math.PI * i) / chunks.length;
          chunk.x = commNode.x + localRadius * Math.cos(localAngle);
          chunk.y = commNode.y + localRadius * Math.sin(localAngle);
          chunk.fx = chunk.x;
          chunk.fy = chunk.y;
          positionedChunks.add(chunk.id);
        });
      }
    });
    
    // Position remaining chunks without community
    const unpositionedChunks = chunkNodes.filter(n => !positionedChunks.has(n.id));
    unpositionedChunks.forEach((node, i) => {
      const angle = (2 * Math.PI * i) / Math.max(unpositionedChunks.length, 1);
      node.x = centerX + chunkRadius * Math.cos(angle);
      node.y = centerY + chunkRadius * Math.sin(angle);
      node.fx = node.x;
      node.fy = node.y;
    });
    
    // Create a dummy simulation that doesn't move nodes (for compatibility)
    this.simulation = d3.forceSimulation(this.nodes)
      .force('link', d3.forceLink(this.links).id(d => d.id).strength(0))
      .alphaTarget(0)
      .alpha(0)
      .stop();
  }
  
  /**
   * Initialize physics-based force simulation with community-based clustering.
   * @private
   */
  _initializePhysicsSimulation() {
    // Calculate community centers for clustering
    const communities = new Set(this.nodes.map(n => n.community));
    const communityCount = communities.size;
    const clusterCenters = new Map();
    
    // Arrange clusters in a circular pattern
    const radius = Math.min(this.width, this.height) * 0.35;
    const centerX = this.width / 2;
    const centerY = this.height / 2;
    
    Array.from(communities).forEach((community, i) => {
      const angle = (2 * Math.PI * i) / communityCount;
      clusterCenters.set(community, {
        x: centerX + radius * Math.cos(angle),
        y: centerY + radius * Math.sin(angle)
      });
    });
    
    // Custom clustering force to pull nodes toward their community center
    const clusterForce = () => {
      const alpha = this.simulation.alpha();
      const strength = 0.15 * alpha; // Adaptive strength
      
      this.nodes.forEach(node => {
        const center = clusterCenters.get(node.community);
        if (center) {
          node.vx += (center.x - node.x) * strength;
          node.vy += (center.y - node.y) * strength;
        }
      });
    };
    
    this.simulation = d3.forceSimulation(this.nodes)
      .force('link', d3.forceLink(this.links)
        .id(d => d.id)
        .distance(d => {
          // Shorter links within same community, longer between communities
          const source = d.source;
          const target = d.target;
          const sameCommunity = source.community === target.community;
          return sameCommunity ? 60 : 150;
        })
        .strength(d => {
          // Stronger links within same community
          const source = d.source;
          const target = d.target;
          const sameCommunity = source.community === target.community;
          return sameCommunity ? 0.8 : 0.3;
        }))
      .force('charge', d3.forceManyBody()
        .strength(d => {
          // Community nodes have stronger repulsion
          return d.label === '__Community__' ? -1200 : -600;
        })
        .distanceMax(250))
      .force('center', d3.forceCenter(this.width / 2, this.height / 2))
      .force('collision', d3.forceCollide()
        .radius(d => d.size + 15)
        .strength(0.8))
      .force('cluster', clusterForce)
      .alphaDecay(0.015)
      .velocityDecay(0.4);
  }
  
  /**
   * Render the graph with premium visuals including cluster boundaries.
   * @private
   */
  _renderGraph() {
    // Create cluster hulls (visual boundaries for communities)
    this.hullGroup = this.mainGroup.append('g').attr('class', 'hulls');
    this._renderClusterHulls(this.hullGroup);
    
    // Create link elements
    const linkGroup = this.mainGroup.append('g').attr('class', 'links');
    
    const linkElements = linkGroup.selectAll('line')
      .data(this.links)
      .enter()
      .append('line')
      .attr('stroke', d => d.color)
      .attr('stroke-width', d => d.width)
      .attr('stroke-opacity', d => {
        // Make IN_COMMUNITY links more visible
        return d.type === 'IN_COMMUNITY' ? 0.8 : 0.6;
      })
      .attr('stroke-dasharray', d => {
        // Dashed lines for IN_COMMUNITY relationships
        return d.type === 'IN_COMMUNITY' ? '5,5' : 'none';
      })
      .attr('class', 'graph-link');
    
    // Create node group
    const nodeGroup = this.mainGroup.append('g').attr('class', 'nodes');
    
    const nodeElements = nodeGroup.selectAll('g')
      .data(this.nodes)
      .enter()
      .append('g')
      .attr('class', 'graph-node')
      .call(this._createDragBehavior());
    
    // Add circles with gradients
    nodeElements.append('circle')
      .attr('r', d => d.size)
      .attr('fill', d => `url(#gradient-${d.label})`)
      .attr('stroke', '#fff')
      .attr('stroke-width', 3)
      .attr('filter', 'url(#drop-shadow)')
      .style('cursor', 'pointer')
      .style('transition', 'all 0.3s ease');
    
    // Add labels with background
    const labelGroups = nodeElements.append('g')
      .attr('class', 'node-label')
      .style('pointer-events', 'none');
    
    labelGroups.append('text')
      .attr('dy', d => d.size + 20)
      .attr('text-anchor', 'middle')
      .style('font-size', '11px')
      .style('font-weight', '600')
      .style('fill', '#1e293b')
      .style('paint-order', 'stroke')
      .style('stroke', '#fff')
      .style('stroke-width', '3px')
      .style('stroke-linecap', 'round')
      .style('stroke-linejoin', 'round')
      .text(d => d.displayName.length > 20 ? d.displayName.substring(0, 20) + '...' : d.displayName);
    
    // Add interactive tooltips
    const tooltip = this._createTooltip();
    
    nodeElements
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget).select('circle')
          .transition()
          .duration(200)
          .attr('r', d.size * 1.2)
          .attr('stroke-width', 4);
        
        this._showTooltip(tooltip, event, d);
      })
      .on('mouseout', (event, d) => {
        if (this.selectedNode !== d) {
          d3.select(event.currentTarget).select('circle')
            .transition()
            .duration(200)
            .attr('r', d.size)
            .attr('stroke-width', 3);
        }
        
        this._hideTooltip(tooltip);
      })
      .on('click', (event, d) => {
        this._handleNodeClick(event, d, nodeElements);
      });
    
    // Update positions on simulation tick (or immediately for static layout)
    const updatePositions = () => {
      // Update cluster hulls
      this._updateClusterHulls(this.hullGroup);
      
      linkElements
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);
      
      nodeElements.attr('transform', d => `translate(${d.x},${d.y})`);
    };
    
    this.simulation.on('tick', updatePositions);
    
    // For static layout, render immediately
    if (!this.physicsEnabled) {
      updatePositions();
    }
    
    // Store elements for later use
    this.nodeElements = nodeElements;
    this.linkElements = linkElements;
  }
  
  /**
   * Render cluster hulls (visual boundaries for communities).
   * @private
   */
  _renderClusterHulls(hullGroup) {
    // Group nodes by community
    const communities = new Map();
    this.nodes.forEach(node => {
      if (!communities.has(node.community)) {
        communities.set(node.community, []);
      }
      communities.get(node.community).push(node);
    });
    
    // Create hull paths for each community with 3+ nodes
    const hullData = Array.from(communities.entries())
      .filter(([_, nodes]) => nodes.length >= 3)
      .map(([community, nodes]) => ({ community, nodes }));
    
    this.hullPaths = hullGroup.selectAll('path')
      .data(hullData)
      .enter()
      .append('path')
      .attr('class', 'cluster-hull')
      .attr('fill', (d, i) => {
        // Generate subtle colors for clusters
        const hue = (i * 137.5) % 360; // Golden angle for color distribution
        return `hsla(${hue}, 60%, 85%, 0.3)`;
      })
      .attr('stroke', (d, i) => {
        const hue = (i * 137.5) % 360;
        return `hsla(${hue}, 60%, 70%, 0.5)`;
      })
      .attr('stroke-width', 2)
      .attr('stroke-dasharray', '8,4')
      .style('pointer-events', 'none');
  }
  
  /**
   * Update cluster hulls on each tick.
   * @private
   */
  _updateClusterHulls(hullGroup) {
    if (!this.hullPaths) return;
    
    this.hullPaths.attr('d', d => {
      const points = d.nodes.map(n => [n.x, n.y]);
      if (points.length < 3) return null;
      
      // Calculate convex hull using gift wrapping algorithm
      const hull = this._convexHull(points);
      if (hull.length < 3) return null;
      
      // Expand hull slightly for padding
      const centroid = hull.reduce((acc, p) => [acc[0] + p[0], acc[1] + p[1]], [0, 0])
        .map(v => v / hull.length);
      
      const expandedHull = hull.map(p => {
        const dx = p[0] - centroid[0];
        const dy = p[1] - centroid[1];
        const dist = Math.sqrt(dx * dx + dy * dy);
        const padding = 40; // Padding around nodes
        return [
          p[0] + (dx / dist) * padding,
          p[1] + (dy / dist) * padding
        ];
      });
      
      // Create smooth curve through hull points
      return this._smoothHullPath(expandedHull);
    });
  }
  
  /**
   * Calculate convex hull using gift wrapping algorithm.
   * @private
   */
  _convexHull(points) {
    if (points.length < 3) return points;
    
    // Find leftmost point
    let leftmost = 0;
    for (let i = 1; i < points.length; i++) {
      if (points[i][0] < points[leftmost][0]) leftmost = i;
    }
    
    const hull = [];
    let current = leftmost;
    
    do {
      hull.push(points[current]);
      let next = (current + 1) % points.length;
      
      for (let i = 0; i < points.length; i++) {
        const cross = (points[next][0] - points[current][0]) * (points[i][1] - points[current][1]) -
                      (points[next][1] - points[current][1]) * (points[i][0] - points[current][0]);
        if (cross < 0) next = i;
      }
      
      current = next;
    } while (current !== leftmost && hull.length < points.length);
    
    return hull;
  }
  
  /**
   * Create smooth path through hull points using cardinal spline.
   * @private
   */
  _smoothHullPath(points) {
    if (points.length < 3) return null;
    
    // Close the path
    const closed = [...points, points[0]];
    
    // Use D3's curve generator for smooth paths
    const line = d3.line()
      .curve(d3.curveCatmullRomClosed.alpha(0.5));
    
    return line(closed);
  }
  
  /**
   * Create drag behavior for nodes.
   * @private
   */
  _createDragBehavior() {
    return d3.drag()
      .on('start', (event, d) => {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on('drag', (event, d) => {
        d.fx = event.x;
        d.fy = event.y;
      })
      .on('end', (event, d) => {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      });
  }
  
  /**
   * Create premium tooltip.
   * @private
   */
  _createTooltip() {
    return d3.select(`#${this.containerId}`)
      .append('div')
      .attr('class', 'graph-tooltip')
      .style('position', 'absolute')
      .style('visibility', 'hidden')
      .style('background', 'rgba(15, 23, 42, 0.95)')
      .style('backdrop-filter', 'blur(10px)')
      .style('color', '#fff')
      .style('padding', '16px')
      .style('border-radius', '12px')
      .style('font-size', '13px')
      .style('box-shadow', '0 20px 25px -5px rgba(0, 0, 0, 0.3)')
      .style('pointer-events', 'none')
      .style('z-index', '1000')
      .style('max-width', '300px')
      .style('transition', 'all 0.2s ease');
  }
  
  /**
   * Show tooltip with node details.
   * @private
   */
  _showTooltip(tooltip, event, node) {
    const props = node.properties;
    const type = node.label.replace(/__/g, '');
    
    let html = `<div style="font-weight: 700; font-size: 14px; margin-bottom: 8px; color: ${this.colorPalette[node.label]?.secondary || '#fff'}">${type}</div>`;
    html += `<div style="opacity: 0.7; font-size: 11px; margin-bottom: 8px; font-family: monospace">${node.id.substring(0, 16)}...</div>`;
    
    if (type === 'Project' && props.name) {
      html += `<div style="margin-top: 8px"><strong>Name:</strong> ${props.name}</div>`;
      if (props.description) html += `<div style="margin-top: 4px; opacity: 0.8">${props.description.substring(0, 100)}...</div>`;
    } else if (type === 'Entity' && props.title) {
      html += `<div style="margin-top: 8px"><strong>Title:</strong> ${props.title}</div>`;
      if (props.type) html += `<div style="margin-top: 4px"><strong>Type:</strong> ${props.type}</div>`;
    } else if (type === 'Document' && props.title) {
      html += `<div style="margin-top: 8px"><strong>Title:</strong> ${props.title}</div>`;
      if (props.source) html += `<div style="margin-top: 4px; opacity: 0.8">${props.source}</div>`;
    } else if (type === 'Chunk' && props.text) {
      html += `<div style="margin-top: 8px; opacity: 0.9">${props.text.substring(0, 150)}...</div>`;
    } else if (type === 'Community') {
      if (props.community !== undefined) html += `<div style="margin-top: 8px"><strong>Community:</strong> ${props.community}</div>`;
      if (props.level !== undefined) html += `<div style="margin-top: 4px"><strong>Level:</strong> ${props.level}</div>`;
    }
    
    tooltip.html(html)
      .style('visibility', 'visible')
      .style('left', (event.pageX + 15) + 'px')
      .style('top', (event.pageY - 15) + 'px');
  }
  
  /**
   * Hide tooltip.
   * @private
   */
  _hideTooltip(tooltip) {
    tooltip.style('visibility', 'hidden');
  }
  
  /**
   * Handle node click event.
   * @private
   */
  _handleNodeClick(event, node, nodeElements) {
    // Reset previous selection
    if (this.selectedNode) {
      nodeElements.filter(d => d === this.selectedNode)
        .select('circle')
        .transition()
        .duration(200)
        .attr('r', this.selectedNode.size)
        .attr('stroke-width', 3);
    }
    
    // Set new selection
    this.selectedNode = node;
    
    d3.select(event.currentTarget).select('circle')
      .transition()
      .duration(200)
      .attr('r', node.size * 1.3)
      .attr('stroke-width', 5);
    
    // Highlight connected nodes
    this._highlightConnectedNodes(node);
    
    console.log('Node selected:', node);
  }
  
  /**
   * Highlight nodes connected to selected node.
   * @private
   */
  _highlightConnectedNodes(node) {
    const connectedNodeIds = new Set();
    
    this.links.forEach(link => {
      if (link.source.id === node.id) connectedNodeIds.add(link.target.id);
      if (link.target.id === node.id) connectedNodeIds.add(link.source.id);
    });
    
    this.nodeElements.style('opacity', d => {
      return d.id === node.id || connectedNodeIds.has(d.id) ? 1 : 0.3;
    });
    
    this.linkElements.style('opacity', d => {
      return d.source.id === node.id || d.target.id === node.id ? 0.8 : 0.1;
    });
  }
  
  /**
   * Reset graph highlighting.
   * @private
   */
  _resetHighlight() {
    this.nodeElements.style('opacity', 1);
    this.linkElements.style('opacity', 0.6);
    this.selectedNode = null;
  }
  
  /**
   * Add premium UI controls.
   * @private
   */
  _addPremiumControls(container, stats) {
    const controlsDiv = document.createElement('div');
    controlsDiv.style.cssText = 'position: absolute; top: 16px; left: 16px; display: flex; flex-direction: column; gap: 12px; z-index: 100;';
    
    // Search box
    const searchBox = this._createSearchBox();
    controlsDiv.appendChild(searchBox);
    
    // Control panel
    const controlPanel = this._createControlPanel();
    controlsDiv.appendChild(controlPanel);
    
    // Legend
    const legend = this._createLegend(stats);
    container.appendChild(legend);
    
    container.appendChild(controlsDiv);
  }
  
  /**
   * Create search box.
   * @private
   */
  _createSearchBox() {
    const searchContainer = document.createElement('div');
    searchContainer.style.cssText = 'background: rgba(255, 255, 255, 0.95); backdrop-filter: blur(10px); border-radius: 12px; padding: 12px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);';
    
    searchContainer.innerHTML = `
      <div style="display: flex; align-items: center; gap: 8px;">
        <svg style="width: 18px; height: 18px; color: #64748b;" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path>
        </svg>
        <input 
          id="graphSearch" 
          type="text" 
          placeholder="Search nodes..." 
          style="border: none; outline: none; background: transparent; font-size: 14px; width: 200px; color: #1e293b;"
        />
      </div>
    `;
    
    const input = searchContainer.querySelector('#graphSearch');
    input.addEventListener('input', (e) => {
      this._handleSearch(e.target.value);
    });
    
    return searchContainer;
  }
  
  /**
   * Create control panel.
   * @private
   */
  _createControlPanel() {
    const panel = document.createElement('div');
    panel.style.cssText = 'background: rgba(255, 255, 255, 0.95); backdrop-filter: blur(10px); border-radius: 12px; padding: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); display: flex; gap: 8px;';
    
    const buttons = [
      { id: 'fitView', icon: '⊡', title: 'Fit to View', action: () => this._fitToView() },
      { id: 'resetGraph', icon: '↻', title: 'Reset', action: () => this._resetGraph() }
    ];
    
    buttons.forEach(btn => {
      const button = document.createElement('button');
      button.innerHTML = btn.icon;
      button.title = btn.title;
      button.style.cssText = 'width: 36px; height: 36px; border: none; background: transparent; border-radius: 8px; cursor: pointer; font-size: 16px; display: flex; align-items: center; justify-content: center; transition: all 0.2s;';
      button.onmouseover = () => button.style.background = '#f1f5f9';
      button.onmouseout = () => button.style.background = 'transparent';
      button.onclick = btn.action;
      panel.appendChild(button);
    });
    
    // Physics toggle button with state indicator
    const physicsButton = document.createElement('button');
    physicsButton.id = 'togglePhysicsBtn';
    this.physicsButton = physicsButton;
    
    const updatePhysicsButton = () => {
      if (this.physicsEnabled) {
        physicsButton.innerHTML = '⏸';
        physicsButton.title = 'Disable Physics (Static Layout)';
        physicsButton.style.background = '#dbeafe';
      } else {
        physicsButton.innerHTML = '▶';
        physicsButton.title = 'Enable Physics (Dynamic Layout)';
        physicsButton.style.background = 'transparent';
      }
    };
    
    physicsButton.style.cssText = 'width: 36px; height: 36px; border: none; border-radius: 8px; cursor: pointer; font-size: 16px; display: flex; align-items: center; justify-content: center; transition: all 0.2s;';
    physicsButton.onmouseover = () => {
      if (!this.physicsEnabled) {
        physicsButton.style.background = '#f1f5f9';
      }
    };
    physicsButton.onmouseout = () => {
      updatePhysicsButton();
    };
    physicsButton.onclick = () => {
      this._togglePhysics();
      updatePhysicsButton();
    };
    
    updatePhysicsButton();
    panel.appendChild(physicsButton);
    
    return panel;
  }
  
  /**
   * Create legend.
   * @private
   */
  _createLegend(stats) {
    const legend = document.createElement('div');
    legend.style.cssText = 'position: absolute; top: 16px; right: 16px; background: rgba(255, 255, 255, 0.95); backdrop-filter: blur(10px); border-radius: 12px; padding: 16px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); font-size: 13px; max-width: 250px; z-index: 100;';
    
    const nodeTypes = stats.node_types || {};
    
    let html = '<div style="font-weight: 700; margin-bottom: 12px; color: #1e293b;">Graph Legend</div>';
    
    Object.entries(this.colorPalette).forEach(([type, colors]) => {
      const count = nodeTypes[type] || 0;
      const displayName = type.replace(/__/g, '');
      html += `
        <div style="display: flex; align-items: center; gap: 10px; margin-bottom: 8px;">
          <div style="width: 16px; height: 16px; border-radius: 50%; background: ${colors.primary}; border: 2px solid white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"></div>
          <span style="flex: 1; color: #475569;">${displayName}</span>
          <span style="font-weight: 600; color: #1e293b;">${count}</span>
        </div>
      `;
    });
    
    html += `
      <div style="margin-top: 12px; padding-top: 12px; border-top: 2px solid #e2e8f0; color: #64748b; font-size: 12px;">
        <div style="display: flex; justify-content: space-between;">
          <span>Total Nodes:</span>
          <span style="font-weight: 600;">${stats.node_count || 0}</span>
        </div>
        <div style="display: flex; justify-content: space-between; margin-top: 4px;">
          <span>Total Edges:</span>
          <span style="font-weight: 600;">${stats.relationship_count || 0}</span>
        </div>
      </div>
    `;
    
    legend.innerHTML = html;
    return legend;
  }
  
  /**
   * Handle search functionality.
   * @private
   */
  _handleSearch(term) {
    this.searchTerm = term.toLowerCase();
    
    if (!term) {
      this._resetHighlight();
      return;
    }
    
    this.nodeElements.style('opacity', d => {
      const match = d.displayName.toLowerCase().includes(this.searchTerm) ||
                    d.id.toLowerCase().includes(this.searchTerm);
      return match ? 1 : 0.2;
    });
    
    this.linkElements.style('opacity', 0.2);
  }
  
  /**
   * Fit view to show all nodes.
   * @private
   */
  _fitToView() {
    const bounds = this.mainGroup.node().getBBox();
    const fullWidth = this.width;
    const fullHeight = this.height;
    const width = bounds.width;
    const height = bounds.height;
    const midX = bounds.x + width / 2;
    const midY = bounds.y + height / 2;
    
    const scale = 0.85 / Math.max(width / fullWidth, height / fullHeight);
    const translate = [fullWidth / 2 - scale * midX, fullHeight / 2 - scale * midY];
    
    this.svg.transition()
      .duration(750)
      .call(
        d3.zoom().transform,
        d3.zoomIdentity.translate(translate[0], translate[1]).scale(scale)
      );
  }
  
  /**
   * Reset graph to initial state.
   * @private
   */
  _resetGraph() {
    this._resetHighlight();
    this.simulation.alpha(1).restart();
    
    this.svg.transition()
      .duration(750)
      .call(d3.zoom().transform, d3.zoomIdentity);
  }
  
  /**
   * Toggle physics simulation.
   * @private
   */
  _togglePhysics() {
    this.physicsEnabled = !this.physicsEnabled;
    
    if (this.physicsEnabled) {
      // Switch to physics-based layout
      console.log('Enabling physics simulation...');
      
      // Remove fixed positions
      this.nodes.forEach(node => {
        node.fx = null;
        node.fy = null;
      });
      
      // Reinitialize with physics
      if (this.simulation) {
        this.simulation.stop();
      }
      this._initializePhysicsSimulation();
      
      // Restart rendering
      this.simulation.on('tick', () => {
        this._updateClusterHulls(this.hullGroup);
        
        this.linkElements
          .attr('x1', d => d.source.x)
          .attr('y1', d => d.source.y)
          .attr('x2', d => d.target.x)
          .attr('y2', d => d.target.y);
        
        this.nodeElements.attr('transform', d => `translate(${d.x},${d.y})`);
      });
      
      this.simulation.alpha(1).restart();
    } else {
      // Switch to static layout
      console.log('Disabling physics simulation...');
      
      if (this.simulation) {
        this.simulation.stop();
      }
      
      // Reinitialize with static layout
      this._initializeStaticLayout();
      
      // Update positions immediately
      this._updateClusterHulls(this.hullGroup);
      
      this.linkElements
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);
      
      this.nodeElements.attr('transform', d => `translate(${d.x},${d.y})`);
    }
  }
  
  /**
   * Show empty state message.
   * @private
   */
  _showEmptyState(container, message) {
    const defaultMessage = 'No graph data available. Upload and process documents to see the knowledge graph.';
    container.innerHTML = `
      <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; color: #64748b; text-align: center; padding: 3rem; background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);">
        <svg style="width: 80px; height: 80px; margin-bottom: 1.5rem; opacity: 0.4;" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path>
        </svg>
        <h3 style="font-size: 1.25rem; font-weight: 700; margin-bottom: 0.75rem; color: #334155;">No Graph Data</h3>
        <p style="max-width: 450px; line-height: 1.6;">${message || defaultMessage}</p>
      </div>
    `;
  }
  
  /**
   * Destroy the visualization and clean up resources.
   */
  destroy() {
    if (this.simulation) {
      this.simulation.stop();
      this.simulation = null;
    }
    
    if (this.svg) {
      this.svg.remove();
      this.svg = null;
    }
    
    this.nodes = [];
    this.links = [];
    this.selectedNode = null;
    this.currentProjectId = null;
  }
}
