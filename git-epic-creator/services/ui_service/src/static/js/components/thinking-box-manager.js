/**
 * Thinking Box Manager Component
 * 
 * Manages thinking boxes with prompt ID routing and state tracking.
 * Centralizes thinking box lifecycle management for chat-based screens.
 * 
 * @module components/thinking-box-manager
 */

'use strict';

import { TypewriterBox } from './typewriter-box.js';

/**
 * Manages thinking boxes with prompt ID routing and state tracking.
 */
export class ThinkingBoxManager {
  constructor(containerElement) {
    if (!containerElement) {
      throw new Error('ThinkingBoxManager requires a container element');
    }
    this.container = containerElement;
    this.boxes = [];
    this.boxesByPromptId = new Map();
    this.activeBox = null;
    this.pendingBox = null;
  }
  
  /**
   * Creates a new thinking box with optional prompt ID.
   * @param {string|null} promptId - Optional prompt ID to associate with box
   * @returns {Object} Box wrapper with TypewriterBox interface
   */
  createBox(promptId = null) {
    const box = new TypewriterBox(this.container, promptId);
    
    const boxWrapper = {
      promptId: box.promptId,
      element: box.getElement(),
      setPromptId: (newId) => {
        box.setPromptId(newId);
        if (newId) {
          boxWrapper.promptId = newId;
          this.boxesByPromptId.set(String(newId), boxWrapper);
        }
      },
      appendStream: (text) => box.appendStream(text),
      appendMarkdown: (md) => box.appendMarkdown(md),
      finish: (status = 'ok') => box.finish(status)
    };
    
    this.boxes.push(boxWrapper);
    if (promptId) {
      this.boxesByPromptId.set(String(promptId), boxWrapper);
    }
    this.activeBox = boxWrapper;
    return boxWrapper;
  }
  
  /**
   * Gets existing box or creates new one for given prompt ID.
   * Handles pending box linking when prompt ID arrives asynchronously.
   * @param {string|null} promptId - Prompt ID to find or create box for
   * @returns {Object} Box wrapper instance
   */
  getOrCreateForPromptId(promptId) {
    if (!promptId) {
      return this.activeBox || this.pendingBox || this.createBox(null);
    }
    
    const promptIdStr = String(promptId);
    if (this.boxesByPromptId.has(promptIdStr)) {
      return this.boxesByPromptId.get(promptIdStr);
    }
    
    // Link pending box if it exists and has no prompt ID yet
    if (this.pendingBox && !this.pendingBox.promptId) {
      this.pendingBox.setPromptId(promptId);
      const box = this.pendingBox;
      this.pendingBox = null;
      this.activeBox = box;
      return box;
    }
    
    return this.createBox(promptId);
  }
  
  /**
   * Clears the pending box reference.
   * Useful after linking pending box to a prompt ID.
   */
  clearPending() {
    this.pendingBox = null;
  }
}

