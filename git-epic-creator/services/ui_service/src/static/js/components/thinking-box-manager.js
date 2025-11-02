/**
 * Thinking Box Manager Component
 * 
 * Manages thinking boxes with prompt ID routing and state tracking.
 * 
 * @module components/thinking-box-manager
 */

'use strict';

import { TypewriterBox } from './typewriter-box.js';
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
  
  getOrCreateForPromptId(promptId) {
    if (!promptId) {
      return this.activeBox || this.pendingBox || this.createBox(null);
    }
    
    const promptIdStr = String(promptId);
    if (this.boxesByPromptId.has(promptIdStr)) {
      return this.boxesByPromptId.get(promptIdStr);
    }
    
    if (this.pendingBox && !this.pendingBox.promptId) {
      this.pendingBox.setPromptId(promptId);
      const box = this.pendingBox;
      this.pendingBox = null;
      this.activeBox = box;
      return box;
    }
    
    return this.createBox(promptId);
  }
  
  clearPending() {
    this.pendingBox = null;
  }
}

