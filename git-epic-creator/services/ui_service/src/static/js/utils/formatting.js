/**
 * Formatting Utilities
 * 
 * Functions for formatting dates, numbers, and other display values.
 * 
 * @module utils/formatting
 */

'use strict';

/**
 * Formats an ISO date string to DD/MM/YYYY format.
 * @param {string} isoString - ISO date string
 * @returns {string} Formatted date or 'N/A' if invalid
 */
export function formatDate(isoString) {
  if (!isoString) return 'N/A';
  try {
    const date = new Date(isoString);
    if (isNaN(date.getTime())) return 'N/A';
    const day = String(date.getDate()).padStart(2, '0');
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const year = date.getFullYear();
    return `${day}/${month}/${year}`;
  } catch {
    return 'N/A';
  }
}

