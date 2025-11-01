/**
 * API Client Service
 * 
 * Provides centralized API communication with standardized error handling,
 * request/response processing, and 401 handling.
 * 
 * @module api-client
 */

'use strict';

/**
 * HTTP Client for API requests with consistent error handling.
 */
export class ApiClient {
  /**
   * @param {Object} config - API configuration
   * @param {Function} [on401Handler] - Optional callback for 401 errors
   * @param {Object} [options] - Additional options
   * @param {string} [options.baseUrl] - Base URL to prepend to all requests
   * @param {Function} [options.loadingManager] - Loading manager with show()/hide() methods
   */
  constructor(config, on401Handler = null, options = {}) {
    this.config = config;
    this.on401Handler = on401Handler;
    this.baseUrl = options.baseUrl || null;
    this.loadingManager = options.loadingManager || null;
  }
  
  /**
   * Resolves a path to a full URL using base URL if configured.
   * @param {string} path - Request path (can be full URL or relative path)
   * @returns {string} Full URL
   * @private
   */
  _resolveUrl(path) {
    // If path is already a full URL, return as-is
    if (path.startsWith('http://') || path.startsWith('https://')) {
      return path;
    }
    
    // If baseUrl is configured, prepend it
    if (this.baseUrl) {
      const base = this.baseUrl.replace(/\/$/, '');
      const cleanPath = path.startsWith('/') ? path : `/${path}`;
      return `${base}${cleanPath}`;
    }
    
    // Otherwise return path as-is
    return path;
  }
  
  /**
   * Wraps a request with loading state management.
   * @param {Function} fn - Async function to execute
   * @returns {Promise} Result of fn()
   * @private
   */
  async _wrapWithLoading(fn) {
    if (this.loadingManager) {
      this.loadingManager.show();
    }
    try {
      return await fn();
    } finally {
      if (this.loadingManager) {
        this.loadingManager.hide();
      }
    }
  }
  
  /**
   * Makes an HTTP request with standardized error handling.
   * 
   * @param {string} url - Request URL (full URL or path if baseUrl configured)
   * @param {Object} [options={}] - Fetch options
   * @param {Object} [options.headers] - Additional headers
   * @param {string} [options.method='GET'] - HTTP method
   * @param {any} [options.body] - Request body (will be JSON-stringified if object)
   * @param {boolean} [options.skipLoading=false] - Skip loading manager for this request
   * @returns {Promise<Object>} Parsed response object
   * @throws {ApiError} On request failure
   */
  async request(url, options = {}) {
    const skipLoading = options.skipLoading || false;
    
    // Execute request with optional loading wrapper
    const executeRequest = async () => {
      // Resolve URL with baseUrl if configured
      const resolvedUrl = this._resolveUrl(url);
      
      return this._executeRequest(resolvedUrl, options);
    };
    
    if (skipLoading || !this.loadingManager) {
      return executeRequest();
    }
    
    return this._wrapWithLoading(executeRequest);
  }
  
  /**
   * Executes the actual HTTP request.
   * @private
   */
  async _executeRequest(url, options = {}) {
    const {
      method = 'GET',
      headers = {},
      body,
      skipLoading,  // Remove from fetchOptions
      ...fetchOptions
    } = options;
    
    // Build request options
    const requestOptions = {
      method,
      headers: {
        'Content-Type': 'application/json',
        ...headers
      },
      ...fetchOptions
    };
    
    // Add body if provided
    if (body !== undefined) {
      requestOptions.body = typeof body === 'string' ? body : JSON.stringify(body);
    }
    
    try {
      const response = await fetch(url, requestOptions);
      
      // Handle 401 Unauthorized
      if (response.status === 401) {
        if (this.on401Handler) {
          this.on401Handler();
        }
        throw new ApiError('Unauthorized', response.status, response);
      }
      
      // Handle other error responses
      if (!response.ok) {
        const error = await this.parseErrorResponse(response);
        throw error;
      }
      
      // Parse successful response
      // Handle 204 No Content - no body to parse
      if (response.status === 204) {
        return null;
      }
      
      return await response.json();
      
    } catch (error) {
      // Re-throw ApiError as-is
      if (error instanceof ApiError) {
        throw error;
      }
      
      // Wrap other errors
      throw new ApiError(
        `Network error: ${error.message}`,
        null,
        null,
        error
      );
    }
  }
  
  /**
   * Parses error response and creates ApiError.
   * @private
   */
  async parseErrorResponse(response) {
    let detail = null;
    
    try {
      const errorData = await response.json();
      detail = errorData.detail || errorData.message || null;
    } catch {
      // Try reading as text
      try {
        detail = await response.text();
      } catch {
        detail = 'Unknown error';
      }
    }
    
    return new ApiError(
      detail || `HTTP ${response.status}`,
      response.status,
      response,
      null,
      detail
    );
  }
  
  /**
   * Makes a GET request.
   * @param {string} url - Request URL
   * @param {Object} [options={}] - Additional fetch options
   * @returns {Promise<Object>} Response data
   */
  async get(url, options = {}) {
    return this.request(url, { ...options, method: 'GET' });
  }
  
  /**
   * Makes a POST request.
   * @param {string} url - Request URL
   * @param {any} body - Request body
   * @param {Object} [options={}] - Additional fetch options
   * @returns {Promise<Object>} Response data
   */
  async post(url, body, options = {}) {
    return this.request(url, { ...options, method: 'POST', body });
  }
  
  /**
   * Makes a PUT request.
   * @param {string} url - Request URL
   * @param {any} body - Request body
   * @param {Object} [options={}] - Additional fetch options
   * @returns {Promise<Object>} Response data
   */
  async put(url, body, options = {}) {
    return this.request(url, { ...options, method: 'PUT', body });
  }
  
  /**
   * Makes a DELETE request.
   * @param {string} url - Request URL
   * @param {Object} [options={}] - Additional fetch options
   * @returns {Promise<Object>} Response data
   */
  async delete(url, options = {}) {
    return this.request(url, { ...options, method: 'DELETE' });
  }
  
  /**
   * Uploads files using FormData.
   * Special handling for multipart/form-data requests.
   * @param {string} url - Request URL
   * @param {FormData} formData - FormData object with files
   * @param {Object} [options={}] - Additional fetch options
   * @returns {Promise<Object>} Response data
   */
  async postForm(url, formData, options = {}) {
    const skipLoading = options.skipLoading || false;
    
    const executeUpload = async () => {
      const resolvedUrl = this._resolveUrl(url);
      
      try {
        // Don't set Content-Type header for FormData - browser will set it with boundary
        const response = await fetch(resolvedUrl, {
          method: 'POST',
          body: formData,
          ...options
        });
        
        // Handle 401
        if (response.status === 401) {
          if (this.on401Handler) {
            this.on401Handler();
          }
          throw new ApiError('Unauthorized', response.status, response);
        }
        
        // Handle errors
        if (!response.ok) {
          const detail = await response.text().catch(() => 'Upload failed');
          throw new ApiError(detail, response.status, response);
        }
        
        // Handle No Content responses
        if (response.status === 204 || response.status === 205) {
          return null;
        }
        
        // Parse response
        const contentType = response.headers.get('content-type') || '';
        if (contentType.includes('application/json')) {
          return response.json();
        }
        return response.text();
        
      } catch (error) {
        if (error instanceof ApiError) {
          throw error;
        }
        throw new ApiError(
          `Upload failed: ${error.message}`,
          null,
          null,
          error
        );
      }
    };
    
    if (skipLoading || !this.loadingManager) {
      return executeUpload();
    }
    
    return this._wrapWithLoading(executeUpload);
  }
  
  /**
   * Formats error for user display.
   * @param {ApiError|Error} error - Error object
   * @returns {string} Formatted error message
   */
  static formatError(error) {
    if (error instanceof ApiError) {
      return error.userMessage;
    }
    return error.message || 'An unexpected error occurred';
  }
}

/**
 * Custom error class for API errors.
 * Provides structured error information for better handling and display.
 */
export class ApiError extends Error {
  /**
   * @param {string} message - Error message
   * @param {number|null} status - HTTP status code
   * @param {Response|null} response - Fetch Response object
   * @param {Error|null} cause - Original error if any
   * @param {string|null} detail - Detailed error message from API
   */
  constructor(message, status = null, response = null, cause = null, detail = null) {
    super(message);
    this.name = 'ApiError';
    this.status = status;
    this.response = response;
    this.cause = cause;
    this.detail = detail;
    
    // User-friendly message
    if (detail) {
      this.userMessage = detail;
    } else if (status) {
      this.userMessage = `Error ${status}: ${message}`;
    } else {
      this.userMessage = message;
    }
  }
  
  /**
   * Checks if error is a specific HTTP status.
   * @param {number} status - HTTP status code to check
   * @returns {boolean} True if error has this status
   */
  isStatus(status) {
    return this.status === status;
  }
  
  /**
   * Checks if error is a client error (4xx).
   * @returns {boolean} True if 4xx error
   */
  isClientError() {
    return this.status >= 400 && this.status < 500;
  }
  
  /**
   * Checks if error is a server error (5xx).
   * @returns {boolean} True if 5xx error
   */
  isServerError() {
    return this.status >= 500 && this.status < 600;
  }
}