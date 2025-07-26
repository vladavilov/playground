function loadGraph() {
  fetch('/visualize')
    .then(resp => resp.json())
    .then(data => {
      document.getElementById('graph').textContent = JSON.stringify(data, null, 2);
    });
}

document.addEventListener('DOMContentLoaded', function() {
  let processForm = document.getElementById('process-form');
  if (processForm) {
    processForm.onsubmit = function(e) {
      e.preventDefault();
      let url = document.getElementById('pdf-url').value;
      fetch('/process?url=' + encodeURIComponent(url), {method: 'POST'})
        .then(resp => resp.json())
        .then(data => {
          document.getElementById('process-result').textContent = JSON.stringify(data, null, 2);
        });
    };
  }

  let queryForm = document.getElementById('query-form');
  if (queryForm) {
    queryForm.onsubmit = function(e) {
      e.preventDefault();
      let prompt = document.getElementById('prompt').value;
      fetch('/query?prompt=' + encodeURIComponent(prompt))
        .then(resp => resp.json())
        .then(data => {
          document.getElementById('query-result').textContent = JSON.stringify(data, null, 2);
        });
    };
  }
});
