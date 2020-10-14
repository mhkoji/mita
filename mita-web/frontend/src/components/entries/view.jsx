import React from 'react';
import ReactDOM from 'react-dom';

function App () {
  const images = window['$mita']['images'];
  return (
      <main>
        {images.length > 0 && images[0].url}
      </main>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
