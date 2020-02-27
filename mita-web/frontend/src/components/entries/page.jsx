import React, { useState } from 'react';
import ReactDOM from 'react-dom';

function ImageList(props) {
  const imageEls = props.imageUrls.map((url, index) => {
    return (
        <img key={index} src={url} />
    );
  });
  return <div>{imageEls}</div>;
}

function App () {
  const pageId = window['$mita']['page']['page-id'];
  const imageUrls = window['$mita']['page']['image-urls'];
  const [text, setText] = useState(window['$mita']['page']['text']);
  function handleChangeText(evt) {
    setText(evt.target.value);
  }
  const [savingStatus, setSavingStatus] = useState(null);

  function handleSaveClick() {
    setSavingStatus('saving');
    fetch('/api/pages/' + pageId + '/text', {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        text: text
      })}).then(() => {
        setSavingStatus('saved');
        setTimeout(() => {
          setSavingStatus(null);
        }, 1000);
      });
  }

  const savingStatusEl = savingStatus && <div>{savingStatus}</div>;

  return (
    <div>
      <form>
        <textarea value={text} onChange={handleChangeText} />
      </form>
      <button onClick={handleSaveClick}>
        Save
      </button>
      {savingStatusEl}
      <ImageList imageUrls={imageUrls} />
    </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
