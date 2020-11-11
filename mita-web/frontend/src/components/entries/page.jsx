import React, { useState } from 'react';
import ReactDOM from 'react-dom';

function ImageList(props) {
  const imageEls = props.images.map((image) => {
    return (
        <img key={image.id} alt={image.id} src={image.url} />
    );
  });
  return <div>{imageEls}</div>;
}

function App () {
  const pageId = window['$mita']['page']['page-id'];
  const images = window['$mita']['page']['images'];
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
      <ImageList images={images} />
    </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
