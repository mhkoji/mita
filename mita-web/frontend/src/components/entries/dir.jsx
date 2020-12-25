import React, { useState } from 'react';
import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import Header from '../header';
import { Images } from '../fa';

function FileList(props) {
  if (props.files.length === 0) {
    return <div>EMPTY!</div>;
  }

  const rowEls = props.files.map((f) => {
    return (
        <tr>
          <td>
            <a href={f.url}>
              {f.name}
            </a>
          </td>
          <td>
            {f.size}
          </td>
        </tr>
    );
  });

  return (
      <table className="table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Size</th>
          </tr>
        </thead>
        <tbody>
          {rowEls}
        </tbody>
      </table>
  );
}

function AddButton(props) {
  return (
      <button
          onClick={props.onClick}
          type="button"
          className="btn btn-primary">
        <Images />
      </button>
  );
}

function App () {
  const { path, files } = window['$mita'];
  
  function handleClickAddButton() {
    return fetch('/api/dir/add-albums', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        'path': path
      })
    }).then((resp) => resp.json()).then((body) => {
      alert(body.success);
    });
  }
  
  return (
      <div>
        <Header />

        <main>
          <div>
            <div className="jumbotron">
              <h1 className="display">{path}</h1>
              <AddButton onClick={() => handleClickAddButton()} />
            </div>
          </div>

          <div className="p-md-5">
            <div className="container">
              <FileList files={files} />
            </div>
          </div>
        </main>
      </div>
  );
}

ReactDOM.render(
    <App />,
    document.getElementById('app'));
