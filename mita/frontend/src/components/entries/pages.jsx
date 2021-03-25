import React, { useState } from 'react';
import ReactDOM from 'react-dom';

function PageList(props) {
  const pageEls = props.pages.map((page, index) => {
    return (
        <a key={index} href={page.url}>{page.name}</a>
    );
  });
  return <div>{pageEls}</div>;
}

function App () {
  function handleClickCreate() {
    fetch('/api/pages/_create', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      }}).then((resp) => resp.json()).then((body) => {
        const url = body['value']['redirect'];
        window.location.href = url;
      });
  }

  const pages = window['$mita']['pages'];

  return (
    <div>
      <button onClick={handleClickCreate}>Create</button>
      <PageList pages={pages} />
    </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
