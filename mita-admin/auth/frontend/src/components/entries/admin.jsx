import React, { useState } from 'react';
import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

function AccountList(props) {
  const { accounts } = props;
  const rows = accounts.map((a) => {
    return (
        <tr key={a.id}>
          <td>{a.id}</td>
          <td>{a.username}</td>
        </tr>
    );
  });
  return (
      <table className="table table-striped">
        <thead>
          <tr>
            <th scope="col">Account Id</th>
            <th scope="col">Username</th>
          </tr>
        </thead>
        <tbody>{rows}</tbody>
      </table>
  );
}

function App () {
  const { accounts } = window['$mita'];
  return (
      <div>
        <main className="p-md-5">
          <div className="container">
            <AccountList accounts={accounts} />
          </div>
        </main>
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
