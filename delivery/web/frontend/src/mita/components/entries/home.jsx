import React from "react";
import ReactDOM from "react-dom";

import Header from "../header";

function App() {
  function LinkItem(props) {
    return (
      <li className="list-group-item">
        <a href={props.href}>
          <span>{props.name}</span>
        </a>
      </li>
    );
  }

  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <ul className="list-group">
            <LinkItem key="dir" name="Dir" href="/dir/" />
            <LinkItem key="album" name="Albums" href="/albums" />
            <LinkItem key="tag" name="Tags" href="/tags" />
          </ul>
        </div>
      </main>
    </div>
  );
}

ReactDOM.render(<App />, document.getElementById("app"));
