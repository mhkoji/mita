import React, { useState } from "react";
import ReactDOM from "react-dom";

import { ArrowLeft, ArrowRight } from "../fa";
import Header from "../header";
import FolderList from "../folder-list";
import ImageList from "../image-list";

function App() {
  const { path, files, folders } = window["$d"];
  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <h2>{path}</h2>

          <p>Images</p>
          <ImageList files={files} viewUrl={"/view/" + path} />

          <p>Folders</p>
          <FolderList folders={folders} />
        </div>
      </main>
    </div>
  );
}

ReactDOM.render(<App />, document.getElementById("app"));
