import React, { useState, useEffect } from "react";
import Modal from "react-modal";
import ReactDOM from "react-dom";
import "bootstrap/dist/css/bootstrap.min.css";

import Header from "../header";
import { Images } from "../fa";

function FileModal(props) {
  function handleDelete() {
    fetch("/api/dir" + props.file.path, {
      method: "DELETE",
    })
      .then((resp) => resp.json())
      .then((body) => {
        alert(body.success);
        if (body.success) {
          location.reload();
        }
      });
  }

  return (
    <Modal isOpen={true} onRequestClose={props.onClose}>
      <div>{props.file.path}</div>
      <button className="btn btn-danger" onClick={handleDelete}>
        Delete
      </button>
    </Modal>
  );
}

function FileList(props) {
  if (props.files.length === 0) {
    return <div>EMPTY!</div>;
  }

  const rowEls = props.files.map((f) => {
    return (
      <tr>
        <td>
          <a href={f.url}>{f.name}</a>
        </td>
        <td>{f.size}</td>
        <td>
          <button
            className="btn btn-secondary mb-2"
            onClick={() => props.onSelect(f)}
          >
            Detail
          </button>
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
          <th>Ops</th>
        </tr>
      </thead>
      <tbody>{rowEls}</tbody>
    </table>
  );
}

function Total(props) {
  const { size } = props;
  if (size === 0) {
    return null;
  }
  return <span>Total: {size} file(s)</span>;
}

function AddButton(props) {
  return (
    <button onClick={props.onClick} type="button" className="btn btn-primary">
      <Images />
    </button>
  );
}

function App() {
  const { path, files } = window["$mita"];
  const [selectedFile, setSelectedFile] = useState(null);

  function handleClickAddButton() {
    return fetch("/api/dir/add-albums", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        path: path,
      }),
    })
      .then((resp) => resp.json())
      .then((body) => {
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
          <div>
            <form
              method="POST"
              className="form-inline"
              enctype="multipart/form-data"
            >
              <div className="form-group mb-2">
                <input
                  type="file"
                  name="upload"
                  className="form-control-file"
                  webkitdirectory="webkitdirectory"
                  mozdirectory="mozdirectory"
                />
              </div>
              <button type="submit" className="btn btn-primary mb-2">
                Submit
              </button>
            </form>
          </div>

          <Total size={files.length} />
          <div className="container">
            <FileList files={files} onSelect={setSelectedFile} />
          </div>
        </div>
      </main>

      {selectedFile && (
        <FileModal file={selectedFile} onClose={() => setSelectedFile(null)} />
      )}
    </div>
  );
}

ReactDOM.render(<App />, document.getElementById("app"));
