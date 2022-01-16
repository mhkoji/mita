import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";

import EditButton from "./edit-tags/edit-button";

function ImageRow(props) {
  if (props.files.length === 0) {
    return null;
  }
  const cardEls = props.files.map((file) => {
    const viewUrl = props.viewUrl + "#from=" + file.path;
    return (
      <div key={file.path} className="col-md-4">
        <div className="card mb-4 shadow-sm">
          <a href={viewUrl}>
            <img
              alt={file.path}
              src={file.url}
              width="100%"
              height={400}
              className="card-img-top bd-placeholder-img"
            />
          </a>
        </div>
      </div>
    );
  });
  return <div className="row">{cardEls}</div>;
}

export default function ImageList(props) {
  if (props.files.length === 0) {
    return <div className="mb-4 text-center">EMPTY!</div>;
  }

  const files = props.files;
  const rowList = [];
  const inDeckCount = 3;
  for (let i = 0; i < files.length; i += inDeckCount) {
    const subFiles = files.slice(i, Math.min(i + inDeckCount, files.length));
    rowList.push(<ImageRow key={i} viewUrl={props.viewUrl} files={subFiles} />);
  }

  return <div>{rowList}</div>;
}
