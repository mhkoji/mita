import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";

import TagEditButton from "./edit-tags/edit-button";

function FolderRow(props) {
  if (props.folders.length === 0) {
    return null;
  }
  const cardEls = props.folders.map((folder) => {
    function handleClickTagEditButton() {
      props.onEditTags(folder.path);
    }
    return (
      <div key={folder.path} className="col-md-4">
        <div className="card mb-4 shadow-sm">
          <a href={folder.url}>
            {folder.thumbnail && (
              <img
                alt={folder.path}
                src={folder.thumbnail.url}
                width="100%"
                height={400}
                className="card-img-top bd-placeholder-img"
              />
            )}
          </a>
          <div className="card-body">
            <div className="title">
              <a
                href={folder.url}
                title={folder.path}
                style={{
                  overflow: "hidden",
                  whiteSpace: "nowrap",
                  textOverflow: "ellipsis",
                }}
              >
                {folder.path}
              </a>
            </div>
            <TagEditButton onClick={handleClickTagEditButton} />
          </div>
        </div>
      </div>
    );
  });
  return <div className="row">{cardEls}</div>;
}

export default function FolderList(props) {
  const folders = props.folders;
  if (folders.length === 0) {
    return <div className="mb-4 text-center">EMPTY!</div>;
  }

  const rowList = [];
  const inDeckCount = 3;
  for (let i = 0; i < folders.length; i += inDeckCount) {
    rowList.push(
      <FolderRow
        key={i}
        folders={folders.slice(i, Math.min(i + inDeckCount, folders.length))}
        onEditTags={props.onEditTags}
      />
    );
  }

  return <div>{rowList}</div>;
}
