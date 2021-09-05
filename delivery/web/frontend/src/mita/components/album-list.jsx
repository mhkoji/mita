import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";

import EditButton from "./edit-tags/edit-button";

function DetailButton(props) {
  return (
    <button className="btn btn-secondary mb-2" onClick={props.onClick}>
      Detail
    </button>
  );
}

function AlbumRow(props) {
  if (props.albums.length === 0) {
    return null;
  }
  const cardEls = props.albums.map((album) => {
    return (
      <div key={album.id} className="col-md-4">
        <div className="card mb-4 shadow-sm">
          <a href={album.url} target="_blank">
            <img
              alt={album.name}
              src={album.thumbnail.url}
              width="100%"
              height={400}
              className="card-img-top bd-placeholder-img"
            />
          </a>
          <div className="card-body">
            <div className="title">
              <div
                title={album.name}
                style={{
                  overflow: "hidden",
                  whiteSpace: "nowrap",
                  textOverflow: "ellipsis",
                }}
              >
                {album.name}
              </div>
            </div>
            <EditButton onClick={() => props.onEditTags(album.id)} />
            <DetailButton onClick={() => props.onDetail(album.id)} />
          </div>
        </div>
      </div>
    );
  });
  return <div className="row">{cardEls}</div>;
}

export default function AlbumList(props) {
  const albums = props.albums;
  if (albums.length === 0) {
    return <div className="mb-4 text-center">EMPTY!</div>;
  }

  const rowList = [];
  const inDeckCount = 3;
  for (let i = 0; i < albums.length; i += inDeckCount) {
    rowList.push(
      <AlbumRow
        key={i}
        albums={albums.slice(i, Math.min(i + inDeckCount, albums.length))}
        onEditTags={props.onEditTags}
        onDetail={props.onDetail}
      />
    );
  }

  return <div>{rowList}</div>;
}
