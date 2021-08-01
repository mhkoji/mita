import React, { useState } from "react";
import ReactDOM from "react-dom";

import { ArrowLeft, ArrowRight } from "../fa";
import Header from "../header";
import AlbumList from "../album-list";
import EditAlbumTagsModal from "../edit-tags/edit-tags";

function Pager(props) {
  const { prev, next } = props;
  return (
    <nav aria-label="pager">
      <ul className="pagination justify-content-center">
        <li className={"page-item" + (prev ? "" : " disabled")}>
          <a className="page-link" href={prev}>
            <ArrowLeft />
          </a>
        </li>
        <li className={"page-item" + (next ? "" : " disabled")}>
          <a className="page-link" href={next}>
            <ArrowRight />
          </a>
        </li>
      </ul>
    </nav>
  );
}

function App() {
  const { albums, pager } = window["$mita"];
  const [editingTagsAlbumId, setEditingTagsAlbumId] = useState(null);
  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <Pager {...pager} />

          <AlbumList
            albums={albums}
            onEditTags={(albumId) => setEditingTagsAlbumId(albumId)}
          />

          <Pager {...pager} />
        </div>
      </main>

      {editingTagsAlbumId && (
        <EditAlbumTagsModal
          albumId={editingTagsAlbumId}
          onClose={() => setEditingTagsAlbumId(null)}
        />
      )}
    </div>
  );
}

ReactDOM.render(<App />, document.getElementById("app"));
