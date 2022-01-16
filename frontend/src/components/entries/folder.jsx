import React, { useState } from "react";
import ReactDOM from "react-dom";

import { ArrowLeft, ArrowRight } from "../fa";
import Header from "../header";
import FolderList from "../folder-list";
import ImageList from "../image-list";
import TagEditButton from "../edit-tags/edit-button";
import EditTagsModal from "../edit-tags/edit-tags";
import * as apis from "../../apis";

function App() {
  const { path, files, folders } = window["$d"];
  const [openEditTagsModal, setOpenEditTagsModal] = useState(false);

  function handleClickTagEditButton() {
    setOpenEditTagsModal(true);
  }

  function handleCloseEditTagsModal() {
    setOpenEditTagsModal(false);
  }

  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <h2>{path}</h2>

          <TagEditButton onClick={handleClickTagEditButton} />

          <p>Images</p>
          <ImageList files={files} viewUrl={"/view/" + path} />

          <p>Folders</p>
          <FolderList folders={folders} />

          {openEditTagsModal && (
            <EditTagsModal
              api={{
                tags: apis.tags,
                putTag: apis.putTag,
                deleteTag: apis.deleteTag,
                contentTags: () => apis.folderTags(path),
                putContentTags: (tags) => apis.putFolderTags(path, tags),
              }}
              onClose={handleCloseEditTagsModal}
            />
          )}
        </div>
      </main>
    </div>
  );
}

ReactDOM.render(<App />, document.getElementById("app"));
