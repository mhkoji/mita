import React, { useState, useEffect } from "react";
import ReactDOM from "react-dom";

import { ArrowLeft, ArrowRight } from "../fa";
import Header from "../header";
import FolderList from "../folder-list";
import ImageList from "../image-list";
import TagEditButton from "../edit-tags/edit-button";
import EditTagsModal from "../edit-tags/edit-tags";
import * as apis from "../../apis";

function AppLoaded(props) {
  const { path, files, folders } = props.detail;
  const [editTagsModalPath, setEditTagsModalPath] = useState(false);

  function handleClickTagEditButton() {
    setEditTagsModalPath(path);
  }

  function handleCloseEditTagsModal() {
    setEditTagsModalPath(null);
  }

  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <h2>{path}</h2>

          <div className="mb-4">
            <TagEditButton onClick={handleClickTagEditButton} />
          </div>

          <p>Images</p>
          <ImageList files={files} viewUrl={"/view/" + path} />

          <p>Folders</p>
          <FolderList folders={folders} onEditTags={setEditTagsModalPath} />

          {editTagsModalPath && (
            <EditTagsModal
              api={{
                tags: apis.tags,
                putTag: apis.putTag,
                deleteTag: apis.deleteTag,
                contentTags: () => apis.folderTags(editTagsModalPath),
                putContentTags: (tags) =>
                  apis.putFolderTags(editTagsModalPath, tags),
              }}
              onClose={handleCloseEditTagsModal}
            />
          )}
        </div>
      </main>
    </div>
  );
}

function App() {
  const [detail, setDetail] = useState();
  useEffect(() => {
    const path =  window.location.pathname.substr("/folder".length);
    apis.folder(path).then((detail) => setDetail(detail));
  }, [window.location.pathname]);

  if (detail) {
    return <AppLoaded detail={detail} />;
  }

  return <div>Loading ...</div>;
};

ReactDOM.render(<App />, document.getElementById("app"));
