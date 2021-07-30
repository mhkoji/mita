import React, { useState, useEffect } from "react";
import Modal from "react-modal";

import { Loading } from "./loading";
import { Editing } from "./editing";
import { Saving } from "./saving";

function ModalFooter(props) {
  return (
    <div className="modal-footer">
      <div className="form-row align-items-center">
        <div className="col-auto">
          <button className="btn" onClick={props.onCancel}>
            Cancel
          </button>
        </div>
        <div className="col-auto">
          <button className="btn btn-primary" onClick={props.onSave}>
            Save
          </button>
        </div>
      </div>
    </div>
  );
}

function EditingModal(props) {
  const [newName, setNewName] = useState("");
  useEffect(() => {
    if (props.isTagAdded) {
      setNewName("");
    }
  }, [props.isTagAdded]);
  return (
    <Modal isOpen={true} onRequestClose={props.onClose}>
      <Editing
        newName={newName}
        onChangeNewName={setNewName}
        onAddTag={() => props.onAddTag(newName)}
        tags={props.tags}
        onDeleteTag={props.onDeleteTag}
        contentTags={props.contentTags}
        onAttachTag={props.onAttachTag}
        onDetachTag={props.onDetachTag}
      />
      <ModalFooter onSave={props.onSave} onCancel={props.onClose} />
    </Modal>
  );
}

export default function EditAlbumTagsModal(props) {
  function addTag(name) {
    props.model.addTag(name);
  }

  function deleteTag(tag) {
    props.model.deleteTag(tag);
  }

  function attachTag(tag) {
    props.model.attachTag(tag);
  }

  function detachTag(tag) {
    props.model.detachTag(tag);
  }

  function saveContentTags() {
    props.model.saveContentTags();
  }

  function stopEditTags() {
    props.model.stopEditTags();
  }

  if (!props.tagEdit) {
    return null;
  }

  if (props.tagEdit.type === "loading") {
    return (
      <Modal isOpen={true} onRequestClose={stopEditTags}>
        <Loading />
      </Modal>
    );
  }

  if (props.tagEdit.type === "editing") {
    return (
      <EditingModal
        isTagAdded={props.tagEdit.isTagAdded}
        onAddTag={addTag}
        tags={props.tagEdit.tags}
        onDeleteTag={deleteTag}
        contentTags={props.tagEdit.contentTags}
        onAttachTag={attachTag}
        onDetachTag={detachTag}
        onSave={saveContentTags}
        onClose={stopEditTags}
      />
    );
  }

  if (props.tagEdit.type === "saving") {
    return (
      <Modal isOpen={true} onRequestClose={stopEditTags}>
        <Saving />
      </Modal>
    );
  }

  if (props.tagEdit.type === "saved") {
    return (
      <Modal isOpen={true} onRequestClose={stopEditTags}>
        <div>Saved!</div>
      </Modal>
    );
  }

  return null;
}

Modal.setAppElement("#app-modal");
