import React, { useState } from "react";
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
  const [tags, setTags] = useState(props.tags);
  const [contentTags, setContentTags] = useState(props.contentTags);

  function handleAddTag() {
    props.api.putTag(newName).then(() => {
      setNewName("");
      props.api.tags().then((tags) => {
        setTags(tags);
      });
    });
  }

  function handleDeleteTag(tag) {
    props.api.deleteTag(tag).then(() => {
      props.api.tags().then((tags) => {
        setTags(tags);
      });
    });
  }

  function handleAttachTag(tag) {
    setContentTags(contentTags.concat([tag]));
  }

  function handleDetachTag(tag) {
    setContentTags(contentTags.filter((t) => t.id !== tag.id));
  }

  return (
    <Modal isOpen={true} onRequestClose={props.onClose}>
      <Editing
        newName={newName}
        onChangeNewName={setNewName}
        onAddTag={handleAddTag}
        onDeleteTag={handleDeleteTag}
        tags={tags}
        contentTags={contentTags}
        onAttach={handleAttachTag}
        onDetach={handleDetachTag}
      />
      <ModalFooter
        onSave={() => props.onSave(contentTags)}
        onCancel={props.onClose}
      />
    </Modal>
  );
}

export default class EditTagsModal extends React.Component {
  constructor(props) {
    super(props);

    this.handleLoaded = this.handleLoaded.bind(this);
    this.handleSave = this.handleSave.bind(this);

    this.state = {
      type: "loading",
    };
  }

  handleLoaded(tags, contentTags) {
    this.setState({
      type: "editing",
      tags: tags,
      contentTags: contentTags,
    });
  }

  handleSave(contentTags) {
    this.setState({
      type: "saving",
      contentTags: contentTags,
    });
  }

  render() {
    const props = this.props;
    const state = this.state;

    if (state.type === "loading") {
      return (
        <Modal isOpen={true} onRequestClose={props.onClose}>
          <Loading api={this.props.api} onLoaded={this.handleLoaded} />
        </Modal>
      );
    }

    if (state.type === "editing") {
      return (
        <EditingModal
          api={this.props.api}
          tags={this.state.tags}
          contentTags={this.state.contentTags}
          onClose={props.onClose}
          onSave={this.handleSave}
        />
      );
    }

    if (state.type === "saving") {
      return (
        <Modal isOpen={true} onRequestClose={props.onClose}>
          <Saving
            api={this.props.api}
            contentTags={state.contentTags}
            onFinishSave={props.onClose}
          />
        </Modal>
      );
    }

    return null;
  }
}

Modal.setAppElement("#app-modal");
