import React from "react";
import Modal from "react-modal";

export default function AlbumDetailModal(props) {
  if (!props.album) {
    return null;
  }

  function handleDelete() {
    fetch("/api/albums/" + props.album.id, {
      method: "DELETE",
    }).then(() => location.reload());
  }

  return (
    <Modal isOpen={true} onRequestClose={props.onClose}>
      <div>
        {props.album.name}
        <button className="btn btn-danger" onClick={handleDelete}>
          Delete
        </button>
      </div>
    </Modal>
  );
}
