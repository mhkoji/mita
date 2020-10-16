import React from 'react';
import '@fortawesome/fontawesome-free/js/fontawesome.min';
import '@fortawesome/fontawesome-free/js/solid.min';
import '@fortawesome/fontawesome-free/js/regular';

export default function EditButton(props) {
  return (
      <button
          type="button"
          className="btn btn-outline-secondary"
          onClick={props.onClick}>
        <i className="fas fa-tags" />
      </button>
  );
}
