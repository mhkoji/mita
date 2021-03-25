import React from 'react';

import { Tags } from '../fa';

export default function EditButton(props) {
  return (
      <button
          type="button"
          className="btn btn-outline-secondary"
          onClick={props.onClick}>
        <Tags />
      </button>
  );
}
