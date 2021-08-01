import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";

export function Editing(props) {
  return (
    <div>
      <div className="input-group">
        <input
          type="text"
          className="form-control"
          value={props.newName}
          onChange={(evt) => props.onChangeNewName(evt.target.value)}
        />
        <div className="input-group-append">
          <button
            type="button"
            className="btn btn-primary"
            onClick={props.onAddTag}
          >
            Add
          </button>
        </div>
      </div>
      <ul className="list-group">
        {props.tags.map((tag) => {
          const isAttached =
            props.contentTags.find((t) => t.id === tag.id) !== undefined;
          const handleChange = isAttached
            ? () => props.onDetach(tag)
            : () => props.onAttach(tag);
          const id = "check-" + tag.id;
          return (
            <li key={tag.id} className="list-group-item">
              <div
                className="form-check"
                style={{
                  display: "inline-block",
                }}
              >
                <input
                  id={id}
                  type="checkbox"
                  className="form-check-input"
                  checked={isAttached}
                  onChange={handleChange}
                />
                <label htmlFor={id} className="form-check-label">
                  {tag.name}
                </label>
              </div>
              <div className="float-right">
                <button
                  type="button"
                  className="btn btn-danger btn-sm"
                  onClick={() => props.onDeleteTag(tag)}
                >
                  Delete
                </button>
              </div>
            </li>
          );
        })}
      </ul>
    </div>
  );
}
