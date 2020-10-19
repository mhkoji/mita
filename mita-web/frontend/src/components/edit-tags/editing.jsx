import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';

export function Editing(props) {
  function handleChangeName(evt) {
    const newName = evt.target.value;
    props.onChangeState((state) => Object.assign({}, state, {
      newName
    }));
  }

  function handleAddTag() {
    props.api.putTag(props.state.newName).then(() => {
      props.onChangeState((state) => Object.assign({}, state, {
        newName: ''
      }));
      props.api.tags().then((tags) => {
        props.onChangeState((state) => Object.assign({}, state, {
          tags
        }));
      });
    });
  }

  function handleDeleteTag(tag) {
    props.api.deleteTag(tag).then(() => {
      props.api.tags().then((tags) => {
        props.onChangeState((state) => Object.assign({}, state, {
          tags
        }));
      });
    });
  }

  function handleAttachTag(tag) {
    props.onChangeState((state) => Object.assign({}, state, {
      contentTags: state.contentTags.concat([tag])
    }));
  }

  function handleDetachTag(tag) {
    props.onChangeState((state) => Object.assign({}, state, {
      contentTags: state.contentTags.filter((t) => t.id !== tag.id)
    }));
  }

  const contentTags = props.state.contentTags;

  return (
      <div>
        <div className="input-group">
          <input type="text"
                 className="form-control"
                 value={props.state.newName}
                 onChange={handleChangeName} />
          <div className="input-group-append">
            <button type="button"
                    className="btn btn-primary"
                    onClick={handleAddTag}>
              Add
            </button>
          </div>
        </div>
        <ul className="list-group">
          {
            props.state.tags.map((tag) => {
              const isAttached = contentTags.find((t) => t.id === tag.id) !== undefined;
              const handleChange = isAttached ?
                  () => handleDetachTag(tag) : () => handleAttachTag(tag);
              const id = 'check-' + tag.id;
              return (
                  <li key={tag.id} className="list-group-item">
                    <div className="form-check"
                         style={{
                           display: 'inline-block'
                         }}>
                      <input id={id}
                             type="checkbox"
                             className="form-check-input"
                             checked={isAttached}
                             onChange={handleChange}/>
                      <label htmlFor={id} className="form-check-label">{tag.name}</label>
                    </div>
                    <div className="float-right">
                      <button type="button"
                              className="btn btn-danger btn-sm"
                              onClick={() => handleDeleteTag(tag)}>
                        Delete
                      </button>
                    </div>
                  </li>
              );
            })
          }
        </ul>
      </div>
  );
}
