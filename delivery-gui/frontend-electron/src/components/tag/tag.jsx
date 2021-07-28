import React, { useState, useEffect } from 'react';

import { Spinner } from '../spinner';
import { Edit } from '../fa';
import Contents from './contents';

function StatefulContents(props) {
  const [state, setState] = useState({});

  useEffect(() => {
    setState({
      type: 'loading',
      value: null
    });
    props.api.tagContents(props.tagId).then((contents) => {
      setState({
        type: 'loaded',
        value: {
          contents: contents
        }
      });
    });
  }, [props.tagId]);

  if (state.type === 'loading') {
    return (<Spinner/>);
  }

  if (state.type === 'loaded') {
    return (<Contents contents={state.value.contents} />);
  }

  return null;
}

function Name(props) {
  if (props.editing.name === null) {
    return (
        <div key="displaying">
          <span>{props.tag.name}</span>
          <button
              className="btn btn-sm btn-outline-secondary"
              onClick={() => props.editing.onChange(props.tag.name)}>
            <Edit/>
          </button>
        </div>
    );
  }
  return (
      <div key="editing" className="input-group">
        <input
            type="text"
            className="form-control"
            value={props.editing.name}
            onChange={(evt) => props.editing.onChange(evt.target.value)} />
        <div className="input-group-append">
          <button
              className="btn btn-primary"
              onClick={props.editing.onSave}>
            Save
          </button>
        </div>
      </div>
  );
}

export default function Tag(props) {
  if (!props.tag) {
    return;
  }

  return (
      <div>
        <Name
            tag={props.tag}
            editing={props.editing} />
        <StatefulContents
            api={props.api}
            tagId={props.tag.id} />
      </div>
  );
}
