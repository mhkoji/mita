import React, { useEffect } from 'react';
import { Spinner } from '../spinner';

export function Saving(props) {
  function saveTags() {
    props.onChangeState({ state: 'saving' });

    props.api.putContentTags(props.state.contentTags)
        .then(() => {
          setTimeout(() => {
            props.onChangeState({ state: 'saved' })

            setTimeout(() => props.onFinishSave(), 500);
          }, 100);
        }, () => {
          props.onChangeState({ state: 'failed' });
        });
  }

  function handleRetryClick() {
    saveTags();
  }

  useEffect(() => saveTags(), []);

  if (props.state.state === 'saving') {
    return (<Spinner />);
  }

  if (props.state.state === 'saved') {
    return (<div>Saved!</div>);
  }

  if (props.state.state === 'failed') {
    return (
        <div>
          <div
              className="alert alert-danger"
              role="alert">
            Saving failed
          </div>
          <button
              className="btn btn-primary"
              onClick={handleRetryClick}>
            Retry
          </button>
        </div>
    );
  }

  return null;
}
