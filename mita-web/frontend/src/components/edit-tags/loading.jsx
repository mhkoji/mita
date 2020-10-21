import React, { useEffect } from 'react';
import { Spinner } from '../spinner';

export function Loading(props) {
  function loadTags() {
    Promise.all([
      props.api.tags(), props.api.contentTags()
    ]).then((result) => {
      const [tags, contentTags] = result;
      props.onLoaded(tags, contentTags);
    }, () => {
      props.onChangeState({
        type: 'failed'
      });
    })
  }

  function handleRetryClick() {
    props.onChangeState(null);
    loadTags();
  }

  useEffect(() => loadTags(), []);

  if (props.state === null) {
    return (<Spinner />);
  }

  if (props.state.type === 'failed') {
    return (
        <div>
          <div
              className="alert alert-danger"
              role="alert">
            Loading failed
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
