import React, { useEffect } from 'react';
import { Spinner } from '../spinner';

export function Loading(props) {
  function loadTags() {
    props.onChangeState('loading');

    Promise.all([
      props.api.tags(), props.api.contentTags()
    ]).then((result) => {
      const [tags, contentTags] = result;
      props.onLoaded(tags, contentTags);
    }, () => {
      props.onChangeState('failed');
    })
  }

  function handleRetryClick() {
    loadTags();
  }

  useEffect(() => loadTags(), []);

  if (props.state === null || props.state === 'loading') {
    return (<Spinner />);
  }

  if (props.state === 'failed') {
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
