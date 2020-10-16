import React, { useEffect } from 'react';
import { Spinner } from '../spinner';

export function Saving(props) {
  useEffect(() => {
    props.onChangeState({ state: 'saving' });

    props.api.updateContentTags(props.state.contentTags).then(() => {
      setTimeout(() => {
        props.onChangeState({ state: 'saved' })

        setTimeout(() => props.onFinishSave(), 500);
      }, 100);
    });
  }, []);

  if (props.state.state === 'saving') {
    return (<Spinner />);
  }

  if (props.state.state === 'saved') {
    return (<div>Saved!</div>);
  }

  return null;
}
