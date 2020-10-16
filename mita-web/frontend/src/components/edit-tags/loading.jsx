import React, { useEffect } from 'react';
import { Spinner } from '../spinner';

export function Loading(props) {
  useEffect(() => {
    props.onChangeState({
      tags: null,
      contentTags: null
    });
    props.api.tags().then((tags) => {
      props.onChangeState(
          (state) => Object.assign({}, state, { tags: tags }));
    });
    props.api.contentTags().then((tags) => {
      props.onChangeState(
          (state) => Object.assign({}, state, { contentTags: tags }));
    });
  }, []);

  useEffect(() => {
    if (props.state && props.state.tags && props.state.contentTags) {
      props.onLoaded(props.state.tags, props.state.contentTags);
    }
  }, [props.state]);

  return (<Spinner />);
}
