import React from 'react';
import Modal from 'react-modal';

import { Loading } from './loading';
import { Editing } from "./editing";
import { Saving } from "./saving";

class Api {
  constructor(albumId) {
    this.albumId = albumId;
  }

  tags() {
    return fetch('/api/tags', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    }).then((resp) => resp.json()).then((body) => {
      return body.value;
    });
  }

  putTag(name) {
    return fetch('/api/tags/_create?name=' + name, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      }
    });
  }

  deleteTag(tag) {
    return fetch('/api/tags/' + tag.id, {
      method: 'DELETE',
      headers: {
        'Content-Type': 'application/json'
      }
    });
  }

  contentTags() {
    return fetch('/api/albumTags/' + this.albumId, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    }).then((resp) => resp.json()).then((body) => {
      return body.value;
    });
  }

  updateContentTags(tags) {
    return fetch('/api/albumTags/' + this.albumId, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        'tag-id-list': tags.map((t) => t.id)
      })
    });
  }
}

function ModalFooter(props) {
  return (
      <div className="modal-footer">
        <div className="form-row align-items-center">
          <div className="col-auto">
            <button className="btn"
                    onClick={props.onCancel}>
              Cancel
            </button>
          </div>
          <div className="col-auto">
            <button className="btn btn-primary"
                    onClick={props.onSave}>
              Save
            </button>
          </div>
        </div>
      </div>
  );
}

export default class EditAlbumTagsModal extends React.Component {
  constructor(props) {
    super(props);

    this.handleChangeCurrentState = this.handleChangeCurrentState.bind(this);
    this.handleLoaded = this.handleLoaded.bind(this);
    this.handleSave = this.handleSave.bind(this);

    this.api = new Api(props.albumId);

    this.state = {
      type: 'loading',
      value: null
    };
  }

  handleChangeCurrentState(value) {
    this.setState((state) => {
      const newValue = (value instanceof Function) ?
          value(state.value) :
          value;
      return Object.assign({}, state, {value: newValue});
    });
  }

  handleLoaded(tags, contentTags) {
    this.setState({
      type: 'editing',
      value: {
        tags: tags,
        contentTags: contentTags,
        newName: ''
      }
    });
  }

  handleSave() {
    this.setState((state) => {
      return {
        type: 'saving',
        value: {
          contentTags: state.value.contentTags
        }
      };
    });
  }

  render() {
    const props = this.props;
    const state = this.state;
    if (!props.albumId) {
      return null;
    }

    if (state.type === 'loading') {
      return (
          <Modal
              isOpen={true}
              onRequestClose={props.onClose}>
            <Loading
                api={this.api}
                state={state.value}
                onChangeState={this.handleChangeCurrentState}
                onLoaded={this.handleLoaded} />
          </Modal>
      );
    }

    if (state.type === 'editing') {
      return (
          <Modal
              isOpen={true}
              onRequestClose={props.onClose}>
            <Editing
                api={this.api}
                state={state.value}
                onChangeState={this.handleChangeCurrentState}
                onSave={this.handleSave}
                onCancle={props.onClose} />
            <ModalFooter
                onSave={this.handleSave}
                onCancel={props.onClose} />
          </Modal>
      );
    }

    if (state.type === 'saving') {
      return (
          <Modal
              isOpen={true}
              onRequestClose={props.onClose}>
            <Saving
                api={this.api}
                state={state.value}
                onChangeState={this.handleChangeCurrentState}
                onFinishSave={props.onClose} />
          </Modal>);
    }

    return null;
  }
}

Modal.setAppElement('#app-modal');
