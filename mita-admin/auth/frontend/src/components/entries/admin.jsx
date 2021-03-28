import React, { useState, useEffect } from 'react';
import Modal from 'react-modal';

import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import { Plus } from '../fa';

function AccountList(props) {
  const { accounts } = props;
  const rows = accounts.map((a) => {
    return (
        <tr key={a.id}>
          <td>{a.id}</td>
          <td>{a.username}</td>
        </tr>
    );
  });
  return (
      <table className="table table-striped">
        <thead>
          <tr>
            <th scope="col">Account Id</th>
            <th scope="col">Username</th>
          </tr>
        </thead>
        <tbody>{rows}</tbody>
      </table>
  );
}

function Editing(props) {
  function setUsername(x) {
    props.onChangeState((state) => Object.assign({}, state, {
      username: x
    }));
  }

  function setPassword(x) {
    props.onChangeState((state) => Object.assign({}, state, {
      password: x
    }));
  }

  function handleSubmit(evt) {
    evt.preventDefault();
    props.onCreate(props.state.username, props.state.password);
  }

  useEffect(() => {
    setUsername('');
    setPassword('');
  }, []);

  if (!props.state) {
        return null;
  }
  
  return (
      <div>
        <form onSubmit={handleSubmit}>
          <label>
            Username
            <input
              value={props.state.username}
              onChange={(e) => setUsername(e.target.value)}/>
          </label>
          <label>
            Password
            <input
              value={props.state.password}
              type="password"
              onChange={(e) => setPassword(e.target.value)}/>
          </label>
          <input
            type="submit"
            value="Create" />
        </form>
      </div>
  );
}

class CreateAccountModal extends React.Component {
  constructor(props) {
    super(props);

    this.handleChangeCurrentState = this.handleChangeCurrentState.bind(this);
    this.handleCreate = this.handleCreate.bind(this);
      
    this.state = {
      type: 'editing',
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

  handleCreate(username, password) {
    this.setState({ type: 'saving' });
    fetch('/admin/api/create-account', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        'username': username,
        'password': password
      })
    }).then((resp) => {
        this.setState({ type: 'saved' });
        setTimeout(() => {
          window.location = window.location;
        }, 1000);
    });
  }

  render() {
    const props = this.props;
    const state = this.state;

    if (state.type === 'editing') {
      return (
          <Modal
              isOpen={true}
              onRequestClose={props.onClose}>
            <Editing
              state={state.value}
              onChangeState={this.handleChangeCurrentState}
              onCreate={this.handleCreate} />
          </Modal>
      );
    }

    if (state.type === 'saving') {
      return (
          <Modal isOpen={true}>
            <span>Saving..</span>
          </Modal>
      );
    }

    if (state.type === 'saving') {
      return (
          <Modal isOpen={true}>
            <span>Saved!</span>
          </Modal>
      );
    }

    return null;
  }
}

function App () {
  const { accounts } = window['$mita'];
  const [isCreatingAccount, setIsCreatingAccount] = useState(false);

  return (
      <div>
        <main className="p-md-5">
          <div className="container">

            <button type="button"
                    className="btn btn-primary"
                    onClick={() => setIsCreatingAccount(true)} >
              <Plus />
            </button>

            <AccountList accounts={accounts} />
          </div>
        </main>

        {
            isCreatingAccount && (
                <CreateAccountModal
                  onClose={() => setIsCreatingAccount(false)} />)
        }
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));

Modal.setAppElement('#app-modal');
