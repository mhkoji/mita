import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom';

import Header from '../header';
import Tag from '../tag/tag';

const Api = {
  tags() {
    return fetch('/api/tags', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    }).then((resp) => resp.json()).then((body) => {
      return body.value;
    });
  },

  putTagName(tagId, name) {
    return fetch('/api/tags/' + tagId + '?name=' + name, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json'
      }
    });
  },

  tagContents(tagId) {
    return fetch('/api/tags/' + tagId + '/contents', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    }).then((resp) => resp.json()).then((body) => {
      return body.value;
    });
  }
};

function TagList(props) {
  const itemEls = props.tags.map((tag) => {
    return (
        <button key={tag.id + '-' + tag.name}
                className="list-group-item list-group-item-action"
                onClick={() => props.onClickTag(tag)} >
          {tag.name}
        </button>
    );
  });
  return (<div className="list-group">{itemEls}</div>);
}

function App(props) {
  const [tags, setTags] = useState([]);
  const [tag, setTag] = useState(null);
  const [editingTagName, setEditingTagName] = useState(null);

  useEffect(() => {
    Api.tags().then((tagList) => setTags(tagList));
  }, []);

  function handleSelectTag(tag) {
    setTag(tag);
    setEditingTagName(null);
  }

  function handleChangeTagName(newName) {
    setEditingTagName(newName);
  }

  function handleSaveTagName() {
    Api.putTagName(tag.id, editingTagName).then(() => {
      Api.tags().then((tagList) => {
        setTags(tagList);
        if (tag) {
          setTag(tagList.find((t) => t.id === tag.id) || null);
        }
        setEditingTagName(null);
      });
    });
  }

  return (
      <div>
        <Header />
        <main>
          <div className="p-md-5">
            <div className="container">
              <div className="row">
                <div className="col-4">
                  <TagList
                      tags={tags}
                      onClickTag={handleSelectTag} />
                </div>
                <div className="col-8">
                  {
                    tag && (
                        <Tag
                            tag={tag}
                            api={Api}
                            editing={{
                              name: editingTagName,
                              onChange: handleChangeTagName,
                              onSave: handleSaveTagName
                            }} />
                    )
                  }
                </div>
              </div>
            </div>
          </div>
        </main>
      </div>
  );
}

ReactDOM.render(
    <App />,
    document.getElementById('app'));
