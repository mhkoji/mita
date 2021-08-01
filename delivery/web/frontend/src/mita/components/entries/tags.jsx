import React from "react";
import ReactDOM from "react-dom";

import * as apis from "../../apis";
import Header from "../header";
import Tag from "../tag/tag";

const Api = {
  tags: apis.tags,
  putTagName: apis.putTagName,
  tagContents: apis.tagContents,
};

function TagList(props) {
  const itemEls = props.tags.map((tag) => {
    let className = "list-group-item list-group-item-action";
    if (tag === props.selectedTag) {
      className += " active";
    }
    return (
      <a
        key={tag.id + "-" + tag.name}
        className={className}
        href={"tags#" + tag.id}
      >
        {tag.name}
      </a>
    );
  });
  return <div className="list-group">{itemEls}</div>;
}

class App extends React.Component {
  constructor(props) {
    super(props);

    this.handleChangeTagName = this.handleChangeTagName.bind(this);
    this.handleSaveTagName = this.handleSaveTagName.bind(this);
    this.selectTagByHash = this.selectTagByHash.bind(this);

    this.state = {
      tags: [],
      tag: null,
      editingTagName: null,
    };
  }

  selectTag(tag) {
    this.setState((state) => {
      return Object.assign({}, state, {
        tag: tag,
        editingTagName: null,
      });
    });
  }

  selectTagByHash() {
    if (!location.hash.startsWith("#")) {
      return;
    }
    const tagId = location.hash.substring(1);
    const tag = this.state.tags.find((t) => t.id === tagId);
    if (tag) {
      this.selectTag(tag);
    }
    return tag;
  }

  handleChangeTagName(newName) {
    this.setState((state) => {
      return Object.assign({}, state, {
        editingTagName: newName,
      });
    });
  }

  handleSaveTagName() {
    Api.putTagName(this.state.tag.id, this.state.editingTagName).then(() => {
      Api.tags().then((tagList) => {
        this.setState((state) => {
          return {
            tags: tagList,
            tag: tagList.find((t) => t.id === state.tag.id) || null,
            editingTagName: null,
          };
        });
      });
    });
  }

  componentDidMount() {
    Api.tags()
      .then((tagList) => {
        this.setState((state) => {
          return Object.assign({}, state, {
            tags: tagList,
          });
        });
      })
      .then(() => {
        if (this.selectTagByHash()) {
          return;
        }
        if (0 < this.state.tags.length) {
          this.selectTag(this.state.tags[0]);
        }
      });
    window.addEventListener("hashchange", this.selectTagByHash);
  }

  componentWillUnmount() {
    window.removeEventListener("hashchange", this.selectTagByHash);
  }

  render() {
    const { tags, tag, editingTagName } = this.state;
    return (
      <div>
        <Header />
        <main>
          <div className="p-md-5">
            <div className="container">
              <div className="row">
                <div className="col-4">
                  <TagList tags={tags} selectedTag={tag} />
                </div>
                <div className="col-8">
                  {tag && (
                    <Tag
                      tag={tag}
                      api={Api}
                      editing={{
                        name: editingTagName,
                        onChange: this.handleChangeTagName,
                        onSave: this.handleSaveTagName,
                      }}
                    />
                  )}
                </div>
              </div>
            </div>
          </div>
        </main>
      </div>
    );
  }
}

ReactDOM.render(<App />, document.getElementById("app"));
