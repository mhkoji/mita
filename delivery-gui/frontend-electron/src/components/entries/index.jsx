import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom';

import { Spinner } from '../spinner';
import { ArrowLeft, ArrowRight } from '../fa';
import Header from '../header';
import AlbumList from '../album-list';
import EditAlbumTagsModal from '../edit-tags/edit-tags';
import EditButton from '../edit-tags/edit-button';
import 'bootstrap/dist/css/bootstrap.min.css';

function ImageRow(props) {
  if (props.images.length === 0) {
    return null;
  }
  const cardEls = props.images.map((image) => {
    return (
        <div key={image.id} className="col-md-4">
          <div className="card mb-4 shadow-sm">
            <img alt={image.id}
                 src={image.path}
                 width="100%"
                 height={400}
                 className="card-img-top bd-placeholder-img" />
          </div>
        </div>
    );
  });
  return (
      <div className="row">
        {cardEls}
      </div>
  );
}

function ImageList(props) {
  const images = props.images;
  if (images.length === 0) {
    return <div>EMPTY!</div>;
  }

  const rowList = [];
  const inDeckCount = 3;
  for (let i = 0; i < images.length; i += inDeckCount) {
    const subImages = images.slice(i, Math.min(i + inDeckCount, images.length));
    rowList.push((<ImageRow key={i} images={subImages} />));
                            
  }

  return <div>{rowList}</div>;
}

function Pager({ onPrev, onNext }) {
  return (
      <nav aria-label="pager">
        <ul className="pagination justify-content-center">
          <li className={'page-item' + (onPrev ? '' : ' disabled')}>
            <button className="page-link" onClick={onPrev}>
              <ArrowLeft />
            </button>
          </li>
          <li className={'page-item' + (onNext ? '' : ' disabled')}>
            <button className="page-link" onClick={onNext}>
              <ArrowRight />
            </button>
          </li>
        </ul>
      </nav>
  );
}


class Model {
  constructor(ws) {
    self.ws = ws
  }
  
  listAlbums() {
    self.ws.send(JSON.stringify({ 'op': 'list-albums', 'limit': 50 }));
  }

  prevAlbums() {
    self.ws.send(JSON.stringify({ 'op': 'prev-albums' }));
  }

  nextAlbums() {
    self.ws.send(JSON.stringify({ 'op': 'next-albums' }));
  }

  selectAlbum(albumId) {
    self.ws.send(JSON.stringify({ 'op': 'select-album', 'album-id': albumId }));
  }
}

function makeModel(ws, onViewUpdate) {
  ws.addEventListener('message', (event) => {
    onViewUpdate(JSON.parse(event.data));
  });
  return new Model(ws);
}

let model = null;

function App({view}) {
  const [editingTagsAlbumId, setEditingTagsAlbumId] = useState(null);

  if (!view) {
    return null;
  }

  if (view.state === 'album-list') {
    if (view.type === 'loading') {
      return (
        <div>
          <Header />
          <Spinner/>
        </div>
      );
    }

    if (view.type === 'album-list') {
      const pagerProps = {
        onNext: view.hasNext && (() => model.nextAlbums()),
        onPrev: view.hasPrev && (() => model.prevAlbums())
      };
      return (
          <div>
            <Header />

              <main className="p-md-5">
              <div className="container">
                <Pager {...pagerProps} />

                <AlbumList
                    albums={view.albums}
                    onSelectAlbum={(albumId) => model.selectAlbum(albumId)}
                    onEditTags={(albumId) => setEditingTagsAlbumId(albumId)} />

                <Pager {...pagerProps} />
              </div>
            </main>
          </div>
      );
    }
  }

  if (view.state === 'album') {
    if (view.type === 'loading') {
      return (
        <div>
          <Header />
          <Spinner/>
        </div>
      );
    }

    if (view.type === 'album') {
      return (
          <div>
            <Header />

            <main>
              <div>
                <div className="jumbotron">
                  <h1 className="display">{view.name}</h1>
                  <EditButton onClick={() => setIsEditingTags(true)} />
                </div>
              </div>

              <div className="container">
                <ImageList images={view.images} />
              </div>
            </main>
          </div>
      );
    }

  }

  return null;
}

function AppWrapper () {
  const [view, setView] = useState(null);

  useEffect(() => {
    const ws = new WebSocket('ws://localhost:16000/ws');
    model = makeModel(ws, setView);
    ws.addEventListener('open', () => model.listAlbums());
    return function cleanup() {
      ws.close();
    };
  }, [])

  return (<App view={view} />);
}

ReactDOM.render(
    <AppWrapper />,
    document.getElementById('app'));
