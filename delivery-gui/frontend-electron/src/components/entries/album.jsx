import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import { Spinner } from '../spinner';
import Header from '../header';
import EditAlbumTagsModal from '../edit-tags/edit-tags';
import EditButton from '../edit-tags/edit-button';

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

class Model {
  constructor(ws) {
    self.ws = ws
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
  const [isEditingTags, setIsEditingTags] = useState(false);

  if (!view) {
    return null;
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
}

function AppWrapper () {
  const [view, setView] = useState(null);

  useEffect(() => {
    const ws = new WebSocket('ws://localhost:16000/ws');
    model = makeModel(ws, setView);
    const albumId = new URL(location).searchParams.get('albumId');
    ws.addEventListener('open', () => model.selectAlbum(albumId));
    return function cleanup() {
      ws.close();
    };
  }, [])

  return (<App view={view} />);
}

ReactDOM.render(
    <AppWrapper />,
    document.getElementById('app'));
