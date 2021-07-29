import React, { useState, useEffect, useRef } from 'react';
import ReactDOM from 'react-dom';

import { Spinner } from '../spinner';
import { ArrowLeft, ArrowRight } from '../fa';
import Header from '../header';
import AlbumList from '../album-list';

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


function AppLoading() {
  return (
    <div>
      <Header />
      <Spinner/>
    </div>
  );
}

function AppAlbumList({pager, albums, onSelectAlbum, onEditTags}) {
  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <Pager {...pager} />
          <AlbumList
            albums={albums}
            onSelectAlbum={onSelectAlbum}
            onEditTags={onEditTags} />
          <Pager {...pager} />
        </div>
      </main>
    </div>
  );
}

function App () {
  const wsRef = useRef(null);

  function listAlbums() {
    wsRef.current.send(JSON.stringify({ 'op': 'list-albums', 'limit': 50 }));
  }

  function prevAlbums() {
    wsRef.current.send(JSON.stringify({ 'op': 'prev-albums' }));
  }

  function nextAlbums() {
    wsRef.current.send(JSON.stringify({ 'op': 'next-albums' }));
  }

  ////

  const [view, setView] = useState(null);
  const [editingTagsAlbumId, setEditingTagsAlbumId] = useState(null);

  function handleSelectAlbum(albumId) {
    window.open('album.html?albumId=' + albumId);
  }

  useEffect(() => {
    const ws = new WebSocket('ws://localhost:16000/ws');
    ws.addEventListener('message', (event) => setView(JSON.parse(event.data)));
    ws.addEventListener('open', () => listAlbums());
    wsRef.current = ws;
    return function cleanup() { ws.close(); };
  }, [])

  if (!view) {
    return null;
  }

  if (view.state === 'album-list') {
    if (view.type === 'loading') {
      return (
        <AppLoading />
      );
    }

    if (view.type === 'album-list') {
      return (
        <AppAlbumList
          pager={{
            onNext: view.hasNext && (() => nextAlbums()),
            onPrev: view.hasPrev && (() => prevAlbums())
          }}
          albums={view.albums}
          onSelectAlbum={handleSelectAlbum}
          onEditTags={(albumId) => setEditingTagsAlbumId(albumId)}
        />
      );
    }
  }

  return null;
}

ReactDOM.render(
    <App />,
    document.getElementById('app'));
