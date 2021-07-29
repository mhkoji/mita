import React, { useState, useEffect, useRef } from 'react';
import ReactDOM from 'react-dom';

import { Spinner } from '../spinner';
import { ArrowLeft, ArrowRight } from '../fa';
import Header from '../header';
import AlbumList from '../album-list';
import EditAlbumTagsModal from '../edit-tags/edit-tags';
import { Model } from '../../model';

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

function AppAlbumList({ pager, albums, onSelectAlbum, onEditTags,
                        editAlbumTagsModal }) {
  return (
    <div>
      <Header />

      <main className="p-md-5">
        <div className="container">
          <Pager {...pager} />
          <AlbumList
            albums={albums}
            onSelectAlbum={onSelectAlbum}
            onEditTags={onEditTags}
          />
          <Pager {...pager} />
        </div>
      </main>

      <EditAlbumTagsModal {...editAlbumTagsModal} />
    </div>
  );
}

function App (props) {
  const [albumList, setAlbumList] = useState(null);
  const [tagEdit, setTagEdit] = useState(null);
  const modelRef = useRef(null);

  function nextAlbums() {
    modelRef.current.nextAlbums();
  }

  function prevAlbums() {
    modelRef.current.prevAlbums();
  }

  function handleSelectAlbum(albumId) {
    window.open('album.html?albumId=' + albumId);
  }

  function startEditTags(albumId) {
    modelRef.current.startEditTags(albumId);
  }

  useEffect(() => {
    const ws = new WebSocket('ws://localhost:16000/ws');
    ws.addEventListener('open', () => {
      const model = new Model(ws, (state) => {
        if (state.name === 'album-list') {
          setAlbumList(state.view);
        } else if (state.name === 'tag-edit') {
          setTagEdit(state.view);
        }
      });
      modelRef.current = model;
      model.listAlbums();
    });
    return function cleanup() {
      if (modelRef.current) {
        modelRef.current.close();
      }
    };
  }, [])

  if (!albumList) {
    return null;
  }

  if (albumList.type === 'loading') {
    return (
      <AppLoading />
    );
  }

  if (albumList.type === 'album-list') {
    return (
      <AppAlbumList
        pager={{
          onNext: albumList.hasNext && nextAlbums,
          onPrev: albumList.hasPrev && prevAlbums
        }}
        albums={albumList.albums}
        onSelectAlbum={handleSelectAlbum}
        onEditTags={startEditTags}

        editAlbumTagsModal={{
          tagEdit: tagEdit,
          model: modelRef.current
        }}
      />
    );
  }

  return null;
}

ReactDOM.render(
    <App />,
    document.getElementById('app'));
