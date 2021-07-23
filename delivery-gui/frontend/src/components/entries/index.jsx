import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom';

import { ArrowLeft, ArrowRight } from '../fa';
import Header from '../header';
import AlbumList from '../album-list';
import EditAlbumTagsModal from '../edit-tags/edit-tags';

function Pager(props) {
  const { onPrev, onNext } = props;
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

const ws = new WebSocket("ws://" + location.host + "/ws");

function App () {
  const [view, setView] = useState(null);

  useEffect(() => {
    ws.onmessage = (evt) => {
      setView(JSON.parse(evt.data));
    };
    ws.onopen = () => {
      ws.send(JSON.stringify({
        op: "view-albums",
        offset: 0,
        limit: 50
      }));
    };
  }, []);


  if (!view) {
    return (
      <div>
        <Header />
      </div>
    );
  }

  if (view.type === 'loading') {
    return (
      <div>
        <Header />
        Loading...
      </div>
    );
  }

  if (view.type === 'viewing') {
    const pagerProps = {
      onPrev: view.prevOffset !== null && function() {
        ws.send(JSON.stringify({
          op: "view-albums",
          offset: view.prevOffset,
          limit: view.limit
        }));
      },

      onNext: view.nextOffset !== null && function() {
        ws.send(JSON.stringify({
          op: "view-albums",
          offset: view.nextOffset,
          limit: view.limit
        }));
      }
    };
    return (
        <div>
    　    <Header />

          <main className="p-md-5">
            <div className="container">
              <Pager {...pagerProps} />

              <AlbumList
                  albums={view.albums}
                  onEditTags={(albumId) => setEditingTagsAlbumId(albumId)} />

              <Pager {...pagerProps} />
            </div>
          </main>

          {
            view.editingTagsAlbumId && (
                <EditAlbumTagsModal
                    albumId={editingTagsAlbumId}
                    onClose={() => setEditingTagsAlbumId(null)} />)
          }
        </div>
    );
  }

  return (
      <div>
        <Header />
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
