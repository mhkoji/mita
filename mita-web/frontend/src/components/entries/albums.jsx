import React, { useState } from 'react';
import ReactDOM from 'react-dom';

import Header from '../header';
import AlbumList from '../album-list';
import EditAlbumTagsModal from '../edit-tags/edit-tags';

function App () {
  const albums = window['$mita']['albums'];
  const [editingTagsAlbumId, setEditingTagsAlbumId] = useState(null);
  return (
      <div>
        <Header />

        <main className="p-md-5">
          <div className="container">
            <AlbumList
                albums={albums}
                onEditTags={(albumId) => setEditingTagsAlbumId(albumId)} />
          </div>
        </main>

        {
          editingTagsAlbumId && (
              <EditAlbumTagsModal
                  albumId={editingTagsAlbumId}
                  onClose={() => setEditingTagsAlbumId(null)} />)
        }
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
