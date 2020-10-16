import React, { useState } from 'react';
import ReactDOM from 'react-dom';

import AlbumList from '../album-list';
import EditAlbumTagsModal from '../edit-tags/edit-tags';

function App () {
  const [editingTagsAlbumId, setEditingTagsAlbumId] = useState(null);
  const albums = window['$mita']['albums'];
  return (
      <main>
        <AlbumList
            albums={albums}
            onEditTags={(albumId) => setEditingTagsAlbumId(albumId)} />
        {
          editingTagsAlbumId && (
              <EditAlbumTagsModal
                  albumId={editingTagsAlbumId}
                  onClose={() => setEditingTagsAlbumId(null)} />)
        }
      </main>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
