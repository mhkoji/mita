import React, { useState } from 'react';
import ReactDOM from 'react-dom';

function AlbumList(props) {
  const albumEls = props.albums.map((album) => {
    return (
        <div key={album.id}>
          {album.name}
          <img src={album.thumbnailImage.url} />
        </div>
    );
  });
  return <div>{albumEls}</div>;
}

function App () {
  const albums = window['$mita']['albums'];
  return (
    <div>
      <AlbumList albums={albums} />
    </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
