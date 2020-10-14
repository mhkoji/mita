import React from 'react';
import ReactDOM from 'react-dom';

import 'bootstrap/dist/css/bootstrap.min.css';

function AlbumRow(props) {
  if (props.albums.length === 0) {
    return null;
  }
  const cardEls = props.albums.map((album) => {
    return (
        <div key={album.id} className="col-md-4">
          <div className="card mb-4 shadow-sm">
            <a href={album.url} target="_blank">
              <img alt={album.name}
                   src={album.thumbnail.url}
                   width="100%"
                   height={400}
                   className="card-img-top bd-placeholder-img" />
            </a>
            <div className="card-body">
              <div className="title">
                <div style={{
                  overflow: "hidden",
                  whiteSpace: "nowrap",
                  textOverflow: "ellipsis" }}
                     title={album.name} >
                  {album.name}
                </div>
              </div>
            </div>
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

function AlbumList(props) {
  if (props.albums.length === 0) {
    return <div>EMPTY!</div>;
  }

  const rowList = [];
  const inDeckCount = 3;
  for (let i = 0; i < props.albums.length; i += inDeckCount) {
    const albums = props.albums.slice(i, Math.min(i + inDeckCount, props.albums.length));
    rowList.push((<AlbumRow key={i} albums={albums} />));
  }

  return <div className="container">{rowList}</div>;
}

function App () {
  const albums = window['$mita']['albums'];
  return (
      <main>
        <AlbumList albums={albums} />
      </main>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
