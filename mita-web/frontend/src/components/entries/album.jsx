import React, { useState } from 'react';
import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import Header from '../header';

function ImageRow(props) {
  if (props.images.length === 0) {
    return null;
  }
  const cardEls = props.images.map((image) => {
    const viewUrl = '/view/album/' + props.albumId + '#from=' + image.id;
    return (
        <div key={image.id} className="col-md-4">
          <div className="card mb-4 shadow-sm">
            <a href={viewUrl}>
              <img alt={image.id}
                   src={image.url}
                   width="100%"
                   height={400}
                   className="card-img-top bd-placeholder-img" />
            </a>
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
  if (props.album.images.length === 0) {
    return <div>EMPTY!</div>;
  }

  const images = props.album.images;
  const rowList = [];
  const inDeckCount = 3;
  for (let i = 0; i < images.length; i += inDeckCount) {
    const subImages = images.slice(i, Math.min(i + inDeckCount, images.length));
    rowList.push((<ImageRow key={i}
                            albumId={props.album.id}
                            images={subImages} />));
  }

  return <div>{rowList}</div>;
}

import EditAlbumTagsModal from '../edit-tags/edit-tags';
import EditButton from '../edit-tags/edit-button';

function App () {
  const album = window['$mita']['album'];
  const [isEditingTags, setIsEditingTags] = useState(false);
  return (
      <div>
        <Header />

        <main>
          <div>
            <div className="jumbotron">
              <h1 className="display">{album.name}</h1>
              <EditButton onClick={() => setIsEditingTags(true)} />
            </div>
          </div>

          <div className="container">
            <ImageList album={album} />
          </div>
        </main>

        {
          isEditingTags && (
              <EditAlbumTagsModal
                  albumId={album.id}
                  onClose={() => setIsEditingTags(false)} />)
        }
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
