import React from 'react';
import ReactDOM from 'react-dom';

import 'bootstrap/dist/css/bootstrap.min.css';

function ImageRow(props) {
  if (props.images.length === 0) {
    return null;
  }
  const cardEls = props.images.map((image) => {
    return (
        <div key={image.id} className="col-md-4">
          <div className="card mb-4 shadow-sm">
            <a href={"/view/album/" + props.albumId}>
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

  return <div className="container">{rowList}</div>;
}

function App () {
  const album = window['$mita']['album'];
  return (
      <main>
        <ImageList album={album} />
      </main>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
