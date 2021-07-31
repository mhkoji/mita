import React, { useState, useEffect, useRef } from "react";
import ReactDOM from "react-dom";
import "bootstrap/dist/css/bootstrap.min.css";

import { Spinner } from "../spinner";
import Header from "../header";
import EditAlbumTagsModal from "../edit-tags/edit-tags";
import EditButton from "../edit-tags/edit-button";
import { Model } from "../../model";
import { Single } from "../viewer";

function ImageRow(props) {
  if (props.images.length === 0) {
    return null;
  }
  const cardEls = props.images.map((image) => {
    return (
      <div key={image.id} className="col-md-4">
        <div className="card mb-4 shadow-sm">
          <img
            alt={image.id}
            src={image.path}
            width="100%"
            height={400}
            className="card-img-top bd-placeholder-img"
            onClick={() => props.onSelectImage(image.id)}
          />
        </div>
      </div>
    );
  });
  return <div className="row">{cardEls}</div>;
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
    rowList.push(
      <ImageRow
        key={i}
        images={subImages}
        onSelectImage={props.onSelectImage}
      />
    );
  }

  return <div>{rowList}</div>;
}

function WindowWideSingleViewer(props) {
  function getWindowSize() {
    return { width: window.innerWidth, height: window.innerHeight };
  }

  const [windowSize, setWindowSize] = useState(getWindowSize());

  function handleResizeWindow() {
    setWindowSize(getWindowSize());
  }

  useEffect(() => {
    window.addEventListener("resize", handleResizeWindow);
    return function cleanup() {
      window.removeEventListener("resize", handleResizeWindow);
    };
  }, []);

  return (
    <Single
      currentImage={props.currentImage}
      thumbnails={props.thumbnails.map((image) => {
        return {
          image: image,
          isHighlighted: props.currentImage.id === image.id,
          onSelect: () => props.onSelectImage(image.id),
        };
      })}
      progress={props.progress}
      size={windowSize}
      onDiff={props.onDiff}
    />
  );
}

const albumId = new URL(location).searchParams.get("albumId");

function App(props) {
  const [album, setAlbum] = useState(null);
  const [tagEdit, setTagEdit] = useState(null);
  const modelRef = useRef(null);

  function startEditTags() {
    modelRef.current.startEditTags(albumId);
  }

  function handleSelectImage(imageId) {
    modelRef.current.viewAlbum(imageId);
  }

  function handleSelectViewerImage(imageId) {
    modelRef.current.viewSetIndex(imageId);
  }

  function handleDiff(diff) {
    modelRef.current.viewDiff(diff);
  }

  useEffect(() => {
    const ws = new WebSocket("ws://localhost:16000/ws");
    ws.addEventListener("open", () => {
      const model = new Model(ws, (state) => {
        if (state.name === "album") {
          setAlbum(state.view);
        } else if (state.name === "tag-edit") {
          setTagEdit(state.view);
        }
      });
      modelRef.current = model;
      model.listImages(albumId);
    });
    return function cleanup() {
      if (modelRef.current) {
        modelRef.current.close();
      }
    };
  }, []);

  if (!album) {
    return null;
  }

  if (album.type === "loading") {
    return (
      <div>
        <Header />
        <Spinner />
      </div>
    );
  }

  if (album.type === "album") {
    return (
      <div>
        <Header />

        <main>
          <div>
            <div className="jumbotron">
              <h1 className="display">{album.name}</h1>
              <EditButton onClick={startEditTags} />
            </div>
          </div>

          <div className="container">
            <ImageList
              images={album.images}
              onSelectImage={handleSelectImage}
            />
          </div>
        </main>

        <EditAlbumTagsModal tagEdit={tagEdit} model={modelRef.current} />
      </div>
    );
  }

  if (album.type === "viewing") {
    return (
      <WindowWideSingleViewer
        {...album}
        onSelectImage={handleSelectViewerImage}
        onDiff={handleDiff}
      />
    );
  }

  return null;
}

ReactDOM.render(<App />, document.getElementById("app"));
