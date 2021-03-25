import React from 'react';

import './viewer.css';
import 'bootstrap/dist/css/bootstrap.min.css';

function Progress(props) {
  const { now, max } = props;
  const width = 100 * ((now + 1) / max);
  return (
      <div className="progress">
        <div className="progress-bar"
             style={{
               width: width + '%'
             }}
             role="progressbar"
             aria-valuenow={width}
             aria-valuemin="0"
             aria-valuemax="max" />
      </div>
  );
}

function resizeViewer(imageEl, thumbEl, size) {
  function setMaxSize(el, width, height) {
    if (width) {
      el.style.maxWidth = width + 'px';
    }
    if (height) {
      el.style.maxHeight = height + 'px';
    }
  }

  if (imageEl && thumbEl && size) {
    setMaxSize(imageEl, size.width - thumbEl.offsetWidth, size.height);
    setMaxSize(thumbEl, undefined, size.height);
  }
}

function makeDiff(isForward) {
  return isForward ? 1 : -1;
}

function isForwardKeydown(evt) {
  const { keyCode } = evt;
  return keyCode === 32 || keyCode === 39 || keyCode === 40;
}

function isForwardWheel(evt) {
  return 0 < evt.deltaY;
}

export class Single extends React.Component {
  constructor(props) {
    super(props);

    this.imageRef = React.createRef();
    this.thumbRef = React.createRef();

    this.handleKeydown = this.handleKeydown.bind(this);
    this.handleOnWheel = this.handleOnWheel.bind(this);
  }

  handleKeydown(evt) {
    this.props.onDiff(makeDiff(isForwardKeydown(evt)));
  }

  handleOnWheel(evt) {
    this.props.onDiff(makeDiff(isForwardWheel(evt)));
  }

  componentDidMount() {
    resizeViewer(this.imageRef.current, this.thumbRef.current, this.props.size);
    window.addEventListener('keydown', this.handleKeydown);
  }

  componentDidUpdate(prevProps, prevState, snapshot) {
    resizeViewer(this.imageRef.current, this.thumbRef.current, this.props.size);
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.handleKeydown);
  }

  render() {
    const { currentImage, thumbnails, progress } = this.props;
    const baseName = 'mita-component-viewer-thumbnail';
    const thumbnailEls = thumbnails.map((thumb) => {
      const {image, link, isHighlighted} = thumb;
      return (
          <div key={image.id}
               className={baseName + (isHighlighted ? ' ' + baseName + '-highlighted' : '')}>
            <a href={link}>
              <img alt={image.id} src={image.url}/>
            </a>
          </div>
      );
    });

    return (
        <div className="mita-component-singlevimageviewer">
          <div className="mita-component-singleimageviewer">
            <div className="mita-component-singleimageviewer-left"
                 style={{position: "absolute", float: "left"}}>
              <div className="mita-component-singleimageviewer-image"
                   onWheel={this.handleOnWheel}>
                <div style={{ display: "inline-block" }}>
                  <img alt={currentImage.id}
                       src={currentImage.url}
                       ref={this.imageRef} />
                </div>
              </div>
            </div>

            <div className="mita-component-singleimageviewer-right">
              <div ref={this.thumbRef} className="mita-component-viewer-thumbnails">
                {thumbnailEls}
                <Progress {...progress} />
              </div>
            </div>
          </div>
        </div>
    );
  }
}
