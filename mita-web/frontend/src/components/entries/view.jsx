import React from 'react';
import ReactDOM from 'react-dom';
import { Single } from '../viewer';

function surroundingImages(images, index, width) {
  const len = images.length;
  const halfWidth = Math.floor(width / 2);
  if (len <= width) {
    return images;
  }
  const begin = index - halfWidth;
  if (begin < 0) {
    return images.slice(0, width);
  }
  if (begin + width <= len) {
    return images.slice(begin, begin + width);
  }
  return images.slice(len - width, len);
}

class WindowWideSingleViewer extends React.Component {
  constructor(props) {
    super(props);
    this.handleResizeWindow = this.handleResizeWindow.bind(this);
    this.handleOnDiff=this.handleOnDiff.bind(this);
    this.state = {
      index: 0,
      size: null
    }
  }

  updateSize(w, h) {
    this.setState((state) => {
      return Object.assign({}, state, {
        size: { width: w, height: h }
      })
    });
  }

  handleResizeWindow() {
    this.updateSize(window.innerWidth, window.innerHeight);
  }

  incrementIndex(diff) {
    this.setState((state) => {
      const addedIndex = state.index + diff;
      const maxIndex = this.props.images.length - 1;
      let newIndex;
      if (addedIndex < 0) {
        newIndex = 0;
      } else if (maxIndex < addedIndex) {
        newIndex = maxIndex;
      } else {
        newIndex = addedIndex;
      }
      return Object.assign({}, state, { index: newIndex });
    })
  }

  handleOnDiff(diff) {
    this.incrementIndex(diff);
  }

  componentDidMount() {
    window.addEventListener('resize', this.handleResizeWindow);
    this.handleResizeWindow();
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.handleResizeWindow);
  }

  render() {
    const currentImage = this.props.images[this.state.index];
    const thumbnails = surroundingImages(this.props.images, this.state.index, 3)
        .map((image) => {
          return {
            image: image,
            link: '',
            isHighlighted: currentImage.id === image.id
          };
        });
    return (
        <Single
            currentImage={currentImage}
            thumbnails={thumbnails}
            progress={{
              now: this.state.index,
              max: this.props.images.length
            }}
            size={this.state.size}
            onDiff={this.handleOnDiff}
        />
    );
  }
}



function App () {
  const images = window['$mita']['images'];
  return (
      <WindowWideSingleViewer images={images} />
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));
