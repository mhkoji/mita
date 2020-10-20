import React from 'react';

function Content(props) {
  const content = props.content;
  return (
      <div key={content.type + ':' + content.id} className="col-md-8">
        <div className="card mb-4 shadow-sm">
          <a href={content.url} target="_blank">
            <img alt={content.name || ''}
                 src={content.thumbnail ? content.thumbnail.url : ''}
                 width="100%"
                 height={400}
                 className="card-img-top bd-placeholder-img"/>
          </a>
          <div className="card-body">
            <div className="title">
              <div title={content.name}
                   style={{
                     overflow: "hidden",
                     whiteSpace: "nowrap",
                     textOverflow: "ellipsis"
                   }}>
                {content.name}
              </div>
            </div>
          </div>
        </div>
      </div>
  );
}

export default function Contents(props) {
  return (
      <div className="p-md-5">
        <div className="row">
          {props.contents.map((content) => <Content content={content} />)}
        </div>
      </div>
  );
}
