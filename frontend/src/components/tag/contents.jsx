import React from "react";

function Content(props) {
  const content = props.content;
  return (
    <div className="col-sm-6">
      <div className="card mb-4 shadow-sm">
        <a href={content.url} target="_blank">
          <img
            alt={content.name || ""}
            src={content.thumbnail ? content.thumbnail.url : ""}
            width="100%"
            height={200}
            className="card-img-top bd-placeholder-img"
          />
        </a>
        <div className="card-body">
          <div className="title">
            <div
              title={content.name}
              style={{
                overflow: "hidden",
                whiteSpace: "nowrap",
                textOverflow: "ellipsis",
              }}
            >
              {content.name}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function ContentRow(props) {
  return (
    <div className="row">
      {props.contents.map((c) => {
        return <Content key={c.type + ":" + c.id} content={c} />;
      })}
    </div>
  );
}

export default function Contents(props) {
  const contents = props.contents;
  const rowList = [];
  const inDeckCount = 2;
  for (let i = 0; i < contents.length; i += inDeckCount) {
    const subList = contents.slice(
      i,
      Math.min(i + inDeckCount, contents.length)
    );
    rowList.push(<ContentRow key={i} contents={subList} />);
  }
  return <div className="p-md-5">{rowList}</div>;
}
