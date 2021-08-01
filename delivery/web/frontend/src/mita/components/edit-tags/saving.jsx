import React, { useEffect, useState } from "react";
import { Spinner } from "../spinner";

export function Saving(props) {
  const [type, setType] = useState("saving");

  function saveTags() {
    setType("saving");

    props.api.putContentTags(props.contentTags).then(
      () => {
        setTimeout(() => {
          setType("saved");
          setTimeout(() => props.onFinishSave(), 500);
        }, 100);
      },
      () => {
        setType("failed");
      }
    );
  }

  function handleRetryClick() {
    saveTags();
  }

  useEffect(() => saveTags(), []);

  if (type === "saving") {
    return <Spinner />;
  }

  if (type === "saved") {
    return <div>Saved!</div>;
  }

  if (type === "failed") {
    return (
      <div>
        <div className="alert alert-danger" role="alert">
          Saving failed
        </div>
        <button className="btn btn-primary" onClick={handleRetryClick}>
          Retry
        </button>
      </div>
    );
  }

  return null;
}
