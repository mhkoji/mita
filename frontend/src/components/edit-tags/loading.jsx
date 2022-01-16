import React, { useEffect, useState } from "react";
import { Spinner } from "../spinner";

export function Loading(props) {
  const [type, setType] = useState("loading");

  function loadTags() {
    setType("loading");

    Promise.all([props.api.tags(), props.api.contentTags()]).then(
      (result) => {
        const [tags, contentTags] = result;
        props.onLoaded(tags, contentTags);
      },
      () => {
        setType("failed");
      }
    );
  }

  function handleRetryClick() {
    loadTags();
  }

  useEffect(() => loadTags(), []);

  if (type === "loading") {
    return <Spinner />;
  }

  if (type === "failed") {
    return (
      <div>
        <div className="alert alert-danger" role="alert">
          Loading failed
        </div>
        <button className="btn btn-primary" onClick={handleRetryClick}>
          Retry
        </button>
      </div>
    );
  }

  return null;
}
