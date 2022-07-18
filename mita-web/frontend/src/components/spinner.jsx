import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";

export function Spinner() {
  return (
    <div className="d-flex justify-content-center">
      <div
        className="spinner-border m-5"
        style={{ width: "3rem", height: "3rem" }}
        role="status"
      >
        <span className="sr-only">Loading...</span>
      </div>
    </div>
  );
}
