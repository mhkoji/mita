import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap/dist/js/bootstrap.bundle.min.js";

export default function Header() {
  const brand = {
    url: "/",
    name: "Mita",
  };

  const pages = [
    {
      id: "folders",
      name: "Folders",
      url: "/folder",
    },
    {
      id: "tags",
      name: "Tags",
      url: "/tags",
    },
  ];

  return (
    <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
      <a className="navbar-brand" href={brand.url}>
        {brand.name}
      </a>

      <button
        className="navbar-toggler"
        type="button"
        data-toggle="collapse"
        data-target="#header-component-alt-markup"
        aria-controls="navbarNavDropdown"
        aria-expanded="false"
        aria-label="Toggle navigation"
      >
        <span className="navbar-toggler-icon" />
      </button>

      <div
        className="collapse navbar-collapse"
        id="header-component-alt-markup"
      >
        <ul className="navbar-nav mr-auto mt-2 mt-lg-0">
          {pages.map((page) => {
            return (
              <li key={page.id} className="nav-item">
                <a className="nav-link" href={page.url}>
                  {page.name}
                </a>
              </li>
            );
          })}
        </ul>
      </div>
    </nav>
  );
}
