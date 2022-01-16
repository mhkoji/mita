export function tags() {
  return fetch("/api/tags", {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  }).then((resp) => resp.json());
}

export function putTag(name) {
  return fetch("/api/tags/_create?name=" + name, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
  });
}

export function putTagName(tagId, name) {
  return fetch("/api/tags/" + tagId + "?name=" + name, {
    method: "PUT",
    headers: {
      "Content-Type": "application/json",
    },
  });
}

export function deleteTag(tag) {
  return fetch("/api/tags/" + tag.id, {
    method: "DELETE",
    headers: {
      "Content-Type": "application/json",
    },
  });
}

export function tagFolders(tagId) {
  return fetch("/api/tags/" + tagId + "/folders", {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  }).then((resp) => resp.json());
}

export function folderTags(path) {
  return fetch("/api/folder/tags?path=" + encodeURIComponent(path), {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  }).then((resp) => resp.json());
}

export function putFolderTags(path, tags) {
  return fetch("/api/folder/tags?path=" + encodeURIComponent(path), {
    method: "PUT",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      "tag-id-list": tags.map((t) => t.id),
    }),
  });
}
