export function tags() {
  return fetch('/api/tags', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

export function putTag(name) {
  return fetch('/api/tags/_create?name=' + name, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    }
  });
}

export function putTagName(tagId, name) {
  return fetch('/api/tags/' + tagId + '?name=' + name, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json'
    }
  });
}

export function deleteTag(tag) {
  return fetch('/api/tags/' + tag.id, {
    method: 'DELETE',
    headers: {
      'Content-Type': 'application/json'
    }
  });
}

export function tagContents(tagId) {
  return fetch('/api/tags/' + tagId + '/contents', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

export function albumTags(albumId) {
  return fetch('/api/albumTags/' + albumId, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

export function putAlbumTags(albumId, tags) {
  return fetch('/api/albumTags/' + albumId, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      'tag-id-list': tags.map((t) => t.id)
    })
  });
}

export function logout() {
  return fetch('/auth/api/logout', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}
