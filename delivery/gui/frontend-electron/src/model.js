export class Model {
  constructor(ws, onChange) {
    this.ws = ws;

    this.ws.addEventListener("message", (event) => {
      onChange(JSON.parse(event.data));
    });
  }

  close() {
    this.ws.close();
  }

  wsSend(json) {
    this.ws.send(JSON.stringify(json));
  }

  listAlbums() {
    this.wsSend({ op: "album-list:list-albums", limit: 50 });
  }

  prevAlbums() {
    this.wsSend({ op: "album-list:prev" });
  }

  nextAlbums() {
    this.wsSend({ op: "album-list:next" });
  }

  listImages(albumId) {
    this.wsSend({ op: "album:list-images", "album-id": albumId });
  }

  viewAlbum(imageId) {
    this.wsSend({ op: "album:view", "image-id": imageId });
  }

  viewDiff(diff) {
    this.wsSend({ op: "album:view-diff", diff: diff });
  }

  viewSetIndex(imageId) {
    this.wsSend({ op: "album:view-set-index", "image-id": imageId });
  }

  startEditTags(albumId) {
    this.wsSend({ op: "tag-edit:start", "album-id": albumId });
  }

  changeNewName(name) {
    this.wsSend({ op: "tag-edit:change-new-name", name: name });
  }

  addTag(name) {
    this.wsSend({ op: "tag-edit:add-tag", name: name });
  }

  deleteTag(tag) {
    this.wsSend({ op: "tag-edit:delete-tag", "tag-id": tag.id });
  }

  attachTag(tag) {
    this.wsSend({ op: "tag-edit:attach-tag", "tag-id": tag.id });
  }

  detachTag(tag) {
    this.wsSend({ op: "tag-edit:detach-tag", "tag-id": tag.id });
  }

  saveContentTags() {
    this.wsSend({ op: "tag-edit:save-content-tags" });
  }

  stopEditTags() {
    this.wsSend({ op: "tag-edit:stop" });
  }
}
