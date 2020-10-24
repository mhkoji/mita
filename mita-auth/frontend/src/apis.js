export function authenticate(username, password) {
  return fetch('/auth/api/authenticate', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      'username': username,
      'password': password
    })
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}
