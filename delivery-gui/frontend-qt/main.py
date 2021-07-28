import sys
import json

import PyQt5.QtCore
import PyQt5.QtGui
import PyQt5.QtWidgets

import websocket

WS_URL = 'ws://localhost:6000/ws'

websocket.enableTrace(True)

LIMIT = 5

class Model():
    def __init__(self):
        self.ws = websocket.WebSocketApp(
            WS_URL,
            on_message=self.on_message
        )
        self.on_change = None

    def set_on_change(self, h):
        self.on_change = h
        
    def view_albums(self, limit):
        req = json.dumps({ 'op': 'view-albums', 'limit': LIMIT })
        self.ws.send(req)

    def next_albums(self):
        req = json.dumps({ 'op': 'next-albums' })
        self.ws.send(req)

    def prev_albums(self):
        req = json.dumps({ 'op': 'prev-albums' })
        self.ws.send(req)

    def on_message(self, ws, msg):
        if self.on_change:
            self.on_change(json.loads(msg))
    
    def run_forever(self):
        self.ws.run_forever()

class MainWindow(PyQt5.QtWidgets.QWidget):
    def __init__(self, on_prev, on_next, parent=None):
        super(MainWindow, self).__init__(parent)

        self.setGeometry(300, 50, 400, 350)
        self.setWindowTitle('Mita')

        self.loading = PyQt5.QtWidgets.QLabel(self)
        self.loading.setText('loading')
        self.loading.adjustSize()
        self.loading.move(100, 100)

        self.box = PyQt5.QtWidgets.QHBoxLayout(self)

        self.labels = [PyQt5.QtWidgets.QLabel(self) for _ in range(LIMIT)]
        for lbl in self.labels:
            self.box.addWidget(lbl)

        self.prevButton = PyQt5.QtWidgets.QPushButton(self)
        self.prevButton.setText('prev')
        self.prevButton.clicked.connect(on_prev)
        self.box.addWidget(self.prevButton)

        self.nextButton = PyQt5.QtWidgets.QPushButton(self)
        self.nextButton.setText('next')
        self.nextButton.clicked.connect(on_next)
        self.box.addWidget(self.nextButton)

        self.setLayout(self.box)

    def update_viewing(self, v):
        if self.loading:
            self.loading.deleteLater()
            self.loading = None

        self.prevButton.setEnabled(v['hasPrev'])
        self.nextButton.setEnabled(v['hasNext'])
        
        # show_thumbnails
        for i, album in enumerate(v['albums']):
            path = album['thumbnail']['path']
            pixmap = PyQt5.QtGui.QPixmap(path)
            self.labels[i].setPixmap(pixmap)

def main():
    model = Model()

    import threading
    t = threading.Thread(target=model.run_forever)
    t.start()

    app = PyQt5.QtWidgets.QApplication(sys.argv)
    wnd = MainWindow(
        on_prev=model.prev_albums,
        on_next=model.next_albums
    )

    def handle_change_view(v):
        if v['type'] == 'loading':
            pass
        elif v['type'] == 'viewing':
            wnd.update_viewing(v)

    model.set_on_change(handle_change_view)

    wnd.show()
    model.view_albums(LIMIT)
   
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()
