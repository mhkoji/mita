import sys
import json

import PyQt5.QtCore
import PyQt5.QtGui
import PyQt5.QtWidgets

import websocket

WS_URL = 'ws://localhost:6000/ws'

websocket.enableTrace(True)

class Model():
    def __init__(self):
        self.ws = websocket.WebSocketApp(
            WS_URL,
            on_message=self.on_message
        )
        self.on_change = None

    def set_on_change(self, h):
        self.on_change = h
        
    def viewAlbums(self, offset, limit):
        req = json.dumps({
            'op': 'view-albums',
            'offset': offset,
            'limit': limit
        })
        self.ws.send(req)

    def on_message(self, ws, msg):
        if self.on_change:
            self.on_change(json.loads(msg))
    
    def run_forever(self):
        self.ws.run_forever()

class MainWindow(PyQt5.QtWidgets.QWidget):
    def __init__(self, on_more, parent=None):
        super(MainWindow, self).__init__(parent)

        self.on_more = on_more

        self.setGeometry(300, 50, 400, 350)
        self.setWindowTitle('Mita')

        self.loading = PyQt5.QtWidgets.QLabel(self)
        self.loading.setText('loading')
        self.loading.adjustSize()
        self.loading.move(100, 100)

        button = PyQt5.QtWidgets.QPushButton(self)
        button.setText('button')
        button.clicked.connect(self.handleClickButton)

        self.box = PyQt5.QtWidgets.QHBoxLayout(self)
        self.labels = [PyQt5.QtWidgets.QLabel(self) for _ in range(5)]
        for lbl in self.labels:
            self.box.addWidget(lbl)
        self.box.addWidget(button)
        self.setLayout(self.box)

    def handleClickButton(self):
        self.on_more()

    def showThumbnails(self, albums):
        for i, album in enumerate(albums):
            path = album['thumbnail']['path']
            pixmap = PyQt5.QtGui.QPixmap(path)
            self.labels[i].setPixmap(pixmap)

data = {
    'offset': 0,
    'limit': 5
}

def main():
    model = Model()

    import threading
    t = threading.Thread(target=model.run_forever)
    t.start()

    def handleMore():
        model.viewAlbums(data['offset'], data['limit'])

    app = PyQt5.QtWidgets.QApplication(sys.argv)
    wnd = MainWindow(on_more=handleMore)

    def handleChangeView(v):
        if v['type'] == 'loading':
            pass
        elif v['type'] == 'viewing':
            if wnd.loading:
                wnd.loading.deleteLater()
                wnd.loading = None
            wnd.showThumbnails(v['albums'])
            data['offset'] = v['nextOffset']

    model.set_on_change(handleChangeView)

    wnd.show()
    model.viewAlbums(data['offset'], data['limit'])
   
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()
