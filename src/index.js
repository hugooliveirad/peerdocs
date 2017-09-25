import './main.css'
import { Main } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

const location = window.location.origin + window.location.pathname
const peer =
  window.location.hash.length > 0 ? window.location.hash.slice(1) : ''

const app = Main.embed(document.getElementById('root'), { location, peer })

var ports = app.ports
var connectedPeer
var conns = {}
function initPeer (options) {
  if (!connectedPeer) {
    connectedPeer = new window.Peer(options.id, { key: options.key })
    connectedPeer.on('connection', function (conn) {
      console.log('connection', conn)
      conn.on('data', function (data) {
        console.log('received', data)
        ports.subscribe.send(data)
      })
    })
  }
}
var cbBuffer = []
function initConn (id, cb) {
  var newConn
  if (conns[id] === 'connecting') {
    // connection being made
    cbBuffer.push([id, cb])
  } else if (conns[id] && conns[id].type === 'data') {
    // connection available
    cb(conns[id])()
  } else {
    // no connection available yet
    cbBuffer.push([id, cb])
    newConn = connectedPeer.connect(id)
    conns[id] = 'connecting'
    newConn.on('open', function () {
      conns[id] = newConn
      cbBuffer.filter(function ([id, cb]) {
        if (conns[id] && conns[id].type === 'data') {
          cb(conns[id])()
          return false
        } else {
          return true
        }
      })
    })
  }
}
function makeSend (options) {
  return function (conn) {
    return function () {
      console.log('sending', options.payload)
      conn.send(options.payload)
    }
  }
}
ports.init.subscribe(initPeer)
ports.send.subscribe(function (options) {
  console.log('sent from Elm', options)
  initPeer({ id: options.id, key: options.key })
  initConn(options.peerId, makeSend(options))
})

registerServiceWorker()
