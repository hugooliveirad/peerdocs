import './main.css'
import {Main} from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

const location = window.location.origin + window.location.pathname
const peer = window.location.hash.length > 0
  ? window.location.hash.slice(1)
  : ''

const app = Main.embed(document.getElementById('root'), {location, peer})

registerServiceWorker()
