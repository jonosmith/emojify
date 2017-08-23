import './main.css'
import { Main } from './Main.elm'

const app = Main.embed(document.getElementById('root'))

app.ports.downloadDataUrl.subscribe(dataUrl => {
  const event = new MouseEvent('click', {
    view: window,
    bubbles: true,
    cancelable: true
  });

  const a = document.createElement('a');
  a.setAttribute('download', 'image.png');
  a.setAttribute('href', dataUrl);
  a.setAttribute('target', '_blank');
  a.dispatchEvent(event);
})
