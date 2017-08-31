import './main.css'
import { Main } from './Main.elm'

const app = Main.embed(document.getElementById('root'))

/**
 * Forces a browser download of an image using the given image url
 */
app.ports.downloadDataUrl.subscribe(url => {
  const event = new MouseEvent('click', {
    view: window,
    bubbles: true,
    cancelable: true
  })

  const a = document.createElement('a')
  a.setAttribute('download', 'image.png')
  a.setAttribute('href', url)
  a.setAttribute('target', '_blank')
  a.dispatchEvent(event)
})

/**
 * Gets the dimensions of an image using the given url
 */
app.ports.imageDimensionsRequest.subscribe(url => {
  const image = new Image()
  image.src = url
  image.onload = () => {
    const width = image.naturalWidth
    const height = image.naturalHeight

    app.ports.imageDimensionsResponse.send({ width, height })
  }
})
