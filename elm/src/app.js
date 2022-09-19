const nullable = (val, defaultVal) => (val === undefined || val === null) ? defaultVal : val

const cookie = {
  set: (name, value) => {
    const days = 1
    const date = new Date()
    date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000)
    const expires = '; expires=' + date.toUTCString()
    document.cookie = name + '=' + (value || '') + expires + '; path=/'
  },
  get: name => {
    const nameEQ = name + '='
    const ca = document.cookie.split(';')
    for (var i = 0; i < ca.length; i++) {
      var c = ca[i]
      while (c.charAt(0) == ' ') c = c.substring(1, c.length)
      if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length)
    }
    return null
  }
}

const url = new URL(window.location.href);
const path = (url.pathname + url.search)
const params = new URLSearchParams(url.search);
const umamiId = '61e2287e-b2c6-44e9-8446-48339059a08c'
const umamiScript = document.querySelector(`script[data-website-id="${umamiId}"]`)

if (cookie.get('utm')) {
  umamiScript.addEventListener('load', () => 
    umami.trackView(path, 'https://withflint.com', umamiId)
  )
}

// does not assume utm_content is the first param
if (params.has('utm')) {
  const utmVal = params.get('utm')
  
  cookie.set('utm', utmVal)
  window.history.pushState({}, "", window.location.href.split('?')[0])
  umamiScript.addEventListener('load', () => 
    umami.trackView(path, 'https://withflint.com', umamiId)
  )
}

