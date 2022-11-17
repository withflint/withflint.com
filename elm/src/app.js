const nullable = (val, defaultVal) => (val === undefined || val === null ? defaultVal : val)

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

const url = new URL(window.location.href)
const path = url.pathname + url.search
const params = new URLSearchParams(url.search)
const utmToken = "utm_source"
const umamiId = "61e2287e-b2c6-44e9-8446-48339059a08c"
const umamiScript = document.querySelector(`script[data-website-id="${umamiId}"]`)

if (params.has(utmToken)) {
  // this branch will never evaluate if there is utm_source param missing
  const queryParams = params.toString()
  cookie.set('utm', queryParams)
  sendPageView(path)
  window.history.pushState({}, '', window.location.href.split('?')[0])

} else if (cookie.get('utm')) {
  const utmParams = cookie.get('utm')
  const utmUrl = `${path}${utmParams}`
  sendPageView(utmUrl)

} else {
  sendPageView(path)
}

function sendPageView(path_) {
  umamiScript.addEventListener('load', () =>
    umami.trackView(path_, 'https://withflint.com', umamiId))
}

app.ports.candidateApplyEvent.subscribe(_msg => {
  sendApplyEvent()
})


function sendApplyEvent() {
  const utm = cookie.get("utm")
  if(utm) {
    const params = utm.split("&")
    const utmInfo = params.map(param => param.split("=")).reduce((acc, [k,v]) => 
      ({ ...acc, [k] : v}), {})
    const evtData = { utm : utmInfo }
    const evtName = "apply" + "-" + evtData.utm.utm_id

    sendUmamiEvent(evtName, JSON.stringify(evtData))
  } else {
    sendUmamiEvent("apply-organic", "nodata")
  }
}

// eventType - apply-{utm_id} / apply-organic
function sendUmamiEvent(eventType, data) {
  // param switched - refactor
  umami.trackEvent(data, eventType)
}
