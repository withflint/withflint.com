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
// const umamiId = "61e2287e-b2c6-44e9-8446-48339059a08c" -- prod id
const umamiId = "c3726ed1-bdc5-4e31-ab03-975852d4f55d" // dev id
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


// get from utmVal from cookie // check for race condition


app.ports.candidateApplyEvent.subscribe(candidate => {
  sendCandidateEvent(candidate)
})


function sendCandidateEvent(candidate) {
  const utm = cookie.get("utm")
  if(utm) {
    const params = utm.split("&")
    const utmInfo = params.map(param => param.split("=")).reduce((acc, [k,v]) => 
      ({ ...acc, [k] : v}), {})
    const evtData = { utm : utmInfo, candidate : candidate}
    console.log("sending utm applicant")
    sendUmamiEvent("apply-utm", evtData)
    console.log("sent utm applicant")
  } else {
    console.log("sending organic applicant")
    sendUmamiEvent("apply-organic", candidate)
    console.log("sent organic applicant")
  }
}

// eventType - apply-utm / apply-organic
function sendUmamiEvent(eventType, data) {
  console.log("sending candidate evt")
  umami.trackEvent(eventType, data)
  console.log("sent candidate evt")
  console.log("evt type", eventType)
  console.log("evt data", data)
}
