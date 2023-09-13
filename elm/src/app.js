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

const gtm = f => window.gtag && f(window.gtag)
const um = f => window.umami && f(window.umami)
const fb = f => window.fbq && f(window.fbq)

const sentToUmami = path =>
  um(umami => {
    const umamiId = '61e2287e-b2c6-44e9-8446-48339059a08c'
    const element = document && document.querySelector(`script[data-website-id="${umamiId}"]`)
    if (element)
      element.addEventListener('load', () =>
        umami.trackView(path, 'https://withflint.com', umamiId))
  })

const load = () => {
  if (app)

    app.ports.toggleNavMenu.subscribe(_ => {
      app.ports.navMenuToggled.send(_)
    })

    app.ports.candidateApplyEvent.subscribe(_msg => {
      gtm(gtm => gtm('event', 'conversion'))
      fb(fbq => fbq('track', 'SubmitApplication'))

      const utm = cookie.get('utm')
      if (utm) {
        const params = utm.split('&')
        const utmInfo = params
          .map(param => param.split('='))
          .reduce((acc, [k, v]) => ({ ...acc, [k]: v }), {})

        um(umami => umami.trackEvent({ utm: utmInfo }, 'apply' + '-' + utmInfo.utm_id))
      } else {
        um(umami => umami.trackEvent('nodata', 'apply-organic'))
      }
    })

  const url = new URL(window.location.href)
  const params = new URLSearchParams(url.search)
  const utm = params.has('utm_source') ? params.toString() : cookie.get('utm')

  sentToUmami(`${url.pathname}${utm}`)

  if (params.has('utm_source')) {
    cookie.set('utm', utm)
    if (window) window.history.pushState({}, '', window.location.href.split('?')[0])
  }
}

load()
