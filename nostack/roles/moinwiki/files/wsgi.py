from werkzeug.wsgi import SharedDataMiddleware
from MoinMoin.web.serving import make_application
from MoinMoin import web
import os

moinmoin = make_application(shared=False)

app = SharedDataMiddleware(moinmoin, {
    '/moin_static198': os.path.join(web.__path__[0], 'static/htdocs')
})

def wsgi_app(env, func):
    env['SCRIPT_NAME'] = '/'
    env['PATH_INFO'] = env['PATH_INFO'][len('/'):]
    return app(env,func)
