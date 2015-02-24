import sys, os

from MoinMoin.config import multiconfig, url_prefix_static


class LocalConfig(multiconfig.DefaultConfig):
    # vvv DON'T TOUCH THIS EXCEPT IF YOU KNOW WHAT YOU DO vvv
    # Directory containing THIS wikiconfig:
    wikiconfig_dir = os.path.abspath(os.path.dirname(__file__))

    # We assume this structure for a simple "unpack and run" scenario:
    # wikiconfig.py
    # wiki/
    #      data/
    #      underlay/
    # If that's not true, feel free to just set instance_dir to the real path
    # where data/ and underlay/ is located:
    #instance_dir = '/where/ever/your/instance/is'
    instance_dir = wikiconfig_dir

    # Where your own wiki pages are (make regular backups of this directory):
    data_dir = os.path.join(instance_dir, 'data', '') # path with trailing /

    # Where system and help pages are (you may exclude this from backup):
    data_underlay_dir = os.path.join(instance_dir, 'underlay', '') # path with trailing /

    DesktopEdition = True # give all local users full powers
    acl_rights_default = u"All:read,write,delete,revert,admin"
    surge_action_limits = None # no surge protection
    sitename = u'MoinMoin DesktopEdition'
    logo_string = u'<img src="%s/common/moinmoin.png" alt="MoinMoin Logo">' % url_prefix_static
    # ^^^ DON'T TOUCH THIS EXCEPT IF YOU KNOW WHAT YOU DO ^^^

    #page_front_page = u'FrontPage' # change to some better value

    # Add your configuration items here.
    secrets = 'This string is NOT a secret, please make up your own, long, random secret string!'

try:
    from wikiconfig_local import Config
except ImportError, err:
    if not str(err).endswith('wikiconfig_local'):
        raise
    Config = LocalConfig
