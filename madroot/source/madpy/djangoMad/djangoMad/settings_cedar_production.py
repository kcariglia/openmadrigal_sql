"""
Django settings for djangoMad project.  Meant as production settings for CEDAR Madrigal databse, since
includes additional cedar app for admins to download log files.

$Id: settings_cedar_production.py 7655 2024-06-27 20:20:49Z kcariglia $
"""

# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
import os
BASE_DIR = os.path.dirname(os.path.dirname(__file__))



# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/1.7/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = '!wa!749ow0!%7t7tr6fr^fvkqyd7yc#mmvpfedr+f2pb!4r)wd'

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG = False


ALLOWED_HOSTS = ['madrigal3.haystack.mit.edu', 'cedar.haystack.mit.edu', 'cedar.openmadrigal.org']

ADMINS = (('Bill Rideout', 'brideout@haystack.mit.edu'),)

EMAIL_HOST = 'hyperion.haystack.mit.edu'

SEND_BROKEN_LINK_EMAILS = True

MANAGERS = (('Bill Rideout', 'brideout@haystack.mit.edu'),)


# Application definition

INSTALLED_APPS = (
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'madweb',
    'bootstrap_calendar',
    'bootstrap3',
    'apps.cedar',
)

MIDDLEWARE_CLASSES = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.auth.middleware.SessionAuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
)

ROOT_URLCONF = 'djangoMad.urls'

WSGI_APPLICATION = 'djangoMad.wsgi.application'



TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [
            os.path.join(BASE_DIR, "templates"),
        ],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.contrib.auth.context_processors.auth',
                'django.template.context_processors.debug',
                'django.template.context_processors.i18n',
                'django.template.context_processors.media',
                'django.template.context_processors.static',
                'django.template.context_processors.tz',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]


# Database
# https://docs.djangoproject.com/en/1.7/ref/settings/#databases

DATABASES = {
}

# Internationalization
# https://docs.djangoproject.com/en/1.7/topics/i18n/

LANGUAGE_CODE = 'en-us'

TIME_ZONE = 'UTC'

USE_I18N = True

USE_L10N = True

USE_TZ = True


# Absolute filesystem path to the directory that will hold user-uploaded files.
# Example: "/home/media/media.lawrence.com/media/"
MEDIA_ROOT = os.path.join(BASE_DIR, 'media')

# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash.
# Examples: "http://media.lawrence.com/media/", "http://example.com/media/"
MEDIA_URL = '/media/'

# Absolute path to the directory static files should be collected to.
# Don't put anything in this directory yourself; store your static files
# in apps' "static/" subdirectories and in STATICFILES_DIRS.
# Example: "/home/media/media.lawrence.com/static/"
# STATIC_ROOT = os.path.join(BASE_DIR, 'static')

# URL prefix for static files.
# May also be something like /madrigal/static if MADSERVERROOT non-zero length
STATIC_URL = '/static/'

BOOTSTRAP3 = {
    # Include jQuery with Bootstrap JavaScript (affects django-bootstrap3 template tags)
    'jquery_url': "{% static 'jquery.min.js' %}",
    'include_jquery': True,
    "css_url": "{% static 'lib/css/bootstrap.min.css' %}",
    "javascript_url": "{% static 'lib/js/bootstrap.min.js' %}"
}

# stuff for email throttling and their defaults
EMAILTHROTTLER_TMPDIR = '/tmp'
EMAILTHROTTLER_PREFIX = 'emailthrottler-'
EMAILTHROTTLER_INTERVAL = 600
EMAILTHROTTLER_TRUNCATE_SUBJECT = None # 128 is also ok
EMAILTHROTTLER_SUBJECT_THRESHOLD = 3 # 0 for no-check
EMAILTHROTTLER_OVERALL_THRESHOLD = 6 # 0 for no-check

LOGGING = {
    'version': 1,
    'disable_existing_loggers': True,
    'handlers': {
        'mail_admins': {
            'level': 'ERROR',
            'class': 'django.utils.log.AdminEmailHandler',
            'email_backend': 'django_email_throttler.throttler.ThrottledEmailBackend',
            'include_html': False,
        }
    },
    'loggers': {
        'django': {
            'handlers': ['mail_admins'],
            'level': 'ERROR',
            'propagate': True,
        },
    }
}
