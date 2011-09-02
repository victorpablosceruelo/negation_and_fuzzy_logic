from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings


from django.contrib import admin
admin.autodiscover()


urlpatterns = patterns('',
                       (r'^$', direct_to_template, {'template':'front.html'}),
                       (r'^success/$', direct_to_template, {'template':'success.html'}),
                       (r'^concept/add/$', 'webInterface.views.createConcept'),
                       (r'^concept/check/$', 'webInterface.views.checkConceptFunction'),
                       (r'^concept/list/$', 'webInterface.list_views.listConcept'),
                       (r'^concept/edit/$', 'webInterface.edit_views.editConcept'),
                       (r'^concept/functions/delete/$', 'webInterface.edit_views.deleteFunction'),
                       (r'^concept/functions/checkandadd/$', 'webInterface.edit_views.checkAndAddConceptFunction'),

                       (r'^negation/add/$', 'webInterface.views.createNegation'),
                       (r'^negation/check/$', 'webInterface.views.checkNegationFunction'),
                       (r'^negation/list/$', 'webInterface.list_views.listNegation'),
                       (r'^negation/edit/$', 'webInterface.edit_views.editNegation'),
                       (r'^negation/functions/delete/$', 'webInterface.edit_views.deleteNegation'),
                       (r'^negation/functions/checkandadd/$', 'webInterface.edit_views.checkAndAddNegationFunction'),

                       (r'^quantification/check/$', 'webInterface.views.checkQuantificationFunction'),
                       (r'^quantification/add/$', 'webInterface.views.createQuantification'),
                       (r'^quantification/list/$', 'webInterface.list_views.listQuantification'),
                       (r'^quantification/edit/$', 'webInterface.edit_views.editQuantification'),
                       (r'^quantification/functions/delete/$', 'webInterface.edit_views.deleteQuantification'),
                       (r'^quantification/functions/checkandadd/$', 'webInterface.edit_views.checkAndAddQuantificationFunction'),

                       (r'^loadingfile/', 'webInterface.views.loadFile'),
                       (r'^query/', 'webInterface.views.query'),
                       (r'^results/', 'webInterface.views.results'),
                       # Added Admin for viewing values
                       (r'^admin/', include(admin.site.urls)),

                       )

if settings.DEBUG:
    urlpatterns += patterns('',
                            (r'^site_media/(?P<path>.*)$', 'django.views.static.serve',
                             {'document_root': settings.STATIC_DOC_ROOT}),
                            )

