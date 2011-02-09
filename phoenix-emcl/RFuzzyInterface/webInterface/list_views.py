from webInterface.models import *
from annoying.decorators import render_to, ajax_request


@render_to("base_list.html")
def listConcept(request):
    name = "concept"
    entities = Concept.objects.all()
    return locals()

@render_to("base_list.html")
def listNegation(request):
    name = "negation"
    entities = Negation.objects.all()
    return locals()

@render_to("base_list.html")
def listQuantification(request):
    name = "quantification"
    entities = Quantification.objects.all()
    return locals()

