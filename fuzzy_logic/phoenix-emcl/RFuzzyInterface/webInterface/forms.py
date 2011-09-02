from django import forms
from webInterface.Lib.validation import *
from webInterface.models import *

domainChoices = DOMAIN_CHOICES

from django.forms import ModelForm

class InputFileForm(ModelForm):
    
    class Meta:
        model = InputFile

class ConceptForm(ModelForm):

    class Meta:
        model = Concept


class NegationForm(ModelForm):
    
    class Meta:
        model = Negation


class QuantificationForm(ModelForm):

    class Meta:
        model = Quantification


class ConceptFunctionForm(ModelForm):

    class Meta:
        model = Function

class NegationFunctionForm(ModelForm):

    class Meta:
        model = Function

class QuantificationFunctionForm(ModelForm):

    class Meta:
        model = Function


class SimpleQueryForm(ModelForm):

    class Meta:
        model = SimpleQuery

