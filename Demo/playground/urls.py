from django.urls import path
from . import views 

urlpatterns = [
    path('hello/',views.say_hello), 
    path('bye/',views.say_bye),
]  