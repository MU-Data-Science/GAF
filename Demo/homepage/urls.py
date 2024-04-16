from django.urls import path
from . import views 

urlpatterns = [
    path('',views.execute_command_view),
]  