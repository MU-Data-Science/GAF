from django.shortcuts import render
from django.http import HttpResponse

# Create your views here.

def say_hello(request):
    return render(request,"hello.html",{'name':'khawar'})

def say_bye(request):
    return render(request,"bye.html")

    
