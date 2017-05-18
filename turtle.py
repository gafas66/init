# Test of turtle

import turtle
    
def pulse (turtle,size = 10):
    turtle.forward(size)
    turtle.left(90)
    turtle.forward(size)
    turtle.right(90)
    turtle.forward(size)
    turtle.right(90)
    turtle.forward(size)
    turtle.left(90)


def ex_gate (t,s=10):
    for _ in range(5):
        pulse(t,s)
    t.pu();t.goto(0,-2*s);t.pd() # Next line
    for _ in range(3):
        pulse(t,s) ;t.fd(s)
    t.fd(10)
    t.pu();t.goto(0,-4*s);t.pd() # Next line
    pulse(t,s) ;t.fd(4*s)
    pulse(t,s) ;t.fd(2*s)
    
def square(turtle,size=10):
    for _ in range(4):
        turtle.forward(size)
        turtle.left(90)
     
# end of script
