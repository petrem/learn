from gpiozero import LED, Button


led = LED(17)
button = Button(2)

on_lit = {
    False: led.on
    True: led.off
}

while True:
    button.wait_for_pressed()
    button.wait_for_released()
    on_lit[led.is_lit]()

