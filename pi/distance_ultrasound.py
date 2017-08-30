import time

from gpiozero import DistanceSensor

ultrasonic = DistanceSensor(echo=17, trigger=4)

while True:
    print("Distance: {} m ({} mm)".format(
        ultrasonic.distance, ultrasonic * 1000)
    time.sleep(0.1)
