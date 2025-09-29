?- problem(no_power).
true.
?- symptom(no_display, S).
S = [turns_on, black_screen, beeps].
?- is_hardware_issue(overheating).
true.
?- is_software_issue(no_internet).
true.
?- can_affect_performance(slow_performance).
true.
?- can_affect_connectivity(printer_not_working).
true.


?- ask('Does the PC turn on?', A).
yes
A = yes.


?- start.
=== ADVANCED PC TROUBLESHOOTER ===
Select the main issue you are experiencing:
1. PC does not turn on
2. PC turns on but no display
3. Very slow performance
4. Overheating or shuts down
5. No internet connection
6. Random restarts
7. Blue screen (BSOD)
8. No sound
9. USB not detected
10. Printer not working
Your choice (1-10): 1

=== Selected problem: no_power
Typical symptoms: [not_turning_on, no_lights, no_fan]

Select the symptoms you notice (e.g., [1,3]):
1. not_turning_on
2. no_lights
3. no_fan
[1,2]
You selected symptoms: [not_turning_on, no_lights]

>>> Ranked possible causes:
 - power_supply (matched symptoms: 2)
 - battery_dead (matched symptoms: 1)


?- is_subcategory(technical_issue, no_power).
true.
?- is_subcategory(technical_issue, blue_screen).
true.
?- is_subcategory(hardware_issue, usb_not_detected).
true.
?- is_subcategory(software_issue, no_sound).
true.
