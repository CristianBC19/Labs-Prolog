
% PC Troubleshooting System


% ---------------------------
% Step 1: Knowledge Base
% ---------------------------

% Define problems

problem(no_power).
problem(no_display).
problem(slow_performance).
problem(overheating).
problem(no_internet).
problem(random_restart).
problem(blue_screen).
problem(no_sound).
problem(usb_not_detected).
problem(printer_not_working).

% Symptoms
symptom(no_power, [not_turning_on, no_lights, no_fan]).
symptom(no_display, [turns_on, black_screen, beeps]).
symptom(slow_performance, [programs_freeze, long_loading, high_cpu]).
symptom(overheating, [fan_noise, hot_surface, shuts_down]).
symptom(no_internet, [wifi_off, no_connection, error_network]).
symptom(random_restart, [sudden_shutdown, reboot_loop, no_error_message]).
symptom(blue_screen, [error_screen, system_crash, restart_required]).
symptom(no_sound, [no_audio, speaker_icon_x, headphone_not_detected]).
symptom(usb_not_detected, [device_not_recognized, port_not_working]).
symptom(printer_not_working, [no_print, paper_jam, driver_error]).

% Possible causes
cause(no_power, power_supply, [not_turning_on, no_lights]).
cause(no_power, battery_dead, [not_turning_on, no_fan]).
cause(no_display, faulty_monitor, [black_screen]).
cause(no_display, ram_issue, [beeps, black_screen]).
cause(slow_performance, too_many_programs, [programs_freeze, long_loading]).
cause(slow_performance, malware, [long_loading, high_cpu]).
cause(overheating, dust_inside, [fan_noise, hot_surface]).
cause(overheating, thermal_paste, [shuts_down, hot_surface]).
cause(no_internet, router_issue, [no_connection, wifi_off]).
cause(no_internet, network_driver, [error_network, no_connection]).
cause(random_restart, faulty_power, [sudden_shutdown, reboot_loop]).
cause(random_restart, overheating, [sudden_shutdown]).
cause(blue_screen, driver_conflict, [error_screen]).
cause(blue_screen, hardware_failure, [system_crash, restart_required]).
cause(no_sound, muted_audio, [no_audio]).
cause(no_sound, driver_problem, [speaker_icon_x, headphone_not_detected]).
cause(usb_not_detected, faulty_usb_port, [port_not_working]).
cause(usb_not_detected, driver_usb, [device_not_recognized]).
cause(printer_not_working, paper_jam_issue, [paper_jam]).
cause(printer_not_working, driver_error, [driver_error]).
cause(printer_not_working, no_connection, [no_print]).

% ---------------------------
% Step 2: Rules for Classification
% ---------------------------

is_hardware_issue(Problem) :-
    member(Problem, [no_power, no_display, overheating, random_restart, usb_not_detected]).

is_software_issue(Problem) :-
    member(Problem, [slow_performance, blue_screen, no_sound, printer_not_working, no_internet]).

% Generalization
can_affect_performance(slow_performance).
can_affect_performance(overheating).
can_affect_performance(blue_screen).

can_affect_connectivity(no_internet).
can_affect_connectivity(usb_not_detected).
can_affect_connectivity(printer_not_working).

% ---------------------------
% Step 4: Inference Engine
% ---------------------------

start :-
    write('=== ADVANCED PC TROUBLESHOOTER ==='), nl,
    write('Select the main issue you are experiencing:'), nl,
    write('1. PC does not turn on'), nl,
    write('2. PC turns on but no display'), nl,
    write('3. Very slow performance'), nl,
    write('4. Overheating or shuts down'), nl,
    write('5. No internet connection'), nl,
    write('6. Random restarts'), nl,
    write('7. Blue screen (BSOD)'), nl,
    write('8. No sound'), nl,
    write('9. USB not detected'), nl,
    write('10. Printer not working'), nl,
    write('Your choice (1-10): '),
    read(Choice),
    map_choice_to_problem(Choice, Problem),
    handle_problem(Problem).

map_choice_to_problem(1, no_power).
map_choice_to_problem(2, no_display).
map_choice_to_problem(3, slow_performance).
map_choice_to_problem(4, overheating).
map_choice_to_problem(5, no_internet).
map_choice_to_problem(6, random_restart).
map_choice_to_problem(7, blue_screen).
map_choice_to_problem(8, no_sound).
map_choice_to_problem(9, usb_not_detected).
map_choice_to_problem(10, printer_not_working).

handle_problem(Problem) :-
    nl, write('=== Selected problem: '), write(Problem), nl,
    symptom(Problem, Symptoms),
    write('Typical symptoms: '), write(Symptoms), nl, nl,
    
    write('Select the symptoms you notice (e.g., [1,3]):'), nl,
    display_symptoms(Symptoms, 1),
    read(SelectedNumbers),
    
    (is_list(SelectedNumbers) ->
        convert_numbers_to_items(SelectedNumbers, Symptoms, UserSymptoms)
    ;
        write('Invalid input. Please enter a list like [1,2]'), nl,
        UserSymptoms = []
    ),
    
    nl, write('You selected symptoms: '), write(UserSymptoms), nl,
    show_diagnosis(Problem, UserSymptoms).

show_diagnosis(Problem, UserSymptoms) :-
    findall(Cause-Score, (
        cause(Problem, Cause, CauseSymptoms),
        intersection(UserSymptoms, CauseSymptoms, Common),
        length(Common, Score),
        Score > 0
    ), ScoredCauses),

    (   ScoredCauses = [] ->
        nl, write('No likely causes identified. Please consult a technician.'), nl
    ;   
        predsort(compare_scores, ScoredCauses, Sorted),
        nl, write('>>> Ranked possible causes:'), nl,
        display_ranked_causes(Sorted)
    ).

compare_scores(>, _-S1, _-S2) :- S1 > S2.
compare_scores(<, _-S1, _-S2) :- S1 < S2.

% ---------------------------
% Step 5: Recursive Rule 
% ---------------------------

% Classification tree
category(technical_issue, hardware_issue).
category(technical_issue, software_issue).

category(hardware_issue, no_power).
category(hardware_issue, no_display).
category(hardware_issue, overheating).
category(hardware_issue, random_restart).
category(hardware_issue, usb_not_detected).

category(software_issue, slow_performance).
category(software_issue, blue_screen).
category(software_issue, no_sound).
category(software_issue, printer_not_working).
category(software_issue, no_internet).

% Recursive ancestor-like reasoning
is_subcategory(X, Y) :- category(X, Y).              
is_subcategory(X, Y) :- category(X, Z), is_subcategory(Z, Y).  

% ---------------------------
% Helper predicates
% ---------------------------

display_symptoms([], _).
display_symptoms([S|Rest], N) :-
    write(N), write('. '), write(S), nl,
    Next is N + 1,
    display_symptoms(Rest, Next).

convert_numbers_to_items([], _, []).
convert_numbers_to_items([N|Rest], AllItems, [Item|Converted]) :-
    nth1(N, AllItems, Item),
    convert_numbers_to_items(Rest, AllItems, Converted).

display_ranked_causes([]).
display_ranked_causes([Cause-Score|Rest]) :-
    write(' - '), write(Cause), write(' (matched symptoms: '), write(Score), write(')'), nl,
    display_ranked_causes(Rest).