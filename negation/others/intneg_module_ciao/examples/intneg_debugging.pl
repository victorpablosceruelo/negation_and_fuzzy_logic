0
even(0)
even(s(s(_2333))):-even(_2333)
odd(_2316):- \+even(_2316)
intneg(even(_2364)):-dist(_2364,0),dist(_2364,s(s(fA(_2333))))
intneg(even(s(s(_2333)))):-intneg(even(_2333))
intneg(odd(_2316)):-intneg(\+even(_2316))
intneg((_2616;_2617)):-intneg(_2616),intneg(_2617)
intneg((_2616,_2617)):-intneg(_2616)
intneg((_2616,_2617)):-intneg(_2617)
intneg(intneg(_2657)):-call(_2657)
end_of_file
