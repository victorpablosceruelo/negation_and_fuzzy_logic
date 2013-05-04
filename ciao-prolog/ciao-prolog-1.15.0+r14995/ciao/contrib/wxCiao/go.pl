:- use_module(wxCiao).

main(_) :- start_gui('initialize').

initialize :- create_window(mainwindow, "Hello World", 640, 480),
    create_button(okbutton, "OK", mainwindow),
    create_line_edit(line,mainwindow),
    create_spin_box(spin,mainwindow),
    create_splitter(splitter,mainwindow),
    create_text_area(text1,splitter),
    create_text_area(text2,splitter),
    split_vertically(splitter,text1,text2),

    create_vbox(mainbox,mainwindow),
    create_hbox(hbox1,mainwindow),
    create_hbox(hbox2,mainwindow),

    layout(hbox1,mainbox,1,[expand]),
    layout(hbox2,mainbox,0,[align_right]),
    layout(spin,hbox2,1,[]),
    layout(line,hbox2,2,[]),
    layout(okbutton,hbox2,0,[]),
    layout(splitter,hbox1, 1, [expand]),

    create_menu_bar(bar),
    create_menu(file,"File",bar),
    create_menu_item(open,"&Open",file),
    create_menu_item(close,"&Close",file),
    create_menu_item(save,"&Save",file),
    create_menu_item(saveas,"Save &As",file),

    create_status_bar(mainwindow,3),
    set_status_text(mainwindow,1,"0 clicks"),
    set_status_text(mainwindow,2,"asdf"),
    
    set_menu_bar(mainwindow, bar),
    set_event_handler(mainwindow,'event',[button_press, menu,spinbox]),
    set_main_sizer(mainwindow,mainbox),
    show(mainwindow).



event(button_press(_)) :-  create_window(test_win, "User Dialog", 640,480),
        create_button(newbutton,"Big Button",test_win),
        set_event_handler(test_win, 'otherhandler', [button_press]),
        show(test_win).

event(menu(open)) :- get_open_file("BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif", File),
    message_box("I would open", File).

event(menu(save)) :- get_save_file("BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif", File),
    message_box("I would save", File).

event(menu(saveas)) :- event(menu(save)).

event(spinbox(spin)) :- message_box("Value Changed", "The value of the spinbox was changed").

otherhandler(X) :- display(X).
