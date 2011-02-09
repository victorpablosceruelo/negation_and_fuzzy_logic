from subprocess import call

def query(program="fuzzy_QA",result="result.db"):
    path = "webInterface/IOFile/"
    call(["ciaoc",path+program])
    print "./"+path+program
    call(["./"+path+program, ">", path+result])
       
