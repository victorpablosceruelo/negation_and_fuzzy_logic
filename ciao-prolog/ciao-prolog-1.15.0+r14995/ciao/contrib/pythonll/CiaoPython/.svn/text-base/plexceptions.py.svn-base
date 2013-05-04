class PLException(Exception) :
    def __init__(self, cause):
        self.cause = cause
    def __str__(self):
        return "A PLException occured, caused by: " + self.cause

    def translateException(prologException):
        """prologException -- PLTerm"""
        return PLException(prologException)

class PLGoalException(PLException):
    def __init__(self, cause):
        PLException.__init__(self)
        self.cause = cause
    def __str__(self):
        return "A PLGoalException occured, caused by: " + cause


