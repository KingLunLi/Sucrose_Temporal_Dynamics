TIME = {}
TIME["LOWER"] = -10
TIME["UPPER"] = 10
TIME["TOTAL"] = int((TIME["LOWER"] - TIME["UPPER"])*-10 + 1)

def updateTimes(new_lower, new_upper):
    TIME["LOWER"] = new_lower
    TIME["UPPER"] = new_upper
    TIME["TOTAL"] = int((TIME["LOWER"] - TIME["UPPER"])*-10 + 1)