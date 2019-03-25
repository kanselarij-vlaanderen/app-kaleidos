import datetime
import re

def sparql_escape(obj):
    if type(obj) is str:
        def replacer(a):
            return "\\"+a.group(0)
        return '"' + re.sub(r'[\\\'"]', replacer, obj) + '"'
    elif type(obj) is datetime.time:
        return '"' + obj.isoformat() + '"^^xsd:dateTime'
    elif type(obj) is datetime.date:
        return '"' + obj.isoformat() + '"^^xsd:date'
    elif type(obj) is int:
        return '"' + str(obj) + '"^^xsd:integer'
    elif type(obj) is float:
        return '"' + str(obj) + '"^^xsd:float'
    elif type(obj) is bool:
        return '"' + str(obj) + '"^^xsd:boolean'
    else:
        return ""

def sparql_escape_string(obj):
    obj = str(obj)
    def replacer(a):
        return "\\"+a.group(0)
    return '"' + re.sub(r'[\\\'"]', replacer, obj) + '"'

def sparql_escape_datetime(obj):
    obj = datetime.strptime(str(obj))
    return '"' + obj.isoformat() + '"^^xsd:dateTime'

def sparql_escape_date(obj):
    obj = datetime.strptime(str(obj))
    return '"' + obj.date().isoformat() + '"^^xsd:date'

def sparql_escape_time(obj):
    obj = datetime.strptime(str(obj))
    return '"' + obj.time().isoformat() + '"^^xsd:time'

def sparql_escape_int(obj):
    return '"' + str(int(obj)) + '"^^xsd:integer'

def sparql_escape_float(obj):
    return '"' + str(float(obj)) + '"^^xsd:float'

def sparql_escape_bool(obj):
    return '"' + str(bool(obj)) + '"^^xsd:boolean'

def sparql_escape_uri(obj):
    obj = str(obj)
    def replacer(a):
        return "\\"+a.group(0)
    return '<' + re.sub(r'[\\\'"]', replacer, obj) + '>'
