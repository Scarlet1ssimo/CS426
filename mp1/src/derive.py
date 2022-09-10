from nis import match
import random
import string
def expr(d):
    if d<=0:
        return random.choice([
        f"new {typeid()}",
        objectid(),
        num(),
        f'"{typeid()}"',
        "true",
        "false",
    ])
    match random.randint(0,14):
        case 0:return        f"{objectid()}<-{expr(d-1)}"
        case 1:return        f"{expr(d-1)}@{typeid()}.{objectid()}({exprs_comma(d-1)})"
        case 2:return        f"if {expr(d-1)} then {expr(d-1)} else {expr(d-1)} fi"
        case 3:return        f"while {expr(d-1)} loop {expr(d-1)} pool"
        case 4:return        "{"+exprs_semicolon(d-1)+"}"
        case 5:return        f"let {letformals(d-1)} in {expr(d-1)}"
        case 6:return        f"case {expr(d-1)} of {cases(d)} esac"
        case 7:return        f"{objectid()}({exprs_comma(d-1)})"
        case 8:return        f"isvoid {expr(d-1)}"
        case 9:return        expr(d-1)+'+'+expr(d-1)
        case 10:return        expr(d-1)+'-'+expr(d-1)
        case 11:return        expr(d-1)+'*'+expr(d-1)
        case 12:return        expr(d-1)+'/'+expr(d-1)
        case 13:return        '~'+expr(d-1)
        # case 14:return        '('+expr(d-1)+'<'+expr(d-1)+')'
        # case 15:return        '('+expr(d-1)+'<='+expr(d-1)+')'
        # case 16:return        '('+expr(d-1)+'='+expr(d-1)+')'
        case 14:return        'not '+expr(d-1)
        # case 15:return        'not '+expr(d-1)
        # case 16:return        'not '+expr(d-1)
        # case 17:return        'not '+expr(d-1)

        # case 18:return        f"({expr(d-1)})"
def cases(d):
    return ' '.join([case(d)+";" for i in range(5)])
def case(d):
    return objectid()+":"+typeid()+"=>"+expr(d-1)
def letformals(d):
    return ','.join([letformal(d) for i in range(5)])
def letformal(d):
    return random.choice([f"{objectid()}:{typeid()} <- {expr(d-1)}",
            f"{objectid()}:{typeid()}",])
def exprs_comma(d):
    return ','.join([expr(d-1) for i in range(5)])
def exprs_semicolon(d):
    return ' '.join([expr(d-1)+';' for i in range(5)])
def typeid():
    return ''.join(random.choice(string.ascii_uppercase) for i in range(5))
def objectid():
    return ''.join(random.choice(string.ascii_lowercase) for i in range(5))
def num():
    return ''.join(random.choice(string.digits) for i in range(5))

print("""
class A {
    beta_reduce() : Expr {
      let 
      a 
      : 
      B 
      in 
	  """+expr(10)+"""
  }
  ;
};"""
)