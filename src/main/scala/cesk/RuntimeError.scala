package cesk

enum RuntimeError:
  case VarNotFound(msg: String)
  case DivisionByZero(msg: String)
  case InvalidVarType(msg : String)
  case MethodNotFound
  case NewInstWrongFieldCount
  case FieldNotFound
  case MethodCallWrongArgCount
  case ValNotAnObject

  case FieldNotFoundInProxy
  case ValDoesntConformToExpType
  case ProxyValDoesntConformToProxyShape
  case FieldNamesDontConformToProxyShape
  case FieldValsDontConformToProxyShape
  case MethodNamesDontConformToProxyShape
  case MethodParamsDontConformToProxyShape