include sugar
import sequtils
import threadpool


# 这是语法糖库 -- tang.nim
# <-符号用于构建for循环,包括单变量,元组变量for循环
# <-也可以用于for循环笛卡尔嵌套
# <-还可以进行循环属性叠加,如步长,并行等
# <-还是基于collect的列表生成符,支持<-的loop操作以及bool操作filter,默认and,手动or
# <-还支持add操作简化,以及return操作模拟
# <-可以作为batch fn宏的简写
# <-还可以作为fn宏的简写
# ->可以一定程度上作为循环<-的对称符号
# <~为类型确定符,只有确定值的类型才能返回,可以与<-联动使用
# ->是sugar库中的符号,用于方便表明函数类型
# =>是sugar库中的符号,用于方便表明闭包函数
# :=是短变量声明,=:是海象声明符,<=>是大小判断符
# :>,>:表明可变量声明
# match是模式识别(暂时仅支持初级匹配),with是协助多参函数
# fn提供函数式语言的调用方法与定义方法
# batch可以提供批量操作
# 对于for循环,由于有块的区分,卫生宏几乎没必要
# ?运算符提供了类C语法的?:三元运算符
# ?.提供空安全调用
# .^为级联操作符,返回级联后结果,^.无返回值


proc generateLoops(vars, iters: seq[NimNode], body: NimNode): NimNode =
  #生成嵌套for循环
  result = body
  for i in countdown(vars.high, 0):
    result = newTree(nnkForStmt, vars[i], iters[i], result)
proc generateAnd(fits: seq[NimNode]): NimNode =
  #生成嵌套and
  var queue = fits
  while queue.len > 1:
    let a = queue.pop()
    let b = queue.pop()
    queue.add newTree(nnkInfix, ident"and", a, b)
  queue[0]
proc getll(inf:NimNode):NimNode=
  #获得最左侧值,即for循环的被赋值者
  result=inf
  while result.kind==nnkInfix:
    result=result[1]
proc assignHelp(con:NimNode,val:NimNode):NimNode=
  #转换表达式为赋值操作
  if con.kind==nnkTupleConstr:
    result=newNimNode nnkVarTuple
    for child in con:
      expectKind child, nnkIdent
      result.add child
    result.add newEmptyNode()
    result.add val
  elif con.kind==nnkIdent:
    result=newTree(nnkIdentDefs,con,newEmptyNode(),val)
  else:
    error "Error: invalid assign pattern"
proc generateIn(l, vals, body: NimNode) =
  #将add操作不断添加到body中
  var current = vals
  var addStack = newSeq[NimNode]()
  while current[1].kind == nnkInfix and eqIdent(current[1][0], "<-"):
    addStack.add current[2]
    current = current[1]
  body.add newCall(ident"add", l, current[2])
  for i in countdown(addStack.high, 0):
    body.add newCall(ident"add", l, addStack[i])
macro `<-`*(left,right,body)=
  runnableExamples:
    #基本循环
    x<-1..10:
      echo x
    #笛卡尔循环
    (x,y)<-(1..3,2..4):
      echo x," ",y
    #支持拆包
    (x,y)<-[(1,2),(3,4),(5,6)]:
      echo x," ",y
    #支持步长,对于..有countUp优化
    #支持属性添加
    x<-1..22<-3.step<-parallel:
      echo x
  if left.kind == nnkInfix and eqIdent(left[0],"<-"):
    let funname=case right.kind:
      of nnkIdent:
        right
      of nnkCall:
        right[0]
      of nnkCommand:
        right[0]
      of nnkDotExpr:
        right
      else: error "Error: invalid syntax"
    var fun=newTree(nnkCall,funname)
    case right.kind:
      of nnkIdent:discard
      of nnkCall:
        for x in right[1..^1]:
          fun.add x
      of nnkCommand:
        for x in right[1..^1]:
          fun.add x
      of nnkDotExpr:discard
      else: error "Error: invalid syntax"
    fun.add left
    fun.add body
    return quote do:
      `fun`
  if right.kind != nnkTupleConstr:
    #一般的for循环
    if left.kind==nnkTupleConstr:
      #元组更正
      let looper = newNimNode nnkVarTuple
      for child in left:
        expectKind child, nnkIdent
        looper.add child
      looper.add newEmptyNode()
      return quote do:
        for `looper` in `right`:
          `body`
    else:
      return quote do:
        for `left` in `right`:
          `body`
  if left.len != right.len:
    error "Error: patterns and iters must have the same length"
  #笛卡尔积式for循环
  let variables = left.children.toSeq
  let ranges = right.children.toSeq
  generateLoops(variables, ranges, body)
macro parallel*(left,body)=
  #对并行循环的尝试,暂时容易出现错误,考虑删除
  #不过作为扩展<-功能的一种方法可供使用者自定义DSL
  runnableExamples:
    parallel x<-1..10:
      echo x
  expectKind left,nnkInfix
  let
    clo=ident(left.repr&" closure0Pf "&body.repr)
    lopname=ident(left.repr&" loopD "&body.repr)
    l=getll left
    t=newTree(nnkInfix,left[0],left[1],left[2],newStmtList(newTree(nnkCall,ident "spawn",newCall(newTree(nnkAccQuoted,clo),l))))
    p=newTree(nnkProcDef,newTree(nnkAccQuoted,clo),newEmptyNode(),newEmptyNode(),newTree(nnkFormalParams,
        newEmptyNode(),newTree(nnkIdentDefs,newTree(nnkAccQuoted,lopname),ident "auto",newEmptyNode())),newTree(nnkPragma,ident "thread"),
        newEmptyNode(),newStmtList(newTree(nnkLetSection,assignHelp(l,newTree(nnkAccQuoted,lopname))),body))
  return newBlockStmt(newEmptyNode(),newStmtList(p,t,newCall(ident"sync")))
macro step*(s:int,left,body)=
  if left.kind == nnkInfix and eqIdent(left[0],"<-"):
  #嵌套<-用于步长操控
    if left[2].kind==nnkInfix and eqIdent(left[2][0],".."):
      #对步长的countUp优化
      let l=left[1]
      let l1=left[2][1]
      let l2=left[2][2]
      return quote do:
        for `l` in countup(`l1`,`l2`,`s`):
          `body`
    let loopFlag=genSym(nskVar,"step_flag")
    return newTree(nnkStmtList,newTree(nnkBlockStmt,newEmptyNode(),newTree(nnkStmtList,
        newTree(nnkVarSection,newTree(nnkIdentDefs,loopFlag,newEmptyNode(),newIntLitNode 0)),
        newTree(nnkInfix,ident "<-",left[1],left[2],newTree(nnkStmtList,
            newTree(nnkIfStmt,newTree(nnkElifBranch,newTree(nnkInfix,ident "==",loopFlag,newIntLitNode 0),body)),
            newTree(nnkCommand,ident "inc",loopFlag),
            newTree(nnkAsgn,loopFlag,newTree(nnkInfix,ident "mod",loopFlag,s))
        ))#生成步长循环
    )))
  else:
    error "Error: invalid step syntax"
macro genlist(val,gen):untyped=
  #生成收集器
  let fits = gen.children.toSeq
  var
    args =newSeq[NimNode]()
    iters =newSeq[NimNode]()
    flen:int=0
    judges = newSeq[NimNode]()
    eqleft=newSeq[NimNode]()
    eqright=newSeq[NimNode]()
  fit<-fits:
    if fit.kind == nnkInfix and eqIdent(fit[0],"<-"):
      #循环器
      inc flen
      if fit[2].kind!=nnkTupleConstr:
        args.add fit[1]
        iters.add fit[2]
      else:
        expectKind fit[1],nnkTupleConstr
        arg<-fit[1]:
          args.add arg
        iter<-fit[2]:
          iters.add iter
    elif fit.kind==nnkInfix and eqIdent(fit[0],":="):
      #赋值器
      eqleft.add fit[1]
      eqright.add fit[2]
    else:
      #判断器
      judges.add fit
  var body = newStmtList()
  (l,r)<-(eqleft,eqright):
    body.add newTree(nnkInfix,ident":=",l,r)
  body.add val
  let juds = if judges.len!=0:newTree(nnkIfStmt,newTree(nnkElifBranch,generateAnd judges,body))else:body
  #循环套判断,判断套赋值,收集在最后
  let coler = generateLoops(args,iters,juds)
  quote do:collect:`coler`
macro genAdd(val,gen):untyped=
  #生成add操作
  if val.kind==nnkinfix and eqIdent(val[0],"<-"):
    #嵌套add
    let col=getll(val)
    var body=newBlockStmt(newEmptyNode(),newStmtList())
    generateIn(col,val,body[1])
    body[1].add newCall(ident"add",col,gen)
    body
  else:
    #直接add
    newCall(ident"add",val,gen)
macro `<-`*(val,gen):untyped=
  runnableExamples:
    #基于collect生成列表
    let list=(x,y,z)<-((x,y)<-(1..4,x..6),z<-8..10,x+y+z>17,z>x+y or x>y)
                            # @[(2, 6, 10), (3, 5, 10), (3, 6, 10), (4, 4, 10), (4, 5, 10)]
    #基于collect生成集合,字典等
    let dict={x}<-(x<-1..10,x mod 2==0)
    #支持内部赋值
    let list0=x<-(a<-1..5,b<-6..10,c:=a+b,x:=a*c+b)
    #简化add操作
    var list=newSeq[int]()
    list<-1<-2<-3           # [1,2,3]
    list<-4                 # [1,2,3,4]
    list<-x<-(x<-7..12)     # [1,2,3,4,7,8,9,10,11,12]
    <-list:                 # [1,2,3,4,7,8,9,10,11,12,5,6]
      5                     #该用法只能是非command语法
      6
    #batch fn简化写法
    <-add list:          # [1,2,3,4,7,8,9,10,11,12,5,6,13,14]
      13
      14
  if gen.kind in {nnkTupleConstr,nnkPar} and gen.len>0 and gen[0].kind==nnkInfix and eqIdent(gen[0][0],"<-"):
    if val.kind==nnkInfix and eqIdent(val[0],"<-"):
      #收集器add操作
      let list=val[1]
      let x=val[2]
      let n=genSym(nskForVar,"data")
      quote do:
        block:
          `n`<-genlist(`x`,`gen`):
            add(`list`,`n`)
    else:
      #生成收集器
      quote do:
        genlist(`val`,`gen`)
  elif gen.kind == nnkStmtList:
    #支持多行情况
    if val.kind==nnkCommand:
      quote do:
        batch(fn `val`,`gen`)
    else:
      var body=newBlockStmt(newEmptyNode(),newStmtList())
      for line in gen:
        body[1].add newTree(nnkCall,ident"add",val,line)
      body
  else:
    #生成add操作
    quote do:
      genAdd(`val`,`gen`)
template `<-`*(t): untyped =
  #暂定为fn的语法糖
  runnableExamples:
    <-echo 1 2 3
    #不可以
    #<-out1 => echo 1
    #因为在=>中缀表达式中,<-会仅被识别为out1的前缀
  fn t
template `->`*(a,b,c)=
  #<-的对称语句
  #可以写有意思的操作
  #不过由于解析规则
  #若要与<-叠加,最多只能在最前面有一个->
  runnableExamples:
    1..5->x:
      echo x
    3->x<1..21:
      echo x
    #展开为x<-1..21<-3:echo x
  b<-a:c
template `<~`*(val,ty):untyped=
  #类型判断符
  block:
    when val is ty:
      val
    else:
      raise newException(ValueError, "Expected a " & $ty)
macro `<~`*(iter,ty,body)=
  #类型判断循环
  runnableExamples:
    x<-1..10<~int:
      echo x
    x<-1..10<~float:#类型不匹配,报错
      echo x
    (x,y)<-(1..3,4..5)<-parallel<~(int,int):
      echo x," ",y
  expectKind iter,nnkInfix
  expectIdent iter[0],"<-"
  let l=getll iter
  #用is判断类型
  var newbody=newStmtList(newTree(nnkWhenStmt,newTree(nnkElifBranch,newTree(nnkPrefix,ident"not",newTree(nnkInfix,ident "is",l,ty)),
          newStmtList(newTree(nnkRaiseStmt,newTree(nnkCall,ident"newException",ident"ValueError",newStrLitNode("Expected a " & $toStrLit(ty))))))))
  for i in body:
    newbody.add i
  return newTree(nnkInfix,iter[0],iter[1],iter[2],newbody)

macro `:=`*(left,right)=
  runnableExamples:
    let x=1
    y:=2
    echo x+y #3
  if left.kind==nnkTupleConstr:
    #对元组转为赋值元组
    let tup = newNimNode nnkVarTuple
    for child in left:
      expectKind child, nnkIdent
      tup.add child
    tup.add newEmptyNode()
    tup.add right
    return newTree(nnkLetSection,tup)
  else:
    #直接赋值
    return quote do:
      let `left` = `right`
macro `:>`*(left,right)=
  runnableExamples:
    var x=1
    y:>2
    echo x+y #3
    y=3
    echo x+y #4
  #可变赋值
  if left.kind==nnkTupleConstr:
    let tup = newNimNode nnkVarTuple
    for child in left:
      expectKind child, nnkIdent
      tup.add child
    tup.add newEmptyNode()
    tup.add right
    return newTree(nnkvarSection,tup)
  else:
    return quote do:
      var `left` = `right`

template `=:`*(left,right): untyped =
  runnableExamples:
    if (x=:2)>1:
      echo x
    else:
      echo "x is less than 2"
  left:=right
  right#返回赋值结果
template `>:`*(left,right): untyped =
  runnableExamples:
    if (x>:2)<1:
      x+=1
      echo x*x
    else:
      echo "x is greater than 2"
  left:>right
  right#返回赋值结果

proc `<=>`*[T](left,right:T): int8 =
  #大小比较
  if left < right:
    -1#左小
  elif left > right:
    1#左大
  else:
    0#相等

template with*(args:varargs[untyped])=discard#match的服务函数
macro match*(x,body)=
  runnableExamples:
    match x:
      1:echo "x is 1"#匹配单个值
      with 2,3,4:echo "x is 2,3,4"#匹配多个值
      5..7:echo "x is between 5 and 7"#匹配范围
      with 8..10,11,13..15:echo "x is 8,9,10,11,13,14,15"#匹配多个范围
      t if t mod 2 == 0:echo "x is even"#匹配条件
      if x>100:echo "x is greater than 100"#匹配条件
      _:echo "x"#匹配默认值
    match tup:
      (1,2):echo "tup is (1,2)"#匹配元组
      (x,y) if x>y:echo "x is greater than y"#匹配条件
      t:echo "tup"#匹配默认值
  x.expectKind nnkIdent
  body.expectKind nnkStmtList
  result = newTree(nnkCaseStmt,x)
  var tail=newTree(nnkElse,newTree(nnkStmtList,newTree(nnkIfStmt,newTree(nnkElifBranch,ident"false",
                newTree(nnkStmtList,newTree(nnkDiscardStmt,newEmptyNode()))))))
  var flag=false#是否需要闭合case
  cir<-body:
    if cir.kind == nnkCall:
      #标识符与stmtlist出现
      if cir[0].kind in {nnkIdent,nnkTupleConstr}:
        #匹配 ident 与 (ident...)
        if cir.len==2:
          if cir[1].kind == nnkStmtList:
            if eqIdent(cir[0],"_"):
              #默认匹配
              tail[0][0].add newTree(nnkElse,cir[1])
              flag=true
              break
            else:
              #匹配它值
              tail[0][0].add newTree(nnkElifBranch,newTree(nnkStmtListExpr,
                    newTree(nnkInfix,ident ":=",cir[0],x),newEmptyNode(),ident "true"),cir[1])
            flag=true
      elif cir.len==2 and cir[1].kind == nnkStmtList:
        #直接case of匹配
        result.add newTree(nnkOfBranch,cir[0],cir[1])
    elif cir.kind == nnkInfix:
      #范围..匹配
      if eqIdent(cir[0],".."):
        if cir.len==4:
          result.add newTree(nnkOfBranch,newTree(nnkInfix,cir[0],cir[1],cir[2]),cir[3])
    elif cir.kind == nnkCommand:
      if eqIdent(cir[0],"with"):
        #with多值匹配
        var vars=newNimNode(nnkOfBranch)
        if cir[^1].kind == nnkStmtList:
          lit<-cir[1..^1]:
            vars.add lit
          result.add vars
      elif cir[1].kind == nnkIfExpr and cir.len==2:
        #赋值后if条件匹配
        tail[0][0].add newTree(nnkElifBranch,newTree(
          nnkInfix,ident "and",newTree(nnkStmtListExpr,newTree(nnkInfix,ident ":=",cir[0],x),newEmptyNode(),ident "true"),cir[1][0][0]
        ),cir[1][0][1])
    elif cir.kind == nnkIfStmt:
      #直接条件匹配
      tail[0][0].add cir[0]
  if flag:
    #增加else分支
    result.add tail
  else:
    error "Error: no covering all cases"

macro hasReturnType(expr: untyped): bool =
  #判断是否有返回值
  result = quote do:
    when typeof(`expr`) is void:
      false
    else:
      true
proc oneTupOne(t:NimNode):NimNode=
  #获取ident或单元素元组内部值
  if t.kind==nnkTupleConstr and t.len==1:t[0]
  else:t
proc fn_add_command_num(fun_and_args: NimNode, command: var NimNode, num: var int, flag: var bool) =
  #fun_and_args:参数组
  #command:命令组
  #num:函数参数数量(声明或调用)
  #flag:是否为声明函数
  var current = fun_and_args
  var stack: seq[NimNode] = @[current]
  while stack.len > 0:
    current = stack.pop()
    if current.kind == nnkInfix and eqIdent(current[0], "=>"):
      #声明函数
      command.add current[1].oneTupOne
      flag = true
      inc num
      continue
    elif current.kind == nnkCommand:
      #函数调用参数或声明函数参数
      command.add current[0].oneTupOne
      stack.add(current[1])
      inc num
    else:
      #最后的一个值
      command.add current.oneTupOne
proc get_n_command(args: NimNode): NimNode =
  #获取函数声明的操作部分
  var current = args
  while true:
    if current.kind == nnkCommand:
      current = current[1]
    elif current.kind == nnkInfix and eqIdent(current[0], "=>"):
      #返回操作部分
      return current[2]
    else:
      error "Invalid syntax. Expected command chain or => operator", current
macro fn*(fun_and_args):untyped=
  runnableExamples:
    fn echo 1 2 3 4 5
    #等价于
    echo(1,2,3,4,5)
    fn add3 x y z=>x+y+z
    #等价于
    func add3(x,y,z:auto):auto=x+y+z
    fn out1=>echo 1
    fn out1
    #等价于
    proc out1()=echo 1
    out1()
    fn add1 x=>(
      block:
        let t=1
        t+x
    )#可通过block实现多行代码
    fn add1 x=>(
      let t=1;
      let y=2;
      t+x+y
    )#这样也可以实现多行,注意分号
  if `fun and args`.kind==nnkCommand:
    var n=0
    var temp=newTree(nnkTupleConstr)
    var is_fun=false
    `fun and args`[1].fn_add_command_num temp,n,is_fun
    if not is_fun:
      #针对fn foo arg1 arg2 ...的情况
      result=newTree(nnkCall,`fun and args`[0])
      for i in temp:
        result.add i
    else:
      #针对fn foo arg1 ... => bar的情况
      let tail=newStmtList get_n_command `fun and args` #获取bar部分
      if hasReturnType tail:
        #针对无返回值函数的情况
        result=newTree(nnkProcDef,`fun and args`[0],newEmptyNode(),newEmptyNode(),
          newTree(nnkFormalParams,ident"auto",newTree(nnkIdentDefs)))
      else:
        #针对有返回值函数的情况
        result=newTree(nnkProcDef,`fun and args`[0],newEmptyNode(),newEmptyNode(),
          newTree(nnkFormalParams,newTree(nnkIdentDefs)))
      for i in temp:
        result[3][1].add i
      result[3][1].add ident"auto"
      result[3][1].add newEmptyNode()
      result.add newEmptyNode()
      result.add newEmptyNode()
      result.add tail
  elif `fun and args`.kind==nnkIdent:
    #针对fn foo的情况
    return newCall(`fun and args`)
  elif `fun and args`.kind==nnkInfix and eqIdent(`fun and args`[0],"=>"):
    #针对fn foo=>bar的情况
    let tail=newStmtList get_n_command `fun and args`
    let name=`fun and args`[1]
    if hasReturnType tail:
      result=quote do:
        proc `name`()=
          `tail`
    else:
      result=quote do:
        proc `name`():auto=
          `tail`
  else:
    error "Error: invalid syntax."

proc insertDeep(fun,arg:NimNode):NimNode=
  #以fn形式链接fun与arg
  result=newTree(nnkCommand)
  var
    tail=result
    n_fun=fun
  while n_fun.kind==nnkCommand:
    tail.add n_fun[0]
    n_fun=n_fun[1]
    tail.add newTree(nnkCommand)
    tail=tail[1]
  tail.add n_fun
  tail.add arg
macro batch*(args,body):untyped=
  #批量操作符
  runnableExamples:
    batch fn foo A:
      a1 b1 c1
      a2 b2
      a3 b3 c3
      a4
    #等价于
    block:
      fn foo A a1 b1 c1
      fn foo A a2 b2
      fn foo A a3 b3 c3
      fn foo A a4
  body.expectKind nnkStmtList
  if args.kind==nnkCommand and eqIdent(args[0],"fn"):
    result=newBlockStmt(newEmptyNode(),newStmtList())
    for line in body:
      let n_line=insertDeep(args,line)
      result[1].add n_line
  else:
    error "Error: invalid syntax."

macro `?`*(judge,t,f):untyped=
  #?:三目运算符
  newTree(nnkIfStmt,newTree(nnkElifBranch,judge,newStmtList(t)),newTree(nnkElse,newBlockStmt(newEmptyNode(),f)))

macro `?.`*(obj, field): untyped =
  #空安全调用
  let tmp = genSym(nskVar)
  quote do:
    block:
      let `tmp` = `obj`
      if `tmp`.isNil: nil
      else: `tmp`.`field`

macro `.^`*(calls, fun): untyped =
  #级联操作符
  let call=genSym(nskForVar,"call")
  quote do:
    block:
      `call`<-`calls`:
        `call`.`fun`
    `calls`
macro `^.`*(calls, fun)=
  #级联操作符,无返回值
  let call=genSym(nskForVar,"call")
  quote do:
    block:
      `call`<-`calls`:
        `call`.`fun`

macro `~>`*(arg,fun):untyped=
  runnableExamples:
    3~>fn echo 1 2
    #等价于
    fn echo 1 2 3
  #参数尾插操作符
  if fun.kind == nnkCommand and eqIdent(fun[0],"fn"):
    var cur=fun
    result=newTree(nnkCommand)
    var tail=result
    while cur.kind==nnkCommand:
      tail.add cur[0]
      tail.add newTree(nnkCommand)
      tail=tail[1]
      cur=cur[1]
    tail.add cur
    tail.add arg
  else:
    error "Error: invalid syntax."


dumpTree:
  x<-1..3<-3<<-a<<-b<<-c:d



#✔功能实现度(前缀单参,前缀双参,中缀双参,中缀三参)
# <-✔✔✔✔ <<- <-| <--
# ->✔✔ ->> |-> -->
# <<= <=| <==
# =>> |=> ==>
# <~✔✔ <~~ <<~
# ~>✔ ~~>
# <-> <=>✔ <~> =>✔
# <| |> <|> </> </ />
# <$> ~@



