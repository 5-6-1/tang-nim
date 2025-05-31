# tang-nim🔥 The elegant sugar in nim lang.

tang is a library that gives you with convenient grammar sugar.

## Examples
```nim
import tang
import threadpool

x<-1..10: #等价 for x in 1..10:
  discard
(x,y)<-[(1,2),(3,4),(5,6)]:
  discard
(x,y)<-(1..3,4..6):#等价 for x in 1..3:for y in 4..6:
  discard
x<-1..10<-3: #等价 for x in countup(1,10,3):
  discard

x:=1 #等价 let x = 1
y:>1 #等价 var x = 1

list:>newSeq[int]()
list<-1 #等价 add(list,1)
list<-2<-3<-4 #等价 add(list,1)...add(list,4)
<-list: #等价 add(list,5);add(list,6)
  5
  6
<-add list:#batch 简化写法
  7
  8
batch fn add list:
  9
  10
echo list  #@[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x<-1..10<<-parallel:#<<-用于扩展,可自行扩展
  discard #等于 parallel x<-1..10:discard
x<-1..10<~int:#<~可判断x类型
  discard

if (x=:1)==1:#x=:1与python的海象操作符类似
  discard

#fn提供函数式写法
fn echo 1 2 3 4 5 #等价 echo(1,2,3,4,5)

```
