Python4Delphi 之用 Delphi 写 Python 模块

# Python 的模块
任何编程语言，都支持把很多代码，分别写在不同的文本文件里面。都是文本文件，当然不同的编程语言，后缀名可能不同。比如 C 语言的 .c，Delphi 的 .pas，Python 的 .py

在 Delphi 里面，一个源代码文件，叫做一个【单元】，unit。比如有一个源代码文件叫做 MyFunctions.pas, 这个文件里面一开头就是 unit MyFunctions 这一行。在其它源代码文件里面，要想使用 MyFunctions.pas 这个文件里面的任何东西，都必须要【use MyFunctions】

在 Python 里面，一个源代码文件比如 MyFunctions.py，在另外一个源代码文件看来，它就是一个【模块】-- Module。在另外一个源代码文件里面要使用 MyFunctions.py 里面的东西，必须要【import MyFunctions】，引入这个模块。

假设我们的 **MyFunctions.py** 里面的代码是：
~~~
def DoAdd(a, b):
  return a + b
~~~

假设我们要在其它的 Python 文件里面调用上述函数的 python 代码：
~~~
import MyFunctions
 
a1 = 3
b1 = 4
c1 = MyFunctions.DoAdd(a1, b1)
print(c1)
~~~

# 用 Delphi 的代码写一个模块，让 Python 来调用
假设上述模块里面的 DoAdd 函数，我们用 Delphi 的代码来实现，然后同样使用上面的代码调用它，该如何做？

1. 建立一个 Delphi 程序（这里我是创建了一个 VCL 程序）。

2. 拖 PythonEngine1 和 PythonModule1 以及 PythonDelphiVar1 到界面 Form1 上面。

3. 再拖一个 PythonGUIInputOutput1 到界面上。这个控件用于输出 Python 的 print 到界面显示。

4. 界面上放两个 TMemo，Memo1 和 Memo2.

5. 设计期属性面板里面指定 PythonGUIInputOutput.Output 是 Memo1。这样当 Python 被执行时，里面的 print() 函数输出的内容，都会在本程序的 Memo1 里面显示出来。

6. 注意：要让 PythonGUIInputOutput1 输出 Python 的 print 内容，必须设置  PythonEngine1.IO := PythonGUIInputOutput1; 这个也同样在设计期属性面板里面设置就可以了。

6.1. 重复一下，要输出 Python 代码的 print（） 的内容到 Delphi 程序界面上的 Memo1 里面，在设计期需要做如下设置(Delphi代码)：
~~~
PythonEngine1.IO := PythonGUIInputOutput1;
PythonGUIInputOutput1.Output := Memo1;
~~~
7. PythonDelphiVar1.VarName := 'C' 在属性面板里面直接填入。 

8. 我们在 Memo2 里面写一些 Python 代码如下：
~~~
import MyModule
 
a = 3
b = 4
 
print("开始执行 Delphi 模块的函数")
 
C.Value = MyModule.DoAdd(a,  b)
 
print(C.Value)
~~~
8. 设置 PythonModule1.ModuleName := 'MyModule';  这个字符串名字就是让 Python 调用模块的名字。

9. 在 Delphi 里面写一个函数：
~~~
function TForm1.DoAdd(pself, args : PPyObject ): PPyObject; cdecl;
var
  i, a, b: Integer;
  v: Variant;
begin
  //这里如何从 args 里面，把 Python 调用这个函数输入的参数读出来？
  with GetPythonEngine do
  begin
    Memo1.Lines.Add(PyObjectAsString(args));
 
    v := PyObjectAsVariant(args);    //果然，在 Python 端输入的逗号隔开的参数 add(a,b) 在 Delphi 这边转换为 Variant 数组。
    a := v[0];
    b := v[1];
    i := a + b;
 
    Result := PyLong_FromLong(i); //输出给 Python 的数据，做一个类型转换。在 Python 那边就能获得。
  end;
end;
~~~
9.1. 上述函数是让 Python 的代码来调用的，因此，上述函数的参数以及返回值，必须是上述函数的那种声明方式，不能随便定义一些参数在里面。

9.2. 上述声明中，有一个 args 参数，当 Python 代码调用传入 2 个参数时 【DoAdd(3, 4】，在 Delphi 这一边如何获得这两个参数？这里是首先使用 PyObjectAsVariant 函数把 PPyObject 类型的变量，转换为一个 Variant 类型的变量。而这个 Variant 类型的变量，刚好是 Array 类型，采用 V[0], V[1] 的方式，就能获取到。

9.3 如果这个函数在 Python 端传入的参数是其它复杂类型，也需要做相应的转换。具体可以参考 Python4Delphi 的官方代码里面的 Demo 程序。

10. 选中 PythonModule1，在属性面板里面，切换其页标签到 Events，最底部一个事件叫做 OnInitialization，双击它，产生代码框架，在里面写：
~~~
procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
    Self.RegPythonMethod;   //这句话必须在 Initialization 这里。
end;
 
procedure TForm1.RegPythonMethod;
begin
  //PythonModule1.AddMethod('DoAdd', DoAdd, 'test Python Module');   //独立的函数就用 AddMethod
  PythonModule1.AddDelphiMethod('DoAdd', DoAdd, 'test Python Module');
end;
~~~
10.1. 解释一下：Delphi 里面的函数，要让 Python 里面的代码看作是一个模块的函数，就必须用这个模块对应的 PythonModule 的方法加入进去。如果这个函数是 Delphi 的对象方法，就是如上面的代码，使用 AddDelphiMethod 这个函数来加入进去。

10.2. 如果这个函数是一个普通的函数(function DoAdd)，而不是一个对象的函数（function TForm1.DoAdd），则使用 AddMethod 这个函数来把它加入到 PythonModule1 里面去。

10.3. 做完这一步，当 Python 里面的代码执行 MyModule.DoAdd(a, b) 的时候，实际上就调用到了模块 MyModule 里面的 DoAdd 函数，而这个 MyModule 模块是由 Delphi 里面的 PythonModule1 来实现的，实际上就是调用到了在 Delphi 里面的 TForm1.DoAdd 这个函数。

11. 拖一个按钮到 Delphi 程序的界面上，用它来执行 Python 代码，代码如下：
~~~
procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
end;
~~~
11.1. 上述代码，执行了前面我们输入到 Memo2 里面的 Python 代码。

12.2. 编译执行 Delphi 程序，可以看到执行结果。可以在运行期，在 Memo2 里面，修改 a 或者 b 的值（Python 代码里面 a = 后面的值），重新点击执行按钮，再次执行 Python 里面的代码，可以看到执行结果改变了 -- 这里没有重新编译 Delphi 的程序，仅仅是修改了 Python 的代码文本。

13. 上述 python 代码里面的 C.Value 的那个 C，就是我们在 Delphi 里面的 PythonDelphiVar1.VarName，是让我们可以用 Delphi 代码获得 python 计算结果的变量的东西。Delphi 的代码如下：关于这个东西的详细解释，可以看我上一篇博客文。
~~~
procedure TForm1.PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
var
  i: Integer;
begin
  i := Data;
  Memo1.Lines.Add(i.ToString);
end;
~~~
注意：我第一次执行，结果出错。检查后才发现，Delphi 的程序默认编译结果是 Win32 的程序，而我的电脑上安装的 Python 是 64 位的。将 Delphi 程序的编译目标平台修改为  Windows 64bit 后再次运行，成功。

# 总结
在 Delphi 程序和 Python 程序的交互，可以用 Delphi 实现一个 Python 模块，用 Delphi 实现这个模块的函数，然后在 Python 里面的代码来调用。实现这个功能的简单架构：

1. PythonModule1 用来代表一个可以给 Python 调用的模块。

2. Delphi 实现的函数，通过 PythonModule1.AddDelphiMethod 的方式成为 Python 的模块的函数。

3. 被 Delphi 的 PythonEngine1 执行的 Python 代码，可以 import 这个模块并执行里面的函数。
