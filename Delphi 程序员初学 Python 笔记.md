# Delphi 程序员初学 Python 笔记

作为熟悉 Delphi 编程的人，当然想在 Delphi 的程序里面调用 Python 的一大堆库的功能，比如现如今火爆的 AI 库。

首先，学习 Python 需要一个编程工具，可能一般人用文本编辑器或者用 VSCode 的比较多。但作为熟悉 Delphi 的人，发现有一个开源工具 PythonScripter 非常好用。这是一个 Python 的开发 IDE，在里面写代码，运行代码，查看结果，设置断点，等等，都非常的方便，操作方式和 Delphi IDE 非常相似。习惯了 Delphi IDE 的，使用这个工具来写 Python 代码，运行调试很方便，无需学习即可上手。

Delphi 这边我是用的 Delphi 11 社区版。去 github 下载 Python4Delphi 那套控件安装上去。

Python 开发有个问题是电脑上各种原因安装的 Python 版本有很多个。要确定 PythonScripter 和 Delphi 在运行时使用的是同一个 Python 版本，免得出现两边执行的 Python Script 结果可能不一致的情况。至于如何设置才能保持一致，这个只要是程序员大概上网查一下资料就能搞定，这里不多说了。

本文仅仅记录一下 Python 初学，对应 Delphi 的 Python4Delphi 控件的一些具体技术细节。

## Python 语法初步
1. Python 语句没有结尾符号。但一定要加上分号结尾，也可以运行。

2. Python 的块，对应 C 语言需要大括号包起来的，对应 Delphi 需要 begin .. end 包起来的，靠的是前面语句有个冒号结尾，整块的语句在回车后的缩进。如果用 PythonScripter 这个 IDE 写代码，你打完冒号后回车，它自动缩进。

3. C 和 Delphi 的变量需要事先声明变量类型后才能使用。Python 里面不需要声明变量，直接用。

4. Python 的类，其类方法，必须要有一个 self 参数。不能是无参数的方法。如果给类写一个无参数的方法，运行到调用该方法的地方，会出错。错误提示大概是： takes 0 positional arguments but 1 was given。

5. 当代码由多个文件组成，一个文件（a.py）里面的代码需要用到另外一个文件（b.py）里面的函数或者类，在 a.py 里面在用到 b.py 的地方，【import b 】引入。

5.1. 如果 【import b】 则调用 b 里面的某个函数，需要：b.MyFunction() 这样的方式来调用。

5.2. 可以直接引入函数：【from b import MyFunction】，这样调用 MyFunction 的时候就不用在前面加上 b 了。还可以给函数加上别名：【from b import MyFunction as mf】，那需要调用 MyFunction 的地方只要写 mf 就可以了。

5.3. Python 的一个 .py 文件，就是一个【模块】。

## Delphi 和 Python 的交互
我们用 Delphi 写一个程序，点运行，它就会编译成 Windows 的一个 exe 文件并运行起来（安卓，iOS，MacOS，Linux 也是可以的。这里暂时不讨论垮平台的情况）。

如果我想在这个 exe 程序里面去调用 Python 的代码库，基于 Python4Delphi 这套控件，这里讲一下大致的概念。因为我也是刚刚花了点时间看了 Demo 然后自己写了个简单的 Delphi 程序以及 Python 程序，做了一下简单的测试。更复杂的内容可以参考 Python4Delphi 提供的那一大堆 Demo 程序。这里只是写我看了 Demo 后总结的简单心得。

### 以输入和输出的眼光来看代码之间的互相调用
不管什么语言，都不至于一个功能几万行代码从头写到尾放在一个文件里面。多半是需要模块化的。最简单的模块是把很多代码，分别写到不同的函数里面。那么，调用一个函数，其实就是输入和输出：传入函数的参数是输入，函数的返回值，或者函数通过改变传入参数的值而返回给外面调入这个函数的东西，就是这个函数的输出。

函数太多，可能会把一堆函数，放到一个类里面。把另外一堆函数，放到另外一个类里面。

类太多，可能就会把这个类放到一个文件里面，把另外一个类放到另外一个文件里面。

但不管怎么做，它们之间的互相调用，从数据流向来看，就是输入和输出。

因此，在 Delphi的代码里面，去执行一个 Python 的程序（可能就几行代码，可能是几个函数，也可能是几个类），其实也就是 Delphi 程序和这个 Python 程序的交互，Delphi 如何输出数据给 Python，Python 执行后，Delphi 如何从 Python 输入数据。

以下的描述，就是采用输入/输出的方式来看代码。

#### 假设一个场景：
有一个 Python 的库，你输入给它一些值，它计算一些东西，然后输出（返回）计算结果。假设我想在 Delphi 里面利用这个 Python 的库。
~~~
# MyLib.py
# Python 代码
 
  def DemoFunction(a, b):
    return a + b
	
~~~
上面的代码很简单，计算 a + b。

#### Delphi 程序如何做？
首先，Delphi 的程序虽然可以调用 Python 运行环境去执行 MyLib.py 里面的 Python 代码，但无法直接调用里面的函数，无法和这个函数交互（用 Delphi 的代码，输入参数给这个函数，获得这个函数的返回值）。这里，我们需要用 Python 写一段 Python 代码作为 MyLib.py 的包装代码，这个我们自己写的代码可以和 Delphi 交互。

~~~
# Python 代码
 
# ForDelphi.py
 
import MyLib
 
Param3.Value = MyLib.DemoFunction(Param1.Value, Param2.Value)
~~~
新建一个 VCL 的程序，有一个 Form1，它对应的是 Unit1；

拖几个 Python4Delph 的控件到 Form1 上面：

1. PythonEngine1

2. PythonDelphiVar1

3. PythonDelphiVar2

4. PythonDelphiVar3

设置上面几个控件的参数（设计期属性面板里面就能设置，这里为描述方便，用代码）：

~~~
// Delphi 代码
PythonEngine1.VarName := 'Param1';
PythonEngine2.VarName := 'Param2';
PythonEngine3.VarName := 'Param3';
~~~
上述代码里面的字符串 Param1 就对应了前面 Python 代码里面的 Param1 变量。另外两个以此类推。这样通过 PythonEngine1 这个控件，设置它的 VarName，就把 Delphi 和 Python 代码里面同名的变量，建立起了联系。

接下来，假设我在 Delphi 里面有两个数字，比如 2 和 3，如果拿去调用这个 Python 的代码，会计算出5.那么，就是让 Delpi 的 PythonEngine1 这个控件，去执行 ForDelphi.py 里面的代码就好了。那么假设执行了这段代码，ForDelphi.py 里面的 Param1 和 Param2 的值哪里来？

因为前面的 PythonEngine1.VarName := 'Param1'; 这个设置，就绑定了 PythonDelphiVar1 这个控件和 Python 代码里面的 Param1 这个变量的关系，因此当 Python 代码在 Delphi 的环境中运行到需要这个 Param1 这个变量的值的时候，在 Delphi 这边会触发 PythonDelphiVar1  的 OnGetData 事件。因此在 Delphi 这边，我们只需要在它的 OnGetData 事件方法里把 Param1 需要的值送进去（对 Delphi 这边来说是输出一个数据给 Python，在 Python 那边来看就是输入一个数据）。上述例子里面，有 2 个数据需要输出给 Python。 
**代码如下：**
~~~
procedure TForm1.PythonDelphiVar1GetData(Sender: TObject; var Data: Variant);
begin
  Data := StrToInt(Edit1.Text);  //这里假设数字是用户在 Form1 上面的 Edit1 这个编辑框里输入的
end;
 
procedure TForm1.PythonDelphiVar2GetData(Sender: TObject; var Data: Variant);
begin
  Data := StrToInt(Edit2.Text);  //这里假设数字是用户在 Form1 上面的 Edit2 这个编辑框里输入的
end;
~~~
假设 Delphi 界面上有两个输入框：Edit1, Edit2 让用户输入数字，当执行 Python 代码时，就会触发事件，自动执行到 Delphi 的上面那两个事件方法，将用户输入的两个数字，输出给两个分别代表了 Python 端的 Param1 和 Param2 两个变量的值。因此 Python 代码可以顺利执行。执行结果在 Python 代码里面，存入了 Param3 这个变量。

##### Delphi 如何获得 Python 的计算结果？
我们在 Delphi 的 Form1 上放一个按钮用来获取 Python 的计算结果，代码：
~~~
procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Add('计算结果 = ' + DmPython.PythonDelphiVar3.ValueAsString);
end;
~~~
再次解释：因为 PythonDelphiVar3.VarName := 'Param3' ，因此 PythonDelphiVar3 就对应了 Python 代码里面的 Param3 这个变量。

Delphi 的代码和 Pytholn 的代码的交互，基本的输入和输出，搞定。

##### Delphi 如何执行 Python 的代码？
其实只要一行代码就能执行。
~~~
procedure TForm1.Button1Click(Sender: TObject);
var
  SL: TStringList;
begin
  if OpenDialog1.Execute() then
  begin
    SL.LoadFromFile(OpenDialog1.FileName);  //从文件把 Python 代码读进来
 
    PythonEngine1.ExecStrings(SL); //只需这一句
  end;
end;
~~~

##### Delphi 要自动获得 Python 的计算结果
上述的 Delphi 代码中，我们用了一个按钮的事件方法，去读 Python 里面的 Param3 这个变量的值来获得 Python 代码的计算结果。

但是，在 Python4Delphi 这个控件的代码框架里面，一旦绑定 Python 的 Param3 这个变量被赋值被改变，就会自动触发 Delphi 这边绑定这个 Python 变量的 PythonDelphiVar3 这个控件的。
**代码如下：**
~~~
procedure TForm1.PythonDelphiVar3SetData(Sender: TObject; Data: Variant);
var
  i: Integer;
begin
  i := Data;
  Memo2.Lines.Add('OnSetData 事件：' + i.ToString);
end;
~~~
上述代码中，因为 Python 端返回的值是整数，因此，在 Delphi 的这个事件方法里面的 Data 这个 Variant 类型的变量就是个整数，我们可以直接用一个整数类型的变量去获取它的值。

有了上述代码，只要我们点按钮开始执行 Python 的代码，Python 那边自动从 Delphi 的代码获得需要的参数，Delphi 这边就自动获得了执行结果。

##### 复杂类型的数据怎么办
Python 里面有一些比较复杂的数据类型，比如元组，列表，字典，对象，等等。这些在 Python4Delphi 这个框架里面，都有对应的 Delphi 类型。因此不是问题。具体的还需要深入学习，且听下回分解（现在我也不会）。

##### 是否可以用 Delphi 写代码让 Python 来调用
Python 的代码调用别的代码库，就是引入模块的方法。在 Delphi 这边，可以把一些代码（函数也好，类也好），通过 PythonModule1 这个控件，设置其 ModuleName 属性（字符串），Python 的代码就可以通过 import ModuleName 的方式，去执行 Delphi 这边实现的函数了。具体操作细节，下回分解。

#### One more thing
调试中我遇到的一个问题：执行完 Python 代码后，如果我修改了被调用的那个库 MyLib.py 里面的代码，再次执行 Python 的代码，执行结果却是修改之前的。当然，重启这个 Delphi 的 EXE 程序可以解决这个问题。但如果程序一直保持运行，仅仅是修改 Python 的代码，然后想看到修改后的结果，怎么办？

最简单的办法，是把和 Python 调用有关的那一堆 Python4Delphi 的控件，都放进一个 DataModule 里面去。修改 Python 代码后，只要释放这个 DataModule 再重新创建，就相当于 Reset 整个 Python 运行时环境了。

## 结论
使用 Delphi 能够调用 Python 的代码，可以：

1. 在 Delphi 程序本身需要动态的 Script 的地方，可以用得上这个功能。这样做可以在程序运行时通过修改 Python script 的字符串代码，动态修改一些功能。

2. 更重要的是，可以用 Delphi 开发程序的便捷性，结合 Python 的很多开源库，做出更强大功能的程序，而无需用 Pascal 代码重新发明轮子。

凌晨3点2分，打完收工。