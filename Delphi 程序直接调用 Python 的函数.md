Delphi 程序直接调用 Python 的函数
在本系列的第一篇博客文章：Delphi 程序员初学 Python 笔记-CSDN博客

在这篇文章里面，我提到，一个 Python 的函数，我用 Delphi 无法直接调用它，所以我对它做了一个 Python 的包装。原因是，那是我第一次学习使用 Python4Delphi 这个控件。那时候要执行 Python 代码，我只会使用 PythonEngine1.ExecStrings(Python代码) 这样的方式。这样做，就是在 Delphi 的程序里面，加载了 Python 的环境，并在这个环境里面去执行了代码里面的可执行的部分。但如果代码里面有函数，而这个函数并没有 Python 代码去调用它，Python 环境里面也无法执行这个函数。

因此，如果 Delphi 的代码想要调用这个 Python 的函数，就需要使用一个 Python 的代码去调用这个函数，而 Delphi 仅仅是需要从这个调用函数的代码里面获得计算结果的返回值。

但是，在 Python4Delphi 的框架里面，使用 Delphi 的代码是可以直接去调用一个 Python 的函数的。为此我自己写了一个程序做一下测试。

### 测试用的 Python 代码：
~~~
def DoAddParams(a, b):
    return a + b + 3
~~~

### 调用这个 Python 代码的 Delphi 代码：
~~~
uses VarPyth;
 
procedure TForm2.Button1Click(Sender: TObject);
var
  OldControlWord: Word;
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
end;
 
procedure TForm2.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  i := MainModule.DoAddParams(1, 2);
 
  Memo1.Lines.Add('DoAddParams = ' + i.ToString);
end;
~~~

### 上述 Delphi 代码的解释：
1. 首先，这个 Delphi 程序，仍然需要使用 PythonEngine1 这个控件，加载执行 Python 代码。其实就是在这个 Delphi 程序内部创建 Python 运行环境并加载上面的 Python 的函数的代码。但那个函数的代码，并没有包含函数被调用执行的代码。

2. 然后，当鼠标点 Button2 的时候，Delphi 代码执行的 MainModule.DoAddParams 就是直接调用了 Python 的函数 DoAddParams。这里的逻辑是，Delphi 程序需要事先加载 Python 运行环境以及这个 Python 函数的代码，然后才能调用它。

3. 这里最有意思的是，一般的 Delphi 程序，一个函数，或者方法，都要有声明有实现。即便是本程序没有实现的第三方库实现的函数，比如 DLL 里面的函数，Delphi 代码里面也要事先声明函数，才能使用这个函数。但对于 Python 的函数，这里没有事先的声明，直接调用。而且，因为这个 MainModule 是一个代表 Python 的 MainModule 的变量，这个变量类型是一个 Variant 类型，因此，在它后面直接加上【点】然后跟函数名，Delphi 编译器也不会报错说没有这个函数。这个大概就是类似于后期动态绑定的方式，不需要前期事先声明就能用。

**另外：**
上述测试代码很简单，仅仅是测试使用 Delphi 代码直接调用 Python 的函数。因此，这里的数据类型很简单，就是整数。如果是其它的复杂的数据类型，就会涉及到 Delphi 的数据类型和 Python 的数据类型的互相转换的代码了。如何做类型转换？下回分解。

### 又及：
增补：2024-3-20；

这篇文章底下有人问那个 MainModule 是哪里来的。

 这里的 MainModule 是对应 Python 代码的 MainModule 的全局函数，声明在 VarPyth.pas 里面。

以下是 Python4Delphi 里面的 Delphi 代码：
~~~
function MainModule : Variant;
~~~
因此，需要使用它的地方，引用 VarPyth.pas 单元后，就可以直接使用它了。