Python4Delphi: 使用Delphi代码调用Python代码里面的类

# 问题：
假设有一个 Python 的开源代码，里面有一个类，比较复杂，它有比较复杂的算法，刚好我们在写 Delphi 程序的时候需要。如何用 Delphi 直接调用这个类，使用它的算法？

# Python 代码如下：
~~~
class MyPerson:
    def __init__(self, FirstName, LastName):
        self.MyFirstName = FirstName
        self.MyLastName = LastName
    def GetMyFirstName(self):
        print(self.MyFirstName)
        return (self.MyFirstName + "  - Hello")
~~~
上面的代码，定义了一个 Python 的一个类。这个类有两个方法：

一个是 init；

另一个是 GetMyFirstName；

注意，按照 Python 语法，一个类的方法，不管有没有参数，必须至少有一个参数：self；

注意，Python 的类，init 方法就是创建这个类的对象时的默认方法，类似 Delphi 的类方法 Create.

Delphi 这边的代码

1. 首先，我们新建一个 Delphi 的 VCL 程序；

2. 拖两个控件到界面上：PythonEngine1 和 PythonGUIInputOutput1；控件属性设置请看前面的文章。

3. 界面用于操作和显示的控件，Button1, Memo1, Memo2 拖到界面上。

4. 在 Memo2.Lines 里面填入上面的 Python 的代码。这个 Delphi 程序的设计就完成了。开始写代码。
~~~
uses
  VarPyth; //用这个获取 Python 的 MainModule
 
{$R *.dfm}
 
procedure TForm3.Button1Click(Sender: TObject);
var
  PyObj: Variant;
  AName: Variant;
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
  PyObj := MainModule.MyPerson('Jack', 'Chen'); //创建 Python 对象。这里是调用了 Python 类的 init 方法
  AName := 'abc';
  AName := PyObj.GetMyFirstName();
  Memo1.Lines.Add(VarToStr(AName));      //VarToStr 需要引用 System.Variants 单元。
end;
~~~

## 上面 Delphi 的代码需要注意：
1. AName 如果声明成 string，程序可以编译通过。运行时，Python 模块会出错。这里必须声明成 Variant；

2. 创建 Python 对象的方式，和 Python 代码的方式相同。直接调用 Python 类的名称，加上其 init 方法里面的参数。这就是 PyObj := MainModule.MyPerson() 这行代码的意思，创建一个 Python 的对象并返回给 Delphi 的一个 Variant 的变量。这里的 MainModule 在上一篇文章里面有解释。

3. 调用 Python 这个类的方法，因为无参数，我一开始没有加括号 --  Delphi 的无参数方法可以不加括号，运行出错。加上括号运行正确。结论：在 Delphi 里面调用 Python 对象的方法，方法有没有参数，都要加上括号。

# 总结：
1. 首先调用 PythonEngine1.ExecStrings 加载运行有这个 Python 类的 Python 代码；

2. 使用 MainModule.类名()  的方式，直接就创建了 Python 类。在 Delphi 里面，Python 类创建的对象保存到一个 Variant 变量里面。

3. 有了对象，就可以用【对象.方法()】的方式调用这个对象的方法了。这里需要注意，方法有没有参数都要加上括号。
