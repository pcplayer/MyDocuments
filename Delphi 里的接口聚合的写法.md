# Delphi 里的接口聚合的写法

本程序演示接口委托聚合：
  TMyClass 拥有并实现了 IMyTask 接口
  TMyPlan 类拥有 IMyTask 接口，但没有去实现这个接口里的 SayHello 方法，而是通过
  property MyTask: IMyTask read FMyClass implements IMyTask; 语句，将这个实现委托给内部的 FMyClass 接口实现。
 
  传统的面向类的代码，当做一个包装类，包装多个子类在里面的时候，要在包装类的 public 里
  再实现一次子类里的方法，在实现的方法里再调用子类的方法。这种办法也有人称为【委托】。
 
  如果采用接口委托，则不用重复写代码，直接写一句：
  property MyTask: IMyTask read FMyClass implements IMyTask; 则包装类
  就不用在 public 里再次重复实现一次接口里的函数方法等等了。这种办法也有人称为【聚合】

## 先看看类的写法

~~~
  TMyClassA = class
  public
    procedure SayHello(const S: string);
  end;
  
  TMyClssB = class
  public
    procedure Walk;
  end;
  
  TMyClassX = class
  private
    Fa: TMyCalssA;
	Fb: TMyClassB;
  public
    procedure SayHello(const S: string);
	procedure Walk;
  end;
  
  
procedure TMyClassX.SayHello(const S: string);
begin
  Fa.SayHello(S);
end;

procedure TMyClassX.Walk;
begin
  Fb.Walk
end;
~~~

## 再看看采用接口聚合的写法

~~~
unit Unit1;
 
{-----------------------------------------------------------------------------
  本程序演示接口委托聚合：
  TMyClass 拥有并实现了 IMyTask 接口
  TMyPlan 类拥有 IMyTask 接口，但没有去实现这个接口里的 SayHell 方法，而是通过
  property MyTask: IMyTask read FMyClass implements IMyTask; 语句，将这个实现委托给内部的 FMyClass 接口实现。
 
  传统的面向类的代码，当做一个包装类，包装多个子类在里面的时候，要在包装类的 public 里
  再实现一次子类里的方法，在实现的方法里再调用子类的方法。这种办法也有人称为【委托】。
 
  如果采用接口委托，则不用重复写代码，直接写一句：
  property MyTask: IMyTask read FMyClass implements IMyTask; 则包装类
  就不用在 public 里再次重复实现一次接口里的函数方法等等了。这种办法也有人称为【聚合】
  以下代码测试通过。
 
  pcplayer 2011-10-23 星期日。
-----------------------------------------------------------------------------}
 
interface
 
uses
 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
 
  Dialogs, StdCtrls;
 
type
 
  IMyTask = Interface
 
    ['{1B9D204E-1662-4280-A8E6-D7F518A425F5}']
 
    function SayHello: string;
  End;
  
  IMyTest = interface
    procedure Walk;
  end;
 
  TMyClass = class(TInterfacedObject, IMyTask)
  private
  public
    function SayHello: string;
  end;
  
  TMyCalssB = class(TInterfacedObject, IMyTest)
  public
    procedure Walk;
  end;
 
 
  TMyPlan = class(TInterfacedObject, IMyTask, IMyTest)
  private
    FMyClass: IMyTask;
    FMyClassB: IMyTest;
  public
 
    constructor Create;
 
    property MyTask: IMyTask read FMyClass implements IMyTask;
	property MyTest: IMyTest read FMyClassB implements IMyTest;
  end;
 
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
 
  public
 
  end;
 
var
 
  Form1: TForm1;
 
implementation
 
 
{$R *.dfm}
 
{ TMyClass }
 
 
function TMyClass.SayHello: string;
begin
  Result := 'Hello';
end;

{TMyClassB} 

procedure TMyCalssB.Walk;
begin
  ShowMessage('Walk');
end;
 
{ TMyPlan }
 
constructor TMyPlan.Create;
begin
  inherited;
 
  FMyClass := TMyClass.Create as IMyTask;
end;
 
procedure TForm1.Button1Click(Sender: TObject);
var
  MyPlan: IMyTask;
begin
  MyPlan := TMyPlan.Create;
  Label1.Caption := MyPlan.SayHello;
  MyPlan := nil;
end;
 
end.
~~~

### 说明
在基于类的写法里面，假设 TMyClassA 和 TMyClassB 有很多方法需要在 TMyClassX 里面出现，则 TMyClassX 就需要写很多方法的实现代码。当然了，TMyClassX 也可以直接暴露 TMyClassA 和 TMyClassB 两个类的对象实例，避免写实现方法，但这样也不够灵活，比如相同的方法但我不想用 TMyClassA 和 TMyClassB 的实现，就需要改代码了。
相同的情况下，在基于接口聚合的方式下，TMyPlan 就只需要写 2 行代码。把两个不同的接口聚合到一起。至于接口方法需要不同的实现，则直接在类工厂里面把实现的类根据程序运行的不同情况换掉就好，不用更改代码，在运行期也可以修改。
