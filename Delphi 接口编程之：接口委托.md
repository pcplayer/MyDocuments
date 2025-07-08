# Delphi 接口编程之：接口委托

本文假设你懂得 Delphi 的接口编程（关键词：Interface）

假设我们定义了一个接口 IA，它有一个或几个方法。这里假设这个接口定义只有一个方法。

然后，我们有两个不同的类，都实现了这个 IA 接口。但对其方法的实现，有点不一样。假设这两个类分别是 X 和 Y。也就是说，调用 X 的接口方法，和调用 Y 的同样接口的同名方法，得到的结果不同。这类似于对象继承的多态所要达到的目标。

然后，我们有一个类 Z，它也实现了接口 IA。但是，它不用真正地实现一次 IA 的方法所需要的代码，它仅仅是委托给 X 或者 Y 就可以了。为什么要这样做？理由大概有几点：

1. 这个 Z 除了需要实现接口 IA 所提供的方法，还需要提供其它一些方法。

2. 这个 Z 作为一些对象或者接口的封装，对外隐藏代码的复杂性。



如果这个 Z 是采用委托的方式，将自己实现的接口，委托给 X 或 Y，则在运行期，是可以动态切换到。这样一来，就可以根据不同的情况，调用相同的接口方法，获得不同的运行结果，实现不同的功能。这样的代码，可以避免一堆的 if.. else，也更方便将来增加新的功能，而无需去更改 Z 的代码。

## 接口委托的实现代码：

~~~
unit UMyIntf;
{------------------------------------------------------------------------
  接口委托及运行期动态更换的演示代码。
  pcplayer 2017-5-31
------------------------------------------------------------------------}
interface
 
uses System.SysUtils, System.Variants, System.Classes;
 
type
  IMyIntf = interface
    ['{9FC4FAA4-04F1-4799-9C30-BB444214E292}']
    function Hello(const S: string): string;
  end;
 
  IHelloIndex = interface
    ['{C994DC32-D4CB-44F3-8A4D-7E3A6798A7DC}']
    procedure SetMyIndex(const Value: Integer);
    property MyIndex: Integer write SetMyIndex;
  end;
 
 
  TEngHello = class(TComponent, IMyIntf)
  public
    function Hello(const S: string): string;
  end;
 
  TChinaHello = class(TComponent, IMyIntf)
  public
    function Hello(const S: string): string;
  end;
 
  TMyHello = class(TComponent, IMyIntf, IHelloIndex)
  private
    FMyIntf: IMyIntf;
    FMyIndex: Integer;
 
    FEngHello: TEngHello;
    FChinaHello: TChinaHello;
    procedure SetMyIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
 
    property MyIntf: IMyIntf read FMyIntf implements IMyIntf;
    property MyIndex: Integer write SetMyIndex;
  end;
 
const
  IID_IHelloIndex: TGUID = '{C994DC32-D4CB-44F3-8A4D-7E3A6798A7DC}';
  IID_IMyIntf: TGUID = '{9FC4FAA4-04F1-4799-9C30-BB444214E292}';
 
implementation
 
{ TEngHello }
 
function TEngHello.Hello(const S: string): string;
begin
  Result := 'Hello, ' + S;
end;
 
{ TChinaHello }
 
function TChinaHello.Hello(const S: string): string;
begin
  Result := '你好, ' + S + ', 吃饭了吗？';
end;
 
{ TMyHello }
 
constructor TMyHello.Create(AOwner: TComponent);
begin
  inherited;
 
  FEngHello := TEngHello.Create(AOwner);
  FEngHello.Name := 'EngHello';
 
  FChinaHello := TChinaHello.Create(AOwner);
  FChinaHello.Name := 'ChinaHello';
 
  FMyIntf := FEngHello as IMyIntf;
end;
 
procedure TMyHello.SetMyIndex(const Value: Integer);
begin
  FMyIndex := Value;
  case value of
    0: FMyIntf := FEngHello as IMyIntf;
    1: FMyIntf := FChinaHello as IMyIntf;
  end;
end;
 
end.
~~~

## 使用上述代码的演示代码
~~~
unit UFmMain;
{------------------------------------------------------------------------------
  接口委托，以及接口的切换。

  本 Demo 程序演示了如何通过接口委托的方式来让一个类实现其它类已经实现的接口。
  同时也演示了如何通过对委托的接口的切换，来达到运行期动态改变实现代码的目的。

  这样的架构，对于比较大的程序，未来可能会有新的更改，就非常方便了。基本上添加一个新的类对于接口做新的实现，主代码基本无需修改就能完成。
  极大减少代码的修改维护工作量。

  UMyIntf.pas 里面，有一个 IMyIntf 接口。这个接口有两个类的不同实现。

  TMyHello 类也实现这个接口。但它不是真的去实现，而是委托给它内部的两个真正实现了这个接口的函数的对象（分别是两个不同的类）。

  对于 TMyHello 的对象实例来说，调用它的 IMyIntf 接口，究竟是最终真正调用到哪个实现，由它的 MyIndex 来决定。
  程序在运行期，可以动态切换 MyIndex 来决定采用哪个接口实现。这样达到了动态转换具体实现的目的（类似对象多态），而无需一堆 if..else..

  备注：从 TMyHello 获得的 IMyIntf 接口，实际上是由它内部的某个对象实现的，因此，
  如果从这个 IMyIntf 接口去做接口转换，转换为 TMyHello 自己实现的 IHelloIndex 是会失败的。
  因为实现 IMyIntf 接口的类，也是从 TComponent 继承的，因此将这个接口转换为 TComponent 实现的 IInterfaceComponentReference 是成功的。
  然后通过调用 IInterfaceComponentReference 的方法获得这个接口对应的对象实例，然后你可以看到这个对象究竟是哪个。

  pcplayer 2017-5-31
------------------------------------------------------------------------------}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UMyIntf, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFmMain = class(TForm)
    BtnCreate: TButton;
    BtnHello: TButton;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    procedure BtnCreateClick(Sender: TObject);
    procedure BtnHelloClick(Sender: TObject);
  private
    { Private declarations }
    FMyHello: IHelloIndex;
  public
    { Public declarations }
  end;

var
  FmMain: TFmMain;

implementation

{$R *.dfm}

procedure TFmMain.BtnCreateClick(Sender: TObject);
var
  O: TMyHello;
begin
  O := TMyHello.Create(Self);
  O.Name := 'MyHello';
  FMyHello := O as IHelloIndex;
end;

procedure TFmMain.BtnHelloClick(Sender: TObject);
var
  Intf: IMyIntf;
  Obj: TComponent;
begin
  if not Assigned(FMyHello) then Exit;

  Obj := (FMyHello as IInterfaceComponentReference).GetComponent;  //通过接口，获得对象实例。这个是 TComponent 实现的 IInterfaceComponentReference 的方法
  Memo1.Lines.Add(Obj.Name);

  FMyHello.MyIndex := RadioGroup1.ItemIndex;
  Intf := FMyHello as IMyIntf;

  Memo1.Lines.Add('当前对象名 = ' + (Intf as IInterfaceComponentReference).GetComponent.Name);
  Memo1.Lines.Add(Intf.Hello(Edit1.Text));
  Memo1.Lines.Add('');
end;

end.
~~~